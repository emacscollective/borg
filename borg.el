;;; borg.el --- assimilate Emacs packages as Git submodules  -*- lexical-binding: t -*-

;; Copyright (C) 2016  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://gitlab.com/emacscollective/borg
;; Keywords: tools

;; This file contains code from GNU Emacs, which is
;; Copyright (C) 1976-2016 Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see https://www.gnu.org/licenses.

;;; Commentary:

;; Assimilate Emacs packages as Git submodules.

;;; Code:

(require 'autoload)
(require 'bytecomp)
(require 'cl-lib)
(require 'info)

(defconst borg-drone-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (or load-file-name buffer-file-name))))
  "Directory beneath with drone submodules are placed.")

(defconst borg-user-emacs-directory
  (file-name-directory (directory-file-name borg-drone-directory))
  "Directory beneath which additional per-user Emacs-specific files are placed.

The value of this variable is usually the same as that of
`user-emacs-directory', except when Emacs is started with
`emacs -q -l /path/to/init.el'.")

(defun borg-repository (drone)
  "Return the top-level of the working tree of the submodule named DRONE."
  (expand-file-name drone borg-drone-directory))

(defun borg-get (drone variable &optional all)
  "Return the value of `submodule.DRONE.VARIABLE' in `~/.emacs.d/.gitmodules'.
If optional ALL is non-nil, then return all values as a list."
  (ignore-errors
    (apply #'process-lines "git" "config" "--file"
           (expand-file-name ".gitmodules" borg-user-emacs-directory)
           (nconc (and all (list "--get-all"))
                  (list (concat "submodule." drone "." variable))))))

(defun borg-get-all (drone variable)
  "Return all values of `submodule.DRONE.VARIABLE' in `~/.emacs.d/.gitmodules'.
Return the values as a list."
  (borg-get drone variable t))

(defun borg-load-path (drone)
  "Return the `load-path' for the drone named DRONE."
  (let ((repo (borg-repository drone))
        (path (borg-get-all drone "load-path")))
    (if  path
        (mapcar (lambda (d) (expand-file-name d repo)) path)
      (let ((lisp (expand-file-name "lisp" repo)))
        (list (if (file-exists-p lisp) lisp repo))))))

(defun borg-info-path (drone &optional setup)
  "Return the `Info-directory-list' for the drone named DRONE.

If optional SETUP is non-nil, then return a list of directories
containing texinfo and/or info files.  Otherwise return a list of
directories containing a file named \"dir\"."
  (let ((repo (borg-repository drone))
        (path (borg-get-all drone "info-path")))
    (cl-mapcan (if setup
                   (lambda (d)
                     (setq d (file-name-as-directory d))
                     (when (directory-files d t "\\.\\(texi\\|info\\)\\'" t)
                       (list d)))
                 (lambda (d)
                   (setq d (file-name-as-directory d))
                   (when (file-exists-p (expand-file-name "dir" d))
                     (list d))))
               (if path
                   (mapcar (lambda (d) (expand-file-name d repo)) path)
                 (list repo)))))

(defun borg-drones ()
  "Return a list of all assimilated drones."
  ;; This is more efficient than using `git submodule'.
  (cl-mapcan
   (lambda (line)
     (and (string-match "\\`submodule\\.[^./]+\\.path=lib/\\(.+\\)" line)
          (list (match-string 1 line))))
   (sort (process-lines "git" "config" "--list" "--file"
                        (expand-file-name ".gitmodules"
                                          borg-user-emacs-directory))
         #'string<)))

(defmacro borg-silencio (regexp &rest body)
  "Execute the forms in BODY while silencing messages that don't match REGEXP."
  (declare (indent 1))
  `(let ((msg (symbol-function 'message)))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (unless (string-match-p ,regexp format-string)
                    (apply msg format-string args)))))
       ,@body)))

(defun borg-initialize ()
  "Initialize all assimilated drones.

For each drone add the appropriate directories to the `load-path'
and `Info-directory-alist', and load the autoloads file if it
exits.

If the value of a Git variable named `submodule.DRONE.disabled'
is `true', then the drone named DRONE is skipped."
  (info-initialize)
  (let ((start (current-time))
        (drones (borg-drones))
        (skipped nil))
    (dolist (drone drones)
      (if (equal (borg-get drone "disabled") "true")
          (push drone skipped)
        (dolist (dir (borg-load-path drone))
          (let (file)
            (cond ((and (file-exists-p
                         (setq file (expand-file-name
                                     (concat drone "-autoloads.el") dir)))
                        (with-demoted-errors "Error loading autoloads: %s"
                          (load file nil t))))
                  ((and (file-exists-p
                         (setq file (expand-file-name
                                     (concat drone "-loaddefs.el") dir)))
                        (with-demoted-errors "Error loading autoloads: %s"
                          (add-to-list 'load-path dir) ; for `org'
                          (load file nil t))))
                  (t (push dir load-path)))))
        (dolist (dir (borg-info-path drone))
          (push  dir Info-directory-list))))
    (message "Initializing drones...done (%s drones in %.3fs%s)"
             (length drones)
             (float-time (time-subtract (current-time) start))
             (if skipped
                 (format ", %d skipped" (length skipped))
               ""))))

(defun borg-batch-rebuild (&optional quick)
  "Rebuild all assimilated drones.

Drones are rebuild in alphabetic order, except that Org is build
first.  `init.el' and `USER-REAL-LOGIN-NAME.el' are also rebuild.

This function is to be used only with `--batch'.

When optional QUICK is non-nil, then do not build drones for
which `gitmodule.DRONE.build' is set, assuming those are the
drones that take longer to be build."
  (unless noninteractive
    (error "borg-batch-rebuild is to be used only with --batch"))
  (let ((drones (borg-drones)))
    (when (member "org" drones)
      ;; `org-loaddefs.el' has to exist when compiling a library
      ;; which depends on `org', else we get warnings about that
      ;; not being so, and other more confusing warnings too.
      (setq drones (cons "org" (delete "org" drones))))
    (dolist (drone drones)
      (unless (and quick (borg-get-all drone "build-step"))
        (dolist (d (borg-load-path drone))
          (dolist (f (directory-files
                      d t "\\(\\.elc\\|-autoloads\\.el\\|-loaddefs\\.el\\)\\'"
                      t))
            (ignore-errors (delete-file f))))))
    (dolist (drone drones)
      (message "\n--- [%s] ---\n" drone)
      (if (and quick (borg-get-all drone "build-step"))
          (message "Skipping...")
        (borg-build drone))))
  (borg-batch-rebuild-init))

(defun borg-batch-rebuild-init ()
  "Rebuild `init.el' and `USER-REAL-LOGIN-NAME.el'.

This function is to be used only with `--batch'."
  (unless noninteractive
    (error "borg-batch-recompile-init is to be used only with --batch"))
  (borg-silencio "\\`%s\\.\\.\\.\\(done\\)?" ; silence use-package
    (let ((default-directory borg-user-emacs-directory))
      (message "\n--- [init.el] ---\n")
      (load-file "init.el")
      (byte-recompile-file (expand-file-name "init.el") t 0)
      (let ((f (concat (user-real-login-name) ".el")))
        (when (file-exists-p f)
          (message "\n--- [%s] ---\n" f)
          (byte-recompile-file (expand-file-name f) t 0))))))

(defun borg-build (drone)
  "Build the drone named DRONE."
  (interactive (list (completing-read "Build drone: " (borg-drones) nil t)))
  (let ((default-directory (borg-repository drone))
        (build (borg-get-all drone "build-step")))
    (if  build
        (dolist (cmd build)
          (message "  Running '%s'..." cmd)
          (if (string-match-p "\\`(" cmd)
              (eval (read cmd))
            (shell-command cmd))
          (message "  Running '%s'...done" cmd))
      (let ((path (mapcar #'file-name-as-directory (borg-load-path drone))))
        (if noninteractive
            (progn (borg-update-autoloads drone path)
                   (borg-byte-compile drone path)
                   (borg-makeinfo drone))
          (let ((process-connection-type nil))
            (start-process
             (format "Build %s" drone)
             (generate-new-buffer (format "*Build %s*" drone))
             (expand-file-name invocation-name invocation-directory)
             "--batch" "-Q"
             "-L" (borg-repository "borg")
             "--eval" "(require 'borg)"
             "--eval" "(borg-initialize)"
             "--eval" (format "(borg-build %S)" drone))))))))

(defconst borg-autoload-format "\
;;;\
 %s --- automatically extracted autoloads
;;
;;;\
 Code:
\(add-to-list 'load-path (directory-file-name \
\(or (file-name-directory #$) (car load-path))))
\
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;;\
 %s ends here\n")

(defun borg-update-autoloads (drone path)
  "Update autoload files for the drone named DRONE in the directories in PATH."
  (let ((autoload-excludes
         (nconc (mapcar #'expand-file-name
                        (borg-get-all drone "no-byte-compile"))
                (cl-mapcan
                 (lambda (dir)
                   (list (expand-file-name (concat drone "-pkg.el") dir)
                         (expand-file-name (concat drone "-test.el") dir)
                         (expand-file-name (concat drone "-tests.el") dir)))
                 path)
                autoload-excludes))
        (generated-autoload-file
         (expand-file-name (format "%s-autoloads.el" drone) (car path))))
    (message " Creating %s..." generated-autoload-file)
    (when (file-exists-p generated-autoload-file)
      (delete-file generated-autoload-file t))
    (let* ((backup-inhibited t)
           (version-control 'never)
           (noninteractive t)
           (filename (file-name-nondirectory generated-autoload-file)))
      (write-region (format borg-autoload-format filename filename)
                    nil generated-autoload-file nil 'silent)
      (apply #'update-directory-autoloads path))
    (let ((buf (find-buffer-visiting generated-autoload-file)))
      (when buf
        (kill-buffer buf)))))

(defun borg-byte-compile (drone path)
  "Compile libraries for the drone named DRONE in the directories in PATH."
  (let ((exclude (borg-get-all drone "no-byte-compile"))
        (topdir (borg-repository drone)))
    (dolist (dir path)
      (with-current-buffer (get-buffer-create byte-compile-log-buffer)
        (setq default-directory (expand-file-name dir topdir))
        (unless (eq major-mode 'compilation-mode)
          (compilation-mode))
        (let ((skip-count 0)
              (fail-count 0)
              (file-count 0))
          (displaying-byte-compile-warnings
           (dolist (file (directory-files dir t emacs-lisp-file-regexp))
             (let ((name (file-name-nondirectory file)))
               (when (and (file-regular-p  file)
                          (file-readable-p file)
                          (not (auto-save-file-name-p file))
                          (not (string-match "\\`\\." name))
                          (not (string-match "-autoloads.el\\'" name))
                          (not (string-equal dir-locals-file name)))
                 (cl-incf
                  (if (or (string-match "-pkg.el\\'" name)
                          (string-match "-tests?.el\\'" name)
                          (member name exclude))
                      (progn (message " Skipping %s...skipped" file)
                             skip-count)
                    (pcase (byte-recompile-file file t 0)
                      ('no-byte-compile
                          (message "Compiling %s...skipped" file)
                          skip-count)
                      ('t file-count)
                      (_  fail-count))))))))
          (message "Done (Total of %d file%s compiled%s%s)"
                   file-count (if (= file-count 1) "" "s")
                   (if (> fail-count 0) (format ", %d failed"  fail-count) "")
                   (if (> skip-count 0) (format ", %d skipped" skip-count) "")
                   )))))) ; o, the horror

(defun borg-makeinfo (drone)
  "Generate Info manuals and the Info index for the drone named DRONE."
  (dolist (default-directory (borg-info-path drone t))
    (let ((exclude (borg-get-all drone "no-makeinfo")))
      (dolist (texi (directory-files default-directory nil "\\.texi\\'"))
        (let ((info (concat (file-name-sans-extension texi) ".info")))
          (when (and (not (member texi exclude))
                     (or (not (file-exists-p info))
                         (= (process-file "git" nil nil nil
                                          "ls-files" "--error-unmatch" info)
                            1)))
            (let ((cmd (format "makeinfo --no-split %s -o %s" texi info)))
              (message "  Running '%s'..." cmd)
              (borg-silencio "\\`(Shell command succeeded with %s)\\'"
                (shell-command cmd))
              (message "  Running '%s'...done" cmd))))))
    (dolist (info (directory-files default-directory nil "\\.info\\'"))
      (let ((cmd (format "install-info %s --dir=dir" info)))
        (message "  Running '%s'..." cmd)
        (borg-silencio "\\`(Shell command succeeded with %s)\\'"
          (shell-command cmd))
        (message "  Running '%s'...done" cmd)))))

(eval-when-compile
  (require 'epkg nil t))
(declare-function eieio-oref        "eieio-core" (obj slot))
(declare-function epkg                    "epkg" (name))
(declare-function epkgs                   "epkg" (&optional select predicates))
(declare-function epkg-git-package-p      "epkg" (obj))
(declare-function epkg-github-package-p   "epkg" (obj))
(declare-function epkg-gitlab-package-p   "epkg" (obj))
(declare-function epkg-orphaned-package-p "epkg" (obj))
(declare-function epkg-read-package       "epkg" (prompt &optional default))

(defun borg-assimilate (name url &optional force)
  "Assimilate the package named NAME from URL.
With a prefix argument pass \"--force\" to \"git submodule\"."
  (interactive
   (if (require 'epkg nil t)
       (let* ((packages (epkgs 'name))
              (name (completing-read "Assimilate package: "
                                     packages nil nil nil
                                     'epkg-package-history)))
         (list name
               (let ((pkg (epkg name)))
                 (if pkg
                     (if (or (epkg-git-package-p pkg)
                             (epkg-github-package-p pkg)
                             (epkg-orphaned-package-p pkg)
                             (epkg-gitlab-package-p pkg))
                         (eieio-oref pkg 'url)
                       (eieio-oref pkg 'mirror-url))
                   (read-string "Url: ")))
               current-prefix-arg))
     (list (read-string "Assimilate package: ")
           (read-string "Url: ")
           current-prefix-arg)))
  (message "Assimilating %s..." name)
  (let* ((default-directory borg-user-emacs-directory)
         (args (list "--name" name url
                     (file-relative-name (borg-repository name)))))
    (apply #'borg--call-git name "submodule" "add"
           (if force (cons "--force" args) args))
    (borg--sort-submodule-sections ".gitmodules")
    (borg--call-git name "add" ".gitmodules"))
  (borg-build name)
  (when (and (derived-mode-p 'magit-mode)
             (fboundp 'magit-refresh))
    (magit-refresh))
  (message "Assimilating %s...done" name))

(defun borg-uninstall (drone)
  "Uninstall the drone named DRONE."
  (interactive (list (completing-read "Uninstall drone: " (borg-drones) nil t)))
  (let ((default-directory borg-user-emacs-directory))
    (borg--call-git "rm" (borg-repository drone))))

(defun borg--call-git (drone &rest args)
  (let ((process-connection-type nil)
        (buffer (generate-new-buffer (format " *Borg Git %s*" drone))))
    (if (eq (apply #'call-process "git" nil buffer nil args) 0)
        (kill-buffer buffer)
      (pop-to-buffer buffer)
      (error "Git failed"))))

(defun borg--sort-submodule-sections (file)
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (revert-buffer t t)
    (goto-char (point-min))
    (re-search-forward "^\\[submodule")
    (sort-regexp-fields
     nil "^\\[submodule \"\\([^\"]+\\)\"][\s\t]*\n\\([\s\t].*\n\\)+"
     "\\1" (line-beginning-position) (point-max))
    (save-buffer)))

(provide 'borg)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; borg.el ends here
