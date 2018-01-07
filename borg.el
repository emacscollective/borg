;;; borg.el --- assimilate Emacs packages as Git submodules  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/emacscollective/borg
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
(declare-function format-spec      "format-spec" (format specification))
(declare-function magit-get             "magit-git" (&rest keys))
(declare-function magit-get-some-remote "magit-git" (&optional branch))

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

(defconst borg-gitmodules-file
  (expand-file-name ".gitmodules" borg-user-emacs-directory)
  "The \".gitmodules\" file of the drone repository.")

;;; Variables

(defvar borg-byte-compile-recursively nil
  "Whether to compile recursively.

You probably don't want this because there are more packages that
have random crap in subdirectories than there are packages that
put actual libraries into subdirectories.")

(defvar borg-build-shell-command nil
  "Optional command used to run shell command build steps.
This variable is documented in the manual (which see).")

;;; Utilities

(defun borg-worktree (clone)
  "Return the top-level of the working tree of the package named CLONE."
  (expand-file-name (file-name-as-directory clone) borg-drone-directory))

(defun borg-gitdir (clone)
  "Return the Git directory of the package named CLONE.

Always return `<borg-user-emacs-directory>/.git/modules/<CLONE>',
even when this repository's Git directory is actually located
inside the working tree."
  (let* ((default-directory borg-user-emacs-directory)
         (super (ignore-errors
                  (car (process-lines "git" "rev-parse" "--git-dir")))))
    (if super
        (expand-file-name (concat super "/modules/" clone "/"))
      (error "Cannot locate super-repository"))))

(defvar borg--gitmodule-cache nil)

(defun borg-get (clone variable &optional all)
  "Return the value of `submodule.CLONE.VARIABLE' in `~/.emacs.d/.gitmodules'.
If optional ALL is non-nil, then return all values as a list."
  (if borg--gitmodule-cache
      (let ((values (plist-get (cdr (assoc clone borg--gitmodule-cache))
                               (intern variable))))
        (if all values (car values)))
    (ignore-errors
      ;; If the variable has no value then the exit code is non-zero,
      ;; but that isn't an error as far as we are concerned.
      (apply #'process-lines "git" "config" "--file" borg-gitmodules-file
             (nconc (and all (list "--get-all"))
                    (list (concat "submodule." clone "." variable)))))))

(defun borg-get-all (clone variable)
  "Return all values of `submodule.CLONE.VARIABLE' in `~/.emacs.d/.gitmodules'.
Return the values as a list."
  (borg-get clone variable t))

(defun borg-load-path (clone)
  "Return the `load-path' for the clone named CLONE."
  (let ((repo (borg-worktree clone))
        (path (borg-get-all clone "load-path")))
    (if  path
        (mapcar (lambda (d) (expand-file-name d repo)) path)
      (let ((lisp (expand-file-name "lisp" repo)))
        (list (if (file-exists-p lisp) lisp repo))))))

(defun borg-info-path (clone &optional setup)
  "Return the `Info-directory-list' for the clone named CLONE.

If optional SETUP is non-nil, then return a list of directories
containing texinfo and/or info files.  Otherwise return a list of
directories containing a file named \"dir\"."
  (let ((repo (borg-worktree clone))
        (path (borg-get-all clone "info-path")))
    (cl-mapcan (if setup
                   (lambda (d)
                     (setq d (file-name-as-directory d))
                     (when (directory-files d t "\\.\\(texi\\(nfo\\)?\\|info\\)\\'" t)
                       (list d)))
                 (lambda (d)
                   (setq d (file-name-as-directory d))
                   (when (file-exists-p (expand-file-name "dir" d))
                     (list d))))
               (if path
                   (mapcar (lambda (d) (expand-file-name d repo)) path)
                 (list repo)))))

(defvar borg--multi-value-variables
  '(build-step load-path no-byte-compile info-path)
  "List of submodule variables which can have multiple values.")

(defun borg-drones (&optional include-variables)
  "Return a list of all assimilated drones.

The returned value is a list of the names of the assimilated
drones, unless optional INCLUDE-VARIABLES is non-nil, in which
case elements of the returned list have the form (NAME . PLIST).

PLIST is a list of paired elements.  Property names are symbols
and correspond to a VARIABLE defined in the Borg repository's
\".gitmodules\" file as \"submodule.NAME.VARIABLE\".

Each property value is either a string or a list of strings.  If
INCLUDE-VARIABLES is `raw' then all values are lists.  Otherwise
a property value is only a list if the corresponding property
name is a member of `borg--multi-value-variables'.  If a property
name isn't a member of `borg--multi-value-variables' but it does
have multiple values anyway, then it is undefined with value is
included in the returned value."
  (if include-variables
      (let (alist)
        (dolist (line (process-lines "git" "config" "--list"
                                     "--file" borg-gitmodules-file))
          (when (string-match
                 "\\`submodule\\.\\(.+\\)\\.\\([^=]+\\)=\\(.+\\)\\'" line)
            (let* ((drone (match-string 1 line))
                   (prop  (intern (match-string 2 line)))
                   (value (match-string 3 line))
                   (elt   (assoc drone alist))
                   (plist (cdr elt)))
              (unless elt
                (push (setq elt (list drone)) alist))
              (setq plist
                    (plist-put plist prop
                               (if (or (eq include-variables 'raw)
                                       (memq prop borg--multi-value-variables))
                                   (nconc (plist-get plist prop)
                                          (list value))
                                 value)))
              (setcdr elt plist))))
        (cl-sort alist #'string< :key #'car))
    (cl-mapcan (lambda (line)
                 (and (string-equal (substring line 50 54) "lib/")
                      (list (substring line 54))))
               (let ((default-directory borg-user-emacs-directory))
                 (process-lines "git" "submodule--helper" "list")))))

(defun borg-clones ()
  "Return a list of cloned packages.

The returned value includes the names of all packages that were
cloned into `borg-drone-directory', including clones that have
not been assimilated yet."
  (cl-mapcan (lambda (file)
               (and (file-directory-p file)
                    (list (file-name-nondirectory file))))
             (directory-files borg-drone-directory t "\\`[^.]")))

(defun borg-read-package (prompt &optional edit-url)
  "Read a package name and url, and return them as a list.

When the `epkg' package is available, then read the name in the
minibuffer and use the url stored in the Epkg database.  If
`epkg' is unavailable, the package is unknown, or when EDIT-URL
is non-nil, then also read the url in the minibuffer.  PROMPT
is used when reading the package name."
  (if (require 'epkg nil t)
      (let* ((name (completing-read prompt (epkgs 'name)
                                    nil nil nil 'epkg-package-history))
             (pkg  (epkg name))
             (url  (if pkg
                       (if (or (epkg-git-package-p pkg)
                               (epkg-github-package-p pkg)
                               (epkg-orphaned-package-p pkg)
                               (epkg-gitlab-package-p pkg))
                           (eieio-oref pkg 'url)
                         (eieio-oref pkg 'mirror-url))
                     (and (require 'magit nil t)
                          (magit-get "remote" (magit-get-some-remote) "url")))))
        (list name
              (if (or (not url) edit-url)
                  (read-string "Url: " url)
                url)))
    (list (read-string prompt)
          (read-string "Url: "))))

(defun borg-read-clone (prompt)
  "Read the name of a cloned package, prompting with PROMPT."
  (require 'epkg nil t)
  (completing-read prompt (borg-clones) nil t nil 'epkg-package-history))

(defmacro borg-silencio (regexp &rest body)
  "Execute the forms in BODY while silencing messages that don't match REGEXP."
  (declare (indent 1))
  `(let ((msg (symbol-function 'message)))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (unless (string-match-p ,regexp format-string)
                    (apply msg format-string args)))))
       ,@body)))

;;; Activation

(defun borg-initialize ()
  "Initialize assimilated drones.

For each drone use `borg-activate' to add the appropriate
directories to the `load-path' and `Info-directory-alist', and
load the autoloads file, if it exists.

If the value of a Git variable named `submodule.DRONE.disabled'
is true in \"~/.emacs.d/.gitmodules\", then the drone named DRONE
is skipped."
  (info-initialize)
  (let ((start (current-time))
        (skipped 0)
        (initialized 0)
        (borg--gitmodule-cache (borg-drones 'raw)))
    (dolist (drone borg--gitmodule-cache)
      (setq  drone (car drone))
      (if (equal (borg-get drone "disabled") "true")
          (cl-incf skipped)
        (cl-incf initialized)
        (borg-activate drone)))
    (message "Initializing drones...done (%s drones in %.3fs%s)"
             initialized
             (float-time (time-subtract (current-time) start))
             (if (> skipped 0)
                 (format ", %d skipped" skipped)
               ""))))

(defun borg-activate (clone)
  "Activate the clone named CLONE.

Add the appropriate directories to `load-path' and
`Info-directory-alist', and load the autoloads file,
if it exists."
  (interactive (list (borg-read-clone "Activate clone: ")))
  (dolist (dir (borg-load-path clone))
    (let (file)
      (cond ((and (file-exists-p
                   (setq file (expand-file-name
                               (concat clone "-autoloads.el") dir)))
                  (with-demoted-errors "Error loading autoloads: %s"
                    (load file nil t))))
            ((and (file-exists-p
                   (setq file (expand-file-name
                               (concat clone "-loaddefs.el") dir)))
                  (with-demoted-errors "Error loading autoloads: %s"
                    (add-to-list 'load-path dir) ; for `org'
                    (load file nil t))))
            (t (push dir load-path)))))
  (dolist (dir (borg-info-path clone))
    (push  dir Info-directory-list)))

;;; Construction

(defun borg-batch-rebuild (&optional quick)
  "Rebuild all assimilated drones.

Drones are rebuilt in alphabetic order, except that Org is built
first.  `init.el' and `USER-REAL-LOGIN-NAME.el' are also rebuilt.

This function is to be used only with `--batch'.

When optional QUICK is non-nil, then do not build drones for
which `submodule.DRONE.build-step' is set, assuming those are the
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

(defun borg-build (clone &optional activate)
  "Build the clone named CLONE.
Interactively, or when optional ACTIVATE is non-nil,
then also activate the clone using `borg-activate'."
  (interactive (list (borg-read-clone "Build drone: ") t))
  (if noninteractive
      (let ((default-directory (borg-worktree clone))
            (build-cmd (if (functionp borg-build-shell-command)
                           (funcall borg-build-shell-command clone)
                         borg-build-shell-command))
            (build (borg-get-all clone "build-step")))
        (if  build
            (dolist (cmd build)
              (message "  Running '%s'..." cmd)
              (cond ((member cmd '("borg-update-autoloads"
                                   "borg-byte-compile"
                                   "borg-makeinfo"))
                     (funcall (intern cmd) clone))
                    ((string-match-p "\\`(" cmd)
                     (eval (read cmd)))
                    (build-cmd
                     (when (or (stringp build-cmd)
                               (setq build-cmd (funcall build-cmd clone cmd)))
                       (require 'format-spec)
                       (shell-command
                        (format-spec build-cmd
                                     `((%s . ,cmd)
                                       (%S . ,(shell-quote-argument cmd)))))))
                    (t
                     (shell-command cmd)))
              (message "  Running '%s'...done" cmd))
          (let ((path (mapcar #'file-name-as-directory (borg-load-path clone))))
            (borg-update-autoloads clone path)
            (borg-byte-compile clone path)
            (borg-makeinfo clone))))
    (save-some-buffers
     nil (let ((top default-directory))
           (lambda ()
             (let ((file (buffer-file-name)))
               (and file
                    (string-match-p emacs-lisp-file-regexp file)
                    (file-in-directory-p file top))))))
    (let ((buffer (get-buffer-create "*Epkg Build*"))
          (process-connection-type nil))
      (switch-to-buffer buffer)
      (with-current-buffer buffer
        (borg-build-mode)
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (insert (format "\n(%s) Building %s\n\n"
                          (format-time-string "%H:%M:%S")
                          clone))))
      (set-process-filter
       (start-process
        (format "emacs --eval (borg-build %S)" clone)
        buffer
        (expand-file-name invocation-name invocation-directory)
        "--batch" "-Q"
        "-L" (borg-worktree "borg")
        "--eval" "(require 'borg)"
        "--eval" "(borg-initialize)"
        "--eval" (format "(borg-build %S)" clone))
       'borg-build--process-filter)))
  (when activate
    (borg-activate clone)))

(defun borg-build--process-filter (process string)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          (goto-char (process-mark process))
          (let ((inhibit-read-only t))
            (insert string))
          (set-marker (process-mark process) (point)))
        (if moving (goto-char (process-mark process)))))))

(defvar borg-build-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-q" 'bury-buffer)
    map)
  "Keymap for `borg-build-mode'.")

(define-derived-mode borg-build-mode special-mode
  "Mode for the \"*Epkg Build*\" buffer."
  (setq buffer-read-only t))

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

(defun borg-update-autoloads (clone &optional path)
  "Update autoload files for the clone named CLONE in the directories in PATH."
  (setq path (borg--expand-load-path clone path))
  (let ((autoload-excludes
         (nconc (mapcar #'expand-file-name
                        (borg-get-all clone "no-byte-compile"))
                (cl-mapcan
                 (lambda (dir)
                   (list (expand-file-name (concat clone "-pkg.el") dir)
                         (expand-file-name (concat clone "-test.el") dir)
                         (expand-file-name (concat clone "-tests.el") dir)))
                 path)
                autoload-excludes))
        (generated-autoload-file
         (expand-file-name (format "%s-autoloads.el" clone) (car path))))
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

(defun borg-byte-compile (clone &optional path)
  "Compile libraries for the clone named CLONE in the directories in PATH."
  (setq path (borg--expand-load-path clone path))
  (let ((exclude (borg-get-all clone "no-byte-compile"))
        (topdir (borg-worktree clone)))
    (dolist (path-dir path)
      (with-current-buffer (get-buffer-create byte-compile-log-buffer)
        (setq default-directory (expand-file-name path-dir topdir))
        (when (> (length path) 1)
          (message "\n Building %s..." default-directory))
        (unless (eq major-mode 'compilation-mode)
          (compilation-mode))
        (let ((skip-count 0)
              (fail-count 0)
              (file-count 0)
              (dir-count  0)
              (dirs (list path-dir))
              dir last-dir)
          (displaying-byte-compile-warnings
           (while (setq dir (pop dirs))
             (cl-incf dir-count)
             (dolist (file (directory-files dir t))
               (let ((file-relative (file-relative-name file topdir))
                     (name (file-name-nondirectory file)))
                 (if (file-directory-p file)
		     (when (and borg-byte-compile-recursively
                                (not (file-symlink-p file))
		                (not (string-prefix-p "." name))
		                (not (member name '("RCS" "CVS"))))
                       (if (or (file-exists-p (expand-file-name ".nosearch" file))
                               (member file-relative exclude))
                           (message " Skipping %s...skipped" file)
                         (setq dirs (nconc dirs (list file)))))
                   (when (and (file-regular-p  file)
                              (file-readable-p file)
                              (string-match-p emacs-lisp-file-regexp name)
                              (not (auto-save-file-name-p file))
                              (not (string-match-p "\\`\\." name))
                              (not (string-match-p "-autoloads.el\\'" name))
                              (not (string-equal dir-locals-file name)))
                     (cl-incf
                      (if (or (string-match-p "-pkg.el\\'" name)
                              (string-match-p "-tests?.el\\'" name)
                              (member file-relative exclude))
                          (progn (message " Skipping %s...skipped" file)
                                 skip-count)
                        (pcase (byte-recompile-file file t 0)
                          ('no-byte-compile
                           (message "Compiling %s...skipped" file)
                           skip-count)
                          ('t file-count)
                          (_  fail-count))))
                     (unless (equal dir last-dir)
                       (setq last-dir dir)
                       (cl-incf dir-count))))))))
          (message
           "Done (Total of %d file%s compiled%s%s%s)"
           file-count (if (= file-count 1) "" "s")
           (if (> fail-count 0) (format ", %d failed"  fail-count) "")
           (if (> skip-count 0) (format ", %d skipped" skip-count) "")
           (if (> dir-count  0) (format " in %d directories" dir-count) "")
           )))))) ; oh, the horror

(defun borg-makeinfo (clone)
  "Generate Info manuals and the Info index for the clone named CLONE."
  (dolist (default-directory (borg-info-path clone t))
    (let ((exclude (borg-get-all clone "no-makeinfo")))
      (dolist (texi (directory-files default-directory nil "\\.texi\\(nfo\\)?\\'"))
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

;;; Assimilation

(defun borg-assimilate (package url &optional partially)
  "Assimilate the package named PACKAGE from URL.

If `epkg' is available, then only read the name of the package
in the minibuffer and use the url stored in the Epkg database.
If `epkg' is unavailable, the package is not in the database, or
with a prefix argument, then also read the url in the minibuffer.

With a negative prefix argument only add the submodule but don't
build and activate the drone."
  (interactive
   (nconc (borg-read-package "Assimilate package: " current-prefix-arg)
          (list (< (prefix-numeric-value current-prefix-arg) 0))))
  (message "Assimilating %s..." package)
  (let ((default-directory borg-user-emacs-directory))
    (borg--maybe-reuse-gitdir package)
    (borg--call-git package "submodule" "add" "--name" package url
                    (file-relative-name (borg-worktree package)))
    (borg--sort-submodule-sections ".gitmodules")
    (borg--call-git package "add" ".gitmodules")
    (borg--maybe-absorb-gitdir package))
  (unless partially
    (borg-build package)
    (borg-activate package))
  (borg--refresh-magit)
  (message "Assimilating %s...done" package))

(defun borg-clone (package url)
  "Clone the package named PACKAGE from URL, without assimilating it.

If `epkg' is available, then only read the name of the package
in the minibuffer and use the url stored in the Epkg database.
If `epkg' is unavailable, the package is not in the database, or
with a prefix argument, then also read the url in the minibuffer."
  (interactive (borg-read-package "Clone package: " current-prefix-arg))
  (message "Cloning %s..." package)
  (let ((gitdir (borg-gitdir package))
        (topdir (borg-worktree package)))
    (when (file-exists-p topdir)
      (user-error "%s already exists" topdir))
    (let ((default-directory borg-user-emacs-directory))
      (borg--maybe-reuse-gitdir package)
      (unless (file-exists-p topdir)
        (borg--call-git package "clone"
                        (concat "--separate-git-dir="
                                ;; Git fails if this ends with slash.
                                (directory-file-name gitdir))
                        url (file-relative-name topdir)))
      (borg--link-gitdir package))
    (borg--refresh-magit)
    (message "Cloning %s...done" package)))

(defun borg-remove (clone)
  "Remove the cloned or assimilated package named CLONE.

Remove the working tree from `borg-drone-directory', regardless
of whether that repository belongs to an assimilated package or a
package that has only been cloned for review using `borg-clone'.
The Git directory is not removed."
  (interactive (list (borg-read-clone "Uninstall clone: ")))
  (message "Removing %s..." clone)
  (let ((topdir (borg-worktree clone)))
    (let ((default-directory topdir))
      (when (or (not (borg--git-success "diff" "--quiet" "--cached"))
                (not (borg--git-success "diff" "--quiet")))
        (user-error "%s contains uncommitted changes" topdir))
      (borg--maybe-absorb-gitdir clone))
    (if (member clone (borg-drones))
        (let ((default-directory borg-user-emacs-directory))
          (borg--call-git nil "rm" "--force" topdir))
      (delete-directory topdir t t)))
  (borg--refresh-magit)
  (message "Removing %s...done" clone))

;;; Internal Utilities

(defun borg--maybe-absorb-gitdir (pkg)
  (if (version< (nth 2 (split-string (car (process-lines "git" "version")) " "))
                "2.12.0")
      (let ((gitdir (borg-gitdir pkg))
            (topdir (borg-worktree pkg)))
        (unless (equal (let ((default-directory topdir))
                         (car (process-lines "git" "rev-parse" "--git-dir")))
                       (directory-file-name gitdir))
          (rename-file (expand-file-name ".git" topdir) gitdir)
          (borg--link-gitdir pkg)
          (let ((default-directory gitdir))
            (borg--call-git pkg "config" "core.worktree"
                            (concat "../../../lib/" pkg)))))
    (borg--call-git pkg "submodule" "absorbgitdirs" "--" (borg-worktree pkg))))

(defun borg--maybe-reuse-gitdir (pkg)
  (let ((gitdir (borg-gitdir pkg))
        (topdir (borg-worktree pkg)))
    (when (and (file-exists-p gitdir)
               (not (file-exists-p topdir)))
      (pcase (read-char-choice
              (concat
               gitdir " already exists.\n"
               "Type [r] to reuse the existing gitdir and create the worktree\n"
               "     [d] to delete the old gitdir and clone again\n"
               "   [C-g] to abort ")
              '(?r ?d))
        (?r (borg--restore-worktree pkg))
        (?d (delete-directory gitdir t t))))))

(defun borg--restore-worktree (pkg)
  (let ((topdir (borg-worktree pkg)))
    (make-directory topdir t)
    (borg--link-gitdir pkg)
    (let ((default-directory topdir))
      (borg--call-git pkg "reset" "--hard" "HEAD"))))

(defun borg--link-gitdir (pkg)
  (let ((gitdir (borg-gitdir pkg))
        (topdir (borg-worktree pkg)))
    (with-temp-file (expand-file-name ".git" topdir)
      (insert "gitdir: " (file-relative-name gitdir topdir) "\n"))))

(defun borg--call-git (pkg &rest args)
  (let ((process-connection-type nil)
        (buffer (generate-new-buffer
                 (concat " *Borg Git" (and pkg (concat " " pkg)) "*"))))
    (if (eq (apply #'call-process "git" nil buffer nil args) 0)
        (kill-buffer buffer)
      (pop-to-buffer buffer)
      (error "Git failed"))))

(defun borg--git-success (&rest args)
  (= (apply #'process-file "git" nil nil nil args) 0))

(defun borg--refresh-magit ()
  (when (and (derived-mode-p 'magit-mode)
             (fboundp 'magit-refresh))
    (magit-refresh)))

(defun borg--expand-load-path (clone path)
  (let ((default-directory (borg-worktree clone)))
    (mapcar (lambda (p)
              (file-name-as-directory (expand-file-name p)))
            (or path (borg-load-path clone)))))

(defun borg--sort-submodule-sections (file)
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (revert-buffer t t)
    (goto-char (point-min))
    (re-search-forward "^\\[submodule")
    (sort-regexp-fields
     nil "^\\[submodule \"\\([^\"]+\\)\"].*\n\\([\s\t].*\n\\)+"
     "\\1" (line-beginning-position) (point-max))
    (save-buffer)))

(provide 'borg)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; borg.el ends here
