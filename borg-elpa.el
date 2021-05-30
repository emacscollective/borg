;;; borg-elpa.el --- use Borg alongside Package.el  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/emacscollective/borg
;; Keywords: tools

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file contains code from GNU Emacs, which is
;; Copyright (C) 1976-2017 Free Software Foundation, Inc.

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

;; Use Borg alongside `package.el'.

;; Borg can be used by itself or alongside `package.el'.  Installing
;; Borg from Melpa is still experimental.  For instructions and help
;; see https://github.com/emacscollective/borg/issues/46.  The manual
;; does not yet cover this topic.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'subr-x)

(require 'borg)
(require 'package)

;; Do not require `epkg' to avoid forcing all `borg' users
;; to install that and all of its numerous dependencies.
(declare-function epkg 'epkg (name))
(eval-when-compile
  (cl-pushnew 'summary eieio--known-slot-names))

(defun borg-elpa-initialize ()
  "Initialize Borg and Elpa in the correct order."
  (add-to-list 'package-directory-list borg-drones-directory)
  (unless (featurep 'epkg)
    (let ((load-path
           (nconc (cl-mapcan
                   (lambda (name)
                     (let ((dir (expand-file-name name borg-drones-directory)))
                       (if (file-directory-p dir)
                           (list dir)
                         nil))) ; Just hope that it is installed using elpa.
                   '("emacsql" "closql" "epkg"))
                  load-path)))
      (require (quote epkg))))
  (borg-initialize)
  (package-initialize))

(defun package-activate-1--borg-handle-activation
    (fn pkg-desc &optional reload deps)
  "For a Borg-installed package, let Borg handle the activation."
  (or (package--borg-clone-p (package-desc-dir pkg-desc))
      (funcall fn pkg-desc reload deps)))

(advice-add 'package-activate-1 :around
            'package-activate-1--borg-handle-activation)

(defun package-load-descriptor--borg-use-database (fn pkg-dir)
  "For a Borg-installed package, use information from the Epkgs database."
  (if-let ((dir (package--borg-clone-p pkg-dir)))
      (let* ((name (file-name-nondirectory (directory-file-name dir)))
             (epkg (epkg name))
             (desc (package-process-define-package
                    (list 'define-package
                          name
                          (borg--package-version name)
                          (if epkg
                              (or (oref epkg summary)
                                  "[No summary]")
                            "[Installed using Borg, but not in Epkgs database]")
                          ()))))
        (setf (package-desc-dir desc) pkg-dir)
        desc)
    (funcall fn pkg-dir)))

(advice-add 'package-load-descriptor :around
            'package-load-descriptor--borg-use-database)

(defun package--borg-clone-p (pkg-dir)
  ;; Currently `pkg-dir' is a `directory-file-name', but that might change.
  (setq pkg-dir (file-name-as-directory pkg-dir))
  (and (equal (file-name-directory (directory-file-name pkg-dir))
              borg-drones-directory)
       pkg-dir))

(defvar borg--version-tag-glob "*[0-9]*")

(defun borg--package-version (clone)
  (or (let ((version
             (let ((default-directory (borg-worktree clone)))
               (ignore-errors
                 (car (process-lines "git" "describe" "--tags" "--match"
                                     borg--version-tag-glob))))))
        (and version
             (string-match
              "\\`\\(?:[^0-9]+\\)?\\([.0-9]+\\)\\(?:-\\([0-9]+-g\\)\\)?"
              version)
             (let ((version (version-to-list (match-string 1 version)))
                   (commits (match-string 2 version)))
               (when commits
                 (setq commits (string-to-number commits))
                 (setq version (seq-take version 3))
                 (when (< (length version) 3)
                   (setq version
                         (nconc version (make-list (- 3 (length version)) 0))))
                 (setq version
                       (nconc version (list commits))))
               (mapconcat #'number-to-string version "."))))
      "9999"))

;;; _
(provide 'borg-elpa)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; borg-elpa.el ends here
