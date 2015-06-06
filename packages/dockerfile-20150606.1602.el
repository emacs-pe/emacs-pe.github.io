;;; dockerfile.el --- Dockerfile major mode          -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/dockerfile.el
;; Package-Version: 20150606.1602
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; [![Travis build status](https://travis-ci.org/emacs-pe/dockerfile.el.svg?branch=master)](https://travis-ci.org/emacs-pe/dockerfile.el)

;; `dockerfile.el' offers a major mode for editing [Dockerfiles][].
;;
;; [Dockerfiles]: https://docs.docker.com/reference/builder/

;;; Code:
(require 'ansi-color)
(require 'compile)
(require 'sh-script)

(defgroup dockerfile nil
  "Dockerfile major mode."
  :prefix "dockerfile-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/emacs-pe/dockerfile.el")
  :link '(emacs-commentary-link :tag "Commentary" "dockerfile"))

(defcustom dockerfile-docker-executable "docker"
  "Path to docker executable."
  :type 'string
  :group 'dockerfile)

(defcustom dockerfile-docker-extra-args nil
  "List of additional arguments for docker executable."
  :type '(repeat string)
  :group 'dockerfile)

(defface dockerfile-build-step-face
  '((t :inherit font-lock-keyword-face))
  "Face for docker build steps."
  :group 'dockerfile)

(defface dockerfile-build-error-face
  '((t :inherit font-lock-warning-face))
  "Face used to docker build errors."
  :group 'dockerfile)

(defvar-local dockefile-image-name nil
  "Image name of a Dockerfile.")
(put 'dockefile-image-name 'safe-local-variable 'stringp)

(defvar dockefile-image-name-history nil)

;; https://github.com/docker/docker/blob/093f57a/builder/command/command.go
(defconst dockerfile-keywords
  '("env" "label" "maintainer" "add" "copy" "from" "onbuild" "workdir"
    "run" "cmd" "entrypoint" "expose" "volume" "user"))


(defun dockerfile-build-filter ()
  "Handle match highlighting escape sequences inserted by the docker process.
This function is called from `compilation-filter-hook'."
  ;; TODO: use `compilation-filter-start' instead of `point-min'
  (ansi-color-apply-on-region (point-min) (point-max)))

(defconst dockerfile-build-error-regexp-alist-alist
  `((build-step ,(rx bol (group "Step" space (+ num)) space ":"  space (group (+ any)))
                (0 "Dockerfile") nil nil 2 nil
                (2  'dockerfile-build-step-face))
    (build-error ,(rx bol (group (= 4 alpha)) "[" (group (+ num)) "]" space (group (+ any)))
                 (0 "Dockerfile") nil nil 2 nil
                 (2 compilation-line-face)
                 (3 'dockerfile-build-error-face)))
  "Specifications for matching errors in docker build invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defconst dockerfile-build-error-regexp-alist (mapcar 'car dockerfile-build-error-regexp-alist-alist))

(define-compilation-mode dockerfile-build-mode "docker-build"
  "Dockefile build results compilation mode."
  (set (make-local-variable 'compilation-error-regexp-alist)
       dockerfile-build-error-regexp-alist)
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       dockerfile-build-error-regexp-alist-alist)
  (add-hook 'compilation-filter-hook 'dockerfile-build-filter nil t))

;;;###autoload
(defun dockerfile-build (path-or-url &optional image-name)
  "Build Dockerfile from PATH-OR-URL with tag IMAGE-NAME."
  (interactive (if current-prefix-arg
                   (list (read-directory-name "Path to Dockerfile: ")
                         (read-string (if dockefile-image-name (format "Image name (default \"%s\"): " dockefile-image-name) "Image name: ")
                                      nil dockefile-image-name-history dockefile-image-name))
                 (list (file-name-directory (buffer-file-name)) dockefile-image-name)))
  (let* ((name-arg (and (not (or (null image-name) (string= "" image-name))) (list "-t" image-name)))
         (args (append (list dockerfile-docker-executable "build") dockerfile-docker-extra-args name-arg (list path-or-url)))
         (command (mapconcat 'identity args " ")))
    (compilation-start command 'dockerfile-build-mode
                       (lambda (_)
                         (format "*docker-build: %s*" command)))))


(defconst dockerfile-font-lock-keywords
  `(,(regexp-opt dockerfile-keywords 'symbols)
    ,@(sh-font-lock-keywords-1 'builtins)))

(defvar dockerfile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'dockerfile-build)
    map)
  "Keymap for `dockerfile-mode'.")

;;;###autoload
(define-derived-mode dockerfile-mode prog-mode "Dockerfile"
  "Major mode for editing Dockerfile files

\\{dockerfile-mode-map}"
  :syntax-table sh-mode-syntax-table
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+[\t ]*")
  (set (make-local-variable 'font-lock-defaults)
       '(dockerfile-font-lock-keywords nil t))
  (set (make-local-variable 'indent-line-function)
       'sh-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(provide 'dockerfile)

;;; dockerfile.el ends here
