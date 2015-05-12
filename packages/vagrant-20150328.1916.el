;;; vagrant.el --- Interact with vagrant machines -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/vagrant.el
;; Package-Version: 20150328.1916
;; Keywords: vagrant, convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; [![Travis build status](https://travis-ci.org/emacs-pe/vagrant.el.png?branch=master)](https://travis-ci.org/emacs-pe/vagrant.el)
;;
;; `vagrant.el' provides easy interaction with [vagrant][] also offers
;; a `vagrant' TRAMP method to interact with vagrant machines.
;;
;;; Installation:
;; To use this `vagrant.el' it's necessary to install the [vagrant-info][] plugin.
;;
;;; Usage:
;; You can get a complete list of your available vagrant machines with
;; `M-x vagrant-list-machines`.
;;
;; The `vagrant' TRAMP method works by using an custom ssh configfile
;; (`vagrant-ssh-config-file') for vagrant machines, so you need to add manually
;; the ssh-config of a machine with `M-x vagrant-add-ssh-config`.
;;
;;; Troubleshooting:
;; + **My machine doesn't shows up**
;;
;;   `vagrant.el' uses [vagrant-info][] and this plugin uses `global-status',
;;   so if the command `vagrant global-status` doesn't shows the information of
;;   your vagrant machine you can follow the intructions described in
;;   https://docs.vagrantup.com/v2/cli/global-status.html
;;
;; + **The `vagrant' TRAMP method show already deleted machines**
;;
;;   You need to execute `M-x vagrant-tramp-cleanup-ssh-config` to cleanup the
;;   vagrant ssh-config.
;;
;; [vagrant]: http://www.vagrantup.com/ "Vagrant"
;; [vagrant-info]: https://github.com/marsam/vagrant-info "vagrant-info plugin"

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'tramp)
(require 'tramp-cache)

(defgroup vagrant nil
  "Interact with vagrant machines"
  :prefix "vagrant-"
  :group 'applications)

(defcustom vagrant-executable "vagrant"
  "Vagrant executable path."
  :type 'string
  :group 'vagrant)

(defcustom vagrant-machines-buffer-name "*vagrant-machines*"
  "Vagrant buffer name for list machines."
  :type 'string
  :group 'vagrant)

(defcustom vagrant-ssh-config-file (expand-file-name "vagrant-ssh-config" user-emacs-directory)
  "Vagrant ssh config filename."
  :type 'string
  :group 'vagrant)

(defcustom vagrant-disable-asking nil
  "Disable asking before destructive operations."
  :type 'boolean
  :group 'vagrant)

;;;###tramp-autoload
(defconst vagrant-tramp-method "vagrant"
  "Method to connect vagrant machines.")

(cl-defstruct (vagrant-machine (:constructor vagrant-machine--create))
  "A structure holding all the information of a vagrant machine."
  id name provider state directory)

(defvar vagrant-machines nil
  "An alist containing available vagrant machines.")

(defvar vagrant-machines-already-fetched nil)

(define-error 'vagrant-error "Unknown Vagrant error")
(define-error 'vagrant-machine-notfound "Vagrant machine not found" 'vagrant-error)
(define-error 'vagrant-command-error "Vagrant command exited abnormally" 'vagrant-error)

(defun vagrant--machines ()
  "Fetch the vagrant machines."
  (unless vagrant-machines-already-fetched
    (setq vagrant-machines-already-fetched t
          vagrant-machines (cl-loop for line in (cdr (process-lines vagrant-executable "info-index"))
                                    for value = (split-string line ",")
                                    collect (cl-multiple-value-bind (id name provider state directory) value
                                              (cons id
                                                    (vagrant-machine--create :id id :name name :provider provider :state state :directory directory))))))
  vagrant-machines)

(defun vagrant--run-subcommand (&rest args)
  "Run vagrant subcommand with ARGS."
  (let ((command (mapconcat 'identity (cons vagrant-executable args) " ")))
    (compilation-start command nil
                       (lambda (_) (format "*Vagrant: %s*" command)))))

;;;###autoload
(defun vagrant-up-machine (id)
  "Start up a vagrant machine with ID."
  (interactive (vagrant--read-machine-id))
  (vagrant--run-subcommand "up" "--no-color" id))

;;;###autoload
(defun vagrant-halt-machine (id)
  "Halt a vagrant machine with ID."
  (interactive (vagrant--read-machine-id))
  (vagrant--run-subcommand "halt" "--no-color" id))

;;;###autoload
(defun vagrant-reload-machine (id)
  "Reload a vagrant machine with ID."
  (interactive (vagrant--read-machine-id))
  (vagrant--run-subcommand "reload" "--no-color" id))

;;;###autoload
(defun vagrant-resume-machine (id)
  "Resume a vagrant machine with ID."
  (interactive (vagrant--read-machine-id))
  (vagrant--run-subcommand "resume" "--no-color" id))

;;;###autoload
(defun vagrant-suspend-machine (id)
  "Suspend a vagrant machine with ID."
  (interactive (vagrant--read-machine-id))
  (vagrant--run-subcommand "suspend" "--no-color" id))

;;;###autoload
(defun vagrant-provision-machine (id)
  "Provision a vagrant machine with ID."
  (interactive (vagrant--read-machine-id))
  (vagrant--run-subcommand "provision" "--no-color" id))

;;;###autoload
(defun vagrant-destroy-machine (id)
  "Provision a vagrant machine with ID."
  (interactive (vagrant--read-machine-id))
  (when (or vagrant-disable-asking
            (y-or-n-p (format "Are you sure you want to destroy the '%s' VM? " id)))
    (vagrant--run-subcommand "destroy" "--no-color" "--force" id)))

;;;###autoload
(defun vagrant-edit-vagrantfile (id)
  "Edit Vagrantfile of a vagrant machine with ID."
  (interactive (vagrant--read-machine-id))
  (find-file (expand-file-name "Vagrantfile"
                               (vagrant-machine-directory (or (assoc-default id vagrant-machines)
                                                              (signal 'vagrant-machine-notfound (list id)))))))

;;;###autoload
(defun vagrant-add-ssh-config (id)
  "Add `ssh-info' of a machine with ID to `vagrant-ssh-config-file'."
  (interactive (vagrant--read-machine-id))
  (with-temp-buffer
    (let ((exit-status (call-process vagrant-executable nil (current-buffer) nil "info-ssh" id)))
      (if (zerop exit-status)
          (write-region (buffer-string) nil vagrant-ssh-config-file 'append)
        (signal 'vagrant-command-error (list (buffer-string)))))))

(defun vagrant--read-machine-id ()
  "Read a vagrant machine id."
  (list (if (eq major-mode 'vagrant-machine-list-mode)
            (tabulated-list-get-id)
          (completing-read "vagrant machine id: "
                           (vagrant--machines)
                           nil nil nil nil
                           (tabulated-list-get-id)))))

;;;###autoload
(defun vagrant-reload-machines ()
  "Reload `vagrant-mahines'."
  (interactive)
  (setq vagrant-machines-already-fetched nil)
  (vagrant--machines))

(defun vagrant--generate-table-entry (item)
  "Generate a tabulate mode entry from an ITEM."
  (cl-destructuring-bind (id . machine) item
    (list id (vector (vagrant-machine-id machine)
                     (vagrant-machine-name machine)
                     (vagrant-machine-provider machine)
                     (vagrant-machine-state machine)
                     (vagrant-machine-directory machine)))))

(defun vagrant--generate-table-entries ()
  "Generate tabulate mode entries from `vagrant-machines'."
  (mapcar #'vagrant--generate-table-entry (vagrant--machines)))

(defvar vagrant-machine-list-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "U") #'vagrant-up-machine)
    (define-key map (kbd "H") #'vagrant-halt-machine)
    (define-key map (kbd "R") #'vagrant-reload-machine)
    (define-key map (kbd "D") #'vagrant-destroy-machine)
    (define-key map (kbd "S") #'vagrant-suspend-machine)
    (define-key map (kbd "P") #'vagrant-provision-machine)
    (define-key map (kbd "E") #'vagrant-edit-vagrantfile)
    (define-key map (kbd "C") #'vagrant-add-ssh-config)
    map)
  "Keymap for vagrant-list-machines-mode.")

(define-derived-mode vagrant-machine-list-mode tabulated-list-mode "Vagrant List"
  "List vagrant machines.

\\{vagrant-machine-list-mode-map}"
  (setq tabulated-list-format [("id" 7 nil)
                               ("name" 10 nil)
                               ("provider" 10 nil)
                               ("state" 10 nil)
                               ("directory" 60 nil)])
  (add-hook 'tabulated-list-revert-hook 'vagrant-reload-machines nil t)
  (setq tabulated-list-entries 'vagrant--generate-table-entries)
  (tabulated-list-init-header))

;;;###autoload
(defun vagrant-list-machines ()
  "Show the list of available vagrant machines."
  (interactive)
  (with-current-buffer (get-buffer-create vagrant-machines-buffer-name)
    (vagrant-machine-list-mode)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(defun vagrant--create-file-if-not-exists (filename)
  "Create an empty file with name FILENAME."
  (unless (file-exists-p filename)
    (call-process "touch" nil nil nil filename)))

;;;###autoload
(defun vagrant-tramp-cleanup-ssh-config ()
  "Cleanup vagrant ssh-config.

This involves:
+ Remove `vagrant-ssh-config-file' if exists.
+ Remove vagrant entries from `tramp-cache-data'.
+ Dump `tramp-persistency-file-name'."
  (interactive)
  (when (file-exists-p vagrant-ssh-config-file)
    (delete-file vagrant-ssh-config-file))
  (vagrant--create-file-if-not-exists vagrant-ssh-config-file)
  (maphash (lambda (key _value)
             (when (and (and (vectorp key))
                        (string-equal vagrant-tramp-method (tramp-file-name-method key)))
               (remhash key tramp-cache-data)))
           tramp-cache-data)
  (setq tramp-cache-data-changed t)
  (if (zerop (hash-table-count tramp-cache-data))
      (when (file-exists-p tramp-persistency-file-name)
        (delete-file tramp-persistency-file-name))
    (tramp-dump-connection-properties)))

;;;###tramp-autoload
(defconst vagrant-tramp-completion-function-alist
  `((vagrant--create-file-if-not-exists ,vagrant-ssh-config-file)
    (tramp-parse-sconfig               ,vagrant-ssh-config-file))
  "Default list of (FUNCTION FILE) pairs to be examined for vagrant method.")

;;;###tramp-autoload
(add-to-list 'tramp-methods
             `(,vagrant-tramp-method
               (tramp-login-program        "ssh")
               (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none")
                                            ("%h") ("-F" ,vagrant-ssh-config-file)))
               (tramp-async-args           (("-q")))
               (tramp-remote-shell         "/bin/sh")
               (tramp-remote-shell-args    ("-c"))
               (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                            ("-o" "UserKnownHostsFile=/dev/null")
                                            ("-o" "StrictHostKeyChecking=no")))
               (tramp-default-port         22)))

;;;###tramp-autoload
(eval-after-load 'tramp
  '(tramp-set-completion-function vagrant-tramp-method vagrant-tramp-completion-function-alist))

(provide 'vagrant)

;;; vagrant.el ends here
