;;; magit-gcm.el --- magit interface to git-config-manager

;; Copyright (c) 2016 Boris Buliga

;; Author: Boris Buliga <d12frosted@gmail.com>
;; Maintainer: Boris Buliga <d12frosted@gmail.com>
;; Created: 12 Jul 2016

;; Keywords: magit
;; Homepage: https://github.com/d12frosted/git-config-manager

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "24") (magit "2.1.0"))

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;
;; This package provides interface for git-config-manager.
;;
;; After this program is available on your `load-path` add following to your
;; init.el
;;
;; (require 'magit-gcm)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gcm)
;;
;; Note that for this to work you have to install git-config-manager executable.

;;; Code:
;;

(require 'magit)

(defvar magit-gcm-exectuable
  (executable-find "git-config-manager")
  "Path to git-config-manager executable.")

(defvar magit-gcm-config-file nil
  "Path to git-config-manager configuration file.

When it's nil, default one is used.")

(defvar magit-gcm-mode-lighter " gcm")

(defvar magit-gcm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C") 'magit-gcm-popup)
    map))

;;;###autoload
(define-minor-mode magit-gcm-mode "git-config-manager interface for magit"
  :lighter magit-gcm-mode-lighter
  :keymap 'magit-gcm-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (if magit-gcm-mode
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-gcm-insert-gcm
       'magit-insert-stashes)
    (remove-hook 'magit-status-sections-hook 'magit-gcm-insert-gcm))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(defun turn-on-magit-gcm ()
  "Unconditionally turn on `magit-gcm-mode'."
  (magit-gcm-mode 1))

(magit-define-popup magit-gcm-popup
  "Show popup buffer featuring git-config-manager commands."
  'magit-commands
  :switches '()
  :actions  '((?g "Refresh" magit-gcm-refresh)
              (?s "Set" magit-gcm-set-scheme)
              (?u "Unset" magit-gcm-unset-scheme)
              (?e "Edit" magit-gcm-edit-config))
  :default-action 'magit-gcm-refresh)

(defun magit-gcm-refresh ()
  "Refresh `magit-gcm'."
  (interactive)
  (magit-refresh))

(defun magit-gcm-insert-gcm ()
  "Insert git-config-manager section to magit status buffer."
  (let ((active (magit-gcm-get-active-schemes))
        (all (magit-gcm-get-available-schemes)))
    (magit-insert-section (gcm)
        (magit-insert-heading "Git Config Manager:")
      (dolist (scheme all)
        (if (member scheme active)
            (progn (magit-insert-section (active-scheme scheme)
                       (insert "+ ")
                     (insert scheme)
                     (insert "\n")))
          (progn (magit-insert-section (inactive-scheme scheme)
                     (insert "- ")
                   (insert scheme)
                   (insert "\n")))))
      (insert "\n"))))

(defun magit-gcm-set-scheme ()
  "Set selected scheme."
  (interactive)
  (magit-section-case
    (inactive-scheme
     (progn (magit-gcm-exec "set" (magit-section-value (magit-current-section)))
            (magit-refresh)))
    (t
     (error "Invalid target."))))

(defun magit-gcm-unset-scheme ()
  "Unset selected scheme."
  (interactive)
  (magit-section-case
    (active-scheme
     (progn (magit-gcm-exec "unset" (magit-section-value (magit-current-section)))
            (magit-refresh)))
    (t
     (error "Invalid target."))))

(defun magit-gcm-edit-config-file ()
  "Edit currently used configuration file."
  (interactive)
  (error "Not yet implemented. Sorry about that."))

(defun magit-gcm-get-active-schemes ()
  "Get list of all active schemes."
  (split-string (magit-gcm-exec "get")))

(defun magit-gcm-get-available-schemes ()
  "Get list of all available schemes."
  (split-string (magit-gcm-exec "list")))

(defun magit-gcm-exec (command &rest args)
  "Execute git-config-manager COMMAND with ARGS.

Passes `magit-gcm-config-file' as config file when it's non-nil."
  (shell-command-to-string
    (format "%s%s %s %s"
            magit-gcm-exectuable
            (if magit-gcm-config-file
                (format " --config-file %s" (shell-quote-argument magit-gcm-config-file))
              "")
            (shell-quote-argument command)
            (combine-and-quote-strings args))))

(provide 'magit-gcm)

;;; magit-gcm.el ends here
