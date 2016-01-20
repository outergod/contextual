;;; contextual.el --- Contextual minor mode          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Alexander Kahl

;; Author: Alexander Kahl <e-user@fsfe.org>
;; Keywords: convenience, tools

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

;; Contextual provides profiles support for Emacs.

;;; Code:

(require 'cl-macs)
(require 'dash)

(define-widget 'ctx-profile-type 'lazy
  "Contextual profile type"
  :offset 4
  :tag "Profile"
  :options '(vars hook)
  :type '(alist))

(define-widget 'ctx-profile-group-type 'lazy
  "Contextual profile group type"
  :offset 4
  :tag "Profile Group"
  :type '(alist :key-type string :value-type ctx-profile-type))

(defgroup contextual nil
  "Contextual global minor mode"
  :group 'convenience
  :prefix "ctx-")

(defcustom ctx-profiles nil
  "List of Contextual profiles"
  :group 'contextual
  :type 'ctx-profile-group-type)

(defcustom ctx-initial-profile nil
  "Initial Contextual profile"
  :group 'contextual
  :type 'string)

(defcustom ctx-keymap-prefix (kbd "C-c c")
  "Contextual keymap prefix."
  :group 'contextual
  :type 'string)

(defcustom contextual-enabled-hook nil
  "Called after Contextual is turned on."
  :group 'contextual
  :type 'hook)

(defcustom contextual-disabled-hook nil
  "Called after Contextual is turned off."
  :group 'contextual
  :type 'hook)

(defvar ctx-active-profile nil
  "Currently active Contextual profile")

;; (cl-defmacro ctx-define-profile-group (name &optional initial)
;;   (let ((prefix (concat (symbol-name name) "-")))
;;     `(defvar ,(intern (concat prefix "profiles")))
;;     `(defvar ,(intern (concat prefix "active-profile")))
;;     `(defvar ,(intern (concat prefix "initial-profile")) ,initial)
;;     `(defvar ,name '(,profiles ,active ,initial))))

(ctx-define-profile-group ctx-default-profile-group
  (ctx-profiles ctx-active-profile ctx-initial-profile))

(defun ctx-mode-line ()
  (format " ctx[%s]" (or ctx-active-profile "(none)")))

(defun ctx-activate-profile (group name)
  (-let [(profiles active) group]
    (unless (string= (symbol-value active) name)
      (-let* (((&alist name (&alist 'vars (vars) 'hook (hook))) (symbol-value profiles)))
        (mapc #'(lambda (pair)
                  (set (car pair) (cadr pair)))
              vars)
        (funcall hook)
        (setf (symbol-value active) name)
        (message "Loaded profile %s" name)))))

(defun ctx-profile-loader (group)
  #'(lambda ()
      (interactive)
      (ctx-activate-profile group (completing-read "Profile: " (symbol-value (car group)) nil t))))

(cl-defmacro ctx-define-profile-loader (name group)
  `(defalias ',name (ctx-profile-loader ,group)))

(ctx-define-profile-loader ctx-load-profile ctx-default-profile-group)

(defun ctx--add-profile (profiles name profile)
  (add-to-list profiles (cons name profile)))

(cl-defmacro ctx-add-profile (name (&optional (profiles 'ctx-profiles)) (&rest vars) &rest body)
  `(ctx--add-profile ',profiles ,name
     '((vars ,(-partition 2 vars))
       (hook (lambda () ,@body)))))

(defun ctx-set-initial-profile (name)
  (setq ctx-initial-profile name)
  (unless ctx-active-profile
    (ctx-activate-profile ctx-default-profile-group name)))

(defvar ctx-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'ctx-load-profile)
    map)
  "Keymap used for Contextual commands after `ctx-keymap-prefix'.")
(fset 'ctx-command-map ctx-command-map)

(defvar ctx-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map ctx-keymap-prefix 'ctx-command-map)
    map)
  "Keymap for Contextual mode.")

;;;###autoload
(define-minor-mode contextual-mode
  "Contextual is an Emacs global minor mode that enables customization
  variables to be changed and hooks to be run whenever a user changes
  her profile."
  nil (:eval (ctx-mode-line))
  :group 'contextual
  :keymap ctx-keymap
  (if contextual-mode
      (progn (run-hooks 'contextual-enabled-hook)
             (when (and (not ctx-active-profile)
                        ctx-initial-profile)
               (ctx-activate-profile ctx-default-profile-group ctx-initial-profile)))
    (run-hooks 'contextual-disabled-hook)))

;;;###autoload
(define-globalized-minor-mode contextual-global-mode contextual-mode contextual-mode)

;; This beast exists for the simple purpose of coloring and indenting
;; some functions.
(dolist (v '(ctx-add-profile ctx-define-profile-group ctx-define-profile-loader))
  (put v 'lisp-indent-function 'defun)
  (dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
    (font-lock-add-keywords mode `((,(concat
                                      "(\\<\\(" (symbol-name v) "\\)\\_>"
                                      "[ \t'\(]*"
                                      "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
                                    (1 font-lock-keyword-face)
                                    (2 font-lock-variable-name-face nil t))))))

(provide 'contextual)
;;; contextual.el ends here
