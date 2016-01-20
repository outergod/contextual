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

(defun ctx-mode-line ()
  (format " ctx[%s]" (or ctx-active-profile "(none)")))

(defun ctx-activate-profile (name)
  (unless (string= ctx-active-profile name)
    (-let* (((&alist name (&alist 'vars (vars) 'hook (hook))) ctx-profiles))
      (mapc #'(lambda (pair)
                (set (car pair) (cadr pair)))
            vars)
      (funcall hook)
      (setq ctx-active-profile name)
      (message "Loaded profile %s" name))))

(defun ctx-load-profile ()
  (interactive)
  (ctx-activate-profile (completing-read "Profile: " ctx-profiles nil t)))

(defun ctx--add-profile (name profile)
  (add-to-list 'ctx-profiles (cons name profile)))

(cl-defmacro ctx-add-profile (name (&rest vars) &rest body)
  `(ctx--add-profile ,name
     '((vars ,(-partition 2 vars))
       (hook (lambda () ,@body)))))

(defun ctx-set-initial-profile (name)
  (setq ctx-initial-profile-name name)
  (unless ctx-active-profile
    (ctx-activate-profile name)))

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
               (ctx-activate-profile ctx-initial-profile)))
    (run-hooks 'contextual-disabled-hook)))

;;;###autoload
(define-globalized-minor-mode contextual-global-mode contextual-mode contextual-mode)

(provide 'contextual)
;;; contextual.el ends here
