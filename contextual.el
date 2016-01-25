;;; contextual.el --- Contextual profile management system	-*- lexical-binding: t; -*-

;; Copyright (C) 2016 LShift Services GmbH

;; Author: Alexander Kahl <alex@lshift.de>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (dash "2.12.1") (cl-lib "0.5"))
;; Keywords: convenience, tools
;; URL: https://github.com/lshift-de/contextual

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

;; Contextual provides profiles support for Emacs. Switching between
;; contexts sets global variables and runs hooks to reflect switching
;; the user's identity or the working environment.

;;; Code:

(require 'cl-lib)
(require 'dash)

;;; Types and customization

(defgroup contextual nil
  "Contextual global minor mode"
  :group 'convenience
  :prefix "ctx-")

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

(define-widget 'ctx-profile-type 'lazy
  "Contextual profile type"
  :offset 4
  :tag "Profile"
  :options '(vars hook)
  :type '(alist))

(define-widget 'ctx-profiles-type 'lazy
  "Contextual profiles type"
  :offset 4
  :tag "Profiles"
  :options '(vars hook)
  :type '(alist :key-type string :value-type ctx-profile-type))

(define-widget 'ctx-context-type 'lazy
  "Contextual context type"
  :offset 4
  :tag "Context"
  :type '(group (ctx-profiles-type :tag "Profiles")
                (string :tag "Active profile name")
                (string :tag "Initial profile name")))

(defcustom ctx-default-context '(nil nil nil)
  "Contextual default context."
  :group 'contextual
  :type 'ctx-context-type)

;;; Implementation

(cl-defmacro ctx-define-context (name &optional initial)
  "Define new context `name'

Optionally, set `initial' value."
  `(defvar ,name '(nil nil ,initial)))

(defun ctx-reset-context (context)
  "Reset CONTEXT's values."
  (setf (nth 0 context) nil)
  (setf (nth 1 context) nil))

(defun ctx-mode-line ()
  "Contextual mode line formatter."
  (format " ctx[%s]" (or (nth 1 ctx-default-context) "(none)")))

(defun ctx-activate-profile (context name)
  "Activate profile NAME in CONTEXT."
  (-let [(profiles active) context]
    (unless (string= active name)
      (-let* (((&alist name (&alist 'vars (vars) 'hook (hook))) profiles))
        (mapc #'(lambda (pair)
                  (set (car pair) (cadr pair)))
              vars)
        (funcall hook)
        (setf (nth 1 context) name)
        (message "Loaded profile %s" name)))))

(defun ctx-context-loader (context)
  "Create interactive profile loader for CONTEXT.

Use this with `ctx-define-context-loader' to create custom context loaders."
  #'(lambda ()
      (interactive)
      (ctx-activate-profile context (completing-read "Profile: " (car context) nil t))))

(defun ctx--add-profile (context name profile)
  "Add new PROFILE with NAME to CONTEXT."
  (setf (nth 0 context)
        (cons (cons name profile)
              (nth 0 context))))

(cl-defmacro ctx-add-profile (name (&optional (context 'ctx-default-context)) (&rest vars) &rest body)
  "Add a new Contextual profile to an existing context.

Use this function to define a new context.
If `context' is not set, it will add to the main context that
is activated with Contextual's minor mode.

`vars' must be an even number of key/value pairs where each key is the
name of a symbol to be set to the corresponding value when this
profile gets activated.

The body is run unconditionally each time the profile is activated."
  `(ctx--add-profile ,context ,name
     '((vars ,vars)
       (hook (lambda () ,@body)))))

(defun ctx-set-initial-profile (name)
  "Set Contextual's initial profile to NAME.

Only has an effect if run before Contextual's minor mode is
activated.  If set during activation, the specified profile will be set
right away."
  (setf (nth 2 ctx-default-context) name)
  (unless (nth 1 ctx-default-context)
    (ctx-activate-profile ctx-default-context name)))

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

(cl-defmacro ctx-define-context-loader (name context &optional key)
  "Define Contextual profile loader `name'

Pass a context created with `ctx-define-context' for `context'.
A `key' may be passed to be added to Contextual's keymap for quick
profile switching."
  `(progn
     (defalias ',name (ctx-context-loader ,context))
     ,(when key
        `(define-key ctx-command-map ,key ',name))))

;; Define the default context loader
(ctx-define-context-loader ctx-load-profile
  ctx-default-context (kbd "c"))

;;;###autoload
(define-minor-mode contextual-mode
  "Contextual is an Emacs global minor mode that enables customization
  variables to be changed and hooks to be run whenever a user changes
  her profile."
  nil (:eval (ctx-mode-line))
  :group 'contextual
  :keymap ctx-keymap
  (if contextual-mode
      (-let [(_ active initial) ctx-default-context]
        (run-hooks 'contextual-enabled-hook)
        (when (and (not active) initial)
          (ctx-activate-profile ctx-default-context initial)))
    (run-hooks 'contextual-disabled-hook)))

;;;###autoload
(define-globalized-minor-mode contextual-global-mode contextual-mode contextual-mode)

;; This beast exists for the simple purpose of coloring and indenting
;; some functions.
(dolist (v '(ctx-add-profile ctx-define-context ctx-define-context-loader))
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
