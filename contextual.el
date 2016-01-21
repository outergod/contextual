;;; contextual.el --- Contextual profile management system	-*- lexical-binding: t; -*-

;; Copyright (C) 2016 LShift Services GmbH

;; Author: Alexander Kahl <alex@lshift.de>
;; Version: 0.1.0
;; Package-Requires: ((dash "2.12.1"))
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
;; profiles sets global variables and runs hooks to reflect switching
;; the user's identity or the working environment.

;;; Code:

(require 'cl-macs)
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

(define-widget 'ctx-profile-group-type 'lazy
  "Contextual profile group type"
  :offset 4
  :tag "Profile Group"
  :type '(group (ctx-profiles-type :tag "Profiles")
                (string :tag "Active profile name")
                (string :tag "Initial profile name")))

(defcustom ctx-default-profiles '(nil nil nil)
  "Contextual default profiles group"
  :group 'contextual
  :type 'ctx-profile-group-type)

;;; Implementation

(cl-defmacro ctx-define-profiles (name &optional initial)
  "Define new profile group `name'

Optionally, set `initial' value."
  `(defvar ,name '(nil nil ,initial)))

(defun ctx-reset-profiles (profiles)
  "Reset profile group values"
  (setf (nth 0 profiles) nil)
  (setf (nth 1 profiles) nil))

(defun ctx-mode-line ()
  "Contextual mode line formatter"
  (format " ctx[%s]" (or (nth 1 ctx-default-profiles) "(none)")))

(defun ctx-activate-profile (group name)
  "Activate profile `name' in `group'"
  (-let [(profiles active) group]
    (unless (string= active name)
      (-let* (((&alist name (&alist 'vars (vars) 'hook (hook))) profiles))
        (mapc #'(lambda (pair)
                  (set (car pair) (cadr pair)))
              vars)
        (funcall hook)
        (setf (nth 1 group) name)
        (message "Loaded profile %s" name)))))

(defun ctx-profile-loader (group)
  "Create interactive profile loader for `group'

Use this with `ctx-define-profile-loader' to create custom profile
group loaders."
  #'(lambda ()
      (interactive)
      (ctx-activate-profile group (completing-read "Profile: " (car group) nil t))))

(defun ctx--add-profile (profiles name profile)
  "Add new `profile' with `name' to `profiles'"
  (setf (nth 0 profiles)
        (cons (cons name profile)
              (nth 0 profiles))))

(cl-defmacro ctx-add-profile (name (&optional (profiles 'ctx-default-profiles)) (&rest vars) &rest body)
  "Add a new Contextual profile to an existing profile group

Use this function to define new Contextual profiles.
If `profiles' is not set, it will add to the main profile group that
is activated with Contextual's minor mode.

`vars' must be an even number of key/value pairs where each key is the
name of a symbol to be set to the corresponding value when this
profile gets activated.

The body is run unconditionally each time the profile is activated."
  `(ctx--add-profile ,profiles ,name
     '((vars ,vars)
       (hook (lambda () ,@body)))))

(defun ctx-set-initial-profile (name)
  "Set Contextual's initial profile

Only has an effect if run before Contextual's minor mode is
activated. If set during activation, the specified profile will be set
right away."
  (setf (nth 2 ctx-default-profiles) name)
  (unless (nth 1 ctx-default-profiles)
    (ctx-activate-profile ctx-default-profiles name)))

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

(cl-defmacro ctx-define-profile-loader (name group &optional key)
  "Define Contextual profile loader `name'

Pass a profile group created with `ctx-define-profiles' for `group'.
A `key' may be passed to be added to Contextual's keymap for quick
profile switching."
  `(progn
     (defalias ',name (ctx-profile-loader ,group))
     ,(when key
        `(define-key ctx-command-map ,key ',name))))

;; Define the default profile group loader
(ctx-define-profile-loader ctx-load-profile
  ctx-default-profiles (kbd "c"))

;;;###autoload
(define-minor-mode contextual-mode
  "Contextual is an Emacs global minor mode that enables customization
  variables to be changed and hooks to be run whenever a user changes
  her profile."
  nil (:eval (ctx-mode-line))
  :group 'contextual
  :keymap ctx-keymap
  (if contextual-mode
      (-let [(_ active initial) ctx-default-profiles]
        (run-hooks 'contextual-enabled-hook)
        (when (and (not active) initial)
          (ctx-activate-profile ctx-default-profiles initial)))
    (run-hooks 'contextual-disabled-hook)))

;;;###autoload
(define-globalized-minor-mode contextual-global-mode contextual-mode contextual-mode)

;; This beast exists for the simple purpose of coloring and indenting
;; some functions.
(dolist (v '(ctx-add-profile ctx-define-profiles ctx-define-profile-loader))
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
