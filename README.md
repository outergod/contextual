# Contextual Profile Management System
Contextual is a profile management system for GNU Emacs. A profile is made up by
an arbitrary number of global variables that influence how Emacs behaves, as
well as custom code. Upon switching profiles, the variables are adapted and the
custom profile code is run, if present. Contextual is based on Emacs'
custom theme system for this purpose.
Typical use cases for such profiles are user identifying variables such as
`user-mail-address` or `epg-user-id` which need to be changed when using the
same instance of Emacs for private and professional purposes.
Other use cases can, for instance, involve switching to different themes or
fonts to adapt to different screens of light conditions.

## Installation
Contextual is available via ELPA, from the
[MELPA repository](https://github.com/milkypostman/melpa#usage).
Running `M-x package-install RET contextual RET` is usually the quickest way to
retrieve Contextual, given that MELPA has been made available as a package
repository.

## Usage
Contextual can be used in two ways: Through its default context and
through custom contexts, which can be used independently from each
other. A context consists of a list of profiles to switch between, as
well as meta data. A profile is comprised of a custom theme and a
hook, which may be empty.

## Basic setup
Basic setup consists of the following steps:

1. Loading Contextual
2. Adding one or more profiles
3. (optional, recommended) specifying an initial profile to load
4. Activating Contextual

```el
(require 'contextual)

(contextual-add-profile "private" ()
  ((user-mail-address "john@doe.name")
   (epg-user-id "1234ABCD")))

(contextual-add-profile "work" ()
  ((user-mail-address "jdoe@megacorp.inc")
   (epg-user-id "5678EFGH")))

(contextual-set-initial-profile "private")

(contextual-global-mode) ; activates the initial profile right away
```

The format for the variable forms is that of `custom-theme-set-variables`.

Switching between profiles in the default context is bound to
`contextual-load-profile`, or `c` in the Contextual prefix map. The default prefix for
Contextual is <kbd>s-c</kbd><sup>super</sup>, so the default key combination is
<kbd>s-c c</kbd>. Contextual provides completion candidates and is
therefore compatible with completion framework such as HELM.

Contextual minor mode is indicated in the mode line by `contextual`
followed by the currently active profile name in brackets,
e.g. `contextual[private]`.

The keymap prefix <kbd>s-c</kbd> can be changed by setting
`contextual-keymap-prefix`:

```el
(setq contextual-keymap-prefix (kbd "..."))
```

<sup>super</sup>The _Super key "s"_ is mapped to the Windows key on Windows
keyboards and the Hyper key on Mac keyboards.

## Custom contexts
Defining custom contexts is useful to define variables to be used
independently from the default context. The following example defines
profiles for different font sizes, which needs custom code to be run.

```el
(contextual-define-profiles font-profiles "normal")

(contextual-add-profile "small" (font-profiles) ((default-frame-alist '((font . "Inconsolata-6"))))
  (set-frame-font "Inconsolata-6"))
(contextual-add-profile "normal" (font-profiles) ((default-frame-alist '((font . "Inconsolata-9"))))
  (set-frame-font "Inconsolata-9"))

(contextual-define-profile-loader font-profile-loader
  font-profiles (kbd "f"))
```

The first form binds a new variable `font-profiles` with an initial profile
of `normal`. `contextual-add-profile` takes the context as an optional parameter
in the first pair of parens. Without the parameter, the profiles would instead
be added to the default context. `set-frame-font` is run every time the
respective profile is being activated.

`contextual-define-profile-loader` binds a new interactive function for a
context and assigns it a key in Contextual's keymap. Here,
`font-profile-loader` is the name of the new symbol to bind the loader
to, `font-profiles` was defined earlier and `(kbd "f")` specifies the
key to bind to.

After running this code, the `normal` font profile would automatically be
preloaded and `M-x font-profile-loader RET` or <kbd>s-c f</kbd> can
be used to switch between font profiles.

## API
The public portion of the API which can be used safely as long as the major version
of the package doesn't change is comprised of the following functions:

- `contextual-add-profile NAME (&optional (PROFILES 'contextual-default-context)) (&rest VARS) &rest BODY`
- `contextual-set-initial-profile NAME`
- `defcontext CONTEXT &optional INITIAL`
- `contextual-global-mode`

Additional functions that may be of use but which are not guaranteed to be
stable are:

- `contextual-activate-profile CONTEXT NAME`

## Copyright
Copyright © 2016 LShift Services GmbH

Contextual is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Contextual is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
