<img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right">

## iexpand.el
*Expand commands at point*

---
[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Calls interactive functions from a typed symbol. This is cool
because one can just type a symbol, and call `iexpand`, which
will trigger the command if there is one associated with the symbol
at point; the typed symbol will be deleted, restoring buffer state.

Users can define expansion tables in the spirit of `abbrev`
tables — that is, in a per-mode fashion, respecting the hierarchy
of modes.

### Usage


```emacs-lisp
(require 'iexpand)
(setq iexpand-default-key "RET") ; default is RET
(iexpand-global-mode t)
(iexpand-define 'emacs-lisp-mode "eb" #'eval-buffer)
(iexpand-define 'prog-mode "compile" #'compile)
```

Now in an emacs-lisp buffer, typing `eb` and calling
`iexpand` (by default bound to `RET`) will evaluate the buffer.
In that same buffer, as `emacs-lisp-mode` inherits from
`prog-mode`, typing `compile` and calling `iexpand` will
prompt for a compilation command (see `C-h C-f compile`).

One can also define multiple expansions in one run:

```emacs-lisp
(iexpand-define-table 'text-mode
 "xs" #'save-buffer
 "ir" #'indent-region
 "indent-region" #'indent-region
 "indent" #'indent-region)

;; define global expansions by adding to the fundamental-mode table
(iexpand-define-table 'fundamental-mode
 "bb" #'switch-to-buffer
 "file" #'find-file
 "time" (lambda () (interactive) (message (format-time-string "%FT%T")))
 "timestamp" (lambda () (interactive) (insert  (format-time-string "%FT%T"))))

```

See it in action:

![screencast](doc/screencast.gif)

> (For those who care, theme is a combination of [modus
operandi](https://gitlab.com/protesilaos/modus-themes)
and [elegance.el](https://github.com/rougier/elegant-emacs); font
is *Roboto mono*.)

### Installation


```bash
mkdir -p ~/.emacs.d/lib; git clone https://github.com/sam217pa/emacs-iexpand ~/.emacs.d/lib/iexpand
```
then in your `init.el` file:
```emacs-lisp
(add-to-list 'load-path (expand-file-name "lib/iexpand" user-emacs-directory))
(require 'iexpand)
```

### Why should you care?


As has been remarked to me on [reddit](https://www.reddit.com/r/emacs/comments/hbbqnc/new_package_iexpandel_calling_commands_by/fv8ojfe?utm_source=share&utm_medium=web2x),
yasnippet or other packages implements something similar to what iexpand does.
However, triggering arbitrary yet simple emacs-lisp commands from yasnippet requires you to 1) write a snippet with the propper (simple) syntax ([yasnippet documentation](https://joaotavora.github.io/yasnippet/snippet-development.html#orgcde188c)), 2) save it to an appropriate place, 3) have yasnippet properly load the snippet (which it does automatically normally).
It does work, but I do think that it is not what yasnippet was primarily intended to do, which is to expand plain text, with some assistance from emacs-lisp when need be.

Iexpand take it the other way around: it is primarily intended to evaluate emacs-lisp commands, which incidentally allows it to expand some simple text snippets.
But I would keep writing snippets for anything more complex than what is displayed in the screencast (the `timestamp` snippet).

Another interesting aspect to me is that you can give plain text orders to emacs, say `save`, press return: it saves the buffer.
```emacs-lisp
(iexpand-define 'prog-mode "stage" #'magit-stage-file)
```
type `stage`, press return, the file is staged.
For some reason this “workflow” suits me well, maybe it'll suit you too.

### Function Documentation


#### `(iexpand-define-table MODE &rest BODY)`

Wrapper around ‘iexpand-define’ for defining multiple KEY-COMMAND expansions.

BODY consists of KEY-COMMAND pairs.

Example:
(iexpand-define-table ’emacs-lisp-mode
  "xs" #’save-buffer
  "hw" (lambda (interactive) (message "hello world")))

#### `(iexpand-define MODE KEY COMMAND)`

Define an expansion for COMMAND associated with KEY for MODE.

Calling ‘iexpand’ when point is after KEY in major-mode
MODE triggers calling COMMAND interactively.

#### `(iexpand &optional ARG)`

Call command associated with symbol at point.

#### `(iexpand-describe)`

Describes the expansions associated with current ‘major-mode’.

#### `(turn-on-iexpand-minor-mode)`

Simple wrapper around ‘iexpand-minor-mode’

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
