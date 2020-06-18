;;; iexpand.el --- Expand commands at point ---                     -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020  Samuel Barreto
;; License: GPLv3
;;
;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Maintainer: Samuel barreto <samuel.barreto8@gmail.com>
;; Keywords: expansion, convenience
;; Version: 0.1
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Calls interactive functions from a typed symbol. This is cool
;; because one can just type a symbol, and call `iexpand`, which
;; will trigger the command if there is one associated with the symbol
;; at point; the typed symbol will be deleted, restoring buffer state.
;;
;; Users can define expansion tables in the spirit of `abbrev`
;; tables — that is, in a per-mode fashion, respecting the hierarchy
;; of modes.
;;
;;; Usage:
;;
;; ```emacs-lisp
;; (require 'iexpand)
;; (iexpand-global-mode t)
;; (iexpand-define 'emacs-lisp-mode "eb" #'eval-buffer)
;; (iexpand-define 'prog-mode "compile" #'compile)
;; ```
;;
;; Now in an emacs-lisp buffer, typing `eb` and calling
;; `iexpand` (by default bound to `RET`) will evaluate the buffer.
;; In that same buffer, as `emacs-lisp-mode` inherits from
;; `prog-mode`, typing `compile` and calling `iexpand` will
;; prompt for a compilation command (see `C-h C-f compile`).
;;
;; One can also define multiple expansions in one run:
;;
;; ```emacs-lisp
;; (iexpand-define-table 'text-mode
;;  "xs" #'save-buffer
;;  "ir" #'indent-region
;;  "indent-region" #'indent-region
;;  "indent" #'indent-region)
;;
;; ;; define global expansions by adding to the fundamental-mode table
;; (iexpand-define-table 'fundamental-mode
;;  "bb" #'switch-to-buffer
;;  "file" #'find-file
;;  "time" (lambda () (interactive) (message (format-time-string "%FT%T")))
;;  "timestamp" (lambda () (interactive) (insert  (format-time-string "%FT%T"))))
;;
;; ```
;;
;; See it in action:
;;
;; ![screencast](doc/screencast.gif)
;;
;; > (For those who care, theme is a combination of [modus
;; operandi](https://gitlab.com/protesilaos/modus-themes)
;; and [elegance.el](https://github.com/rougier/elegant-emacs); font
;; is *Roboto mono*.)
;;
;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(require 'thingatpt)

;; CUSTOM ------------------------------------------------------------

(defcustom iexpand-default-key "RET"
  "Default keybinding for calling `iexpand'"
  :group 'iexpand
  :type 'string)


;; CORE --------------------------------------------------------------

(defun iexpand--group (source n)
  "Divide SOURCE list in N groups and stack together the last
elements. (from P. Graham's On Lisp.)"
  (if (zerop n) (error "Zero length"))
  (cl-labels ((rec (source acc)
                   (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons (cl-subseq source 0 n) acc))
                       (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun iexpand--mode-name (mode)
  "Return the name of the iexpand-table associated with MODE."
  (intern (format "iexpand--%s-table"
                  (string-trim (symbol-name mode) nil "-mode"))))

(defun iexpand--get-table (mode)
  "Get the iexpand-table associated with MODE."
  (get (iexpand--mode-name mode) 'table))

(defun iexpand--make-table (mode)
  "Create an iexpand-table for MODE if none exists."
  (or (iexpand--get-table mode)
      (put (iexpand--mode-name mode) 'table (make-hash-table :test #'equal))))

(defmacro iexpand-define-table (mode &rest body)
  "Wrapper around `iexpand-define' for defining multiple KEY-COMMAND expansions.

BODY consists of KEY-COMMAND pairs.

Example:
(iexpand-define-table 'emacs-lisp-mode
  \"xs\" #'save-buffer
  \"hw\" (lambda (interactive) (message \"hello world\")))"
  (declare (indent 1))
  `(progn
     (iexpand--make-table ,mode)
     ,@(mapcar
        (lambda (x) `(iexpand-define ,mode ,(car x) ,(cadr x)))
        (iexpand--group body 2))))

(defun iexpand-define (mode key command)
  "Define an expansion for COMMAND associated with KEY for MODE.

Calling `iexpand' when point is after KEY in major-mode
MODE triggers calling COMMAND interactively."
  (let ((iexpand-table (iexpand--make-table mode)))
    (puthash key command iexpand-table)))

(defun iexpand--get-cmd (key)
  "Return command associated with KEY in iexpand tables up the
hierarchy of modes from major-mode to fundamental-mode.

Return nil if no key was found.
Major-mode table is searched first, fundamental last."
  (cl-labels ((cmd (table) (and (hash-table-p table) (gethash key table)))
              (f (mode)
                 (or (cmd (iexpand--get-table mode))
                     (when-let ((parent-mode (get mode 'derived-mode-parent)))
                       (f parent-mode)))))
    (or (f major-mode)
        (f 'fundamental-mode))))

(defun iexpand--symbol ()
  "Call command associated with symbol at point."
  (when-let ((be (bounds-of-thing-at-point 'symbol)))
    (when-let ((cmd (iexpand--get-cmd (buffer-substring-no-properties (car be) (cdr be)))))
      (unwind-protect
          (progn (delete-region (car be) (cdr be)) t)
        (call-interactively cmd)))))

(defun try-iexpand (old)
  (unless old (iexpand--symbol)))

(defvar-local iexpand--fallback-cmd nil
  "Command to call when no expansion was found")

(defun iexpand--get-expanding-key ()
  (car (where-is-internal 'iexpand iexpand-mode-map)))

(defun iexpand--set-fallback-behaviour ()
  (setq-local iexpand--fallback-cmd
              (lookup-key (current-global-map) (iexpand--get-expanding-key))))

(defun iexpand (&optional arg)
  "Call command associated with symbol at point."
  (interactive "p")
  (or (iexpand--symbol)
      (call-interactively iexpand--fallback-cmd arg)))

;; DOCUMENTATION -----------------------------------------------------

(defun iexpand--get-derived-modes ()
  "Returns the parents of major mode up to fundamental mode."
  (cl-labels ((f (mode)
                 (if-let ((parent-mode (get mode 'derived-mode-parent)))
                     (cons mode (f parent-mode))
                   (cons mode (cons 'fundamental-mode nil)))))
    (f major-mode)))

(defun iexpand-describe ()
  "Describes the expansions associated with current `major-mode'."
  (interactive)
  (with-help-window (help-buffer)
    (princ "[Iexpand Help]

Commands associated with current major-mode in iexpand:

")
    (mapc
     (lambda (mode) (when-let ((h (iexpand--get-table mode)))
                 (princ (format "MODE: %s\n" (symbol-name mode)))
                 (maphash (lambda (k v) (princ (format "%20s\t%s\n" k v))) h)))
     (iexpand--get-derived-modes))))

;; KEYBINDINGS -------------------------------------------------------

(defvar iexpand-mode-map (make-sparse-keymap))

(defun iexpand--define-key (key)
  "Correctly bind KEY to `iexpand', erasing previous bindings
made to it."
  ;; reset for erasing previous definition
  (setq iexpand-mode-map (make-sparse-keymap))
  (define-key iexpand-mode-map (kbd key) #'iexpand))

(iexpand--define-key iexpand-default-key)

;;;###autoload
(define-minor-mode iexpand-minor-mode
  "A simple minor mode that leverages typed text to call
interactive commands."
  nil "iexp"
  :keymap iexpand-mode-map
  (when iexpand-minor-mode (iexpand--set-fallback-behaviour)))

;;;###autoload
(defun turn-on-iexpand-minor-mode ()
  "Simple wrapper around `iexpand-minor-mode'"
  (interactive)
  (unless (provided-mode-derived-p major-mode '(help-mode minibuffer-inactive-mode calc-mode))
    (iexpand-minor-mode t)))

;;;###autoload
(define-globalized-minor-mode iexpand-global-mode iexpand-minor-mode
  turn-on-iexpand-minor-mode)

(provide 'iexpand)
;;; iexpand.el ends here
