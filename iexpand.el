;;; iexpand.el --- Expand commands at point ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Samuel Barreto
;; License: GPLv3

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Maintainer: Samuel barreto <samuel.barreto8@gmail.com>
;; Keywords: expansion, convenience
;; Version: 0.1

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

;;; Commentary:

;; Add an expansion to hippie-expand that allows calling functions from a typed symbol.

;;; Do something


;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(require 'hippie-exp)
(require 'thingatpt)

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
  \"hw\" (lambda (interactive) (message \"hello world\")))
"
  (declare (indent 1))
  `(progn
     (iexpand--make-table ,mode)
     ,@(mapcar
        (lambda (x) `(iexpand-define ,mode ,(car x) ,(cadr x)))
        (iexpand--group body 2))))

(defun iexpand-define (mode key command)
  "Define an expansion for COMMAND associated with KEY for MODE.

Calling `hippie-expand' when point is after KEY in major-mode
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
    (he-init-string (car be) (cdr be))
    (when-let ((cmd (iexpand--get-cmd he-search-string)))
      (unwind-protect
          (progn (he-substitute-string "") t)
        (call-interactively cmd)))))

(defun try-iexpand (old)
  (unless old (iexpand--symbol)))

;;;###autoload
(define-minor-mode iexpand-minor-mode
  "A simple minor mode that leverages `hippie-expand'
to call interactive commands."
  nil "iexp" nil
  (if iexpand-minor-mode
      (add-to-list 'hippie-expand-try-functions-list 'try-iexpand)
    (setq hippie-expand-try-functions-list
          (delq 'try-iexpand hippie-expand-try-functions-list))))

;;;###autoload
(defun turn-on-iexpand-minor-mode ()
  "Simple wrapper around `iexpand-minor-mode'"
  (interactive)
  (iexpand-minor-mode t))

;;;###autoload
(define-globalized-minor-mode iexpand-global-mode iexpand-minor-mode
  turn-on-iexpand-minor-mode)

(provide 'iexpand)
;;; iexpand.el ends here
