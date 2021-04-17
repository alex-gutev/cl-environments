;;;; hook.lisp
;;;;
;;;; Copyright 2017 Alexander Gutev
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

;;;; Adds a macro expansion hook

(in-package :cl-environments.cltl2)

(defvar *previous-hook* #'funcall
  "Contains the previous *MACROEXPAND-HOOK* function, prior to calling
   ENABLE-HOOK.")


(defun pre-expand-walk (form)
  "If FORM is a function macro-form and the macro symbol is a member
   of the list +WALK-MACROS+, walks the form, otherwise returns FORM
   as is."

  (match form
    ((list* (guard op (member op +walk-macros+)) _)
     (walk-form form))

    (_ form)))

(defun walker-hook (fn form *env*)
  "Macro-expansion hook function. Walks the result of the expansion of
   FORM."

  (let ((*macroexpand-hook* *previous-hook*))
    (let* ((form (pre-expand-walk form))
	   (expansion (funcall *previous-hook* fn form *env*)))

      (match form
	((list* (not '%walk-form) _)

	 (let ((walked-form (walk-form expansion)))
	   (if (equal walked-form form)
	       form
	       walked-form)))

	(_ expansion)))))

(defun enable-hook (&optional (previous-hook *macroexpand-hook*))
  "Sets the code-walker as the macro-expansion hook, this allows
   information about the lexical-environment to be stored and
   retrieved later. The optional PREVIOUS-HOOK argument is the next
   hook function to call after the current hook is enabled."

  (setf *previous-hook* previous-hook)
  (setf *macroexpand-hook* #'walker-hook))

(defun disable-hook (&optional (previous-hook *previous-hook*))
  "Restores the macro-expansion hook to FUNCALL, thus disabling the
   top-level form code-walker. Lexical-environment information will no
   longer be stored and thus will no longer be retrievable. The
   optional PREVIOUS-HOOK argument is the value to set
   *MACROEXPAND-HOOK* to. By default it is bound to *PREVIOUS-HOOK*
   which contains the value of the PREVIOUS-HOOK argument to
   ENABLE-HOOK."

  (setf *macroexpand-hook* previous-hook))
