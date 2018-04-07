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

(in-package :cl-environments)

(defconstant +walk-macros+
  '(cl:defun
    cl:defgeneric
    cl:defmethod
    cl:defparameter
    cl:defvar
    cl:defmacro
    cl:define-symbol-macro
    cl:declaim)

  "List of macros which should be walked prior expansion.")

(defun pre-expand-walk (form env)
  "If FORM is a function macro-form and the macro symbol is a member
   of the list +WALK-MACROS+, walks the form, otherwise returns FORM
   as is."
  
  (match form
    ((list* (guard op (member op +walk-macros+)) _)
     (walk-form form env))

    (_ form)))


;; TODO: Call the previous *MACROEXPAND-HOOK*, requires that
;; *OLD-MACROEXPAND-HOOK* be set correctly first

(defun walker-hook (fn form env)
  "Macro-expansion hook function. Walks the result of the expansion of
   FORM."
  
  (let* ((form (pre-expand-walk form env))
	 (expansion (funcall fn form env)))
    (match form
      ((list* (not '%walk-form) _)
       (walk-form expansion env))
      
      (_ expansion))))

(defun enable-hook ()
  "Sets the code-walker as the macro-expansion hook, this allows
   information about the lexical-environment to be stored and
   retrieved later."
  
  (setf *macroexpand-hook* #'walker-hook))

;; TODO: Restore to previous value, requires that
;; *OLD-MACROEXPAND-HOOK* be set correctly first

(defun disable-hook ()
  "Restores the macro-expansion hook to FUNCALL, thus disabling the
   top-level form code-walker. Lexical-environment information will no
   longer be stored and thus will no longer be retrievable."
  
  (setf *macroexpand-hook* #'funcall))
