;;;; util.lisp
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

(in-package :cl-environments.util)

(in-readtable lol-syntax)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *debug-type-errors* nil
    "If true the IGNORE-TYPE-ERRORS macro will simply expand into a
     PROGN thus type-errors occurring within the body of the macro
     will not be ignored. This is used to debug type-errors not
     related to malformed lists in the code being walked."))

(defun gensyms (syms &key (key #'identity))
  (mapcar (compose #'gensym #'symbol-name (curry #'funcall key)) syms))

(defmacro! let-if ((&rest bindings) condition &body body)
  "Allows variables to be initialized with different init-forms based
   on a condition. BINDINGS is a list of bindings where the first
   element is the symbol the second element is the init-form to
   evaluated if CONDITION evaluates to true, the second element is the
   init-form to evaluate if CONDITION evaluates to false."

  (let ((gensyms (gensyms bindings :key #'car)))
    `(let ((,g!test ,condition) ,@gensyms)
       (if ,g!test
       	   (progn
       	     ,@(mapcar #2`(setf ,a1 ,(second a2)) gensyms bindings))
       	   (progn
       	     ,@(mapcar #2`(setf ,a1 ,(third a2)) gensyms bindings)))
       (let
       	   ,(mapcar #2`(,(first a1) ,a2) bindings gensyms)
	 ,@body))))

(defmacro! slot-values ((&rest slots) o!form &body body)
  (flet ((parse-slot (slot)
	  (if (listp slot)
	      slot
	      (list slot slot))))
    `(let-if
	 ,(loop
	     for slot in slots
	     for (var slot-name) = (parse-slot slot)
	     collect `(,var (slot-value ,g!form ',slot-name)))
	 ,g!form
       ,@body)))


(defmacro! ignore-type-errors ((&optional fail-result) &body forms)
  "Catches error conditions of type TYPE-ERROR signalled from FORMS
   and returns FAIL-RESULT. If no error condition is signaled the
   value returned by the last form in FORMS is returned."

  (if *debug-type-errors*
      `(progn ,@forms)
      
      `(handler-case
	   (progn ,@forms)
	 (type-error () ,fail-result))))


(defun mapc-ite (fn list &rest more-lists)
  "Same as MAPC however ignores type errors."

  (ignore-type-errors (list)
    (apply #'mapc fn list more-lists)))


(defmacro! dolist-ite ((var list &optional o!result) &body forms)
  "Same as DOLIST however ignores type errors."

  `(ignore-type-errors (,g!result)
     (dolist (,var ,list ,g!result)
       ,@forms)))
