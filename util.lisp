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
