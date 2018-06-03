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


(defun gensyms (syms &key (key #'identity))
  (mapcar (compose #'gensym #'symbol-name (curry #'funcall key)) syms))

(defun reexport-all-symbols (from-package)
  (let ((shadowed (mapcar #'symbol-name (package-shadowing-symbols *package*))))
    (do-external-symbols (sym from-package)
      (export (if (member (symbol-name sym) shadowed :test #'string=)
		  (find-symbol (symbol-name sym) *package*)
		  (list sym))
	      *package*))))

(defmacro! let-if ((&rest bindings) condition &body body)
  "Allows variables to be initialized with different init-forms based
   on a condition. BINDINGS is a list of bindings where the first
   element is the symbol the second element is the init-form to
   evaluated if CONDITION evaluates to true, the second element is the
   init-form to evaluate if CONDITION evaluates to false."

  (labels ((make-setf (sym value)
	     `(setf ,sym ,value))
	   (make-binding (binding sym)
	     `(,(first binding) ,sym)))
    
    (let ((gensyms (gensyms bindings :key #'car)))
      `(let ((,g!test ,condition) ,@gensyms)
	 (if ,g!test
	     (progn
	       ,@(mapcar #'make-setf gensyms (mapcar #'second bindings)))
	     (progn
	       ,@(mapcar #'make-setf gensyms (mapcar #'third bindings))))
	 (let
	     ,(mapcar #'make-binding bindings gensyms)
	   ,@body)))))

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
