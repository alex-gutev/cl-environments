;;;; declarations.lisp
;;;;
;;;; Copyright 2018 Alexander Gutev
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

;;;; Tests that declarations are being parsed correctly and that the
;;;; declaration information is added to the environment correctly.

(in-package :cl-environments.test)

(plan nil)


;;; Test user-defined declarations

(defun static-dispatch-decl-fn (args env)
  (declare (ignore env))
  (values :function
	  (loop for fn in args
	     collect `(,fn dispatch static))))

(defun dispatch-type-decl-fn (args env)
  (declare (ignore env))
  (destructuring-bind (type &rest vars) args
    (values :variable
	    (loop for var in vars
	       collect `(,var dtype ,type)))))

(defun gc-optimize-decl-fn (args env)
  (declare (ignore env))
  (values :declare (cons 'gc-policy (first args))))


(subtest "User-defined declarations"
  (let ((env (make-instance 'environment)))
    (flet ((test-binding (binding key value)
	     (let ((pair (assoc key binding)))
	       (ok pair (format nil "~s added to declaration info of binding" key))
	       (is (cdr pair) value (format nil "Value of ~s is ~s" key value)))))

      ;; Add a declaration function for the STATIC-DISPATCH declaration

      (setf (declaration-function 'static-dispatch env) #'static-dispatch-decl-fn)

      ;; Test walking user-defined declarations, with a declaration
      ;; function.

      (walk-declaration 'static-dispatch '(f g) env)

      (test-binding (function-binding 'f env) 'dispatch 'static)
      (test-binding (function-binding 'g env) 'dispatch 'static)

      ;; Test user-defined declarations affecting variables

      (setf (declaration-function 'dispatch-type env) #'dispatch-type-decl-fn)

      (walk-declaration 'dispatch-type '(number x y) env)

      (test-binding (variable-binding 'x env) 'dtype 'number)
      (test-binding (variable-binding 'y env) 'dtype 'number)

      ;; Test user-defined declarations not affecting neither
      ;; functions nor variables

      (setf (declaration-function 'gc-optimize env) #'gc-optimize-decl-fn)

      (walk-declaration 'gc-optimize '(1) env)

      (is (declaration-info 'gc-policy env) 1 "GC-POLICY is (1)"))))


;;; Test that multiple DECLARE expressions containing multiple
;;; declarations are parsed and processed correctly.

(subtest "Walking DECLARE expressions"
  (let ((env (make-instance 'environment)))

    (flet ((test-binding (binding key value)
	     (let ((pair (assoc key binding)))
	       (ok pair (format nil "~s added to declaration info of binding" key))
	       (is (cdr pair) value (format nil "Value of ~s is ~s" key value)))))

      ;; Add a declaration function for the STATIC-DISPATCH declaration

      (setf (declaration-function 'static-dispatch env) #'static-dispatch-decl-fn)

      (walk-declarations '((declare (static-dispatch f g) (static-dispatch h))
			   (declare (static-dispatch t))) env)

      (test-binding (function-binding 'f env) 'dispatch 'static)
      (test-binding (function-binding 'g env) 'dispatch 'static)
      (test-binding (function-binding 'h env) 'dispatch 'static)
      (test-binding (function-binding 't env) 'dispatch 'static))))

(finalize)
