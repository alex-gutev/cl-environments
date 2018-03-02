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

;;; Tests TYPE declarations (variable types) and FTYPE declarations
;;; (function types).

(subtest "TYPE and FTYPE declarations"
  (let ((env (make-instance 'environment)))
    (labels ((test-var-binding (var type var-type)
	     (let ((binding (variable-binding var env)))	     
	       (ok binding (format nil "Variable binding ~s added" var))

	       (when binding
		 (is (type binding) type (format nil "Binding type of ~s is ~s" var type))
		 (is (cdr (assoc 'type (declarations binding))) var-type
		     (format nil "Declared type of ~s is ~s" var var-type)))))
	   
	     (test-fn-binding (fn type fn-type)
	       (let ((binding (function-binding fn env)))
		 (ok binding (format nil "Function binding ~s added" fn))

		 (when binding
		   (is (type binding) type (format nil "Binding type of ~s is ~s" fn type))
		   (is (cdr (assoc 'ftype (declarations binding))) fn-type
		       (format nil "Declared type of ~s is ~s" fn fn-type))))))
	     
	   
      (add-variable 'x env :type :lexical :local t)
      (add-function 'x env :type :function :local t)
  
      (walk-declaration 'type '(number x y z) env)
      (walk-declaration 'ftype '((function (integer) number) x y) env)

      (test-var-binding 'x :lexical 'number)
      (test-var-binding 'y nil 'number)
      (test-var-binding 'z nil 'number)

      (test-fn-binding 'x :function '(function (integer) number))
      (test-fn-binding 'y nil '(function (integer) number)))))


;;; Tests local and global SPECIAL declarations.

(subtest "SPECIAL declarations"
  (let ((env (make-instance 'environment)))
    (flet ((test-binding (var type local)
	     (let ((binding (variable-binding var env)))
	       (ok binding (format nil "Binding ~s added" var))
	       (when binding
		 (is (type binding) type (format nil "~s is ~s" var type))
		 (is (local binding) local (format nil "~s is local: ~s" var local))))))
	
      (add-variable 'x env :type :lexical :local t)

      ;; Lexical bound and free SPECIAL declarations
      
      (walk-declaration 'special '(x y) env)

      ;; Test that a lexical bound SPECIAL declaration changes the
      ;; type of the existing binding to :SPECIAL however does not
      ;; modify the rest of the binding's slots.
      
      (test-binding 'x :special t)
      
      ;; Test that a lexical free SPECIAL declaration creates a new
      ;; (non-local) binding
      
      (test-binding 'y :special nil)

      ;; Global SPECIAL declarations
      
      ;; Test that a global :SPECIAL declaration creates a new global
      ;; binding.
      
      (walk-declaration 'special '(z) env t)
    
      (add-variable 'x env :type :lexical :local t)
      (add-variable 'y env :type :lexical :local t)
      (add-variable 'z env :type :lexical :local t)

      ;; Test that when adding a lexical variable, and a non-global
      ;; special variable exists, the added variable is lexical.
      
      (test-binding 'x :lexical t)
      (test-binding 'y :lexical t)

      ;; Test that when adding a lexical variable, and a global
      ;; special variable exists, the added variable is special.
      
      (test-binding 'z :special t))))


;;; Tests DYNAMIC-EXTENT declarations

(subtest "DYNAMIC-EXTENT declaratins"
  (let ((env (make-instance 'environment)))
    (flet ((test-binding (sym binding dynamic-extent)
	     (when binding
	       (is (cdr (assoc 'dynamic-extent (declarations binding))) dynamic-extent
		   (format nil "DYNAMIC-EXTENT of ~s is ~s" sym dynamic-extent)))))
		 

    (add-variable 'x env)
    (add-variable 'f env)
    (add-function 'f env)

    (walk-declaration 'dynamic-extent '(x (function f)) env)

    ;; Test that (DYNAMIC-EXTENT . T) is added to the declaration
    ;; information both for variable X and function F

    (test-binding 'x (variable-binding 'x env) t)
    (test-binding 'f (function-binding 'f env) t)
    (test-binding 'f (variable-binding 'f env) nil))))


;;; Tests IGNORE declarations.

(subtest "IGNORE declarations"
  (let ((env (make-instance 'environment)))
    (flet ((test-binding (var ignore)
	     (let ((binding (variable-binding var env)))
	       (is (cdr (assoc 'ignore (declarations binding))) ignore
		   (format nil "~s ignored: ~s" var ignore)))))
    
    (add-variable 'x env)
    (add-variable 'y env)
    (add-variable 'z env)

    (walk-declaration 'ignore '(x y) env)

    (test-binding 'x t)
    (test-binding 'y t)
    (test-binding 'z nil))))


;;; Test INLINE and NOTINLINE declarations

(subtest "INLINE and NOTINLINE declarations"
  (let ((env (make-instance 'environment)))
    (flet ((test-binding (fn inline)
	     (let ((binding (function-binding fn env)))
	       (is (cdr (assoc 'inline (declarations binding))) inline
		   (format nil "~s inline: ~s" fn inline)))))
    
    (add-function 'f env)
    (add-function 'g env)
    (add-function 'h env)

    (walk-declaration 'inline '(f) env)
    (walk-declaration 'notinline '(g) env)

    (test-binding 'f 'inline)
    (test-binding 'g 'notinline)
    (test-binding 'h nil))))


;;; Test OPTIMIZE declarations

(subtest "OPTIMIZE declarations"
  (let ((env (make-instance 'environment)))
    (labels ((count-quality (quality list)
	       (count quality list :key #'first))
	       
	     (test-qualities (expected)
	       (let ((qualities (declaration-info 'optimize env)))
		 
		 (ok (every (rcurry #'count-quality qualities) +optimize-qualities+)
		     "No duplicate optimize qualities")

		 (ok (subsetp expected qualities :test #'equal)
		     (format nil "Optimize qualities are: ~s" expected)))))

      ;; Test abbreviated OPTIMIZE qualities
      
      (walk-declaration 'optimize '(speed safety compilation-speed space debug) env)
      (test-qualities '((speed 3) (safety 3) (compilation-speed 3) (space 3) (debug 3)))

      ;; Test changin OPTIMIZE quality values

      (walk-declaration 'optimize '((safety 0) (space 1)) env)
      (test-qualities '((speed 3) (safety 0) (compilation-speed 3) (space 1) (debug 3))))))


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
	     (let ((pair (assoc key (declarations binding))))
	       (ok pair (format nil "~s added to declaration info of binding" key))
	       (is (cdr pair) value (format nil "Value of ~s is ~s" key value)))))
      
      ;; Test that user/implementation-defined declarations (which are
      ;; declared so using the DECLARATION declaration) are added to the
      ;; list of known declarations
      
      (walk-declaration 'declaration '(static-dispatch) env t)
      (ok (member 'static-dispatch (declaration-info 'declaration env))
	  "Declaration STATIC-DISPATCH added to list of known declarations")

      ;; Test that walking a non-standard declaration for which there is
      ;; no function does nothing, currently it is only checked that
      ;; walking such a declaration does not signal an error.

      (walk-declaration 'static-dispatch '(f g) env)

      ;; Add a declaration function for the STATIC-DISPATCH declaration
      
      (setf (declaration-function 'static-dispatch env) #'static-dispatch-decl-fn)

      ;; Test walking user-defined declarations, with a declaration
      ;; function.

      (add-function 'f env)
      (add-function 'g env)

      (walk-declaration 'static-dispatch '(f g) env)

      (test-binding (function-binding 'f env) 'dispatch 'static)
      (test-binding (function-binding 'g env) 'dispatch 'static)

      ;; Test user-defined declarations affecting variables

      (setf (declaration-function 'dispatch-type env) #'dispatch-type-decl-fn)

      (add-variable 'x env)
      (add-variable 'y env)

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
    (flet ((test-binding (var binding-type var-type)
	     (let ((binding (variable-binding var env)))
	       (with-slots (type declarations) binding
		 (is type binding-type
		     (format nil "Binding type of ~s is ~s" var binding-type))
		 (is (cdr (assoc 'type declarations)) var-type
		     (format nil "~s is of type: ~s" var var-type))))))
      
      (walk-declarations '((declare (special x y) (type number x))
			   (declare (type string y))) env)

      (test-binding 'x :special 'number)
      (test-binding 'y :special 'string))))

(finalize)
