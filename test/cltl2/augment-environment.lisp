;;;; augment-environment.lisp
;;;;
;;;; Copyright 2021 Alexander Gutev
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

;;;; Test augmented environments created using AUGMENT-ENVIRONMENT

(defpackage :cl-environments.test.cltl2.augment-environment
  (:use :cl-environments-cl
	:cl-environments.test.cltl2
	:alexandria
	:fiveam))

(in-package :cl-environments.test.cltl2.augment-environment)

(def-suite augment-environment
    :description "Test augmenting environments with information."
    :in cltl2-test)

(in-suite augment-environment)


;;; Test Utilities

(defun expansion= (expected got)
  "Test that the macroexpansion result GOT is equal to EXPECTED.

   Both GOT and EXPECTED are expected to be lists of two elements
   where the first element is the macroexpanded form and second
   element is a generalized boolean which is true if the form was
   macroexpanded."

  (destructuring-bind (got-form got-expanded-p) got
    (destructuring-bind (expected-form expected-expanded-p) expected
      (and (equal got-form expected-form)
	   (not (xor got-expanded-p expected-expanded-p))))))

(defun optimize= (expected got)
  "Test that the OPTIMIZE declaration information GOT is equal to
   EXPECTED."

  (subsetp expected got :test #'equal))


;;; Test Macro Definitions

(defmacro global-macro (form)
  "Global macro used to test macroexpansion in augmented
   environments."

  `(in-global ,form))

(define-symbol-macro a-global-symbol-macro "A Global Symbol Macro!")


;;; Macro Augmented Environment Tests

(test macroexpand-1
  "Test MACROEXPAND-1 in an augmented environment with AUGMENT-ENVIRONMNT."

  (is
   (expansion=
    '((in-macro (test-macro (+ 1 2))) t)

    (in-lexical-environment (env)

      (flet ((macro-function (form env)
	       (declare (ignore env))
	       `(in-macro ,form)))

	(let ((env (augment-environment env :macro (list (list 'test-macro #'macro-function)))))
	  (macroexpand-1 '(test-macro (+ 1 2)) env)))))))

(test macroexpand-1-multiple
  "Test MACROEXPAND-1 in an environment augmented with multiple macros."

  (is
   (expansion=
    '((macro2 (pprint "Hello World")) t)

    (in-lexical-environment (env)
      (flet ((macro1 (form env)
	       (declare (ignore env))

	       (destructuring-bind (name thing) form
		 (declare (ignore name))
		 `(macro2 (pprint ,thing))))

	     (macro2 (form env)
	       (declare (ignore env))

	       (destructuring-bind (name form) form
		 (declare (ignore name))
		 `(in-macro2 ,form))))

	(let ((env (augment-environment
		    env
		    :macro `((macro1 ,#'macro1)
			     (macro2 ,#'macro2)))))

	  (macroexpand-1 '(macro1 "Hello World") env)))))))

(test macroexpand
  "Test MACROEXPAND in an environment augmented with multiple macros."

  (is
   (expansion=
    '((in-macro2 (pprint "Hello World")) t)

    (in-lexical-environment (env)
      (flet ((macro1 (form env)
	       (declare (ignore env))

	       (destructuring-bind (name thing) form
		 (declare (ignore name))
		 `(macro2 (pprint ,thing))))

	     (macro2 (form env)
	       (declare (ignore env))

	       (destructuring-bind (name form) form
		 (declare (ignore name))
		 `(in-macro2 ,form))))

	(let ((env (augment-environment
		    env
		    :macro `((macro1 ,#'macro1)
			     (macro2 ,#'macro2)))))

	  (macroexpand '(macro1 "Hello World") env)))))))

(test macroexpand-1-explicit-macroexpand
  "Test MACROEXPAND-1 in an environment on macro which calls MACROEXPAND."

  (is
   (expansion=
    '((in-macro2 (pprint "Hello World")) t)

    (in-lexical-environment (env)
      (flet ((macro1 (form env)
	       (declare (ignore env))

	       (destructuring-bind (name macro thing) form
		 (declare (ignore name))
		 (macroexpand `(,macro (pprint ,thing)) env)))

	     (macro2 (form env)
	       (declare (ignore env))

	       (destructuring-bind (name form) form
		 (declare (ignore name))
		 `(in-macro2 ,form))))

	(let ((env (augment-environment
		    env
		    :macro `((macro1 ,#'macro1)
			     (macro2 ,#'macro2)))))

	  (macroexpand-1 '(macro1 macro2 "Hello World") env)))))))

(test macroexpand-global-macro
  "Test macroexpansion of global macros in augmented environment."

  (is
   (expansion=
    '((in-global (wrap 'hello)) t)

    (in-lexical-environment (env)
      (flet ((macro (form env)
	       (declare (ignore env))

	       (destructuring-bind (name thing) form
		 (declare (ignore name))

		 `(in-global (wrap ,thing)))))

	(let ((env (augment-environment
		    env
		    :macro `((local-macro ,#'macro)))))

	  (macroexpand '(local-macro 'hello) env)))))))


;;; Macro Augmented Environment within MACROLET

(test macroexpand-in-macrolet
  "Test macroexpansion in augmented environment within MACROLET."

  (is
   (expansion=
    '((in-macro2 (wrap (+ 4 5))) t)

    (macrolet ((macro2 (form)
		 `(in-macro2 ,form)))

      (in-lexical-environment (env)
	(flet ((macro1 (form env)
		 (declare (ignore env))

		 (destructuring-bind (name thing) form
		   (declare (ignore name))
		   `(macro2 (wrap ,thing)))))

	  (let ((env (augment-environment
		      env
		      :macro `((macro1 ,#'macro1)))))

	    (macroexpand '(macro1 (+ 4 5)) env))))))))

(test macroexpand-in-macrolet-explicit-macroexpand
  "Test macroexpansion, of macro which calls MACROEXPAND, in augmented
environment within MACROLET."

  (is
   (expansion=
    '((in-macro2 (wrap (+ 4 5))) t)

    (macrolet ((macro2 (form)
		 `(in-macro2 ,form)))

      (in-lexical-environment (env)
	(flet ((macro1 (form env)
		 (declare (ignore env))

		 (destructuring-bind (name thing) form
		   (declare (ignore name))
		   (macroexpand `(macro2 (wrap ,thing)) env))))

	  (let ((env (augment-environment
		      env
		      :macro `((macro1 ,#'macro1)))))

	    (macroexpand-1 '(macro1 (+ 4 5)) env))))))))

(test macroexpand-macrolet
  "Test macroexpansion of a MACROLET macro in augmented environment."

  (is
   (expansion=
    '((in-macro2 (wrap (+ 10 5))) t)

    (macrolet ((macro1 (thing)
		 `(macro2 (wrap ,thing))))

      (in-lexical-environment (env)
	(flet ((macro2 (form env)
		 (declare (ignore env))
		 (destructuring-bind (name form) form
		   (declare (ignore name))
		   `(in-macro2 ,form))))

	  (let ((env (augment-environment
		      env
		      :macro `((macro2 ,#'macro2)))))

	    (macroexpand '(macro1 (+ 10 5)) env))))))))

(test macroexpand-macrolet-with-macroexpand
  "Test macroexpansion of a MACROLET macro, which calls MACROEXPAND,
in augmented environment."

  (is
   (expansion=
    '((in-macro2 (wrap (+ 10 5))) t)

    (macrolet ((macro1 (thing &environment env)
		 (macroexpand `(macro2 (wrap ,thing)) env)))

      (in-lexical-environment (env)
	(flet ((macro2 (form env)
		 (declare (ignore env))
		 (destructuring-bind (name form) form
		   (declare (ignore name))
		   `(in-macro2 ,form))))

	  (let ((env (augment-environment
		      env
		      :macro `((macro2 ,#'macro2)))))

	    (macroexpand-1 '(macro1 (+ 10 5)) env))))))))


;;; Symbol Macro Augmented Environment Tests

(test symbol-macroexpand-1
  "Test MACROEXPAND-1 in symbol-macro augmented environment."

  (is
   (expansion=
    '("Hello World" t)

    (in-lexical-environment (env)
      (let ((env (augment-environment env :symbol-macro '((greeting "Hello World")))))
	(macroexpand-1 'greeting env))))))

(test symbol-macroexpand-1-multiple
  "Test MACROEXPAND-1 in augmented environment with multiple symbol-macros."

  (is
   (expansion=
    '(formula t)

    (in-lexical-environment (env)
      (let ((env (augment-environment
		  env
		  :symbol-macro '((symmacro formula)
				  (formula (* (+ x y) z))))))

	(macroexpand-1 'symmacro env))))))

(test symbol-macroexpand
  "Test MACROEXPAND in symbol-macro augmented environment."

  (is
   (expansion=
    '((* (+ x y) z) t)

    (in-lexical-environment (env)
      (let ((env (augment-environment
		  env
		  :symbol-macro '((symmacro formula)
				  (formula (* (+ x y) z))))))

	(macroexpand 'symmacro env))))))

(test global-symbol-macroexpand
  "Test MACROEXPAND of global symbol-macro in symbol-macro augmented environment."

  (is
   (expansion=
    '("A Global Symbol Macro!" t)

    (in-lexical-environment (env)
      (let ((env (augment-environment env :symbol-macro '((local-macro a-global-symbol-macro)))))
	(macroexpand 'local-macro env))))))

(test macroexpand-symbol-macro-and-macro
  "Test MACROEXPAND in augmented environment with macros and symbol-macros."

  (is
   (expansion=
    '((* (+ x y) z) t)

    (in-lexical-environment (env)
      (flet ((macro (form env)
	       (declare (ignore env))

	       (destructuring-bind (name thing) form
		 (declare (ignore name))

		 thing)))

	(let ((env (augment-environment
		    env
		    :symbol-macro '((symmacro (pass-through formula))
				    (formula (* (+ x y) z)))

		    :macro `((pass-through ,#'macro)))))

	  (macroexpand 'symmacro env)))))))

(test macroexpand-in-symbol-macrolet
  "Test MACROEXPAND in augmented environment within SYMBOL-MACROLET"

  (is
   (expansion=
    '((list "Hello" "Bye") t)

    (symbol-macrolet ((greetings greeting-list)
		      (greeting-list (list "Hello" "Bye")))

      (in-lexical-environment (env)
	(let ((env (augment-environment env :symbol-macro '((the-greetings greetings)))))
	  (macroexpand 'the-greetings env)))))))

(test macroexpand-symbol-macrolet
  "Test MACROEXPAND a SYMBOL-MACRO, defined with SYMBOL-MACROLET, in augmented environment."

  (is
   (expansion=
    '((list "Hello" "Bye") t)

    (symbol-macrolet ((the-greetings greetings)
		      (greeting-list (list "Hello" "Bye")))

      (in-lexical-environment (env)
	(let ((env (augment-environment env :symbol-macro '((greetings greeting-list)))))
	  (macroexpand 'the-greetings env)))))))


;;; Shadowing Global Macros with local functions/variables

(test shadow-global-macro
  "Test shadowing of a global macro with local function."

  (is
   (expansion=
    '((global-macro (+ 1 2)) nil)

    (in-lexical-environment (env)
      (let ((env (augment-environment env :function '(global-macro local-fn))))
	(macroexpand '(global-macro (+ 1 2)) env))))))

(test shadow-global-symbol-macro
  "Test shadowing of a global symbol-macro with local variable."

  (is
   (expansion=
    '(a-global-symbol-macro nil)

    (in-lexical-environment (env)
      (let ((env (augment-environment env :variable '(x y z a-global-symbol-macro))))
	(macroexpand 'a-global-symbol-macro env))))))


;;; Declaration Information

(test augmented-variable-information
  "Test VARIABLE-INFORMATION on augmented environment."

  (is
   (info=
    (let ((x 1) (y 2))
      (declare (ignorable x y))
      (in-lexical-environment (env)
	(let ((env (augment-environment env :declare '((type integer x y) (special y)))))
	  (variable-information 'x env))))

    '(:lexical t ((type . integer)))))

  (is
   (info=
    (let ((x 1) (y 2))
      (declare (ignorable x y))
      (in-lexical-environment (env)
	(let ((env (augment-environment env :declare '((type integer x y) (special y)))))
	  (variable-information 'y env))))

    #-ccl
    '(:special t ((type . integer)))

    ;; CCL does not recognize Y as a local binding for some reason.
    #+ccl
    '(:special nil ((type . integer))))))

(test augmented-variable-information-symbol-macro
  "Test VARIABLE-INFORMATION on symbol-macro augmented environment."

  (is
   (info=
    (in-lexical-environment (env)
      (let ((env (augment-environment env :symbol-macro '((local-sym-macro (+ 1 2))))))
	(variable-information 'local-sym-macro env)))

    '(:symbol-macro t nil))))

(test augmented-function-information
  "Test FUNCTION-INFORMATION on augmented environment."

  (is
   (info=
    (flet ((add (a b) (+ a b)))
      (in-lexical-environment (env)
	(let ((env (augment-environment
		    env
		    :declare '((ftype (function (number number) number) add)))))
	  (function-information 'add env))))

    '(:function t ((ftype . (function (number number) number)))))))

(test augmented-function-information-macro
  "Test FUNCTION-INFORMATION on macro augmented environment."

  (is
   (info=
    (in-lexical-environment (env)
      (flet ((macro (form env)
	       (declare (ignore env))

	       (destructuring-bind (name thing) form
		 (declare (ignore name))
		 thing)))

	(let ((env (augment-environment env :macro `((loc-macro1 ,#'macro)))))
	  (function-information 'loc-macro1 env))))

    '(:macro t nil))))

(test augmented-declaration-information
  (is
   (optimize=
    '((speed 3) (safety 0) (space 1))

    (first
     (in-lexical-environment (env)
       (let ((env (augment-environment env :declare '((optimize (speed 3) (safety 0) (space 1))))))
	 (declaration-information 'optimize env)))))))

(test augment-variable-keep-information
  "Test that augmenting an environment with variables does not replace existing information."

  (is
   (info=
    (let ((x 1))
      (declare (type integer x))
      (in-lexical-environment (env)
	(let ((env (augment-environment
		    env
		    :variable '(w y z)
		    :declare '((type integer y) (special y)))))
	  (variable-information 'x env))))

    '(:lexical t ((type . integer))))))

(test augment-function-keep-information
  "Test that augmenting an environment with functions does not replace existing information."

  (is
   (info=
    (flet ((add (a b) (+ a b)))
      (declare (ftype (function (number number) number) add))

      (in-lexical-environment (env)
	(let ((env (augment-environment
		    env
		    :function '(inc f g)
		    :declare '((ftype (function (integer) *) inc)))))
	  (function-information 'add env))))

    '(:function t ((ftype . (function (number number) number)))))))


;;; Augmenting Augmented Environments

(test augment-augmented-macro
  "Test augmenting a macro augmented environment."

  (is
   (expansion=
    '((macro2 (wrap1 "Hello World")) t)

    (in-lexical-environment (env)
      (flet ((macro1 (form env)
	       (declare (ignore env))

	       (destructuring-bind (name thing) form
		 (declare (ignore name))
		 `(macro2 (wrap1 ,thing))))

	     (macro2 (form env)
	       (declare (ignore env))

	       (destructuring-bind (name form) form
		 (declare (ignore name))
		 `(in-macro2 ,form))))

	(let* ((env1 (augment-environment
		      env
		      :macro `((macro1 ,#'macro1))))

	       (env2 (augment-environment
		      env1
		      :macro `((macro2 ,#'macro2)))))

	  (macroexpand-1 '(macro1 "Hello World") env2)))))))

(test augment-augmented-symbol-macro
  "Test augmenting a symbol-macro augmented environment."

  (is
   (expansion=
    '(formula t)

    (in-lexical-environment (env)
      (let* ((env1 (augment-environment
		    env
		    :symbol-macro '((symmacro formula))))

	     (env2 (augment-environment
		    env1
		    :symbol-macro '((formula (* (+ x y) z))))))

	(macroexpand-1 'symmacro env2))))))

(test augment-augmented-variable
  "Test augmenting a variable augmented environment."

  (let* ((env1 (augment-environment
		nil
		:variable '(x)
		:declare '((type integer x) (special x))))

	 (env2 (augment-environment
		env1
		:variable '(y)
		:declare '((type string y)))))

    (is (info=
	 (multiple-value-list (variable-information 'x env2))
	 '(:special t ((type . integer)))))

    (is (info=
	 (multiple-value-list (variable-information 'y env2))
	 '(:lexical t ((type . string)))))))

(test augment-augmented-function
  "Test augmenting a function augmented environment."

  (let* ((env1 (augment-environment
		nil
		:function '(f1)
		:declare '((ftype (function (number) number) f1))))

	 (env2 (augment-environment
		env1
		:function '(f2)
		:declare '((ftype (function (string string) string) f2)))))

    (is (info=
	 (multiple-value-list (function-information 'f1 env2))
	 '(:function t ((ftype . (function (number) number))))))

    (is (info=
	 (multiple-value-list (function-information 'f2 env2))
	 '(:function t ((ftype . (function (string string) string))))))))

(test augment-augmented-declaration
  "Test augmenting a declaration augmented environment."

  (let* ((env1 (augment-environment
		nil
		:variable '(x)
		:declare '((special x)
			   (optimize (speed 3)))))

	 (env2 (augment-environment
		env1
		:declare '((type integer x)
			   (optimize (space 3) (safety 0))))))

    (is (info=
	 (multiple-value-list (variable-information 'x env2))
	 '(:special t ((type . integer)))))

    (is (optimize=
	 '((speed 3) (space 3) (safety 0))
	 (declaration-information 'optimize env2)))))

;; Only perform this test when a non-native AUGMENT-ENVIRONMENT
;; implementation is used.

#+cl-environments-full
(test augment-augmented-copy
  "Test that augmenting an augmented environment does not modify the original."

  (let* ((env1 (augment-environment
		nil
		:variable '(x)
		:declare '((special x)
			   (optimize (speed 3)))))

	 (env2 (augment-environment
		env1
		:declare '((type integer x)
			   (optimize (space 3) (safety 0))))))

    (is (not (eq env1 env2))
	"Environment not copied. Original returned.")

    (is (not (eq (cl-environments.cltl2::variables env1)
		 (cl-environments.cltl2::variables env2)))

	"Variable information not copied.")

    (is (not (eq (cl-environments.cltl2::functions env1)
		 (cl-environments.cltl2::functions env2)))

	"Function information not copied.")

    (is (not (eq (cl-environments.cltl2::declarations env1)
		 (cl-environments.cltl2::declarations env2)))

	"Declaration information not copied.")

    (is (not (eq (cl-environments.cltl2::macro-functions env1)
		 (cl-environments.cltl2::macro-functions env2)))

	"Macro function table not copied.")

    (is (not (eq (cl-environments.cltl2::symbol-macros env1)
		 (cl-environments.cltl2::symbol-macros env2)))

	"Symbol-macro expansion table not copied.")))


;;; ENCLOSE Tests

(test enclose-nil-environment
  "Test ENCLOSE function in NIL (global) environment."

  (let ((f (enclose '(lambda (a b) (+ 1 a b)) nil)))
    (is-every =
      (4 (funcall f 1 2))
      (4 (funcall f 2 1))
      (6 (funcall f 2 3))
      (1 (funcall f 0 0)))))

(test enclose-lexical-environment
  "Test ENCLOSE in lexical environment."

  (macrolet ((local-wrap (form)
	       ``(:wrap ,,form)))

    (let ((f (first
	      (in-lexical-environment (env)
		(enclose '(lambda (x) (local-wrap (* 2 x))) env)))))

      (is-every equal
	('(:wrap 2) (funcall f 1))
	('(:wrap 10) (funcall f 5))
	('(:wrap 14) (funcall f 7))
	('(:wrap 0) (funcall f 0))))))

(test enclose-augmented-environment
  "Test ENCLOSE in augmented environment."

  (macrolet ((local-mac (form)
	       ``(:in-local ,,form)))

    (let ((f
	   (first
	    (in-lexical-environment (env)
	      (let ((env (augment-environment env :symbol-macro '((delta 15)))))
		(enclose '(lambda (x) (local-mac (+ delta x))) env))))))

      (is-every equal
	('(:in-local 15) (funcall f 0))
	('(:in-local 16) (funcall f 1))
	('(:in-local 20) (funcall f 5))
	('(:in-local 25) (funcall f 10))))))


;;; ENCLOSE-MACRO Tests

(test enclose-macro-nil-environment
  "Test ENCLOSE-MACRO in a NIL (global) environment."

  (let ((mac (enclose-macro
	      'a-macro '(a (b c) &body forms)
	      '(`(let ((,a (+ ,b ,c)))
		   ,@forms))
	      nil)))

    (is
     (equal
      '(let ((x (+ 1 2)))
	(pprint x)
	(fn x))

      (funcall mac '(a-macro x (1 2) (pprint x) (fn x)) nil)))))

(test enclose-macro-with-environment-parameter
  "Test ENCLOSE-MACRO with &ENVIRONMENT parameter."

  (let ((var-x 1) (var-y 3))
    (declare (special var-y))

    (is (equal
	 '((:lexical :special nil))

	 (in-lexical-environment (env)
	   (let ((mac (enclose-macro
		       'info-mac '(type &rest things &environment env)
		       '((let ((fn (symbolicate type '-information)))
			   (mapcar (lambda (x) (funcall fn x env)) things))))))

	     (funcall mac '(info-mac variable var-x var-y var-z) env)))))))

(test enclose-macro-lexical-environment
  "Test ENCLOSE-MACRO in a lexical environment."

  (macrolet ((local-pass (sym form)
	       ``(,',sym ,,form)))

    (is (equal
	 '((:macro-wrap (* (+ 1 2) 3)))

	 (in-lexical-environment (env)
	   (let ((mac (enclose-macro
		       'wrap-macro '(form)
		       '((return-from wrap-macro (local-pass :macro-wrap form)))

		       env)))

	     (funcall mac '(wrap-macro (* (+ 1 2) 3)) nil)))))))

(test enclose-macro-augmented-environment
  "Test ENCLOSE-MACRO in an augmented environment."

  (macrolet ((local-wrap (sym form)
	       ``(,,sym ,,form)))

    (is (equal
	 '((:in-wrapper (main-form a b c)))

	 (in-lexical-environment (env)
	   (let* ((env (augment-environment env :symbol-macro '((wrapper :in-wrapper))))
		  (mac (enclose-macro
			'wrap-macro '(form)
			'((local-wrap wrapper form))

			env)))

	     (funcall mac '(wrap-macro (main-form a b c)) nil)))))))
