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

    '(:special t ((type . integer))))))

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
