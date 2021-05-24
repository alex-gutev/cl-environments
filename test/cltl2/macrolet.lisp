;;;; macrolet.lisp
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

;;;; Test that environment information is extracted from MACROLET and SYMBOL-MACROLET forms.

(defpackage :cl-environments.test.cltl2.macrolet-forms
  (:use :cl-environments-cl
	:cl-environments.test.cltl2
	:fiveam))

(in-package :cl-environments.test.cltl2.macrolet-forms)

(def-suite macrolet-forms
    :description "Test extraction of environment information from MACROLET and SYMBOL-MACROLET forms"
    :in cltl2-test)

(in-suite macrolet-forms)

(defmacro test-macro (form)
  form)

(defun global-fn (a b c)
  (/ (* a b) c))

(define-symbol-macro global-symbol-macro "Hello World")

(test (macro-types :compile-at :run-time)
  "Test extracting lexical macro information"

  (macrolet ((pass-through (form)
	       "Pass through macro"
	       form))

    (is (info= '(:macro t nil)
	       (info function pass-through)))))

(test (macro-shadowing :compile-at :run-time)
  "Test shadowing of global macros by lexical macros"

  (is (info= '(:macro nil nil)
	     (info function test-macro)))

  (macrolet ((test-macro (form)
	       form))

    (is (info= '(:macro t nil)
	       (info function test-macro)))))

(test (function-shadowing :compile-at :run-time)
  "Test shadowing of global functions by lexical macros"

  (is (info= '(:function nil nil)
	     (info function global-fn)))

  (macrolet ((global-fn (form)
	       form))

    (is (info= '(:macro t nil)
	       (info function global-fn)))))

(test (symbol-macro-types :compile-at :run-time)
  "Test extraction of lexical symbol macro information"

  (symbol-macrolet ((sym-macro "a symbol macro")
		    (sym-macro2 2))

    (is-every info=
      ('(:symbol-macro t nil) (info variable sym-macro))
      ('(:symbol-macro t nil) (info variable sym-macro2)))))

(test (symbol-macro-shadowing :compile-at :run-time)
  "Test shadowing of global symbol macros with lexical symbol macros"

  (is (info= '(:symbol-macro nil nil)
	     (info variable global-symbol-macro)))

  (symbol-macrolet ((global-symbol-macro "Local symbol macro"))
    (is (info= '(:symbol-macro t nil)
	       (info variable global-symbol-macro)))))

(test (var-shadow-symbol-macro :compile-at :run-time)
  "Test shadowing of symbol macros with lexical variables"

  (symbol-macrolet ((sym-macro "macro"))
    (is (info= '(:symbol-macro t nil)
	       (info variable sym-macro)))

    (let ((sym-macro 1))
      (declare (type integer sym-macro))

      (is (info= '(:lexical t ((type . integer)))
		 (info variable sym-macro))))))
