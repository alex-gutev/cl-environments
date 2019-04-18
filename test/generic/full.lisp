;;;; full.lisp
;;;;
;;;; Copyright 2019 Alexander Gutev
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

;;;; External API Tests.

(defpackage :cl-environments.test.external
  (:use :cl-environments
	:alexandria
	:prove)

  (:import-from :cl-environments.util
		:defmacro!))

(in-package :cl-environments.test.external)


;;;; Get Environment Information Macros

(defun get-info (fn xs env)
  "Returns a list where each element of XS is replaced with the list
   of values which are the result of applying FN on that element and
   ENV."

  (loop for x in xs collect (multiple-value-list (funcall fn x env))))

(defmacro var-info (&rest vars &environment env)
  "Returns a list of the result of applying VARIABLE-INFORMATION on
   each symbol in VARS."

  `',(get-info #'variable-information vars env))

(defmacro fn-info (&rest fns &environment env)
  "Returns a list of the result of applying FUNCTION-INFORMATION on
   each symbol in VARS."

  `',(get-info #'function-information fns env))

(defmacro decl-info (&rest decls &environment env)
  "Returns a list of the result of applying DECLARATION-INFORMATION on
   each symbol in VARS."

  `',(get-info #'declaration-information decls env))


;;;; Test Utility Functions

(defun is-binding (got expected)
  "Returns true if the binding (variable or function) GOT is
   equivalent to the binding information EXPECTED.

   The binding type (first element) and local-p flag (second element)
   are compared by EQUAL, the declaration information (third element)
   is compared by DECL-EQUAL"

  (subtest "Test Binding"
   (destructuring-bind (&optional got-type got-local-p got-decls) got
     (destructuring-bind (&optional exp-type exp-local-p exp-decls) expected
       (is got-type exp-type)
       (is got-local-p exp-local-p)
       (is got-decls exp-decls :test #'decl-equal)))))

(defun eval-with-hook (form)
  "Evaluates form, by EVAL, with the *MACROEXPAND-HOOK* set by
   ENABLE-HOOK. The hook is reset, by DISABLE-HOOK, when the
   evaluation finishes, whether by a local or non-local exit."

  (enable-hook)
  (unwind-protect
       (handler-bind
	   ((warning #'muffle-warning))
	 (eval form))
    (disable-hook)))

(defun decl-equal (got expected)
  "Returns true if the declaration information GOT is equivalent to
   the declaration information EXPECTED.

   The declaration information is considered equivalent if for each
   key value pair (KEY . VALUE) in EXPECTED, the first pair in GOT
   with key KEY has the value VALUE."

  (loop for (key . value) in expected
     always (equal (cdr (assoc key got)) value)))

(defmacro test-form (desc form &rest tests)
  "Tests that the evaluation of FORM, by EVAL-WITH-HOOK, returns a
   list where each element is equivalent, by IS-BINDING, to the
   corresponding element in TESTS. DESC is a description for the
   SUBTEST."

  `(subtest ,(format nil "Test: ~a" desc)
     (diag (format nil "Form: ~s" ',form))
     (mapcar #'is-binding (eval-with-hook ',form) ',tests)))


;;;; Tests

(plan 3)

(subtest "Test VARIABLE-INFORMATION"
  (subtest "Test LET forms"
    (test-form
     "Single LET form"

     (let ((x 1))
       (declare (type number x))
       (var-info x))

     (:lexical t ((type . number))))

    (test-form
     "Multiple Bindings"

     (let ((x 'a) (y 'b) z)
       (declare (type symbol x y))
       (declare (ignore x y))
       (var-info x y z))

     (:lexical t ((ignore . t) (type . symbol)))
     (:lexical t ((ignore . t) (type . symbol)))
     (:lexical t))

    (test-form
     "Nested LET forms"

     (let ((x "hello"))
       (declare (type string x))
       (let ((y 1))
	 (declare (type number y))
	 (var-info x y)))

     (:lexical t ((type . string)))
     (:lexical t ((type . number))))

    (test-form
     "Shadowing declarations in nested LET forms"

     (let ((x "hello"))
       (declare (type string x) (dynamic-extent x))
       (let ((y 1))
	 (declare (type number x y))
	 (var-info x y z)))

     (:lexical t ((type . number) (dynamic-extent . t)))
     (:lexical t ((type . number)))
     (nil nil nil))

    (test-form
     "Shadowing bindings in nested LET forms"

     (let ((x "hello"))
       (declare (type string x) (dynamic-extent x))
       (let ((x 1) (y 2))
	 (declare (type number x) (type number y))
	 (var-info x y)))

     (:lexical t ((type . number) (dynamic-extent . nil)))
     (:lexical t ((type . number))))))

(finalize)
