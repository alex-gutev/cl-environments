;;;; define-declaration
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

;;;; Test defining custom declarations

(defpackage :cl-environments.test.cltl2.define-declaration
  (:use :cl-environments-cl
	:cl-environments.test.cltl2
	:alexandria
	:fiveam))

(in-package :cl-environments.test.cltl2.define-declaration)

(def-suite define-declaration
    :description "Test defining custom declarations using DEFINE-DECLARATION"
    :in cltl2-test)

(in-suite define-declaration)


;;; Variable Declarations

(define-declaration foo-type (args)
  (destructuring-bind (foo-type &rest vars) args
    (values
     :variable
     (mapcar
      (lambda (var)
	`(,var foo-type ,foo-type))
      vars))))


;;; Function Declarations

(define-declaration bar-type (args)
  (destructuring-bind (bar-type &rest fns) args
    (values
     :function
     (mapcar
      (lambda (fn)
	`(,fn bar-type ,bar-type))
      fns))))


;;; Other Declarations

(define-declaration foobar-types (args)
  (destructuring-bind (&rest types) args
    (values
     :declare
     (cons 'foobar-types types))))


;;; Custom declarations in global functions

(defun test-function (a b)
  (declare (foo-type foo1 a b))

  (+ a b)
  (values
   (info variable a)
   (info variable b)))

(defmacro test-macro (form)
  (declare (foo-type macfoo form))

  (list form)
  `',(info variable form))

(defgeneric test-generic (a)
  (:method ((a number))
    (declare (foo-type genericfoo a))
    (1+ a)
    (info variable a)))

(defmethod test-generic ((str string))
  (declare (foo-type stringfoo str))
  (string-capitalize str)
  (info variable str))


;;; Tests

(test (variable-declarations :compile-at :run-time)
  "Custom declarations applying to variables"

  (is
   (info=
    '(:lexical t ((foo-type . my-foo)))

    (let ((x 1))
      (declare (foo-type my-foo x))
      (1+ x)
      (info variable x)))))

(test (function-declarations :compile-at :run-time)
  "Custom declarations applying to functions"

  (is
   (info=
    '(:function t ((bar-type . my-bar)))

    (flet ((f (x) (1+ x)))
      (declare (bar-type my-bar f))
      (f 1)
      (info function f)))))

(test (other-declarations :compile-at :run-time)
  "Custom declarations neither applying to variables nor functions"

  (is
   (equal
    '(foo1 foo2 bar3)

    (locally (declare (foobar-types foo1 foo2 bar3))
      (first (info declaration foobar-types))))))

(test in-global-definitions
  "Test usage of custom declarations in global definitions"

  (multiple-value-bind (info1 info2)
      (test-function 1 2)

    (is (info= '(:lexical t ((foo-type . foo1))) info1))
    (is (info= '(:lexical t ((foo-type . foo1))) info2)))

  (is
   (info= '(:lexical t ((foo-type . macfoo))) (test-macro x)))

  (is
   (info= '(:lexical t ((foo-type . genericfoo)))
	  (test-generic 1)))

  (is
   (info= '(:lexical t ((foo-type . stringfoo)))
	  (test-generic "hello"))))

(test (augment-environment-declarations :compile-at :run-time)
  "Test custom declarations added using AUGMENT-ENVIRONMENT"

  (let ((env (augment-environment
              nil
              :variable '(x)
              :function '(fn1)
              :declare '((foo-type some-foo x)
                         (bar-type some-bar fn1)
                         (foobar-types type1 type2)))))

    (is
     (info=
      '(:lexical t ((foo-type . some-foo)))
      (multiple-value-list (variable-information 'x env))))

    (is
     (info=
      '(:function t ((bar-type . some-bar)))
      (multiple-value-list (function-information 'fn1 env))))

    (is
     (equal
      '(type1 type2)
      (declaration-information 'foobar-types env)))))
