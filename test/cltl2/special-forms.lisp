;;;; special-forms.lisp
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

;;;; Test extracting environment information in special forms

(defpackage :cl-environments.test.cltl2.special-forms
  (:use :cl-environments-cl
	:cl-environments.test.cltl2
	:alexandria
	:fiveam))

(in-package :cl-environments.test.cltl2.special-forms)

(def-suite special-forms
    :description "Test extraction of environment information in special forms"
    :in cltl2-test)

(in-suite special-forms)

(test block
  "Accessing environment information in BLOCK forms"

  (is
   (info=
    '(:lexical t ((type . integer)))

    (walk-environment
      (block x
	(cl:let ((x 1))
	  (declare (type integer x))

	  (1+ x)
	  (return-from x (info variable x))))))))

(test catch
  "Accessing environment information in CATCH forms"

  (is
   (info=
    '(:lexical t ((type . integer)))

    (walk-environment
      (catch 'x
	(cl:let ((x 1))
	  (declare (type integer x))

	  (1+ x)
	  (throw 'x (info variable x))))))))

(test eval-when
  "Accessing environment information in CATCH forms"

  (is
   (info=
    '(:lexical t ((ignore . t)))

    (walk-environment
      (eval-when (:compile-toplevel :load-toplevel :execute)
	(cl:let ((x 1))
	  (declare (ignore x))
	  (info variable x)))))))

(test if
  "Accessing environment information in IF forms"

  (is
   (info=
    '(:lexical t ((ignore . t)))

    (walk-environment
     (if (evenp (* 2 3))
	 (cl:let ((var 1))
	   (declare (ignore var))
	   (info variable var))))))

  (is
   (info=
    '(:lexical t ((ignore . t)))

    (walk-environment
     (if (oddp (* 2 3))
	 'true

	 (cl:let ((var 1))
	   (declare (ignore var))
	   (info variable var)))))))

(test locally
  "Accessing environment information in LOCALLY forms"

  (is (subsetp
       '((speed 2) (safety 3) (space 0))
       (locally (declare (optimize (speed 2) safety (space 0)))
	 (first (info declaration optimize)))

       :test #'equal)))

(test multiple-value-call
  "Accessing environment information in MULTIPLE-VALUE-CALL forms"

  (is
   (info=
    '(:lexical t ((ignore . t)))

    (walk-environment
      (multiple-value-call #'identity
	(cl:let ((x 1))
	  (declare (ignore x))

	  (info variable x)))))))

(test multiple-value-prog1
  "Accessing environment information in MULTIPLE-VALUE-PROG1 forms"

  (is
   (info=
    '(:lexical t ((ignore . t)))

    (walk-environment
      (multiple-value-prog1
	  (cl:let ((z "hello"))
	    (declare (ignore z))

	    (info variable z)))))))

(test progn
  "Accessing environment information in PROGN forms"

  (let (info-a info-b)
    (setf
     info-b
     (progn
       (setf info-a
	     (cl:let ((a 1))
	       (declare (type integer a))
	       (info variable a)))

       (cl:let ((z "hello"))
	 (declare (type string z))

	 (info variable z))))

    (is (info= '(:lexical t ((type . integer))) info-a))
    (is (info= '(:lexical t ((type . string))) info-b))))

(test progv
  "Accessing environment information in PROGV forms"

  (is
   (info=
    '(:lexical t ((ignore . t)))

    (walk-environment
      (progv nil nil
	(cl:let ((z "hello"))
	  (declare (ignore z))

	  (info variable z)))))))

(test setq
  "Accessing environment information in SETQ forms"

  (let (info1 info2)
    (setq
     info1
     (cl:let ((var #(1 2 3)))
       (declare (ignore var))
       (info variable var))

     info2
     (cl:let ((var2 123))
       (declare (type integer var2))
       (info variable var2)))

    (is-every info=
      ('(:lexical t ((ignore . t))) info1)
      ('(:lexical t ((type . integer))) info2))))

(test tagbody
  "Accessing environment information in TAGBODY forms"

  (is
   (info=
    '(:lexical t ((type . string)))

    (walk-environment
      (block nil
	(tagbody
	 tag1
	   (go tag3)

	 tag2
	   (pprint "Hello World")
	   (go tag1)

	 tag3
	   (return
	     (cl:let ((z "hello"))
	       (declare (type string z))
	       (info variable z)))))))))

(test the
  "Accessing environment information in THE forms"

  (is
   (info=
    '(:lexical t ((ignore . t)))

    (walk-environment
      (the list
	   (cl:let ((z "hello"))
	     (declare (ignore z))

	     (info variable z)))))))

(test unwind-protect
  "Accessing environment information in UNWIND-PROTECT forms"

  (is
   (info=
    '(:lexical t ((ignore . t)))

    (walk-environment
      (unwind-protect
	   (cl:let ((a-var "hello"))
	     (declare (ignore a-var))

	     (info variable a-var))

	(* 2 3))))))
