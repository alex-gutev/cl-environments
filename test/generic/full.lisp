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

(defconstant +some-constant+ 'x
  "Constant used in constant tests")

(subtest "Test VARIABLE-INFORMATION"
  (subtest "Test lexical binding using LET"
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
     (:lexical t ((type . number))))

    (test-form
     "Free Declaration"

     (let ((a 1))
       (declare (type integer b))

       (var-info b))

     (nil nil ((type . integer))))

    (test-form
     "Additional declarations to outer LET binding"

     (let ((a 1))
       (declare (type integer a))

       (append
	(var-info a)

	(let ((b 2))
	  (declare (dynamic-extent a))
	  (var-info a))))

     (:lexical t ((type . integer) (dynamic-extent . nil)))
     (:lexical t ((type . integer) (dynamic-extent . t)))))

  (subtest "Test special bindings and special declarations"
    (test-form
     "Global Special Variables"

     (progn
       (defvar *test* 1)
       (declaim (type integer *test*))
       (var-info *test*))

     (:special nil ((type . integer))))

    (test-form
     "Predefined Global Special Variables"

     (var-info *standard-input*)
     (:special nil nil))

    (test-form
     "Shadowing Special Variables"

     (progn
       (defvar *test* 1)
       (declaim (type integer *test*))

       (let ((*test* 2))
	 (var-info *test*)))

     (:special t ((type . nil))))

    (test-form
     "Shadowing Predefined Special Variables"

     (let ((*standard-output* (make-broadcast-stream)))
       (var-info *standard-output*))

     (:special t nil))

    (test-form
     "Special Declarations"

     (let ((x 1))
       (declare (type fixnum x))
       (declare (special x))

       (var-info x))

     (:special t ((type . fixnum))))

    (test-form
     "LET nested in LET with SPECIAL declaration"

     (let ((x 1))
       (declare (type fixnum x))
       (declare (special x))

       (let ((x #\a))
	 (var-info x)))

     (:lexical t ((type . nil)))))

  (subtest "Test LET* forms"
    (test-form
     "LET* init-form environment"

     (let ((x 1) (y (var-info x)))
       (declare (type integer x))
       y)

     (nil nil ((type . nil))))

    (test-form
     "LET* init-form environment"

     (let* ((x 1) (y (var-info x z)) (z (var-info x y)))
       (declare (type integer x z))
       (append y z))

     (:lexical t ((type . integer)))
     (nil nil ((type . nil)))

     (:lexical t ((type . integer)))
     (:lexical t))

    (test-form
     "LET* body environment"

     (let* ((x 1) (y 2) (z 3))
       (declare (type integer x z))
       (declare (type fixnum y) (ignore y))

       (var-info x y z))

     (:lexical t ((type . integer)))
     (:lexical t ((type . fixnum) (ignore . t)))
     (:lexical t ((type . integer))))

    (test-form
     "Nested LET* forms"

     (let* ((x 1) (y 2))
       (declare (type integer x y))
       (let* ((z 3))
	 (declare (type number z))
	 (var-info x y z)))

     (:lexical t ((type . integer)))
     (:lexical t ((type . integer)))
     (:lexical t ((type . number)))))

  (subtest "Test LOCALLY forms"
    (test-form
     "Additional declarations to LET variable"

     (let ((x 4))
       (declare (dynamic-extent x))

       (append
	(var-info x)

	(locally
	    (declare (type number x))
	  (var-info x))))

     (:lexical t ((dynamic-extent . t) (type . nil)))
     (:lexical t ((dynamic-extent . t) (type . number))))

    (test-form
     "Free declaration"

     (locally
	 (declare (type integer a))

       (var-info a))

     (nil nil ((type . integer))))

    (test-form
     "Special declaration"

     (let ((a 1))
       (declare (type integer a))

       (append
	(var-info a)

	(locally (declare (special a b))
	  (var-info a b))))

     (:lexical t ((type . integer)))
     (:special t ((type . integer)))
     (:special nil nil))

    (test-form
     "Nested LOCALLY forms"

     (locally
	 (declare (dynamic-extent x))
       (locally
	   (declare (type character x))
	 (var-info x)))

     (nil nil ((dynamic-extent . t) (type . character)))))

  (subtest "Symbol Macros"
    (test-form
     "SYMBOL-MACROLET"

     (symbol-macrolet ((hello "hello"))
       (var-info hello))

     (:symbol-macro t nil))

    (test-form
     "Nested SYMBOL-MACROLET"

     (symbol-macrolet ((hello "hello"))
       (symbol-macrolet ((bye "bye"))
	 (var-info hello bye)))

     (:symbol-macro t nil)
     (:symbol-macro t nil))

    (test-form
     "Symbol-macro shadowing lexical variable"

     (let ((x 1))
       (declare (type integer))

       (symbol-macrolet ((x 2))
	 (var-info x)))

     (:symbol-macro t ((type . nil))))

    (test-form
     "Global Symbol Macros"

     (progn
       (define-symbol-macro sym-macro-test (+ 1 2))
       (var-info sym-macro-test))

     (:symbol-macro nil nil)))

  (subtest "Constants"
    (test-form
     "Constant defined using DEFCONSTANT"

     (progn
       (defconstant +a-constant+ 'a)
       (var-info +a-constant+))

     (:constant nil nil))

    (test-form
     "Previously defined constant"

     (var-info +some-constant+)

     (:constant nil nil))

    (test-form
     "Builtin constants"

     (var-info t :keyword)

     (:constant nil nil)
     (:constant nil nil)))

  (subtest "Test LAMBDA Forms"
    (test-form
     "LAMBDA with required arguments only"

     ;; LOCALLY is necessary for the form to be intercepted by the
     ;; macroexpand hook, since LOCALLY is a macro that shadows the CL
     ;; special form.

     (locally
	 ((lambda (x y)
	    (declare (type number x) (type fixnum y))

	    (var-info x y))

	  1 2))

     (:lexical t ((type . number)))
     (:lexical t ((type . fixnum))))

    (test-form
     "LAMBDA with optional arguments only"

     (locally
	 ((lambda (&optional x (y 1 y-sp))
	    (declare (type integer y))
	    (var-info x y y-sp))))

     (:lexical t)
     (:lexical t ((type . integer)))
     (:lexical t))

    (test-form
     "LAMBDA with required and optional arguments"

     (locally
	 ((lambda (x &optional (y) (z 1))
	    (declare (type number x z))
	    (var-info x y z))

	  1))

     (:lexical t ((type . number)))
     (:lexical t)
     (:lexical t ((type . number))))

    (test-form
     "LAMBDA with rest argument"

     (let ((f (lambda (&rest args) (var-info args))))
       (funcall f))

     (:lexical t))

    (test-form
     "LAMBDA with required and rest argument"

     (locally
	 ((lambda (x &rest xs)
	    (declare (type number x))
	    (var-info x xs))
	  1))

     (:lexical t ((type . number)))
     (:lexical t))

    (test-form
     "LAMBDA with optional and rest argument"

     (locally
	 ((lambda (&optional first &rest more)
	    (var-info first more))))

     (:lexical t)
     (:lexical t))

    (test-form
     "LAMBDA with keyword arguments"

     (locally
	 ((lambda (&key a (b) (c 2) (d 'x d-sp) ((:key e)) ((:key f) 1 f-sp))
	    (var-info a b c d e f f-sp))))

     (:lexical t)
     (:lexical t)
     (:lexical t)
     (:lexical t)
     (:lexical t)
     (:lexical t)
     (:lexical t))

    (test-form
     "LAMBDA with required and keyword arguments"

     (locally
	 ((lambda (x &key first last &allow-other-keys)
	    (var-info x first last y))
	  1))

     (:lexical t)
     (:lexical t)
     (:lexical t)
     (nil nil nil))

    (test-form
     "LAMBDA with auxiliary arguments"

     (locally
	 ((lambda (x &rest xs &key &allow-other-keys &aux first (last (last xs)))
	    (var-info x xs first last))

	  1))

     (:lexical t)
     (:lexical t)
     (:lexical t)
     (:lexical t))

    (test-form
     "LAMBDA optional argument init-form environment"

     (locally
	 ((lambda (x &optional (y (var-info x y y-sp) y-sp) (z (var-info x y y-sp)))
	    (append y z))

	  1))

     (:lexical t)
     nil
     nil

     (:lexical t)
     (:lexical t)
     (:lexical t))

    (test-form
     "LAMBDA keyword and auxiliary argument init-form environment"

     (locally
	 ((lambda (a &key (b (var-info a b b-sp) b-sp) ((:argc c) (var-info a b b-sp)) &aux (d (var-info a b b-sp c d e)) (e (var-info d)))
	    (append b c d e))
	  1))

     ;; Environment of b
     (:lexical t) nil nil

     ;; Environment of c
     (:lexical t)
     (:lexical t)
     (:lexical t)

     ;; Environment of d
     (:lexical t)
     (:lexical t)
     (:lexical t)
     (:lexical t)
     nil
     nil

     ;; Environment of e
     (:lexical t))

    (test-form
     "Nested LAMBDA forms"

     (locally
	 ((lambda (x)
	    (declare (type number x))

	    ((lambda (y)
	       (declare (type fixnum y))
	       (var-info x y))
	     1))
	  2))

     (:lexical t ((type . number)))
     (:lexical t ((type . fixnum))))

    (test-form
     "Nested LAMBDA form shadowing variable"

     (let ((x 1))
       (declare (type number x))

       (append
	((lambda (x)
	   (declare (ignore x))
	   (var-info x))
	 1)

	(var-info x)))

     (:lexical t ((ignore . t) (type . nil)))
     (:lexical t ((ignore . nil) (type . number)))))

  (subtest "Test FLET and LABELS"
    ;; Test that the information about the arguments in the FLET and
    ;; LABELS functions. It is assumed that the lambda lists of the
    ;; functions are processing in the same way as for LAMBDA forms,
    ;; thus there is no need to do an exhaustive test for each type of
    ;; lambda list argument.

    (test-form
     "Test FLET Form"

     (flet ((f (x)
	      (declare (ignore x))
	      (var-info x y))

	    (g (y)
	      (declare (type number y))
	      (var-info y x)))

       (append (f 1) (g 2) (var-info x y)))

     (:lexical t ((ignore . t)))
     nil

     (:lexical t ((type . number)))
     nil

     nil nil)

    (test-form
     "Test LABELS form"

     (labels ((f (x)
		(declare (ignore x))
		(var-info x y))

	      (g (y)
		(declare (type number y))
		(var-info y x)))
       (append (f 1) (g 2) (var-info x y)))

     (:lexical t ((ignore . t)))
     nil

     (:lexical t ((type . number)))
     nil

     nil nil)

    (test-form
     "Nested FLET in LET"

     (let ((x 1))
       (declare (type number x))

       (flet ((f (a)
		(declare (ignore a))
		(var-info a x)))

	 (append (f 1) (var-info x))))

     (:lexical t ((ignore . t)))
     (:lexical t ((type . number)))
     (:lexical t ((type . number))))


    (test-form
     "FLET function argument shadowing LET variable"

     (let ((x 1))
       (declare (type number x))

       (flet ((f (x)
		(declare (ignore x))
		(var-info x)))

	 (append (f 1) (var-info x))))

     (:lexical t ((ignore . t) (type . nil)))
     (:lexical t ((ignore . nil) (type . number))))

    (test-form
     "Nested LABELS in LET"

     (let ((x 1))
       (declare (type number x))

       (labels ((f (a)
		  (declare (ignore a))
		  (var-info a x)))

	 (append (f 1) (var-info x))))

     (:lexical t ((ignore . t)))
     (:lexical t ((type . number)))
     (:lexical t ((type . number))))

    (test-form
     "LABELS function argument shadowing LET variable"

     (let ((x 1))
       (declare (type number x))

       (labels ((f (x)
		  (declare (ignore x))
		  (var-info x)))

	 (append (f 1) (var-info x))))

     (:lexical t ((ignore . t) (type . nil)))
     (:lexical t ((ignore . nil) (type . number)))))

  (subtest "Test MACROLET forms"
    (test-form
     "Test MACROLET Form"

     (macrolet ((test-x (x (y &rest z) &body body)
		  (declare (ignore x y z body))
		  `',(var-info x y z body)))
       (test-x 1 (2 3)))

     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t))))

    (test-form
     "Test MACROLET with &environment parameter"

     (macrolet ((test-x ((x y) &optional z &environment e &body body)
		  (declare (ignore x y z e body))
		  `',(var-info x y z e body)))
       (test-x (1 2) 3))

     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t))))

    (test-form
     "Test MACROLET with multiple destructuring levels"

     (macrolet ((test-x (a (b (c &optional d) &rest e) f)
		  (declare (ignore a b c d e f))
		  `',(var-info a b c d e f)))
       (test-x 1 (2 (3)) 4))

     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t))))

    ;; Test that parameter variables of one macrolet don't "leak" into
    ;; another macrolet.
    (test-form
     "Test MACROLET with multiple macros"

     (macrolet ((test-x (a)
		  (declare (ignore a))
		  `',(var-info a b))

		(test-y (b)
		  (declare (ignore b))
		  `',(var-info b a)))
       (append (test-x 1) (test-y 2)))

     (:lexical t ((ignore . t)))
     nil
     (:lexical t ((ignore . t)))
     nil)

    (test-form
     "Nested MACROLET"

     (let ((x 1))
       (declare (type number x))
       (macrolet ((add (x y) `(+ ,x ,y)))
	 (var-info x)))

     (:lexical t ((type . number)))))

  (subtest "Test DEFUN forms"
    (test-form
     "Top-level DEFUN"

     (progn
       (defun nonsense-function (a b &optional c &rest d)
	 (declare (type integer a b))
	 (declare (ignore c))
	 (declare (dynamic-extent d))

	 (var-info a b c d))

       (nonsense-function 1 2))

     (:lexical t ((type . integer)))
     (:lexical t ((type . integer)))
     (:lexical t ((ignore . t)))
     (:lexical t ((dynamic-extent . t))))

    (test-form
     "DEFUN nested in LET"

     (let ((x 1))
       (declare (type number x))

       (defun nonsense-function-2 (&optional (a 1 a-sp) &key (b 2 b-sp) c ((:arg d) 5))
	 (declare (type integer a b))
	 (declare (ignore c))
	 (declare (dynamic-extent d))

	 (var-info a a-sp b b-sp c d x))

       (nonsense-function-2))

     (:lexical t ((type . integer)))
     (:lexical t)

     (:lexical t ((type . integer)))
     (:lexical t)

     (:lexical t ((ignore . t)))
     (:lexical t ((dynamic-extent . t)))
     (:lexical t ((type . number))))

    (test-form
     "DEFUN shadowing variable in LET"

     (let ((x 1))
       (declare (type number x))

       (defun nonsense-function-3 (a b &aux (x (+ a b)))
	 (declare (type integer a b))
	 (var-info a b x))

       (nonsense-function-3 1 2))

     (:lexical t ((type . integer)))
     (:lexical t ((type . integer)))
     (:lexical t ((type . nil)))))

  (subtest "Test DEFMACRO forms"
    (test-form
     "Top-level DEFMACRO"

     (progn
       (defmacro nonsense-macro (a (b c) &body d)
	 (declare (ignore a b c))

	 `',(var-info a b c d))

       (nonsense-macro 1 (2 3)))

     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t))

    (test-form
     "DEFMACRO nested in LET"

     (let ((x 1))
       (declare (type number x))

       (defmacro nonsense-macro-2 (a b &optional (c 0 c-sp))
	 (declare (ignore a b c))

	 `',(var-info x a b c c-sp))

       (nonsense-macro-2 1 2 3))

     (:lexical t ((type . number)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t))

    (test-form
     "DEFMACRO shadowing variable in LET"

     (let ((x 1))
       (declare (type number x))

       (defmacro nonsense-macro-3 (a b &aux (x (+ a b)))
	 (declare (ignore a b))
	 `',(var-info a b x))

       (nonsense-macro-3 1 2))

     (:lexical t ((ignore . t)))
     (:lexical t ((ignore . t)))
     (:lexical t ((type . nil))))))

(finalize)
