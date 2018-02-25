(in-package :cl-user)
(defpackage :my-test
  (:use :cl
	:prove))

(in-package :my-test)

(plan 3)

(ok (not (find 4 '(1 2 3))))
(is 4 4)
(isnt 1 #\1)

(finalize)
