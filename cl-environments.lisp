;;;; cl-environments.lisp
;;;;
;;;; Copyright 2017 Alexander Gutev
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

(in-package :cl-environments)

;;; Provides a compatibility layer around the CLTL2 environment access
;;; functions. Some implementations already provide CLTL2 environment
;;; access, however with slightly different API's and subtle bugs.

;;; SBCL, CCL and Allegro CL are known to support environments. CCL
;;; has a few bugs involving user-defined declarations. Allegro CL
;;; deviates from the CLTL2 API.


;;; This will be implemented by providing macros which shadow the
;;; standard CL special forms and macros, they simply expand to the CL
;;; forms and call into the code walker. Additionaly a macro expansion
;;; hook (*macroexpand-hook*) will be added to capture the environment
;;; information in macro expansions of macros which were not compiled
;;; using this package.

;;; Code walking is performed using a recursive macro which expands
;;; into the same form, with the arguments wrapped in the macro, CL
;;; special forms are handled appropriately (as per the form) and
;;; macros are expanded prior to wrapping the arguments. The actual
;;; extended environment information is stored in a lexical symbol
;;; macro, by wrapping the subforms of the environment in a
;;; SYMBOL-MACROLET. This provides a natural mapping between the
;;; implementation specific environment objects and the extended
;;; environment objects.

