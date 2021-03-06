;;;; macro-util.lisp
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

(in-package :cl-environments.util)


(defmacro! match-state (arg &body states)
  "Implements an FSM where each state may specify a pattern and a list
   of from states, when the argument matches the pattern of a
   particular and the current state is in the state's from states
   list, the FSM transitions to that state. Each element in STATES is
   a list of the following form: (STATE PATTERN [:FROM STATES] . BODY)
   where STATE is a symbol identifying the state, PATTERN is the
   pattern to be matched, and STATES is the optional list of from
   states (it and the :FROM keyword may be excluded), if there is only
   one state it does not have to be in a list. If a state specifies no
   from states, it is as if all declared states are specifed as from
   states. When a state becomes the current state the forms in its
   BODY are executed, in which the machine may either transition to
   the next state using the internal function (NEXT NEW-ARG) where
   NEW-ARG is the new argument to be matched against the patterns. If
   NEXT is never called in body the return value of the last form in
   BODY becomes the return value of the MATCH-STATE form. The intiial
   argument is given by evaluated the form ARG. The initial state may
   be optionally specified, when the first element of STATES is :START
   the second element is taken as the form to be evaluated to produce
   the start state, otherwise the start state defaults
   to :START. Patterns are matched in the order given, the first state
   whose pattern matches (both the argument pattern and FROM list)
   becomes the current state."

  (let ((next (intern (string 'next)))
	(from-state (intern (string 'from-state))))
    (labels ((make-quote (thing)
	       `(quote ,thing))
	     
	     (extract-from (body)
	       (if (eq (first body) :from)
		   (let ((states (second body)))
		     (values (if (listp states)
				 (cons 'or (mapcar #'make-quote states))
				 (list 'quote states))
			     (cddr body)))
		   (values '_ body)))
	   
	     (make-clause (state)
	       (destructuring-bind (name pattern . body) state
		 (multiple-value-bind (from body) (extract-from body)
		   `(((guard ,from (not (and ,g!force (eq ,from-state ',name)))) ,pattern)
		     (flet ((,next (,g!arg &key force (from ',name))
			      (,g!next from force ,g!arg)))
		       (declare (ignorable (function,next)))
		       ,@body))))))

      (let-if ((start (second states) :start)
	       (body (cddr states) states))
	  (eq (first states) :start)
      
	`(labels ((,g!next (,from-state ,g!force ,g!arg)
		    (multiple-value-match (values ,from-state ,g!arg)
		      ,@(mapcar #'make-clause body))))
	   (,g!next ,start nil ,arg))))))



