;;;; -*- mode: lisp -*-
;;;;
;;;; Simple lexical analyzer generator
;;;;
;;;; Copyright (C) 2008 Ryan Moe
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package :simple-lexer)

(defun int (n)
  (read-from-string n))

(defun combine-patterns (actions)
  "Given a list of (regex action) pairs returns a scanner matching
those regexes. It builds up an alternation of each pattern and places
each pattern in a register so the matching pattern can later be identified."
   (loop :for pattern :in actions 
      :with regex = (list :ALTERNATION)
      :finally (return (nreverse regex)) :do
      (push `(:REGISTER ,(cl-ppcre::parse-string (car pattern)))
	    regex)))
     
(defun build-case (actions)
  "Given a list of (regex action) pairs will build the body of a case
statement."
  (loop :for action :in actions
     :for index :from 0 :with case = (list)
     :finally (return (nreverse case)) :do
     (push `(,index (progn (,@(second action)))) case)))

(defun find-matching-reg (reg-list)
  "CL-PPCRE returns when a particular register match begins, this is used
to figure out which regex matched."
  (loop
     :for x :across reg-list
     :for i :from 0
     :until (not (eq x nil))
     :finally (return i)))

(defmacro deflexer (name &rest patterns)
"Defines a lexical analyzer function.  This function takes a string and returns
a function that when called returns the next value."
  (let ((scanner-var (gensym))
	(string-var (gensym))
	(prev-var (gensym))
	(match-start (gensym))
	(match-end (gensym))
	(reg-start (gensym))
	(actions (build-case patterns)))
    `(progn
       (defparameter ,scanner-var (combine-patterns ',patterns))
       (defun ,name (,string-var &key (start 0) (end (length ,string-var))
		     &aux ,match-start ,match-end ,reg-start)
	 (symbol-macrolet ((%0 (subseq ,string-var ,match-start ,match-end)))
	   (lambda ()
	     (loop :while (< start end) :with ,prev-var = -1
		:when (= ,prev-var start) :do
		(error "Error")
		:do
		(setf ,prev-var start)
		(multiple-value-setq (,match-start ,match-end ,reg-start)
		    (scan ,scanner-var ,string-var :start start))
		(if ,match-start
		    (incf start (- ,match-end ,match-start))
		    (error "Token unrecognized in ~S at position ~D" 
			   ,string-var start))
		(case (find-matching-reg ,reg-start)
		  ,@actions
		  (otherwise
		   (error "Unknown token at ~S." start))))))))))
