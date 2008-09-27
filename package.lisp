;;;; -*- mode: lisp -*-
;;;;
;;;; Package definitions
;;;;
;;;; Copyright (C) 2008 Ryan Moe
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License

(defpackage simple-lexer
  (:use :cl :cl-ppcre)
  (:export #:deflexer
	   #:int
	   #:%0))

(defpackage simple-lexer-tests
  (:use :cl :simple-lexer :fiveam))
