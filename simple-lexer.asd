;;;; -*- mode: lisp -*-
;;;;
;;;; System definition for a simple lexical analyzer generator
;;;;
;;;; Copyright (C) 2008 Ryan Moe
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defpackage :simple-lexer-asdf
  (:use :cl :asdf))
    
(in-package :simple-lexer-asdf)

(defsystem simple-lexer
  :name "simple-lexer"
  :author "Ryan Moe <ryan.moe@gmail.com>"
  :version 1.0
  :license "Lisp Lesser GNU General Public License"
  :description "Simple lexical analyzer generator"
  :depends-on (:fiveam :cl-ppcre)
  :components
  ((:file "package")
   (:file "lexer" :depends-on ("package"))))
