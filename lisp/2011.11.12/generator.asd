;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:com.derekv.mandlebrots.lisp.2011-11-12.generator-asd
  (:use :asdf :cl))
(in-package :com.derekv.mandlebrots.lisp.2011-11-12.generator-asd) 

(defsystem generator
  :name "Mandlebrot 2011-11-12"
  :author "Derek VerLee (dlverlee@mtu.edu)"
  :version "1.0"
  :licence "BSD"
  :description "Mandlebrot set generator, personal kata exercise instance"
  :long-description ""
  :serial t ;; the dependencies are linear.
  :components ((:file "packages")
               (:file "generatorm")))

;  :depends-on ()

