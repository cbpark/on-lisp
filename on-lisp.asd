(in-package :cl-user)
(defpackage on-lisp-asd
  (:use :cl :asdf))
(in-package :on-lisp-asd)

(defsystem on-lisp
  :version "0.1"
  :author "Chan Beom Park"
  :license "BSD"
  :description "Codes from On Lisp"
  :long-description ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "chapter03")
                 (:file "chapter04")
                 (:file "chapter05" :depends-on ("chapter04"))
                 (:file "chapter06")
                 (:file "chapter07")
                 (:file "chapter08")
                 (:file "chapter09")
                 (:file "chapter10")
                 (:file "chapter11" :depends-on ("chapter04"))
                 (:file "chapter12" :depends-on ("chapter11"))
                 (:file "chapter13" :depends-on ("chapter11"
                                                 "chapter04"))
                 (:file "chapter14")
                 (:file "chapter15" :depends-on ("chapter04"
                                                 "chapter05"))
                 (:file "chapter16" :depends-on ("chapter04"
                                                 "chapter12"))
                 (:file "chapter17" :depends-on ("chapter04"
                                                 "chapter05"))))))
