(in-package :cl-user)

(defpackage :trivial-types-asd
  (:use :cl :asdf))
(in-package :trivial-types-asd)

(defsystem :trivial-types
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "specials")
                             (:file "combinators")
                             (:file "streams")
                             (:file "lists")
                             (:file "designators")))))
