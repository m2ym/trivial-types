(in-package :cl-user)

(defpackage :cl-more-types-asd
  (:use :cl :asdf))
(in-package :cl-more-types-asd)

(defsystem :cl-more-types
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
