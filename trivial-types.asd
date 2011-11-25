(in-package :cl-user)

(defpackage :trivial-types-asd
  (:use :cl :asdf))
(in-package :trivial-types-asd)

(defsystem :trivial-types
  :description "Trivial type definitions"
  :long-description "TRIVIAL-TYPES provides missing but important type
definitions such as PROPER-LIST, ASSOCIATION-LIST, PROPERTY-LIST and
TUPLE.

By using these types, you can keep type declarations more
accurate. For example, you may write a class definition like:

    (defclass person ()
      ((name :type string))
      ((age :type fixnum))
      ((friends :type list)))

However, it is not obvious for anyone except you that FRIENDS slot has
only a list of person. If you want declare FRIENDS slot more
accurately, PROPER-LIST is the best for that:

    (defclass person ()
      ((name :type string))
      ((age :type fixnum))
      ((friends :type (proper-list person))))

In addition, TRIVIAL-TYPES also provides standard designators defined
in ANSI standard such as PACKAGE-DESIGNATOR. They are useful when you
write a function that takes a package-oid argument like:

    (defun list-external-symbols (package)
      (declare (package-designator package))
      (loop for symbol being the external-symbol of package
            collect symbol))"
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "specials")
                             (:file "lists")
                             (:file "designators")
                             (:file "streams")
                             (:file "combinators")
                             (:file "typespecs")))))
