(in-package :cl-user)

(defpackage :trivial-types
  (:use :cl)
  (:export #:proper-list-p
           #:proper-list
           #:property-list-p
           #:property-list
           #:association-list-p
           #:association-list
           #:tuplep
           #:tuple

           #:character-designator
           #:function-designator
           #:file-position-designator
           #:list-designator
           #:package-designator
           #:pathname-designator
           #:stream-designator
           #:string-designator

           #:file-associated-stream-p
           #:file-associated-stream

           #:non-nil

           #:type-specifier-p
           #:type-expand))
