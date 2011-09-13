(in-package :cl-user)

(defpackage :cl-more-types
  (:nicknames :more-types)
  (:use :cl)
  (:export ;; combinators.lisp
           #:non-nil
           ;; streams.lisp
           #:file-associated-stream-p
           #:file-associated-stream
           ;; lists.lisp
           #:proper-list-p
           #:proper-list
           #:property-list-p
           #:property-list
           #:association-list-p
           #:association-list
           #:tuplep
           #:tuple
           ;; designators.lisp
           #:character-designator
           #:function-designator
           #:file-position-designator
           #:list-designator
           #:package-designator
           #:pathname-designator
           #:stream-designator
           #:string-designator))
