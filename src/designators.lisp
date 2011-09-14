(in-package :trivial-types)

(deftype character-designator ()
  '(or (string 1)
       character))

(deftype function-designator ()
  '(or symbol
       function))

(deftype file-position-designator ()
  '(or (member :start :end)
       (integer 0)))

(deftype list-designator ()
  '(or (non-nil atom)
       proper-list))

(deftype package-designator ()
  '(or string-designator
       package))

(deftype pathname-designator ()
  '(or string
       file-associated-stream
       pathname))

(deftype stream-designator ()
  '(or (member t nil)
       stream))

(deftype string-designator ()
  '(or character
       symbol
       string))
