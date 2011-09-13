(in-package :cl-more-types)

(deftype non-nil (&optional type)
  (if type
      `(and (not null) ,type)
      '(not null)))
