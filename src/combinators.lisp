(in-package :trivial-types)

(deftype non-nil (&optional type)
  "Equivalent to `(and (not null) TYPE)` if TYPE is given,
otherwise `(not null)`.

Examples:

    (typep nil '(non-nil symbol)) => NIL"
  (if type
      `(and (not null) ,type)
      '(not null)))
