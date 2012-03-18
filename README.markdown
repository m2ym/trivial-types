trivial-types - Trivial type definitions
========================================

TRIVIAL-TYPES provides missing but important type
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
            collect symbol))

[Package] trivial-types
-----------------------

## [Function] proper-list-p

    proper-list-p object

Returns true if OBJECT is a proper list.

Examples:

    (proper-list-p 1) => NIL
    (proper-list-p '(1 . 2)) => NIL
    (proper-list-p nil) => T
    (proper-list-p '(1 2 3)) => T

## [Type] proper-list

    proper-list &optional (element-type '*)

Equivalent to `(and list (satisfies proper-list-p))`. ELEMENT-TYPE
is just ignored.

Examples:

    (typep '(1 2 3) '(proper-list integer)) => T
    (typep '(1 2 3) '(proper-list string)) => T

## [Function] property-list-p

    property-list-p object

Returns true if OBJECT is a property list.

Examples:

    (property-list-p 1) => NIL
    (property-list-p '(1 2 3)) => NIL
    (property-list-p '(foo)) => NIL
    (property-list-p nil) => T
    (property-list-p '(foo 1)) => T
    (property-list-p '(:a 1 :b 2)) => T

## [Type] property-list

    property-list &optional (value-type '*)

Equivalent to `(and list (satisfies
property-list-p))`. VALUE-TYPE is just ignored.

Examples:

    (typep '(:a 1 :b 2) '(property-list integer)) => T
    (typep '(:a 1 :b 2) '(property-list string)) => T

## [Function] association-list-p

    association-list-p var

Returns true if OBJECT is an association list.

Examples:

    (association-list-p 1) => NIL
    (association-list-p '(1 2 3)) => NIL
    (association-list-p nil) => T
    (association-list-p '((foo))) => T
    (association-list-p '((:a . 1) (:b . 2))) => T

## [Type] association-list

    association-list &optional (key-type '*) (value-type '*)

Equivalent to `(proper-list (cons KEY-TYPE VALUE-TYPE))`. KEY-TYPE
and VALUE-TYPE are just ignored.

Examples:

    (typep '((:a . 1) (:b . 2)) '(association-list integer)) => T
    (typep '((:a . 1) (:b . 2)) '(association-list string)) => T

## [Function] tuplep

    tuplep object

Returns true if OBJECT is a tuple, meaning a proper list.

Examples:

    (tuplep 1) => NIL
    (tuplep '(1 . 2)) => NIL
    (tuplep nil) => T
    (tuplep '(1 2 3)) => T

## [Type] tuple

    tuple &rest element-types

Equivalent to `(and list (cons ARG1 (cons ARG2 (cons ARG3 ...))))`
where ARGn is each element of ELEMENTS-TYPES.

Examples:

    (typep 1 'tuple) => NIL
    (typep '(1 . 2) 'tuple) => NIL
    (typep '(1 2 3) 'tuple) => NIL
    (typep '(1 2 3) '(tuple integer integer)) => NIL
    (typep '(1 2 3) '(tuple string integer integer)) => NIL
    (typep nil 'tuple) => T
    (typep nil '(tuple)) => T
    (typep '(1 2 3) '(tuple integer integer integer)) => T

## [Type] character-designator

    character-designator

## [Type] function-designator

    function-designator

## [Type] file-position-designator

    file-position-designator

## [Type] list-designator

    list-designator

## [Type] package-designator

    package-designator

## [Type] pathname-designator

    pathname-designator

## [Type] stream-designator

    stream-designator

## [Type] string-designator

    string-designator

## [Function] file-associated-stream-p

    file-associated-stream-p stream

Returns true if STREAM is a stream associated to a file.

## [Type] file-associated-stream

    file-associated-stream

Equivalent to `(and stream (satisfies file-associated-stream-p))`.

## [Type] non-nil

    non-nil &optional type

Equivalent to `(and (not null) TYPE)` if TYPE is given,
otherwise `(not null)`.

Examples:

    (typep nil '(non-nil symbol)) => NIL

## [Function] type-specifier-p

    type-specifier-p type-specifier

Returns true if TYPE-SPECIFIER is a valid type specfiier.

## [Function] type-expand

    type-expand type-specifier &optional env

Expand TYPE-SPECIFIER in the lexical environment ENV.

Authors
-------

* Tomohiro Matsuyama

License
-------

LLGPL
