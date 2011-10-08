(in-package :trivial-types)

(defun type-specifier-p (type-specifier)
  "Returns true if TYPE-SPECIFIER is a valid type specfiier."
  (or (documentation type-specifier 'type)
      #+sbcl (sb-ext:valid-type-specifier-p type-specifier)
      #+openmcl (ccl:type-specifier-p type-specifier)
      #+ecl (c::valid-type-specifier type-specifier)))
