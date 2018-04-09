(in-package :trivial-types)

(defun type-specifier-p (type-specifier)
  "Returns true if TYPE-SPECIFIER is a valid type specfiier."
  (or (documentation type-specifier 'type)
      (block nil
        #+sbcl (return (sb-ext:valid-type-specifier-p type-specifier))
        #+openmcl (return (ccl:type-specifier-p type-specifier))
        #+ecl (return (c::valid-type-specifier type-specifier))
        (error "Not implemented"))))

(defun type-expand (type-specifier &optional env)
  "Expand TYPE-SPECIFIER in the lexical environment ENV."
  #+sbcl (sb-ext::typexpand type-specifier env)
  #+openmcl (ccl::type-expand type-specifier env)
  #-(or sbcl openmcl) type-specifier)
