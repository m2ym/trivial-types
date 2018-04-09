(in-package :trivial-types)

(defun type-specifier-p (type-specifier)
  "Returns true if TYPE-SPECIFIER is a valid type specfiier."
  (or (documentation type-specifier 'type)
      (block nil
        #+sbcl (return (sb-ext:valid-type-specifier-p type-specifier))
        #+openmcl (return (ccl:type-specifier-p type-specifier))
        #+ecl (return (c::valid-type-specifier type-specifier))
        #+clisp (return (null
                         (nth-value 1 (ignore-errors
                                       (ext:type-expand type-specifier)))))
        (error "TYPE-SPECIFIER-P not available for this implementation"))))

(defun type-expand (type-specifier &optional env)
  "Expand TYPE-SPECIFIER in the lexical environment ENV."
  (or (block nil
        #+sbcl (return (sb-ext::typexpand type-specifier env))
        #+openmcl (return (ccl::type-expand type-specifier env))
        #+clisp (return (ext:type-expand type-specifier)))
      (prog1 type-specifier
        (warn "TYPE-EXPAND not available for this implementation"))))
