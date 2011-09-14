(in-package :trivial-types)

(declaim (inline proper-list-p
                 property-list-p
                 association-list-p
                 tuplep))

(defmacro %proper-list-p (var &optional (element-type '*))
  `(loop
     (typecase ,var
       (null (return t))
       (cons (if (or ,(eq element-type '*)
                     (typep (car ,var) ,element-type))
                 (setq ,var (cdr ,var))
                 (return)))
       (t    (return)))))

(defun proper-list-p (object)
  (declare (optimize . #.*standard-optimize-qualities*))
  (%proper-list-p object))

(deftype proper-list (&optional (element-type '*))
  (declare (ignore element-type))
  '(and list (satisfies proper-list-p)))

(defun property-list-p (object)
  (declare (optimize . #.*standard-optimize-qualities*))
  (typecase object
    (null t)
    (cons
     (loop
       (if (null object) 
           (return t)
           (let ((key (car object))
                 (next (cdr object)))
             (if (or (not (keywordp key))
                     (not (consp next)))
                 (return)
                 (setq object (cdr next)))))))))

(deftype property-list (&optional (value-type '*))
  (declare (ignore value-type))
  '(and list (satisfies property-list-p)))

(defun association-list-p (var)
  (declare (optimize . #.*standard-optimize-qualities*))
  (%proper-list-p var 'cons))

(deftype association-list (&optional (key-type '*) (value-type '*))
  `(proper-list (cons ,key-type ,value-type)))

(defun tuplep (object)
  (declare (optimize . #.*standard-optimize-qualities*))
  (%proper-list-p object))

(deftype tuple (&rest element-types)
  `(and list
        ,(reduce (lambda (element-type type) `(cons ,element-type ,type))
                 element-types
                 :from-end t
                 :initial-value 'null)))
