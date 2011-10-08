(in-package :trivial-types)

(defun file-associated-stream-p (stream)
  "Returns true if STREAM is a stream associated to a file."
  (declare (optimize . #.*standard-optimize-qualities*))
  (or (typep stream 'file-stream)
      (and (typep stream 'synonym-stream)
           (let* ((target-symbol (synonym-stream-symbol stream))
                  (target-stream (symbol-value target-symbol)))
             (declare (type symbol target-symbol)
                      (type stream target-stream))
             (file-associated-stream-p target-stream)))))

(deftype file-associated-stream ()
  "Equivalent to `(and stream (satisfies file-associated-stream-p))`."
  '(and stream (satisfies file-associated-stream-p)))
