(defparameter *hash-table-from-test* #'equal)

(defun merge-hash-tables (h1 h2)
  (when (not (eql (hash-table-test h1) (hash-table-test h2)))
    (error "cannot merge hash tables with different tests"))

  (let ((result (make-hash-table
                 :test (hash-table-test h1)
                 :size (hash-table-count h1))))
    (flet ((update-result (key value)
               (setf (gethash key result) value)))
    (maphash #'update-result h1)
    (maphash #'update-result h2)
    result)))


(defun alist-to-hash (kvs &optional (test #'equal))
  (let ((result (make-hash-table
                 :test test
                 :size (length kvs))))
    (loop for (key . value) in kvs
         do (let ((key key)
                  (value value))
              (setf (gethash key result) value)))
    result))


(defun hash-from (&rest args)
  (assert (evenp (length args)))
  (let ((h (make-hash-table :test *hash-table-from-test*
                            :size (floor (/ (length args) 2)))))
    (loop for (key value) on args while value
         do (let ((key key)
                  (value value))
              (setf (gethash key h) value)))
    h))


(defun print-hash (h)
  (format nil "{~{~A~^ ~}}" (loop
                             for key being the hash-keys of h
                             using (hash-value value)
                             nconc (list key value))))
