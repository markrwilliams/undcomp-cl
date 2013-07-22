(defun merge-hash-tables (h1 h2)
  (when (not (eql (hash-table-test h1) (hash-table-test h2)))
    (error "cannot merge hash tables with different tests"))

  (let ((result (make-hash-table
                 :test (hash-table-test h1)
                 :size (hash-table-count h1))))
    (labels ((update-result (key value)
               (setf (gethash key result) value)))
    (maphash #'update-result h1)
    (maphash #'update-result h2)
    result)))


(defun alist-to-hash (alist &optional (test #'equal))
  (let ((result (make-hash-table
                 :test test
                 :size (length alist))))
    (loop for (key . value) in alist
         do (let ((key key)
                  (value value))
              (setf (gethash key result) value)))
    result))
