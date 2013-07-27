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


(defun hash-literal (stream char)
  (declare (ignore char))
  (let ((kvs (gensym))
        (result (gensym))
        (key (gensym))
        (value (gensym)))
    `(let* ((,kvs (mapcar #'(lambda (el)
                              (if (and (not (eq el 'quote))
                                       (listp el))
                                  (eval el)
                                  el))
                          (quote ,(read-delimited-list #\} stream))))
            (,result (make-hash-table
                      :test #'equal
                      :size (length ,kvs))))
       (loop for (,key ,value) on ,kvs while ,value
            do (let ((,key ,key)
                     (,value ,value))
                 (setf (gethash ,key ,result) ,value)))
       ,result)))

(set-macro-character #\{ #'hash-literal)
(set-macro-character #\} (get-macro-character #\) nil))
