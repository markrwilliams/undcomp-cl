(defun new (&rest args)
  (apply #'make-instance args))

(defun partition-list (l &key on (test #'eq))
  (let (found)
    (loop for (el mid?) on l
       if found collect el into rhs
       else collect el into lhs
       when (funcall test mid? on) do (setf found t)
       finally (return (list lhs (cdr rhs))))))

(defun split-list (l &key on (test #'eq))
  (let ((cur l))
    (loop for (lhs rhs) = (partition-list cur :on on :test test)
       while cur collect lhs
       do (setf cur rhs))))

(defun make-keyword (name)
  (intern (string-upcase name) "KEYWORD"))

(defun substitute-multiple (news olds sequence)
    (mapc #'(lambda (new old)
              (setf sequence (mapcar #'(lambda (el)
                                         (cond
                                           ((eql el old) new)
                                           ((listp el) (substitute-multiple news olds el))
                                           (t el)))
                                     sequence)))
          news olds)
    sequence)
