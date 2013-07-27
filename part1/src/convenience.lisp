(defun new (&rest args)
  (apply #'make-instance args))
