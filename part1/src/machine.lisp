(defclass machine ()
  ((expr :initarg :expr
         :initform nil
         :accessor get-machine-expr)))

(defgeneric machine-step (m)
  (:method ((m machine))
    (setf (get-machine-expr m)
          (reduce-node (get-machine-expr m)))))

(defgeneric machine-run (m)
  (:method ((m machine))
    (loop
       do (progn
            (infix-print (get-machine-expr m))
            (machine-step m))
       until (not (reduciblep (get-machine-expr m))))
    (infix-print (get-machine-expr m))))

(let ((m (make-instance
          'machine
          :expr (make-instance 'add-node
                               :left
                               (make-instance 'multiply-node
                                              :left (make-instance 'number-node :value 1)
                                              :right (make-instance 'number-node :value 2))
                               :right
                               (make-instance 'multiply-node
                                              :left (make-instance 'number-node :value 3)
                                              :right (make-instance 'number-node :value 4))))))
  (machine-run m))
