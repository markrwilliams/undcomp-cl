(defclass machine ()
  ((expr :initarg :expr
         :initform nil
         :accessor get-machine-expr)
   (env :initarg :env
        :initform nil
        :accessor get-machine-env)))

(defgeneric machine-step (m)
  (:method ((m machine))
    (setf (get-machine-expr m)
          (reduce-node (get-machine-expr m) (get-machine-env m)))))

(defgeneric machine-run (m)
  (:method ((m machine))
    (loop
       do (progn
            (format t "~A~%" (get-machine-expr m))
            (machine-step m))
       until (not (reduciblep (get-machine-expr m))))
    (format t "~A~%" (get-machine-expr m))))


(let ((m (new 'machine
              :expr (new 'add-node
                         :left
                         (new 'multiply-node
                              :left (new 'number-node :value 1)
                              :right (new 'number-node :value 2))
                         :right
                         (new 'multiply-node
                              :left (new 'number-node :value 3)
                              :right (new 'number-node :value 4))))))
  (machine-run m))


(let* ((m (new 'machine
               :expr (new 'add-node
                          :left (new 'variable-node :name 'x)
                          :right (new 'variable-node :name 'y))
               :env {x (new 'number-node :value 3)
                     y (new 'number-node :value 4)})))

  (machine-run m))
