(defclass machine ()
  ((expr :initarg :expr
         :initform nil
         :accessor get-machine-expr)
   (env :initarg :env
        :initform nil
        :accessor get-machine-env)))

(defmethod print-object ((m machine) stream)
  (format stream "~A" (get-machine-expr m)))


(defclass statement-machine (machine)
  ((stmt :initarg :stmt
         :initform nil
         :accessor get-machine-stmt)))

(defmethod print-object ((m statement-machine) stream)
  (format stream "~A, ~A"
          (get-machine-stmt m)
          (print-hash (get-machine-env m))))


(defgeneric machine-step (m)
  (:method ((m machine))
    (setf (get-machine-expr m)
          (reduce-node (get-machine-expr m) (get-machine-env m)))))

(defmethod machine-step ((m statement-machine))
  (multiple-value-bind (stmt env)
      (reduce-node (get-machine-stmt m) (get-machine-env m))
    (setf (get-machine-stmt m) stmt)
    (setf (get-machine-env m) env)))


(defgeneric machine-run (m)
  (:method ((m machine))
    (loop
       do (progn
            (print m)
            (machine-step m))
       until (not (reduciblep (get-machine-expr m))))
    (print m)))


(defmethod machine-run ((m statement-machine))
  (loop
       do (progn
            (print m)
            (machine-step m))
       until (not (reduciblep (get-machine-stmt m))))
  (print m))


(let ((machines
       (list (new 'machine
                  :expr (new 'add-node
                             :left
                             (new 'multiply-node
                                  :left (new 'number-node :value 1)
                                  :right (new 'number-node :value 2))
                             :right
                             (new 'multiply-node
                                  :left (new 'number-node :value 3)
                                  :right (new 'number-node :value 4))))
             (new 'machine
                  :expr (new 'add-node
                          :left (new 'variable-node :name 'x)
                          :right (new 'variable-node :name 'y))
                  :env (hash-from 'x (new 'number-node :value 3)
                                  'y (new 'number-node :value 4)))
             (new 'statement-machine
                  :stmt (new 'assign-node
                             :name 'x
                             :expression
                             (new 'add-node
                                  :left (new 'variable-node :name 'x)
                                  :right (new 'number-node :value 1)))
                  :env (hash-from 'x (new 'number-node :value 2))))))
  (dolist (m machines)
    (machine-run m)))
