(defclass ast-node ()
  ((reducible :initform t
              :reader reduciblep)))

(defclass expression (ast-node) ())


(defmethod print-object ((n ast-node) stream)
  (let* ((cls (class-of n))
         (name (class-name cls))
         (slot-names (mapcar #'sb-mop:slot-definition-name
                             (sb-mop:class-slots cls)))
         (slot-names-values (mapcar #'(lambda (sn)
                                        (list sn (slot-value n sn)))
                                    slot-names)))
    (format stream "#<~A ~{~{~A=~A~}~^, ~}>" name slot-names-values)))

(defgeneric as-string (node))
(defgeneric reduce-node (node &optional env))

(defgeneric infix-print (node)
  (:method ((node expression))
    (format t "~A~%" (as-string node))))


(defclass number-node (expression)
  ((value :initarg :value
          :initform nil
          :accessor get-value)
   (reducible :initform nil)))

(defmethod as-string ((node number-node))
  (format nil "~A" (get-value node)))

(defclass binop-node (expression)
  ((left :initarg :left
         :initform nil
         :accessor get-left)
   (right :initarg :right
          :initform nil
          :accessor get-right)
   (op :initform nil
       :accessor get-op)))

(defmethod as-string ((node binop-node))
  (format nil "~A ~A ~A"
          (as-string (get-left node))
          (get-op node)
          (as-string (get-right node))))


(defmethod reduce-node ((node binop-node) &optional env)
  (let ((cls (class-of node))
        (left (get-left node))
        (right (get-right node))
        (op (get-op node)))
    (cond ((reduciblep left)
         (make-instance cls
                        :left (reduce-node left)
                        :right right))
        ((reduciblep right)
         (make-instance cls
                        :left left
                        :right (reduce-node right)))
        (t
         (make-instance 'number-node
                        :value (funcall (symbol-function op)
                                      (get-value left)
                                      (get-value right)))))))


(defclass add-node (binop-node)
  ((op :initform '+)))


(defclass multiply-node (binop-node)
  ((op :initform '*)))


(defclass statement (ast-node) ())


(defclass do-nothing-node (statement)
  ((reducible :initform nil)))


(defclass assign-node (statement)
  ((name :initarg :name
         :initform nil
         :accessor get-name)
   (expression :initarg :expression
               :initform nil
               :accessor get-expression)))

(defmethod as-string ((node assign-node))
  (format nil "~A = ~A"
          (get-name node)
          (get-expression node)))

(defmethod reduce-node ((node statement) &optional env)
  (let ((expr (get-expression node)))
    (if (reduciblep expr)
        (values (make-instance 'assign-node
                               :expression (reduce-node expr env))
                env)
        (values (make-instance 'do-nothing)
                (merge-hash-tables env
                                   (alist-to-hash
                                    '(((get-name node) .
                                       (get-expression node)))))))))
