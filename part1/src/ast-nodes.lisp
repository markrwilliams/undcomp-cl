(defclass ast-node ()
  ((reducible :initform t
              :reader reduciblep)))

(defclass expression (ast-node) ())

(defgeneric reduce-node (node env))

(defclass value-node (expression)
  ((value :initarg :value
          :initform nil
          :accessor get-value)
   (reducible :initform nil)))

(defmethod print-object ((node value-node) stream)
  (format stream "~A" (get-value node)))


(defclass number-node (value-node) ())


(defclass binop-node (expression)
  ((left :initarg :left
         :initform nil
         :accessor get-left)
   (right :initarg :right
          :initform nil
          :accessor get-right)
   (op :initform nil
       :reader get-op)
   (result-class :initform nil
                 :reader get-result-class)))

(defmethod print-object ((node binop-node) stream)
  (format stream "~A ~A ~A"
          (get-left node)
          (get-op node)
          (get-right node)))


(defmethod reduce-node ((node binop-node) env)
  (let ((cls (class-of node))
        (left (get-left node))
        (right (get-right node)))
    (cond ((reduciblep left)
           (new cls
                :left (reduce-node left env)
                :right right))
          ((reduciblep right)
           (new cls
                :left left
                :right (reduce-node right env)))
          (t
           (new (get-result-class node)
                :value (funcall (symbol-function
                                 (get-op node))
                                (get-value left)
                                (get-value right)))))))


(defclass add-node (binop-node)
  ((op :initform '+)
   (result-class :initform 'number-node)))


(defclass multiply-node (binop-node)
  ((op :initform '*)
   (result-class :initform 'number-node)))


(defclass boolean-node (value-node) ())


(defclass less-than-node (binop-node)
  ((op :initform '<)
   (result-class :initform 'boolean-node)))


(defclass variable-node (ast-node)
  ((name :initarg :name
         :reader get-name)))

(defmethod print-object ((v variable-node) stream)
  (format stream "~A" (get-name v)))

(defmethod reduce-node ((v variable-node) env)
  (gethash (get-name v) env))


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

(defmethod print-object ((node assign-node) stream)
  (format stream "~A = ~A"
          (get-name node)
          (get-expression node)))


(defmethod reduce-node ((node assign-node) env)
  (let ((expr (get-expression node)))
    (if (reduciblep expr)
        (values (new 'assign-node
                     :name (get-name node)
                     :expression (reduce-node expr env))
                env)
        (values (new 'do-nothing-node)
                (merge-hash-tables
                 env
                 (hash-from (get-name node)
                            (get-expression node)))))))
