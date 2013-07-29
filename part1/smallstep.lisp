;; convenience!
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

(defun instantiation-shorthand (stream char)
  (declare (ignore char))
  (destructuring-bind (class-name &rest args)
      (read-delimited-list #\} stream)
    `(make-instance ',class-name ,@args)))

(set-macro-character #\{ 'instantiation-shorthand)
(set-macro-character #\} (get-macro-character #\) nil))

;; implementation
(defclass ast-node () ())

(defgeneric reduciblep (obj)
  (:method ((obj ast-node)) t)
  (:method (obj) nil))

(defgeneric reduced (obj)
  (:method (obj) nil))

(defvar *environment* (make-hash-table))

(defun evaluate (tree)
  (loop do
       (print tree)
       (setf tree (reduced tree))
     while (reduciblep tree)))

(defmacro create-node (name fields)
  (let ((slots (mapcar #'(lambda (f)
                           (list f :initarg (make-keyword (symbol-name f))))
                       fields)))
    `(defclass ,name (ast-node) ,slots)))


(defmacro generate-reductions (class reductions class-fields)
  (flet ((which-to-reduce (reduction)
           (dolist (el reduction)
             (when (and (listp el) (eq (first el) 'reduced))
               (return (cadr el))))))
    (let* ((reductions (reverse reductions))
           (t-clause (first (pop reductions)))
           (cond-clauses (list (list t t-clause))))
      `(progn
         (defmethod print-object ((obj ,class) stream)
           (with-slots ,class-fields obj
             (let ((news (mapcar #'(lambda (f)
                                     (format nil "~A" f))
                                 (list ,@class-fields))))
               (format stream "~A" (substitute-multiple
                                    news
                                    ',class-fields
                                    ',(if (listp t-clause)
                                          t-clause
                                          (list t-clause)))))))
         (defmethod reduced ((obj ,class))
           (with-slots ,class-fields obj
             (cond
               ,@(dolist (reduction reductions cond-clauses)
                         (let ((reduced-class (first reduction))
                               (args (rest reduction)))
                           (push
                            `((reduciblep ,(which-to-reduce args))
                              (make-instance ',reduced-class ,@args))
                            cond-clauses))))))))))


(defmacro parse-rules (rules)
  (let (body)
    (dolist (rule rules)
      (destructuring-bind (definition or-reductions)
          (partition-list rule :on '->)
        (let ((class-name (first definition))
              (class-fields (rest definition))
              (reductions (split-list or-reductions :on 'or)))
          (push `(create-node ,class-name ,class-fields) body)
          (push `(generate-reductions ,class-name ,reductions
                                      ,class-fields) body))))
    `(progn ,@(reverse body))))

;; rules
(parse-rules ((Add left right -> Add :left (reduced left)
                                     :right right
                              or Add :left left
                                     :right (reduced right)
                              or (+ left right))
              (Multiply left right -> Multiply :left (reduced left)
                                               :right right
                                   or Multiply :left left
                                               :right (reduced right)
                                   or (* left right))
              (LessThan left right -> LessThan :left (reduced left)
                                               :right right
                                   or LessThan :left left
                                               :right (reduced right)
                                   or (< left right))
              (Variable_ name -> (gethash name *environment*))
              (Assign! name value -> Assign! :name name
                                             :value (reduced value)
                                 or (setf (gethash name *environment*) value))
              (Sequence! first second -> Sequence! :first (reduced first)
                                                   :second second
                                      or second)
              (If! condition consequence alternative -> If! :condition (reduced condition)
                                                            :consequence consequence
                                                            :alternative alternative
                                                     or (if condition
                                                            (reduced consequence)
                                                            (reduced alternative)))))
;; sometimes the rules aren't enough
(defmethod print-object ((obj Sequence!) stream)
  (with-slots (first second) obj
    (format stream "~A; ~A" first second)))

(defclass While! (ast-node)
  ((condition :initarg :condition)
   (body :initarg :body)))

(defmethod reduced ((obj While!))
  (with-slots (condition body) obj
    {If! :condition condition
    :consequence {Sequence! :first body :second obj}
    :alternative nil}))

(defmethod print-object ((obj While!) stream)
  (with-slots (condition body) obj
      (format stream "(WHILE-LOOP ~A ~A)" condition body)))

;; small step
(setf (gethash 'x *environment*) 1)

(evaluate {While! :condition {LessThan :left {Variable_ :name 'x}
                                       :right 5}
                  :body {Assign! :name 'x
                                 :value {Multiply :left {Variable_ :name 'x}
                                                  :right 3}}})
(print (gethash 'x *environment*))
