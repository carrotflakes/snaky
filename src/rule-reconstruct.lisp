(defpackage snaky.rule-reconstruct
  (:use :cl
        :snaky.operators)
  (:export :rule-reconstruct))
(in-package :snaky.rule-reconstruct)

(defun rule-reconstruct (name rules)
  (let ((new-rules (make-hash-table :test 'eq))
        (rule-name-map (make-hash-table :test 'equal)))
    (labels
        ((traverse (name arguments)
           (let* ((rule (or (gethash name rules)
                            (error "rule ~a is undefined" name)))
                  (parameters (rule-parameters rule))
                  (expression (rule-expression rule))
                  (key (cons name arguments))
                  (new-name (or (gethash key rule-name-map)
                                (setf (gethash key rule-name-map)
                                      (gensym (symbol-name name))))))
             (when (/= (length parameters) (length arguments))
               (error "rule ~a takes ~a arguments (given ~a)"
                      name (length parameters) (length arguments)))
             (unless (gethash new-name new-rules)
               (setf (gethash new-name new-rules)
                     :dummy
                     (gethash new-name new-rules)
                     (make-rule :name new-name
                                :expression (reconstruct expression
                                                         (mapcar #'cons
                                                                 parameters arguments)))))
             new-name))
         (reconstruct (expression bindings)
           (setf expression (copy-structure expression))
           (cond
             ((typep expression 'call)
              (setf expression
                    (or (cdr (assoc (call-symbol expression) bindings)) ; FIXME
                        (call (traverse (call-symbol expression)
                                        (mapcar (lambda (expression)
                                                  (reconstruct expression bindings))
                                                (call-arguments expression)))))))
             ((slot-exists-p expression 'expressions)
              (setf (slot-value expression 'expressions)
                    (mapcar (lambda (expression) (reconstruct expression bindings))
                            (slot-value expression 'expressions))))
             ((slot-exists-p expression 'expression)
              (setf (slot-value expression 'expression)
                    (reconstruct (slot-value expression 'expression) bindings))))
           expression))
      (traverse name '()))
    (values (gethash (list name) rule-name-map) new-rules)))
