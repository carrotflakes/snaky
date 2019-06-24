(defpackage snaky.left-recursion-check
  (:use :cl
        :snaky.operators)
  (:export :rule-left-recursive))
(in-package :snaky.left-recursion-check)

(defvar *rules*)

(defun expression-left-recursive (exp rule &optional visited-rules)
  (ecase (type-of exp)
    ((seq)
     (assert (consp (seq-expressions exp)))
     (loop
       for exp in (seq-expressions exp)
       for property = (expression-left-recursive exp rule visited-rules)
       when (member property '(:progressive :left-recursive))
       return property
       finally (return :stagnant)))
    ((ordered-choice)
     (let ((property :progressive))
       (loop
         for exp in (ordered-choice-expressions exp)
         for property* = (expression-left-recursive exp rule visited-rules)
         when (eq property* :left-recursive)
         do (setf property :left-recursive)
         when (and (eq property* :stagnant) (eq property :progressive))
         do (setf property :stagnant))
       property))
    ((str)
     (if (zerop (length (str-string exp)))
         :stagnant
         :progressive))
    ((character-class any)
     :progressive)
    ((repeat)
     (let ((property
             (expression-left-recursive (slot-value exp 'expression)
                                        rule
                                        visited-rules)))
       (if (and (zerop (repeat-min exp)) (eq property :progressive))
           :stagnant
           property)))
    ((capture & ! @ modify guard waste group)
     (expression-left-recursive (slot-value exp 'expression)
                                rule
                                visited-rules))
    ((call)
     (let ((symbol (call-symbol exp)))
       (cond
         ((eq rule symbol)
          :left-recursive)
         ((member symbol visited-rules)
          :stagnant)
         (t
          (expression-left-recursive
           (rule-expression (or (gethash symbol *rules*)
                                (error "rule ~a is undefined" symbol)))
           rule
           (cons symbol visited-rules))))))
    ((ret)
     :stagnant)))

(defun rule-left-recursive (rule rules)
  (let* ((*rules* rules)
         (property (expression-left-recursive (rule-expression rule) (rule-name rule))))
    (assert property)
    property))
