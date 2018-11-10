(defpackage snaky.operators
  (:use :cl)
  (:shadow :copy-seq)
  (:export :rule
           :make-rule
           :rule-name
           :rule-expression
           :rule-matching-name
           :expression
           :seq
           :seq-expressions
           :ordered-choice
           :ordered-choice-expressions
           :str
           :str-string
           :charactor-class
           :charactor-class-source
           :charactor-class-negative
           :charactor-class-chars
           :charactor-class-ranges
           :any
           :repeat
           :repeat-expression
           :repeat-min
           :repeat-max
           :?
           :%*
           :%+
           :call
           :call-symbol
           :capture
           :capture-expression
           :&
           :&-expression
           :!
           :!-expression
           :@
           :@-expression
           :ret
           :ret-value
           :modify
           :modify-expression
           :modify-function
           :guard
           :guard-expression
           :guard-function
           :group
           :group-name
           :group-expression
           :list-expressions))
(in-package :snaky.operators)

(cl:use-package :cl)

(defstruct seq
  expressions)

(defstruct ordered-choice
  expressions)

(defstruct str
  string)

(defstruct charactor-class
  source
  negative
  chars
  ranges)

(defstruct any)

(defstruct repeat
  expression
  min
  max)

(defstruct call
  symbol)

(defstruct capture
  expression)

(defstruct &
  expression)

(defstruct !
  expression)

(defstruct @
  expression)

(defstruct ret
  value)

(defstruct modify
  expression
  function)

(defstruct guard
  expression
  function)

(defstruct group
  name
  expression)

(defstruct rule
  name
  expression
  (inline nil)) ; future work?


(defun seq (&rest expressions)
  (make-seq :expressions expressions))

(defun ordered-choice (&rest expressions)
  (make-ordered-choice :expressions expressions))

(defun str (string)
  (make-str :string string))

(defun charactor-class (string)
  (cl:let ((i 0)
        (negative nil)
        (chars nil)
        (ranges nil))
    (when (eq (aref string i) #\^)
      (incf i)
      (setf negative t))
    (loop
      while (< i (length string))
      do (if (and (< i (- (length string) 2))
                  (eq (aref string (1+ i)) #\-))
             (progn
               (push (cons (aref string i) (aref string (cl:+ i 2))) ranges)
               (incf i 3))
             (progn
               (push (aref string i) chars)
               (incf i))))
    (make-charactor-class :source string
                          :negative negative
                          :chars chars
                          :ranges ranges)))

(defun any ()
  (make-any))

(defun repeat (expression min max)
  (make-repeat :expression expression :min (cl:or min 0) :max max))

(defun ? (expression)
  (make-repeat :expression expression :min 0 :max 1))

(defun %* (expression)
  (make-repeat :expression expression :min 0 :max nil))

(defun %+ (expression)
  (make-repeat :expression expression :min 1 :max nil))

(defun call (symbol)
  (make-call :symbol symbol))

(defun capture (expression)
  (make-capture :expression expression))

(defun & (expression)
  (make-& :expression expression))

(defun ! (expression)
  (make-! :expression expression))

(defun @ (expression)
  (make-@ :expression expression))

(defun ret (value)
  (make-ret :value value))

(defun modify (expression function)
  (make-modify :expression expression :function function))

(defun guard (expression function)
  (make-guard :expression expression :function function))

(defun group (name expression)
  (make-group :name name :expression expression))


(defun list-expressions (exp)
  (cons exp
        (case (type-of exp)
          ((seq ordered-choice)
           (mapcan (lambda (exp) (list-expressions exp))
                   (slot-value exp 'expressions)))
          ((repeat capture & ! @ modify group)
           (list-expressions (slot-value exp 'expression))))))
