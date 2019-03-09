(defpackage snaky.operators
  (:use :cl)
  (:shadow :copy-seq)
  (:export :rule
           :make-rule
           :rule-name
           :rule-parameters
           :rule-expression
           :rule-inline
           :rule-matching-name
           :expression
           :expressions
           :seq
           :seq-expressions
           :ordered-choice
           :ordered-choice-expressions
           :str
           :str-string
           :character-class
           :character-class-source
           :character-class-negative
           :character-class-chars
           :character-class-ranges
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
           :call-arguments
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
           :waste
           :waste-expression
           :group
           :group-name
           :group-expression
           :list-expressions
           :copy-expression))
(in-package :snaky.operators)

(cl:use-package :cl)

(defstruct seq
  expressions)

(defstruct ordered-choice
  expressions)

(defstruct str
  string)

(defstruct character-class
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
  symbol
  arguments)

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

(defstruct waste
  expression)

(defstruct group
  name
  expression)

(defstruct rule
  name
  parameters
  expression
  (inline nil))


(defun seq (&rest expressions)
  (make-seq :expressions expressions))

(defun ordered-choice (&rest expressions)
  (make-ordered-choice :expressions expressions))

(defun str (string)
  (make-str :string string))

(defun parse-hex (string)
  (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from parse-hex))))
    (parse-integer string :radix 16)))

(defun character-class (string)
  (setf string
        (concatenate
         'string
         (loop
           with i = 0
           while (< i (length string))
           collect (cond
                     ((and (<= 6 (- (length string) i))
                           (char= (char string i) #\backslash)
                           (char= (char string (+ i 1)) #\u)
                           (parse-hex (subseq string (+ i 2) (+ i 6))))
                      (incf i 6)
                      (code-char (parse-hex (subseq string (- i 4) i))))
                     ((and (<= 4 (- (length string) i))
                           (char= (char string i) #\backslash)
                           (char= (char string (+ i 1)) #\x)
                           (parse-hex (subseq string (+ i 2) (+ i 4))))
                      (incf i 4)
                      (code-char (parse-hex (subseq string (- i 2) i))))
                     (t
                      (incf i)
                      (char string (1- i)))))))
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
    (make-character-class :source string
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

(defun call (symbol &optional arguments)
  (make-call :symbol symbol :arguments arguments))

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

(defun waste (expression)
  (make-waste :expression expression))

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

(defun copy-expression (exp)
  (setf exp (copy-structure exp))
  (when (slot-exists-p exp 'expressions)
    (setf (slot-value exp 'expressions)
          (mapcar #'copy-expression (slot-value exp 'expressions))))
  (when (slot-exists-p exp 'expression)
    (setf (slot-value exp 'expression)
          (copy-expression (slot-value exp 'expression))))
  exp)
