(defpackage snaky.operators
  (:use :cl)
  (:shadow :and 
           :or
           :*
           :+
           :class)
  (:export :and
           :and-expressions
           :or
           :or-expressions
           :str
           :str-string
           :class
           :class-negative
           :class-chars
           :class-ranges
           :any
           :repeat
           :repeat-expression
           :repeat-min
           :repeat-max
           :?
           :*
           :+
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
           :ret-value))
(in-package :snaky.operators)

(cl:use-package :cl)

(defstruct snaky.operators:and
  expressions)

(defstruct snaky.operators:or
  expressions)

(defstruct str
  string)

(defstruct class
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


(defun snaky.operators:and (&rest expressions)
  (make-and :expressions expressions))

(defun snaky.operators:or (&rest expressions)
  (make-or :expressions expressions))

(defun str (string)
  (make-str :string string))

(defun class (string)
  (cl:let ((i 0)
        (negative nil)
        (chars nil)
        (ranges nil))
    (when (eq (aref string i) #\^)
      (incf i)
      (setf negative t))
    (loop
      while (< i (- (length string) 2))
      do (if (eq(aref string (1+ i)) #\-)
             (progn
               (push (cons (aref string i) (aref string (cl:+ i 2))) ranges)
               (incf i 3))
             (progn
               (push (aref string i) chars)
               (incf i))))
    (loop
      for i from i below (length string)
      do (push (aref string i) chars))
    (make-class :negative negative :chars chars :ranges ranges)))

(defun any ()
  (make-any))

(defun repeat (expression min max)
  (make-repeat :expression expression :min min :max max))

(defun ? (expression)
  (make-repeat :expression expression :min nil :max 1))

(defun snaky.operators:* (expression)
  (make-repeat :expression expression :min nil :max nil))

(defun snaky.operators:+ (expression)
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
