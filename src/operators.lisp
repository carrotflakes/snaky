(defpackage snaky.operators
  (:import-from :cl
                :in-package
                :defstruct
                :defun
                :&rest)
  (:export :and
           :and-expressions
           :or
           :or-expressions
           :str
           :str-string
           :any
           :repeat
           :repeat-expression
           :repeat-min
           :repeat-max
           :call
           :call-symbol
           :capture
           :capture-expression))
(in-package :snaky.operators)

(defstruct and
  expressions)

(defstruct or
  expressions)

(defstruct str
  string)

(defstruct any)

(defstruct repeat
  expression
  min
  max)

(defstruct call
  symbol)

(defstruct capture
  expression)

(defun and (&rest expressions)
  (make-and :expressions expressions))

(defun or (&rest expressions)
  (make-or :expressions expressions))

(defun str (string)
  (make-str :string string))

(defun any ()
  (make-any))

(defun repeat (expression min max)
  (make-repeat :expression expression :min min :max max))

(defun call (symbol)
  (make-call :symbol symbol))

(defun capture (expression)
  (make-capture :expression expression))
