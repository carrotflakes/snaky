(defpackage snaky.util
  (:use :cl)
  (:export :string-line-column))
(in-package :snaky.util)

(defun string-line-column (string pos)
  (unless (<= 0 pos (length string))
    (error "out of range"))
  (loop
    with line = 0
    with column = 0
    with after-cr = nil
    for i from 0 below pos
    for char = (aref string i)
    do (cond
         ((eq char #\cr)
          (incf line)
          (setf after-cr t
                column 0))
         ((eq char #\lf)
          (unless after-cr
            (incf line))
          (setf after-cr nil
                column 0))
         (t
          (incf column)))
    finally (return (values line column))))
