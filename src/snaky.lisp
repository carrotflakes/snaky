(defpackage snaky
  (:use :cl
        :snaky.core)
  (:export :with-rules
           :defrule
           :defparser
           :parse))
(in-package :snaky)

(defun read-expression (exp)
  (cond
    ((stringp exp)
     (snaky.operators:str exp))
    ((listp exp)
     (cond
       ((string= (first exp) 'and)
        (apply #'snaky.operators:and (mapcar #'read-expression (cdr exp))))
       ((string= (first exp) 'or)
        (apply #'snaky.operators:or (mapcar #'read-expression (cdr exp))))
       ((string= (first exp) 'str)
        (snaky.operators:str (second exp)))
       ((string= (first exp) 'class)
        (snaky.operators:class (second exp)))
       ((string= (first exp) 'any)
        (snaky.operators:any))
       ((string= (first exp) 'repeat)
        (snaky.operators:repeat (read-expression (second exp))
                                (third exp) (fourth exp)))
       ((string= (first exp) '?)
        (snaky.operators:? (read-expression (second exp))))
       ((string= (first exp) '*)
        (snaky.operators:* (read-expression (second exp))))
       ((string= (first exp) '+)
        (snaky.operators:+ (read-expression (second exp))))
       ((string= (first exp) 'call)
        (snaky.operators:call (second exp)))
       ((string= (first exp) 'capture)
        (snaky.operators:capture (read-expression (second exp))))
       ((string= (first exp) '&)
        (snaky.operators:& (read-expression (second exp))))
       ((string= (first exp) '!)
        (snaky.operators:! (read-expression (second exp))))
       ((string= (first exp) '@)
        (snaky.operators:@ (read-expression (second exp))))
       ((string= (first exp) 'ret)
        (snaky.operators:ret (second exp)))
       ((string= (first exp) '->)
        (snaky.operators:-> (read-expression (second exp)) (third exp))))) ;; TODO check argument length
    (t
     (snaky.operators:call exp))))

(defmacro with-rules (&body body)
  `(let ((*rules* (make-hash-table :test 'eq)))
     ,@body))

(defmacro defrule (name exp)
  `(setf (gethash ',name *rules*) (read-expression ',exp)))

(defmacro defparser (name rule)
  `(defun ,name (text)
     (declare (optimize (speed 3) (space 0) (safety 2)))
     ,(build-parser-body rule 'text)))

(defun parse (rule text)
  (eval (build-parser-body rule text)))

;; -? -| ~ \V $input $pos $row $colu,m
