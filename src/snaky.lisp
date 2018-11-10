(defpackage snaky
  (:use :cl
        :snaky.operators
        :snaky.core)
  (:export :with-rules
           :defrule
           :defparser
           :parse))
(in-package :snaky)

(defun read-expression (exp)
  (cond
    ((stringp exp)
     (str exp))
    ((listp exp)
     (cond
       ((string= (first exp) 'and)
        (apply #'seq (mapcar #'read-expression (cdr exp))))
       ((string= (first exp) 'or)
        (apply #'ordered-choice (mapcar #'read-expression (cdr exp))))
       ((string= (first exp) 'str)
        (str (second exp)))
       ((string= (first exp) 'class)
        (charactor-class (second exp)))
       ((string= (first exp) 'any)
        (any))
       ((string= (first exp) 'repeat)
        (repeat (read-expression (second exp))
                (third exp) (fourth exp)))
       ((string= (first exp) '?)
        (? (read-expression (second exp))))
       ((string= (first exp) '*)
        (%* (read-expression (second exp))))
       ((string= (first exp) '+)
        (%+ (read-expression (second exp))))
       ((string= (first exp) 'call)
        (call (second exp)))
       ((string= (first exp) 'capture)
        (capture (read-expression (second exp))))
       ((string= (first exp) '&)
        (& (read-expression (second exp))))
       ((string= (first exp) '!)
        (! (read-expression (second exp))))
       ((string= (first exp) '@)
        (@ (read-expression (second exp))))
       ((string= (first exp) 'ret)
        (ret (second exp)))
       ((string= (first exp) '->)
        (-> (read-expression (second exp)) (third exp))))) ;; TODO check argument length
    (t
     (call exp))))

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
