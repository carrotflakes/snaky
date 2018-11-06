(defpackage snaky
  (:use :cl
        :snaky.generate)
  (:export :defrule
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
        (snaky.operators:ret (second exp))))) ;; TODO check argument length
    (t
     (snaky.operators:call exp))))

(defmacro defrule (name exp)
  `(defun ,name ()
     ,(print (generate (read-expression exp)
               `(return-from ,name t)
               `(return-from ,name nil)))))

(defun parse (name text &optional (pos 0))
  (let ((*text* text)
        (*text-length* (length text))
        (*pos* pos)
        (*values* nil))
    (if (and (funcall (symbol-function name))
             (= (length *text*) *pos*))
        (first *values*)
        (error "parse failed"))))

;; -> -? -| ~ \V $input $pos $row $colu,m
