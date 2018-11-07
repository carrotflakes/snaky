(defpackage snaky
  (:use :cl
        :snaky.generate)
  (:export :defrule
           :parse))
(in-package :snaky)

(defvar *cache*)

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

(defmacro read-cache (name)
  `(let ((cache (gethash (cons ',name pos*) *cache*)))
     (when cache
       (if (eq cache 'failed)
           (return-from ,name nil)
           (return-from ,name (values t (car cache) (cdr cache)))))))

(defmacro write-cache-succ (name)
  `(setf (gethash (cons ',name pos*) *cache*)
         (cons pos values)))

(defmacro write-cache-fail (name)
  `(setf (gethash (cons ',name pos*) *cache*)
         'failed))

(defmacro defrule (name exp)
  `(defun ,name (pos &aux values (pos* pos))
     (declare (optimize (speed 3) (space 0) (safety 2)))
     (read-cache ,name)
     ,(print (generate (read-expression exp)
                       `(progn
                          (write-cache-succ ,name)
                          (return-from ,name (values t pos values)))
                       `(progn
                          (write-cache-fail ,name)
                          (return-from ,name nil))))))

(defun parse (name text)
  (let ((*text* text)
        (*text-length* (length text))
        (*cache* (make-hash-table :test 'equal)))
    (multiple-value-bind
          (succ pos values) (funcall (symbol-function name) 0)
      (if (and succ (= (length *text*) pos))
          (first values)
          (error "parse failed")))))

;; -? -| ~ \V $input $pos $row $colu,m
