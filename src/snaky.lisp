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
    ((characterp exp)
     (str (format nil "~a" exp)))
    ((listp exp)
     (cond
       ((string= (first exp) 'and)
        (apply #'seq (mapcar #'read-expression (cdr exp))))
       ((string= (first exp) 'or)
        (apply #'ordered-choice (mapcar #'read-expression (cdr exp))))
       ((string= (first exp) 'str)
        (unless (= (length exp) 2)
          (error "`str` operator takes 1 arguments"))
        (str (second exp)))
       ((string= (first exp) 'cc)
        (unless (= (length exp) 2)
          (error "`cc` operator takes 1 arguments"))
        (character-class (second exp)))
       ((string= (first exp) 'any)
        (unless (= (length exp) 1)
          (error "`any` operator takes no argument"))
        (any))
       ((string= (first exp) 'rep)
        (unless (= (length exp) 4)
          (error "`rep` operator takes 3 arguments"))
        (repeat (read-expression (second exp)) (third exp) (fourth exp)))
       ((string= (first exp) '?)
        (unless (= (length exp) 2)
          (error "`?` operator takes 1 arguments"))
        (? (read-expression (second exp))))
       ((string= (first exp) '*)
        (unless (= (length exp) 2)
          (error "`*` operator takes 1 arguments"))
        (%* (read-expression (second exp))))
       ((string= (first exp) '+)
        (unless (= (length exp) 2)
          (error "`+` operator takes 1 arguments"))
        (%+ (read-expression (second exp))))
       ((string= (first exp) 'call)
        (unless (>= (length exp) 2)
          (error "`call` operator takes at least 1 arguments"))
        (call (second exp) (mapcar #'read-expression (cddr exp))))
       ((string= (first exp) 'cap)
        (unless (= (length exp) 2)
          (error "`cap` operator takes 1 arguments"))
        (capture (read-expression (second exp))))
       ((string= (first exp) '&)
        (unless (= (length exp) 2)
          (error "`&` operator takes 1 arguments"))
        (& (read-expression (second exp))))
       ((string= (first exp) '!)
        (unless (= (length exp) 2)
          (error "`!` operator takes 1 arguments"))
        (! (read-expression (second exp))))
       ((string= (first exp) '@)
        (unless (= (length exp) 2)
          (error "`@` operator takes 1 arguments"))
        (@ (read-expression (second exp))))
       ((string= (first exp) 'ret)
        (unless (= (length exp) 2)
          (error "`ret` operator takes 1 arguments"))
        (ret (second exp)))
       ((string= (first exp) 'mod)
        (unless (= (length exp) 3)
          (error "`mod` operator takes 2 arguments"))
        (modify (read-expression (second exp)) (third exp)))
       ((string= (first exp) 'grd)
        (unless (= (length exp) 3)
          (error "`grd` operator takes 2 arguments"))
        (guard (read-expression (second exp)) (third exp)))
       ((string= (first exp) 'grp)
        (unless (= (length exp) 3)
          (error "`grp` operator takes 2 arguments"))
        (group (second exp) (read-expression (third exp))))
       ((string= (first exp) 'wst)
        (unless (= (length exp) 2)
          (error "`wst` operator takes 1 arguments"))
        (waste (read-expression (second exp))))
       (t
        (call (first exp) (mapcar #'read-expression (cdr exp))))))
    ((symbolp exp)
     (call exp))
    (t
     (error "invalid rule form: ~s" exp))))

(defmacro with-rules (&body body)
  `(let ((*rules* (make-hash-table :test 'eq)))
     ,@body))

(defmacro defrule (name-or-list exp)
  (let ((name name-or-list)
        (parameters nil))
    (when (listp name-or-list)
      (setf name (first name-or-list)
            parameters (cdr name-or-list))
      (unless (every #'symbolp parameters)
        (error "~a must be a list of symbols" parameters)))
    `(setf (gethash ',name *rules*)
           (make-rule :name ',name
                      :parameters ',parameters
                      :expression (read-expression ',exp)))))

(defmacro defparser (name rule)
  `(eval (list 'defun ',name '(text)
               '(declare (optimize (speed 3) (space 0) (safety 2)))
               (build-parser-body ',rule 'text))))

(defun parse (rule text)
  (eval (build-parser-body rule text)))
