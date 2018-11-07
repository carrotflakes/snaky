(defpackage snaky.core
  (:use :cl
        :snaky.generate)
  (:import-from :snaky.operators
                :call
                :list-expressions)
  (:export :*cache*
           :*rules*
           :build-parser-body))
(in-package :snaky.core)

(defvar *cache*)
(defvar *rules* (make-hash-table :test 'eq))

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

(defun build-rule-definition (name rule)
  `((pos &aux values (pos* pos))
    (read-cache ,name)
    ,(generate rule
               `(progn
                  (write-cache-succ ,name)
                  (return-from ,name (values t pos values)))
               `(progn
                  (write-cache-fail ,name)
                  (return-from ,name nil)))))

(defun rule-dependencies (name &optional dependencies)
  (dolist (exp (list-expressions (gethash name *rules*)))
    (when (typep exp 'call)
      (let ((symbol (slot-value exp 'symbol)))
        (unless (member symbol dependencies)
          (setf dependencies (rule-dependencies symbol (cons symbol dependencies)))))))
  dependencies)

(defun build-parser-body (initial-rule text)
  (let ((definitions
            (mapcar
             (lambda (name)
               `(,name ,@(build-rule-definition name (gethash name *rules*))))
             (cons initial-rule (rule-dependencies initial-rule)))))
    `(let* ((*text* ,text)
            (*text-length* (length *text*))
            (*cache* (make-hash-table :test 'equal)))
       (labels ,definitions
         (multiple-value-bind (succ pos values)
             (,initial-rule 0)
           (if (and succ (= (length *text*) pos))
               (first values)
               (error "parse failed")))))))
