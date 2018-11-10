(defpackage snaky.core
  (:use :cl
        :snaky.operators
        :snaky.generate)
  (:export :*cache*
           :*rules*
           :build-parser-body))
(in-package :snaky.core)

(defvar *cache*)
(defvar *undetermined*)
(defvar *rules* (make-hash-table :test 'eq))

(defun expression-left-recursive (exp rule &optional visited-rules)
  (ecase (type-of exp)
    ((seq)
     (loop
       for exp in (seq-expressions exp)
       for property = (expression-left-recursive exp rule visited-rules)
       when (member property '(:progressive :left-recursive))
       return property))
    ((ordered-choice)
     (let ((property :progressive))
       (loop
         for exp in (ordered-choice-expressions exp)
         for property* = (expression-left-recursive exp rule visited-rules)
         when (eq property* :left-recursive)
         do (setf property :left-recursive)
         when (and (eq property* :stagnant) (eq property :progressive))
         do (setf property :stagnant))
       property))
    ((str)
     (if (zerop (length (str-string exp)))
         :stagnant
         :progressive))
    ((charactor-class any)
     :progressive)
    ((repeat)
     (let ((property
             (expression-left-recursive (slot-value exp 'expression)
                                        rule
                                        visited-rules)))
       (if (and (zerop (repeat-min exp)) (eq property :progress))
           :stangnant
           property)))
    ((capture & ! @ ->)
     (expression-left-recursive (slot-value exp 'expression)
                                rule
                                visited-rules))
    ((call)
     (let ((symbol (call-symbol exp)))
       (cond
         ((eq rule symbol)
          :left-recursive)
         ((member symbol visited-rules)
          :stagnant)
         (t
          (expression-left-recursive (gethash symbol *rules*)
                                     rule
                                     (cons symbol visited-rules))))))
    ((ret))))

(defun build-read-cache (name)
  `(let ((cache (gethash (cons ',name origin-pos) *cache*)))
     (when cache
       (if (eq cache 'failed)
           (return-from ,name nil)
           (return-from ,name (values t (car cache) (cdr cache)))))))

(defun build-write-cache (name form)
  `(setf (gethash (cons ',name origin-pos) *cache*)
         ,form))

(defun build-rule-definition (name rule)
  (when (eq (expression-left-recursive rule name) :left-recursive)
    (return-from build-rule-definition
      (build-rule-definition-allow-left-recursion name rule)))
  `(,name (pos &aux values (origin-pos pos))
    ,(build-read-cache name)
    ,(generate rule
               `(progn
                  ,(build-write-cache name '(cons pos values))
                  (return-from ,name (values t pos values)))
               `(progn
                  ,(build-write-cache name ''failed)
                  (return-from ,name nil)))))

(defun build-rule-definition-allow-left-recursion (name rule)
  `(,name (pos &aux (origin-pos pos))
    ,(build-read-cache name)
    (incf (aref *undetermined* origin-pos))
    ,(build-write-cache name ''failed)
    (loop
      named loop
      with last-pos = pos
      for values = nil
      do (progn
           (setf pos origin-pos)
           (block body
             ,(generate rule
                        '(if (<= pos last-pos)
                          (return-from loop)
                          (return-from body))
                        '(return-from loop)))
           (setf last-pos pos)
           ,(build-write-cache name '(cons pos values))))
    (let ((cache (gethash (cons ',name origin-pos) *cache*)))
      (unless (zerop (decf (aref *undetermined* origin-pos)))
        (remhash (cons ',name origin-pos) *cache*))
      (if (eq cache 'failed)
          (return-from ,name nil)
          (return-from ,name (values t (car cache) (cdr cache)))))))

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
               (build-rule-definition name (gethash name *rules*)))
             (cons initial-rule (rule-dependencies initial-rule)))))
    `(let* ((*text* ,text)
            (*text-length* (length *text*))
            (*cache* (make-hash-table :test 'equal))
            (*undetermined* (make-array (length *text*)
                                        :element-type 'fixnum
                                        :initial-element 0)))
       (labels ,definitions
         (multiple-value-bind (succ pos values)
             (,initial-rule 0)
           (if (and succ (= (length *text*) pos))
               (first values)
               (error "parse failed")))))))
