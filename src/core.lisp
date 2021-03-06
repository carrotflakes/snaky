(defpackage snaky.core
  (:use :cl
        :snaky.operators
        :snaky.generate
        :snaky.util
        :snaky.left-recursion-check
        :snaky.rule-reconstruct)
  (:export :*cache*
           :*rules*
           :build-parser-body))
(in-package :snaky.core)

(defvar *cache*)
(defvar *undetermined*)
(defvar *rules* (make-hash-table :test 'eq))

(declaim (type string *text*)
         (type (simple-array fixnum *) *undetermined*)
         (type fixnum *text-length* *failed-pos*))


(defun build-read-cache (name)
  `(let ((cache (gethash (cons ',name origin-pos) *cache*)))
     (when cache
       (if (eq cache 'failed)
           (return-from ,name (values nil 0 nil))
           (return-from ,name (values t (the fixnum (car cache)) (cdr cache)))))))

(defun build-write-cache (name form)
  `(setf (gethash (cons ',name origin-pos) *cache*)
         ,form))

(defun build-rule-definition (rule)
  (when (eq (rule-left-recursive rule *rules*) :left-recursive)
    (return-from build-rule-definition
      (build-rule-definition-allow-left-recursion rule)))
  (let ((name (rule-name rule))
        (exp (rule-expression rule)))
    `((pos &aux values (origin-pos pos))
      (declare (type fixnum pos origin-pos))
      (block ,name
        ,(build-read-cache name)
        ,(generate exp
                   `(progn
                      ,(build-write-cache name '(cons pos values))
                      (return-from ,name (values t pos values))))
        ,(build-write-cache name ''failed)
        (return-from ,name (values nil 0 nil))))))

(defun build-rule-definition-allow-left-recursion (rule)
  (let ((name (rule-name rule))
        (exp (rule-expression rule)))
    `((pos &aux (origin-pos pos))
      (declare (type fixnum pos origin-pos))
      (block ,name
        ,(build-read-cache name)
        (setf (aref *undetermined* origin-pos)
              (fixnum-+ (aref *undetermined* origin-pos) 1))
        ,(build-write-cache name ''failed)
        (loop
          named loop
          with last-pos fixnum = pos
          for values = nil
          do (progn
               (setf pos origin-pos)
               (block body
                 ,(generate exp
                            '(if (<= pos last-pos)
                              (return-from loop)
                              (return-from body)))
                 (return-from loop))
               (setf last-pos pos)
               ,(build-write-cache name '(cons pos values))))
        (let ((cache (gethash (cons ',name origin-pos) *cache*)))
          (setf (aref *undetermined* origin-pos)
                (the fixnum (1- (the fixnum (aref *undetermined* origin-pos)))))
          (unless (zerop (aref *undetermined* origin-pos))
            (remhash (cons ',name origin-pos) *cache*))
          (if (eq cache 'failed)
              (return-from ,name (values nil 0 nil))
              (return-from ,name (values t (car cache) (cdr cache)))))))))

(defun build-parser-body (expression text)
  (multiple-value-bind (expression *rules*) (rule-reconstruct expression *rules*)
    (maphash (lambda (key value)
               (declare (ignore value))
               (eval `(declaim (ftype (function (fixnum) (values t t t)) ,key))))
             *rules*)
    (maphash (lambda (key value)
               (setf (symbol-function key)
                     (eval `(lambda ,@(build-rule-definition value)))))
             *rules*)
    `(let* ((*text* ,text)
            (*text-length* (length *text*))
            (*failed-matches* '())
            (*failed-pos* 0)
            (*cache* (make-hash-table :test 'equal))
            (*undetermined* (make-array (1+ (length *text*))
                                        :element-type 'fixnum
                                        :initial-element 0))
            (succ nil)
            (pos 0)
            (values nil))
       (declare (type fixnum pos))
       ,(let ((block (gensym "BLOCK")))
          `(block ,block
             ,(generate expression `(progn (setf succ t)
                                           (return-from ,block)))))
       (unless (and succ (= *text-length* pos))
         (when succ
           (fail pos "end of input"))
         (when (null *failed-matches*)
           (push "something" *failed-matches*))
         (setf *failed-matches*
               (remove-duplicates *failed-matches* :test #'string=))
         (let ((found
                 (if (= *failed-pos* *text-length*)
                     "end of input"
                     (prin1-to-string (subseq *text*
                                              *failed-pos*
                                              (1+ *failed-pos*))))))
           (multiple-value-bind (line column)
               (string-line-column *text* *failed-pos*)
             (error "parse failed at line ~a column ~a: expected ~{~a~^, ~} but ~a found."
                    line
                    column
                    *failed-matches*
                    found))))
       (first values))))
