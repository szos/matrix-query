
(in-package :matrix-query)

(defparameter *threading-lock* (bt:make-lock "matrix-query api lock"))

(defmacro threaded-interface (&rest forms)
  `(bt:with-lock-held (*threading-lock*)
     ,@forms))


(defvar *process-queue* '())
(defvar *process-queue-lock* (bt:make-lock "process-queue-lock"))
(defvar *process-queue-semaphore*
  (bt:make-semaphore :name "process-queue-semaphore"))
(defparameter *process-queue-thread*
  (bt:make-thread (lambda ()
		    (loop
		      (bt:wait-on-semaphore *process-queue-semaphore*)
		      (evaluate-request)))))

(defvar *form-log* '()
  "this stores a log of all forms executed by process-request")

(defun evaluate-request ()
  (let (form)
    (bt:with-lock-held (*process-queue-lock*)
      (setf form (pop *process-queue*)))
    (setf *form-log* (cons form *form-log*))
    (eval form)))

(defmacro request-processing (form)
  `(bt:with-lock-held (*process-queue-lock*)
     (setf *process-queue* (append *process-queue* (list ',form)))
     (bt:signal-semaphore *process-queue-semaphore*)))

;; (setf *process-queue* (append *process-queue* '(loop for x in )))

;; (defmacro for (var in &body do)
;;   `(loop for ,var in ,in
;; 	 do ,@do))
