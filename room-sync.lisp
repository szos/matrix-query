;;; room-sync.lisp

(in-package :matrix-query)

(defun sync-room-test ()
  (multiple-value-bind (stream return)
      (drakma:http-request (make-api-call "_matrix/client/r0/sync")
			   :want-stream t
			   :method :get
			   :additional-headers (authorization-header))
    ;; (destructuring-bind (next
    ;; 			 dev-one-time-key
    ;; 			 grp)
    ;; 	(yason:parse stream :object-as :alist)
    ;;   (print next)
    ;;   (print dev-one-time-key))
    (let ((parsed-stream (yason:parse stream :object-as :alist)))
      (print return)
      (print parsed-stream)
      (let* ((next-batch (cdar parsed-stream))
	     (device-one-time-keys-count (cadr parsed-stream))
	     (groups (caddr parsed-stream))
	     (rooms (cadddr parsed-stream))
	     (join (cadddr rooms)))
	(print "join here:")
	(print join)
	(loop for room in (cdr join)
	   collect (let ((room-id (car room))
			 (summary (cadr room))
			 (notifications (caddr room))
			 (ephemeral (cadddr room))
			 (account-data (fifth room))
			 (state (sixth room))
			 (timeline (seventh room)))
		     (make-instance 'matrix-room :room-id room-id
				    :prev-batch (cdr (third timeline))
				    :timeline timeline)))))))

;; (defmethod sync-room (()))
