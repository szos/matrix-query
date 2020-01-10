;;; room-sync.lisp

(in-package :matrix-query)

(defparameter *next-batch* nil
  "")

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
	(setf *next-batch* next-batch)
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

(defun generate-events-from-room-timeline (room-timeline-list)
  (let ((timeline-events (strsoc "events" (if (stringp (car room-timeline-list))
					      (cdr room-timeline-list)
					      room-timeline-list))))
    (loop for event in timeline-events
       collect (make-event-object event))))

(defparameter *new-room-text* (sync-room-test))

;; (defmethod sync-room (()))

(defun update-sync ()
  (multiple-value-bind (stream return)
      (drakma:http-request (make-api-call "_matrix/client/r0/sync?since=" *next-batch*)
			   :want-stream t
			   :method :get
			   :additional-headers (authorization-header))
    (let ((parsed-stream (yason:parse stream :object-as :alist)))
      (destructuring-bind ((batch . next-batch)
			   device-one-time-keys
			   (groups g-leave g-invite g-join)
			   (rooms (leave-desc &rest leave-rooms)
				  (invite-desc &rest invite-rooms)
				  (join-desc &rest join-rooms))
			   presence
			   device-list
			   to-device
			   x) 
	  parsed-stream
	(print next-batch)
	(print join-rooms)
	(loop for room-alist in join-rooms
	   do (let ((room (get-room-from-id (car room-alist))))
		(let ((new-room-timeline-events (cdddr (assoc "timeline" room-alist :test #'string-equal))))
		  (print new-room-timeline-events))
		(print room)))))
    
    return))

(defun get-room-from-id (room-id)
  (loop for room in *rooms-test*
     do (when (string-equal room-id (room-id room))
	  (return-from get-room-from-id room))))

