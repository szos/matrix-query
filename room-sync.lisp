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

(defparameter *initial-sync-raw-room-data* nil)
(defparameter *rooms* nil)

(defun initial-sync (&optional dont-set-next-batch)
  (multiple-value-bind (stream return)
      (drakma:http-request (make-api-call "_matrix/client/r0/sync")
			   :want-stream t
			   :method :get
			   :additional-headers (authorization-header))
    (print return)
    (let ((parsed-stream (yason:parse stream :object-as :alist)))
      ;; (print return)
      ;; (print parsed-stream)
      (let* ((next-batch (cdar parsed-stream))
	     (device-one-time-keys-count (cadr parsed-stream))
	     (groups (caddr parsed-stream))
	     (rooms (cadddr parsed-stream))
	     (join (cadddr rooms)))
	(unless dont-set-next-batch
	  (setf *next-batch* next-batch))
	(setf *initial-sync-raw-room-data* rooms)
	;; (print "join here:")
	;; (print join)
	(setf *rooms*
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
					  :timeline (generate-events-from-room-timeline timeline)
					  :state (generate-events-from-room-timeline state)))))))))

(defparameter *testing-update-sync* nil)

(defun update-sync ()
  (multiple-value-bind (ret stream)
      (call-api-get ("_matrix/client/r0/sync?since=" *next-batch*))
    (let* ((parsed-stream (yason:parse stream :object-as :alist))
	   (next-batch (cdar parsed-stream))
	   (device-one-time-keys-count (cadr parsed-stream))
	   (groups (caddr parsed-stream))
	   (rooms (cadddr parsed-stream))
	   (join (cadddr rooms))
	   (join2 (assoc "join" (cdr rooms) :test #'string-equal)))
      (setf *testing-update-sync* parsed-stream)
      ;; (setf *next-batch* next-batch)
      (loop :for room :in (cdr join)
	 do (let ((room-obj (room-id->room (car room)))
		  (timeline (sevent room)))
	      (setf *testing-update-sync*
		    (append (timeline room-obj)
			    (generate-events-from-room-timeline timeline)))))
      ;; (loop :for room :in *rooms*
      ;; 	 do (let ((room-updates (assoc (room-id room) (cdr join) :test #'string-equal))
      ;; 		  (roomer (cdr join)))
      ;; 	      (let ((timeline (seventh roomer)))
      ;; 		;; (print roomer)
      ;; 		;; (print timeline)
      ;; 		(setf (timeline room)
      ;; 		      (append (timeline room)
      ;; 			      (generate-events-from-room-timeline timeline))))))
      ;; (print join)
      ;; (print join2)
      ;; (print rooms)
      )))

(defun setup-rooms-from-state ()
  (loop for room in *rooms*
     do (loop for event in (state room)
	   do (case (type-of event)
		(room-topic-event (setf (topic room) (topic event)))
		(create-room-event (setf (creator room) (creator event)))
		(room-name-event (setf (name room) (name event)))
		(room-canonical-alias-event (setf (canonical-alias room)
						  (alias event)))))))

(defun view-room-timeline (room)
  (loop for event in (timeline room)
     do (print (generate-text event))))

(defun test-get-image () ;; (mcx-url)
  (multiple-value-bind (stream return)
      (drakma:http-request (make-api-call "_matrix/media/r0/download/" "matrix.org/KQFNMVsHYyBNwPmRyYfHuwVS")
			   :method :get
			   :want-stream t
			   :additional-headers (authorization-header))
    (print "http request done")
    (with-open-file
	(outfile "/home/szos/new-imgage.jpg"
		 :direction :output
		 :if-exists :supersede
		 :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte stream nil)
	 while byte
	 do (write-byte byte outfile)))))

;; (defun aquire-image (matrix-api-call)
;;   (multiple-value-bind (stream return)
;;       (drakma:http-request matrix-api-call
;; 			   :want-stream t
;; 			   :method :get
;; 			   :additional-headers (authorization-header))
;;     (with-open-file )))

(defparameter *new-room-text* nil ;; (sync-room-test)
  )

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

