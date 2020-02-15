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

(defun generate-events-from-event-list (events-list &key (reverse nil))
  (if reverse
      (let ((ac nil))
	(loop for event in events-list
	   do (push (make-event-object event) ac))
	ac)
      (loop for event in events-list
	 collect (make-event-object event))))

(defun generate-events-from-room-timeline (room-timeline-list)
  (let ((timeline-events (strsoc "events" (if (stringp (car room-timeline-list))
					      (cdr room-timeline-list)
					      room-timeline-list))))
    (generate-events-from-event-list timeline-events)))

(defparameter *initial-sync-raw-room-data* nil)
(defparameter *rooms* nil)

(defun initial-sync (&optional dont-set-next-batch)
  (multiple-value-bind (stream return)
      (drakma:http-request (make-api-call "_matrix/client/r0/sync")
			   :want-stream t
			   :method :get
			   :additional-headers (authorization-header))
    (print return)
    (let ((parsed-stream (yason:parse stream :object-as :alist))
	  (newrooms nil))
      ;; (print return)
      ;; (print parsed-stream)
      (let* ((next-batch (cdar parsed-stream))
	     (device-one-time-keys-count (cadr parsed-stream))
	     (groups (caddr parsed-stream))
	     (rooms (cadddr parsed-stream))
	     (join (cadddr rooms)))
	(unless dont-set-next-batch (setf *next-batch* next-batch))
	(setf *initial-sync-raw-room-data* rooms)
	;; (print "join here:")
	;; (print join)
	(setf newrooms
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
					  :state (generate-events-from-room-timeline state)))))
	(print newrooms)
	(setf *rooms* newrooms)
	(setup-rooms-from-state)))))

(defun initial-sync-threaded ()
  (bt:make-thread
   (lambda ()
     (initial-sync)
     (print "initial sync finished"))))

(defparameter *testing-update-sync* nil)

(defun update-sync ()
  "updates the timeline stuff... and only that"
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
      (setf *next-batch* next-batch)
      (loop :for room :in (cdr join)
	 do (let ((room-obj (room-id->room (car room)))
		  (timeline (seventh room)))
	      (setf (timeline room-obj)
		    (append (timeline room-obj)
			    (generate-events-from-room-timeline timeline)))))
      ;; (setup-rooms-from-state)
      )))

(defun update-sync-threaded ()
  (bt:make-thread
   (lambda ()
     (update-sync)
     (print "update sync finished"))))

(defun setup-rooms-from-state ()
  (loop for room in *rooms*
     do (loop for event in (timeline room)
	   do (case (type-of event)
		(room-topic-event (setf (topic room) (topic event)))
		(create-room-event (setf (creator room) (creator event)))
		(room-name-event (setf (name room) (name event)))
		(room-canonical-alias-event (setf (canonical-alias room)
						  (alias event)))))
       (loop for event in (state room)
	  do (case (type-of event)
	       (room-topic-event (setf (topic room) (topic event)))
	       (create-room-event (setf (creator room) (creator event)))
	       (room-name-event (setf (name room) (name event)))
	       (room-canonical-alias-event (setf (canonical-alias room)
						 (alias event)))))))

(defun get-prior-events (room &optional (number-of-events 10))
  (multiple-value-bind (ret stream)
      (call-api-get ("_matrix/client/r0/rooms/" (room-id room) "/messages?from="
						(prev-batch room) "&dir=b&limit="
						number-of-events))
    (let* ((parsed-stream (yason:parse stream :object-as :alist))
	   (end (strsoc "end" parsed-stream)) ;should replace prev-batch
	   (start (strsoc "start" parsed-stream))
	   (events (strsoc "chunk" parsed-stream)))
      (declare (ignore start))
      (print ret)
      (setf (prev-batch room) end)
      (setf (timeline room)
	    (append (generate-events-from-event-list events :reverse t)
		    (timeline room))))))

(defun view-room-timeline (room)
  (loop for event in (timeline room)
	do (print (generate-text event))))

(defun upload-file (path-to-file)
  "this function takes a path to a file, and returns the mxc uri of the uploaded 
file. "
  (multiple-value-bind (a b c d stream)
      (let ((k (drakma:http-request
		(make-api-call "_matrix/media/r0/upload?filename="
			       (car (last (str:split "/" path-to-file))))
		:method :post
		:want-stream t
		:content-type "image/png"
		;; :content-type (get-mime-type-from-extension path-to-file)
		:content :continuation
		:additional-headers (authorization-header))))
	(funcall k (lambda (stream)
		     (with-open-file (f path-to-file
					:element-type '(unsigned-byte 8))
		       (loop for byte = (read-byte f nil)
			     while byte
			     do (write-byte byte stream))))))
    (declare (ignore a b c d))
    (let ((url-stuff (cdar (yason:parse stream :object-as :alist))))
      (print url-stuff)
      url-stuff)))

(defun download-file (new-file-name mxc-uri &optional (homeserver *homeserver*))
  "takes a new file name or path, and a uri that contains the file. If the file 
resides on a different homeserver that homeserver must be provided - the function
treats uri's as residing on the matrix.org homeserver by default."
  (multiple-value-bind (s r)
      (drakma:http-request (make-explicit-api-call
			    homeserver 
			    "_matrix/media/r0/download/"
			    (car (last (str:split "mxc://" mxc-uri))))
       :method :get
       :want-stream t
       :additional-headers (authorization-header))
    (declare (ignore r))
    (with-open-file
	(outfile new-file-name
		 :direction :output
		 :if-exists :supersede
		 :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte s nil)
	    while byte
	    do (write-byte byte outfile)))))

(defun test/send-file (path-to-file)
  "this finally works!! we have to loop through the file as bytes"
  (let ((k (drakma:http-request (make-api-call
				 "_matrix/media/r0/upload?filename="
				 (car (last (str:split "/" path-to-file))))
				:method :post
				:want-stream t
				;; :content-type "application/pdf"
				:content :continuation
				:additional-headers (authorization-header))))
    (funcall k (lambda (stream)
    		 (with-open-file (f path-to-file
    				    :element-type '(unsigned-byte 8))
    		   (loop for byte = (read-byte f nil)
    			 while byte
    			 do (write-byte byte stream)))))))

(defun test/get-file (new-file-path mxc-uri)
  (multiple-value-bind (s r)
      (drakma:http-request (make-api-call "_matrix/media/r0/download/"
					  (car (last (str:split "mxc://" mxc-uri))))
			   :method :get
			   :want-stream t
			   :additional-headers (authorization-header))
    (print r)
    (with-open-file
	(outfile new-file-path
		 :direction :output
		 :if-exists :supersede
		 :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte s nil)
	    while byte
	    do (write-byte byte outfile)))))

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

