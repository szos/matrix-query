;;; events.lisp

;;; this holds the event classes hierarchys class definitions

(in-package :matrix-query)

(defclass event ()
  ((event-type :initarg :event-type
	       :accessor event-type)
   (content :initarg :content
	    :accessor content)))

(defclass room-event (event)
  ((event-id :initarg :event-id
	     :accessor event-id)
   (sender :initarg :sender
	   :accessor sender)
   (origin-server-ts :initarg :origin-server
		     :accessor origin-server)
   (unsigned :initarg :unsigned
	     :accessor unsigned)
   (room-id :initarg :room-id
	    :accessor room-id)))

(defclass state-event-wrong (room-event)
  ((state-key :initarg :state-key
	      :accessor state-key)
   (previous-content :initarg :previous-content
		     :accessor previous-content)))

(defclass message-event (room-event)
  ())

(defclass state-event (message-event)
  ((state-key :initarg :state-key
	      :accessor state-key)
   (previous-content :initarg :previous-content
		     :accessor previous-content)))

(defclass text-message-event (message-event)
  ((body :initform nil
	 :initarg :body
	 :accessor body)))

(defclass member-message-event (state-event)
  ((avatar-url :initform nil
	       :initarg :avatar-url
	       :accessor avatar-url)
   (display-name :initform nil
		 :initarg :display-name
		 :accessor display-name)
   (membership :initform nil
	       :initarg :membership
	       :accessor membership)))

;; (defclass image-message-event (message-event)
;;   ;; ((image-host-server :image-host-server))
;;   )

;; (("event_id" . "$hKOu44aLCt3M1lPJkAdM7uHXOEvpYbluuhpqlkhaqqw")
;;  ("unsigned" ("age" . 78757815)) ("origin_server_ts" . 1578153432635)
;;  ("state_key" . "@testing-for-lisp-client:matrix.org")
;;  ("content" ("avatar_url") ("displayname" . "testing-for-lisp-client")
;; 	    ("membership" . "join"))
;;  ("sender" . "@testing-for-lisp-client:matrix.org")
;;  ("type" . "m.room.member"))

;; (defclass alias-event (state-event)
;;   ;; This should not be implemented as an object - it is best stored as an alist of room aliases
;;   ;; 
;;   ((aliases :initarg :alias
;; 	    :)))

(defclass create-room-event (state-event)
  ((creator :initform nil
	    :initarg :creator
	    :accessor creator
	    :documentation "the user_id of the room creator. this is set by the homeserver")
   (federate :initform nil
	     :initarg :federate
	     :accessor federate
	     :documentation "whether users on other servers can join this room")
   (room-version :initform nil
		 :initarg :room-version
		 :accessor room-version
		 :documentation "the version of the room, defaulting to \"1\" if the key does not exist")
   (predecessor :initform nil
		:initarg :predecessor
		:accessor predecessor
		:documentation "a reference to the room this room replaces, if the previous room was upgraded")))

(defclass create-room-event-previous-room ()
  ((room-id :initform nil
	    :initarg :room-id
	    :accessor room-id
	    :documentation "room id of previous version of this room")
   (event-id :initform nil
	     :initarg :event-id
	     :accessor event-id
	     :documentation "lask known event in the previous room.")))

(defclass content () ())

(defclass test-message-content (content)
  ((body :initform nil
	 :initarg :body
	 :accessor body
	 :documentation "text body of a message event containing only text")))

(defmacro string-case (string matches &body otherwise)
  "this macro takes a string and a list of matches and what they will do, formed like so:
\(\"match-string\" \(do this thing\)
                   \(do this other thing\)\)
and generates a conditional statement using string-equal to compare everything."
  `(cond ,@(mapcar (lambda (match)
		    "match is a list with car as the string to match against"
		    `((string-equal ,string ,(car match))
		      ,@(cdr match)))
		   matches)
	 (t
	  ,@otherwise)))

(defun strsoc (string alist)
  "returns the value portion of the alist"
  (cdr (assoc string alist :test #'string-equal)))

;; (defgeneric display-text (event))

;; (defmethod display-text ((event event) stream))

(defgeneric generate-text (event))

(defmethod generate-text ((event event))
  (format nil "~a~%~a" (event-type event) (content event)))

(defmethod generate-text ((event room-event))
  (format nil "Sender: ~a~%  ~a~%  ~a" (sender event) (event-type event) (content event)))

(defmethod generate-text ((event message-event))
  (format nil "Sender: ~a~%  ~a~%  ~a" (sender event) (event-type event) (content event)))

(defmethod generate-text ((event text-message-event))
  (format nil "Sender: ~a~%  ~a" (sender event) (body event)))

(defmethod generate-text ((event member-message-event))
  (string-case (membership event)
	(("join" (format nil "~a joined the room" (display-name event)))
	 ("invite" (format nil "~a was invited to the room by ~a" (display-name event) (sender event))))
      (format nil "Member Name: ~a~%Sender: ~a~%~a~%~a" (display-name event) (sender event)
	  (membership event) (content event))))

(defmethod generate-text ((event create-room-event))
  (format nil "Creator: ~a" (creator event)))

(defun make-event-object (event)
  (let ((event-id (cdr (assoc "event_id" event :test #'string-equal)))
	(origin-server-ts (cdr (assoc "origin_server_ts" event :test #'string-equal)))
	(type (cdr (assoc "type" event :test #'string-equal)))
	(sender (cdr (assoc "sender" event :test #'string-equal)))
	(content (cdr (assoc "content" event :test #'string-equal))))
    (string-case type
		 (("m.room.message"
		   (let ((message-type (cdr (assoc "msgtype" content :test #'string-equal))))
		     (string-case message-type
			 (("m.text"
			   (make-instance 'text-message-event
					  :event-id event-id
					  :origin-server origin-server-ts
					  :sender sender
					  :content content
					  :body (cdr (cadr content)))))
		       (make-instance 'message-event
				      :event-id event-id
				      :origin-server origin-server-ts
				      :event-type type
				      :sender sender
				      :content content))))
		  ("m.room.member"
		   (make-instance 'member-message-event
				  :event-id event-id
				  :origin-server origin-server-ts
				  :sender sender
				  :content content
				  :state-key (cdr (assoc "state_key" event :test #'string-equal))
				  :avatar-url (cdr (assoc "avatar_url" content
							  :test #'string-equal))
				  :display-name (cdr (assoc "displayname" content
							    :test #'string-equal))
				  :membership (cdr (assoc "membership" content
							  :test #'string-equal))))
		  ("m.room.create"
		   (make-instance 'create-room-event
				  :event-id event-id
				  :origin-server origin-server-ts
				  :sender sender
				  :content content
				  :state-key (strsoc "state_key" event)
				  :creator (strsoc "creator" content)
				  :federate (strsoc "m.federate" content)
				  :room-version (strsoc "room_version" content))))
      (make-instance 'message-event
		     :event-id event-id
		     :event-type type
		     :origin-server origin-server-ts
		     :sender sender
		     :content content))))

;; (defparameter *rooms-test* (sync-room-test))

(defun update-rooms-test-to-objs ()
  (loop for room in *rooms-test*
     do
       (setf (timeline room)
	     (loop for event in (cdr (fourth (timeline room)))
		collect (let ((event-id (assoc "event_id" event :test #'string-equal))
			      (origin-server-ts (assoc "origin_server_ts" event :test #'string-equal))
			      (type (assoc "type" event :test #'string-equal))
			      (sender (assoc "sender" event :test #'string-equal))
			      (content (assoc "content" event :test #'string-equal)))
			  (string-case (cdr type)
			      (("m.room.message"
				(let ((message-type (cdr (assoc "msgtype" (cdr content) :test #'string-equal))))
				  (cond ((string-equal message-type "m.text") 
					 (make-instance 'text-message-event :event-id (cdr event-id)
							:origin-server (cdr origin-server-ts)
							:sender (cdr sender)
							:content (cdr content)
							:body (cdr (cadr (cdr content)))))
					(t
					 (make-instance 'message-event :event-id (cdr event-id)
							:origin-server (cdr origin-server-ts)
							:event-type type
							:sender (cdr sender)
							:content (cdr content))))))
			       ("m.room.member"
				(make-instance 'member-message-event
					       :event-id (cdr event-id)
					       :origin-server (cdr origin-server-ts)
					       :sender (cdr sender)
					       :content (cdr content)
					       :state-key (cdr (assoc "state_key" event :test #'string-equal))
					       :avatar-url (cdr (assoc "avatar_url" (cdr content)
								       :test #'string-equal))
					       :display-name (cdr (assoc "displayname" (cdr content)
									 :test #'string-equal))
					       :membership (cdr (assoc "membership" (cdr content)
								       :test #'string-equal))))
			       ("m.room.create"
				(make-instance 'create-room-event
					       :event-id (cdr event-id)
					       :origin-server (cdr origin-server-ts)
					       :sender (cdr sender)
					       :content (cdr content)
					       :state-key (cdr (assoc "state_key" event :test #'string-equal))
					       :creator (cdr (assoc "creator" (cdr content)
								    :test #'string-equal))
					       :federate (cdr (assoc "m.federate" (cdr content)
								     :test #'string-equal))
					       :room-version (cdr (assoc "room_version" (cdr content)
								     :test #'string-equal)))))
			    (make-instance 'message-event
					   :event-id (cdr event-id)
					   :event-type type
					   :origin-server (cdr origin-server-ts)
					   :sender (cdr sender)
					   :content (cdr content)))
			  ))
	     )))

;; (defparameter *new-room-list* nil)

;; (defparameter *event-testing* ; needs rooms-test
;;   (loop for event in (cdr (fourth (timeline (car *rooms-test*))))
;;      collect (let ((event-id (assoc "event_id" event :test #'string-equal))
;; 		   (origin-server-ts (assoc "origin_server_ts" event :test #'string-equal))
;; 		   (type (assoc "type" event :test #'string-equal))
;; 		   (sender (assoc "sender" event :test #'string-equal))
;; 		   (content (assoc "content" event :test #'string-equal)))
;; 	       (string-case (cdr type)
;; 		   (("m.room.message"
;; 		     (let ((message-type (cdr (assoc "msgtype" (cdr content) :test #'string-equal))))
;; 		       (cond ((string-equal message-type "m.text") 
;; 			      (make-instance 'text-message-event :event-id (cdr event-id)
;; 					     :origin-server (cdr origin-server-ts)
;; 					     :sender (cdr sender)
;; 					     :content (cdr content)
;; 					     :body (cdr (cadr (cdr content)))))
;; 			     (t
;; 			      (make-instance 'message-event :event-id (cdr event-id)
;; 					     :origin-server (cdr origin-server-ts)
;; 					     :sender (cdr sender)
;; 					     :content (cdr content))))))
;; 		    ("m.room.member"
;; 		     (make-instance 'member-message-event
;; 				    :event-id (cdr event-id)
;; 				    :origin-server (cdr origin-server-ts)
;; 				    :sender (cdr sender)
;; 				    :content (cdr content)
;; 				    :state-key (cdr (assoc "state_key" event :test #'string-equal))
;; 				    :avatar-url (cdr (assoc "avatar_url" (cdr content)
;; 							    :test #'string-equal))
;; 				    :display-name (cdr (assoc "displayname" (cdr content)
;; 							      :test #'string-equal))
;; 				    :membership (cdr (assoc "membership" (cdr content)
;; 							    :test #'string-equal)))))
;; 		 (make-instance 'message-event
;; 				:event-id (cdr event-id)
;; 				:origin-server (cdr origin-server-ts)
;; 				:sender (cdr sender)
;; 				:content (cdr content))))))
  



  




