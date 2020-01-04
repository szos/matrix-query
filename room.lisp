;;; room.lisp

(in-package :matrix-query)

(defclass matrix-room ()
  ((name :initarg :name
	 :accessor name)
   (room-id :initarg :room-id
	    :accessor room-id)
   (aliases :initarg :aliases
	    :accessor aliases)
   (members :initarg :members ; this should be a list of room-member objects
	    :accessor members)
   (topic :initarg :topic
	  :accessor topic)))

(defclass room-member ()
  ;; holds info on a member of a room, their power levels, etc
  ((name :initarg :name
	 :accessor name)
   (power-level :initarg :power-level
		:accessor power-level)))

;; (defmethod add-alias ((room matrix-room) (new-alias string))
;;   (with-slots (aliases) room
;;     (let* ((whole (assoc ))))))

(defparameter *joined-rooms* nil
  "a list of joined rooms, in the form of room objects")

(defun list-joined-rooms ()
  "querys the users joined rooms and returns a list of room objects not present in *joined-rooms*"
  (multiple-value-bind (stream ret)
      (drakma:http-request (concatenate 'string *homeserver* "_matrix/client/r0/joined_rooms")
			   :want-stream t
			   :method :get
			   :content-type "application/json"
			   :additional-headers
			   (cons (cons "Authorization" (concatenate 'string "Bearer "
								    *session-user-auth*))
				 nil))
    (print ret)
    ;; (print (yason:parse stream :object-as :alist))
    (let* ((parsed (yason:parse stream :object-as :alist))
	   (room-ids (cdr (assoc "joined_rooms" parsed :test #'string-equal))))
      (print parsed)
      (loop for room in *joined-rooms*
	 :when (member (room-id room) room-ids :test #'string-equal)
	   :do (setf room-ids (remove (room-id room) room-ids :test #'string-equal)))
      (print room-ids)
      (mapcar (lambda (id)
		(make-instance 'matrix-room :room-id id))
	      room-ids))))

(defun update-joined-rooms ()
  "TODO: update this to remove a room if it isnt present in what the server returns. 
perhas have an «old rooms» list which holds rooms which weve left...? read the spec to find out 
what to do. "
  (setf *joined-rooms* (concatenate 'list (list-joined-rooms) *joined-rooms*)))

(defmethod invite-to-room ((matrix-user-id string) (room matrix-room))
  (multiple-value-bind (stream return)
      (drakma:http-request (concatenate 'string *homeserver*
					"_matrix/client/r0/rooms/"
					(room-id room)
					"/invite")
			   :want-stream t
			   :method :post
			   :content-type "application/json"
			   :content (make-json-from-plist (list "user_id" matrix-user-id))
			   :additional-headers
			   (cons (cons "Authorization" (concatenate 'string "Bearer "
								    *session-user-auth*))
				 nil))
    (let ((parsed-stream (yason:parse stream :object-as :alist)))
      (cond ((eq return 200)
	     (print "user successfully invited"))
	    ((eq return 400)
	     (print "request is invalid")
	     (print parsed-stream))
	    ((eq return 403)
	     (print "there is a permission error")
	     (print parsed-stream))
	    ((eq return 429)
	     (print "the request was rate limited - you have sent to many requests to quickly"))))))

(defmethod join-room ((room matrix-room))
  (multiple-value-bind (stream return)
      (drakma:http-request (make-api-call "_matrix/client/r0/join/" (room-id room))
			   :want-stream t
			   :method :post
			   ;; :content-type "application/json"
			   
			   :additional-headers (authorization-header))
    (let ((parsed-stream (yason:parse stream :object-as :alist)))
      (cond ((= return 200)
	     (print "you have joined the room"))
	    ((= return 403)
	     (print "you do not have permission to join this room")
	     (print parsed-stream))
	    ((= return 429)
	     (print "youve sent to many requests to fast"))))))

(defmethod join-room ((alias-or-id string))
  (multiple-value-bind (stream return)
      (drakma:http-request (make-api-call "_matrix/client/r0/join/" alias-or-id)
			   :want-stream t
			   :method :post
			   ;; :content-type "application/json"
			   
			   :additional-headers (authorization-header))
    (let ((parsed-stream (yason:parse stream :object-as :alist)))
      (cond ((= return 200)
	     (print "you have joined the room"))
	    ((= return 403)
	     (print "you do not have permission to join this room")
	     (print parsed-stream))
	    ((= return 429)
	     (print "youve sent to many requests to fast"))))))

(defgeneric leave-room (room))

(defmethod leave-room ((room matrix-room))
  (multiple-value-bind (stream return)
      (drakma:http-request (make-api-call "_matrix/client/r0/rooms/" (room-id room) "/leave")
			   :want-stream t
			   :method :post
			   :additional-headers (authorization-header))
    (let ((parsed-stream (yason:parse stream :object-as :alist)))
      (cond ((= return 200)
	     (print "You have successfully left the room"))
	    ((= return 429)
	     (print "you have sent to many requests to quickly"))))))

(defmethod leave-room ((room-id string))
  "TODO: we have to update this to modify *joined-rooms*, otherwise the client and the server will
not be in sync"
  (multiple-value-bind (stream return)
      (drakma:http-request (make-api-call "_matrix/client/r0/rooms/" room-id "/leave")
			   :want-stream t
			   :method :post
			   :additional-headers (authorization-header))
    (let ((parsed-stream (yason:parse stream :object-as :alist)))
      (cond ((= return 200)
	     (print "You have successfully left the room"))
	    ((= return 429)
	     (print "you have sent to many requests to quickly"))))))

(defgeneric forget-room (room))

(defmethod forget-room ((room-id string))
  (print "dont use this - we need to leave a room before we can forget it. "))

(defmethod forget-room ((room matrix-room))
  (print "dont use this - we need to leave a room before we can forget it. "))

(defgeneric room-visibility (room))

(defmethod room-visibility ((room matrix-room))
  (multiple-value-bind (stream return)
      (drakma:http-request (make-api-call "_matrix/client/r0/directory/list/room/" (room-id room))
			   :want-stream t
			   :method :get)
    (let ((parsed-stream (yason:parse stream :object-as :alist)))
      (cond ((= return 200)
	     (print parsed-stream))
	    ((= return 404)
	     (print "unknown room"))))))

(defmethod room-visibility ((room string))
  (multiple-value-bind (stream return)
      (drakma:http-request (make-api-call "_matrix/client/r0/directory/list/room/" room)
			   :want-stream t
			   :method :get)
    (let ((parsed-stream (yason:parse stream :object-as :alist)))
      (cond ((= return 200)
	     (print parsed-stream))
	    ((= return 404)
	     (print "unknown room"))))))
