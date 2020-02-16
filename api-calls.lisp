
(in-package :matrix-query)

(defparameter *thread-lock* (bt:make-lock))

(defmacro call-api-get ((&body api-string) &key (authorization-header t))
  `(multiple-value-bind (stream return-code)
       (drakma:http-request (make-api-call ,@(if (listp api-string)
						 api-string
						 (list api-string)))
			    :want-stream t
			    :method :get
			    ,@(when authorization-header
				'(:additional-headers (authorization-header)))
			    )
     (values return-code stream)))

(defmacro call-api-post ((&body api-string) content-type content
			 &key (authorization-header t))
  `(multiple-value-bind (stream return-code)
       (drakma:http-request (make-api-call ,@(if (listp api-string)
						 api-string
						 (list api-string)))
			    :want-stream t
			    :method :post
			    ,@(when authorization-header
				'(:additional-headers (authorization-header)))
			    ,@(when content-type
				`(:content-type ,content-type))
			    ,@(when content
				`(:content ,content)))
     (values return-code stream)))

(defmacro call-api-put ((&body api-string) content-type content
			 &key (authorization-header t))
  `(multiple-value-bind (stream return-code)
       (drakma:http-request (make-api-call ,@(if (listp api-string)
                                                 api-string
						 (list api-string)))
			    :want-stream t
			    :method :put
			    ,@(when authorization-header
				'(:additional-headers (authorization-header)))
			    ,@(when content-type
				`(:content-type ,content-type))
			    ,@(when content
				`(:content ,content)))
     (values return-code stream)))

(defun login (username password &optional (homeserver "https://matrix.org/"
						      homeserver-provided-p))
  (when homeserver-provided-p (setf *homeserver* homeserver))
  (multiple-value-bind (return-code stream)
      (call-api-post "_matrix/client/r0/login" "application/json"
		     (make-json-from-alist
		      (list (cons "type" "m.login.password")
			    (cons "user" username)
			    (cons "password" password)))
		     :authorization-header nil)))

(defparameter *file-types-to-message-types*
  '(("png" . "m.image")
    ("mp3" . "m.audio")
    ("ogg" . "m.audio")
    ("jpg" . "m.image")))

(defun file->message-type (filename)
  (let ((message-type
	  (strsoc (string-downcase (car (last (str:split "." filename))))
		  *file-types-to-message-types*)))
    (or message-type "m.file")))

(defun test/send-file (room-id file &optional msgtype)
  "This takes a room id, a path to a file, and optionally a message type. First 
uploading the file, then sending a message to the room. All files are sent as a "
  (restart-case 
      (let* ((filename (car (last (str:split "/" file))))
	     (url (upload-file file))
	     (json  (make-json-from-alist
		     (list (cons "msgtype" (or msgtype
					       (file->message-type file)))
			   (cons "body" filename)
			   (cons "filename" filename)
			   (cons "url" url)))))
	(print filename)
	(print url)
	(multiple-value-bind (ret stream)
	    (call-api-put ("_matrix/client/r0/rooms/" room-id
			    "/send/m.room.message/" (unique-txid))
			  "application/json" json)
	  (let ((parsed (yason:parse stream :object-as :alist)))
	    ;; (print parsed)
	    (case ret
	      (200 (print "success"))
	      (t
	       (format t "~%Sending failed. ~%  HTTP return code: ~a~%  Error message: ~a~%  Error Code: ~a"
		       ret (strsoc "error" parsed) (strsoc "errcode" parsed)))))))
    (skip-message ()
      (print "due to an invalid URI the message was aborted"))))

;; (defgeneric test/send-text-message (room message-text))

(defmethod test/send-text-message ((room matrix-room) (message-text string))
  "requires transactional id [txnId]"
  (multiple-value-bind (ret stream)
      (call-api-put ("_matrix/client/r0/rooms/" (room-id room) "/send/m.room.message/" (unique-txid))
		    "application/json" (make-json-from-alist
					(list (cons "msgtype" "m.text")
					      (cons "body" message-text))))
    (let ((parsed (yason:parse stream :object-as :alist)))
      (case ret
	(200 (print "success")
	     (update-sync))
	(t (print "failure of some sort")
	 (print ret))))))

;;; these generic functions dont work in this file as the event classes
;;; havent been loaded yet

;; (defgeneric test/send-message (room message))

;; (defmethod test/send-message ((room matrix-room) (message string))
;;   (multiple-value-bind (ret stream)
;;       (call-api-put ("_matrix/client/r0/rooms/" (room-id room) "/send/m.room.message/" (unique-txid))
;; 		    "application/json" (make-json-from-alist
;; 					(list (cons "msgtype" "m.text")
;; 					      (cons "body" message))))
;;     (let ((parsed (yason:parse stream :object-as :alist)))
;;       (case ret
;; 	(200 (print "success")
;; 	 (update-sync))
;; 	(t (print "failure of some sort")
;; 	 (print ret))))))
;; (defmethod test/send-message ((room matrix-room) (message text-message-event))
;;   (multiple-value-bind (ret stream)
;;       (call-api-put ("_matrix/client/r0/rooms/" (room-id room) "/send/m.room.message/" (unique-txid))
;; 		    "application/json" (make-json-from-alist
;; 					(list (cons "msgtype" "m.text")
;; 					      (cons "body" (body message)))))
;;     (let ((parsed (yason:parse stream :object-as :alist)))
;;       (case ret
;; 	(200 (print "success")
;; 	 (update-sync))
;; 	(t (print "failure of some sort")
;; 	 (print ret))))))
;; (defmethod test/send-message ((room matrix-room) (message media-message-event)))

;;; (defgeneric test/package-and-ship-event ())

;;; (defun test/send-)
