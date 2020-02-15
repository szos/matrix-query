
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

;;; these generic functions dont work in this file as the event classes
;;; havent been loaded yet

(defmethod test/send-file ((room string) file)
  "this doesnt currently work - something may be wrong with the upload."
  (let ((filename (car (last (str:split "/" file))))
	(url (upload-file file)
	     ;; (car (last (str:split "mxc://" (upload-file file))))
	     ))
    (print filename)
    (print url)
    (multiple-value-bind (ret stream)
	(call-api-put ("_matrix/client/r0/rooms/" room
			"/send/m.room.message/" (unique-txid))
		      "application/json"
		      ;; (make-json-from-alist
		      ;;  (list (cons "msgtype" "m.file")
		      ;; 	     (cons "body" filename)
		      ;; 	     (cons "filename" filename)
		      ;; 	     (cons "url" url)))
		      (make-json-from-alist
		       (list (cons "msgtype" "m.file")
			     (cons "body" filename)
			     (cons "filename" filename)
			     (cons "url" url)
			     (cons "info"
				   "{ \"mimetype\": \"image/png\"}"))))
      (let ((parsed (yason:parse stream :object-as :alist)))
	(print parsed)
	(case ret
	  (200 (print "success"))
	  (t (print "failure of some sort")
	   (print ret)))))))

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
