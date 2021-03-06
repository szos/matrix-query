;; session.lisp

(in-package :matrix-query)

(defparameter *session-user-auth* nil
  "this is the users session authorization token, which is used for sending requests")

(defparameter *device-id* nil
  "this is the device id, which is currently \"LISPclient\" by default")

(defparameter *user-address* nil
  "this is the users full user address in the form: @user:homeserver
for example: @bob:matrix.org")

(define-condition invalid-login-error (error)
  ((text :initarg :text :reader text)))

(defun read-username ()
  (princ "Enter username: ")
  (read-line))

(defun read-password ()
  (princ "Enter password: ")
  (read-line))

(defparameter *login-info* nil)

(defun login-from-repl ()
  "logs in a user and stores relevant data such as access token. handles reading username
and password. If an invalid password error is returned from the server, signals the 
invalid-login-error and reruns. "
  (flet ((send-recv-login-data (un pw)
	   (let ((stream
		  (drakma:http-request "https://matrix.org/_matrix/client/r0/login"
				       :want-stream t
				       :method :post
				       :content-type "application/json"
				       :content (concatenate 'string
				       		    "{\"type\":"
				       		    "\"m.login.password\", "
				       		    "\"user\":\"" un
				       		    "\", \"password\":\"" pw
				       		    "\"}"))))
	     (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
	     (yason:parse stream :object-as :alist))))
    (if (not *session-user-auth*)
	(handler-case 
	    (let* ((username (read-username))
		   (password (read-password))
		   (data (send-recv-login-data username password))
		   (token (cdr (assoc "access_token" data :test #'string=)))
		   (device-id (cdr (assoc "device_id" data :test #'string=)))
		   (user-id (cdr (assoc "user_id" data :test #'string=))))
	      (when (equal (car data) '("error" . "Invalid password"))
		;;(return-from login :invalid-credentials)
		(error 'invalid-login-error :text "invalid password, try again"))
	      (setf *session-user-auth* token)
	      (setf *device-id* device-id)
	      (setf *user-address* user-id))
	  (invalid-login-error () (progn
				    (format t "Incorrect username or password, try again~%")
				    (login))))
	(format nil "User ~a is already logged in" *user-address*))))

(defun logout ()
  (multiple-value-bind (r s)
      (drakma:http-request (concatenate 'string *homeserver*
					"_matrix/client/r0/logout")
			   :want-stream t
			   :method :post
			   :content-type "application/json"
			   :additional-headers
			   `(("Authorization" . ,(concatenate 'string "Bearer "
							      *session-user-auth*))))
    (declare (ignore r))
    (print s)
    (setf *session-user-auth* nil)
    (setf *device-id* nil)
    (setf *user-address* nil)
    (setf *rooms* nil)))

(defun login (username password &optional (homeserver "https://matrix.org/"
						      homeserver-provided-p))
  "like login, except experiments with grabbing extra info from drakma. "
  (when homeserver-provided-p (setf *homeserver* homeserver))
  (flet ((send-recv-login-data (un pw)
	   (let ((stream
		  (multiple-value-bind (return-code response)
		      (call-api-post "_matrix/client/r0/login"  "application/json"
				     (make-json-from-alist
				      (list (cons "type" "m.login.password")
					    (cons "user" un)
					    (cons "password" pw))))
		    (setf *login-info* return-code)
		    (print return-code)
		    response)))
	     (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
	     (yason:parse stream :object-as :alist))))
    (if (not *session-user-auth*)
	(let* ((data (send-recv-login-data username password))
		   (token (cdr (assoc "access_token" data :test #'string=)))
		   (device-id (cdr (assoc "device_id" data :test #'string=)))
		   (user-id (cdr (assoc "user_id" data :test #'string=))))
	      (when (equal (car data) '("error" . "Invalid password"))
		;;(return-from login :invalid-credentials)
		(error 'invalid-login-error :text "invalid password, try again"))
	      (setf *session-user-auth* token)
	      (setf *device-id* device-id)
	      (setf *user-address* user-id))
	(format nil "User ~a is already logged in" *user-address*))))

(defun whoami ()
  (multiple-value-bind (response status-code)
      (drakma:http-request (concatenate 'string *homeserver* "_matrix/client/r0/account/whoami")
			   :want-stream t
			   :method :get
			   :additional-headers
			   `(("Authorization" . ,(concatenate 'string "Bearer "
							      *session-user-auth*))))
    (print status-code)
    (yason:parse response :object-as :alist)))

