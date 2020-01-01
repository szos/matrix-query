;; session.lisp

(in-package :matrix-query)

(defparameter *homeserver* nil
  "this is the users homeserver, where their user resides.")

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


(defun login ()
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
							     "\", \"device_id\":"
							     "\"LISPclient\"}"))))
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
  (recieve-json (concatenate 'string "https://matrix.org/" "_matrix/client/r0/logout"))
  (setf *session-user-auth* nil)
  (setf *device-id* nil)
  (setf *user-address* nil)
  :logged-out)
