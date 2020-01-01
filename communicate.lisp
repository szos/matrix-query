;;; communicate.lisp

(in-package :matrix-query)

(defparameter *homeserver* "https://matrix.org/"
  "this is the users homeserver, where their user resides.")

(defun set-homeserver (homeserver)
  (setf *homeserver* homeserver))

(defun send-json (url content &rest key-plist)
  "sends a «post» to the specified url, with the users auth token included. parses results as 
an alist"
  (if key-plist
      (let ((stream (drakma:http-request url
					 :want-stream t
					 :method :post
					 :content-type "application/json"
					 :content content
					 :additional-headers
					 `(("Authorization" . ,(concatenate 'string "Bearer "
									    *session-user-auth*)))
					 key-plist)))
	
	(yason:parse stream :object-as :alist))
      (let ((stream (drakma:http-request url
					 :want-stream t
					 :method :post
					 :content-type "application/json"
					 :content content
					 :additional-headers
					 `(("Authorization" . ,(concatenate 'string "Bearer "
									    *session-user-auth*))))))
	
	;; (get-json-from-stream stream :utf-8 :alist)
	(yason:parse stream :object-as :alist))))

(defun recieve-json (url)
  "send a «get» to the specified url, with the users authorization token included. parse the 
results as an alist."
  (let ((stream (drakma:http-request url
				     :want-stream t
				     :method :get
				     :additional-headers
				     `(("Authorization" . ,(concatenate 'string "Bearer "
									*session-user-auth*))))))
    (yason:parse stream :object-as :alist)))




