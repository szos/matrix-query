;;; communicate.lisp

(in-package :matrix-query)

(defparameter *homeserver* "https://matrix.org/"
  "this is the users homeserver, where their user resides.")

(defun set-homeserver (homeserver)
  (setf *homeserver* homeserver))

(defun make-api-call (&rest api-strings)
  "takes an api string WITHOUT THE LEADING SLASH and creates an api call to the homeserver"
  (format nil "~a~{~a~}" *homeserver* api-strings))

(defun authorization-header ()
  (cons (cons "Authorization" (concatenate 'string "Bearer "
					   *session-user-auth*))
	nil))

(defun make-json-from-plist (list)
  "takes a plist and generates json from it"
  (with-output-to-string (str)
    (yason:encode-plist list str)))

(defun make-json-from-alist (list)
  "takes a alist and generates json from it"
  (with-output-to-string (str)
    (yason:encode-alist list str)))

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

(defun recv-json (url)
  (multiple-value-bind (response status-code)
      (drakma:http-request url
			   :want-stream t
			   :method :get
			   :additional-headers
			   `(("Authorization" . ,(concatenate 'string "Bearer "
			   				      *session-user-auth*))))
    (values response status-code)))




