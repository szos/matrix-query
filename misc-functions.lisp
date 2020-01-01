;;; misc functions that dont currently fit into other files. These functions should eventually be
;;; deleted or moved to another file - this file is only for prototyping behaviour

(in-package :matrix-query)

(defun send-json-default-homeserver (api))

(defun recieve-json-from-homeserver (api)
  "send a «get» to the specified url, with the users authorization token included. parse the 
results as an alist."
  (let ((stream (drakma:http-request (concatenate 'string *homeserver* api)
				     :want-stream t
				     :method :get
				     :additional-headers
				     `(("Authorization" . ,(concatenate 'string "Bearer "
									*session-user-auth*))))))
    (yason:parse stream :object-as :alist)
    ;; stream
    ))

(defun get-client-versions ()
  (recieve-json-from-homeserver "_matrix/client/versions"))

(defun get-well-known ()
  (recieve-json-from-homeserver ".well-known/matrix/client"))
