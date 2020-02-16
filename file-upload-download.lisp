
(in-package :matrix-query)

(define-condition invalid-uri-error (error)
  ((invalid-uri :initarg :invalid-uri
		:reader invalid-uri)))

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
      (if (string-equal (subseq url-stuff 0 6)
			"mxc://")
	  url-stuff
	  (error 'invalid-uri-error :invalid-uri url-stuff)))))

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

;; (defun test/send-file (path-to-file)
;;   "this finally works!! we have to loop through the file as bytes"
;;   (let ((k (drakma:http-request (make-api-call
;; 				 "_matrix/media/r0/upload?filename="
;; 				 (car (last (str:split "/" path-to-file))))
;; 				:method :post
;; 				:want-stream t
;; 				;; :content-type "application/pdf"
;; 				:content :continuation
;; 				:additional-headers (authorization-header))))
;;     (funcall k (lambda (stream)
;;     		 (with-open-file (f path-to-file
;;     				    :element-type '(unsigned-byte 8))
;;     		   (loop for byte = (read-byte f nil)
;;     			 while byte
;;     			 do (write-byte byte stream)))))))

;; (defun test/get-file (new-file-path mxc-uri)
;;   (multiple-value-bind (s r)
;;       (drakma:http-request (make-api-call "_matrix/media/r0/download/"
;; 					  (car (last (str:split "mxc://" mxc-uri))))
;; 			   :method :get
;; 			   :want-stream t
;; 			   :additional-headers (authorization-header))
;;     (print r)
;;     (with-open-file
;; 	(outfile new-file-path
;; 		 :direction :output
;; 		 :if-exists :supersede
;; 		 :element-type '(unsigned-byte 8))
;;       (loop for byte = (read-byte s nil)
;; 	    while byte
;; 	    do (write-byte byte outfile)))))
