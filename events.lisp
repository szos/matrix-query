;;; events.lisp

;;; this holds the event classes hierarchys class definitions

(in-package :matrix-query)

(defclass event ()
  ((event-type :initarg :type
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

(defclass state-event (room-event)
  ((state-key :initarg :state-key
	      :accessor state-key)
   (previous-content :initarg :previous-content
		     :accessor previous-content)))

(defclass message-event (room-event) ())

(defclass alias-event (state-event)
  ;; This should not be implemented as an object - it is best stored as an alist of room aliases
  ;; 
  ((aliases :initarg :alias
	  :)))
