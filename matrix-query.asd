;;;; matrix-query.asd

(asdf:defsystem #:matrix-query
  :description "Describe matrix-query here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:drakma
	       #:flexi-streams
	       #:yason
	       #:string-case
	       #:bt-semaphore)
  :components ((:file "package")
               (:file "matrix-query")
	       (:file "room")
	       (:file "api-calls")
	       (:file "session")
	       (:file "communicate")
	       (:file "room-sync")
	       (:file "events")))
