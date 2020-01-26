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
	       #:string-case)
  :components ((:file "package")
               (:file "matrix-query")
	       (:file "session")
	       (:file "communicate")
	       (:file "room")
	       (:file "room-sync")
	       (:file "events")
	       (:file "api-calls")))
