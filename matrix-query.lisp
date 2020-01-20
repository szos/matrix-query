;;;; matrix-query.lisp

(in-package #:matrix-query)

(defmacro string-case (string matches &body otherwise)
  "this macro takes a string and a list of matches and what they will do, formed like so:
\(\"match-string\" \(do this thing\)
                   \(do this other thing\)\)
and generates a conditional statement using string-equal to compare everything."
  `(cond ,@(mapcar (lambda (match)
		     "match is a list with car as the string to match against"
		     `((string-equal ,string ,(car match))
		       ,@(cdr match)))
		   matches)
	 (t ,@(or otherwise '(nil)))))

;;; obsolete, dont document. 

(defun terminal-echo-on ()
  "enables echoing user input while in a terminal. only functions with SBCL"
  (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
    (setf (sb-posix:termios-lflag tm)
      (logior (sb-posix:termios-lflag tm) sb-posix:echo))
    (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))

(defun terminal-echo-off ()
  "disables echoing user input while in a terminal. only functions with SBCL"
  (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
    (setf (sb-posix:termios-lflag tm)
      (logandc2 (sb-posix:termios-lflag tm) sb-posix:echo))
    (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))

(defun read-silently-from-terminal ()
  "reads a single line without echoing user input. Fails unless in a terminal"
  (prog2
    (terminal-echo-off)
    (read-line sb-sys:*tty*)
    (terminal-echo-on)))
