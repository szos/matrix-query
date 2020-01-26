;;;; matrix-query.lisp

(in-package #:matrix-query)

(defmacro string-case (string &body matches)
  `(string-case:string-case (,string)
     ,@matches))

(let ((txid 0))
  (defun unique-txid ()
    (incf txid)))

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
