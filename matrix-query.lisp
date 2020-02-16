;;;; matrix-query.lisp

(in-package #:matrix-query)

;; (defmacro string-case (string &body matches)
;;   "i believe that string case should more closely match case."
;;   `(string-case:string-case (,string)
;;      ,@matches))

;; the above string case doesnt allow for multiple matches to be mapped to a
;; single case, the two macros below do. One uses reverse and one uses append,
;; im keeping them both here because I dont know which is more expensive.

;; (defmacro string-case (string &body matches)
;;   (let ((acc nil))
;;     (loop for m in matches
;; 	  do (if (consp (car m))
;; 		 (loop for x in (car m)
;; 		       do (setf acc (cons `(,x ,@(cdr m))
;; 					  acc)))
;; 		 (setf acc (cons m acc))))
;;     `(string-case:string-case (,string)
;;        ,@(reverse acc))))

(defmacro string-case (string &body matches)
  "this macro wraps around the string-case:string-case macro and extends its 
functionality. Firstly, I see no reason to wrap the string to match against inside
a list. Secondly, it allows for multiple matches per statment. the following 
\(string-case \"foo\"
   \(\(\"foo\" \"bar\"\) 0\)
   \(\"baz\" 1\)
   \(t 2\)\)
expands into
\(string-case:string-case \(\"foo\"\)
  \(\"foo\" 0\)
  \(\"bar\" 0\)
  \(\"baz\" 1\)
  \(t 2\)\)
which propperly duplicates the cases when the matches are a list"
  (let ((acc nil))
    (loop for m in matches
	  do (if (consp (car m))
		 (loop for x in (car m)
		       do (setf acc (append acc `((,x ,@(cdr m))))))
		 (setf acc (append acc (list m)))))
    `(string-case:string-case (,string)
       ,@acc)))

(setf (symbol-function 'unique-txid)
      (let ((id 0))
	(lambda ()
	  (incf id))))

;; (let ((txid 0))
;;   (defun unique-txid ()
;;     (incf txid)))

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
