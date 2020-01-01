;;;; matrix-query.lisp

(in-package #:matrix-query)

(defun terminal-echo-on ()
  (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
    (setf (sb-posix:termios-lflag tm)
      (logior (sb-posix:termios-lflag tm) sb-posix:echo))
    (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))

(defun terminal-echo-off ()
  (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
    (setf (sb-posix:termios-lflag tm)
      (logandc2 (sb-posix:termios-lflag tm) sb-posix:echo))
    (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))

(defun read-silently-from-terminal ()
  (prog2
    (terminal-echo-off)
    (read-line sb-sys:*tty*)
    (terminal-echo-on)))
