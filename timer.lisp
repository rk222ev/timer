(in-package #:timer)

(defun echo-off ()
  (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
    (setf (sb-posix:termios-lflag tm)
      (logandc2 (sb-posix:termios-lflag tm) sb-posix:echo))
    (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))

(defun echo-on ()
  (let ((tm (sb-posix:tcgetattr sb-sys:*tty*)))
    (setf (sb-posix:termios-lflag tm)
      (logior (sb-posix:termios-lflag tm) sb-posix:echo))
    (sb-posix:tcsetattr sb-sys:*tty* sb-posix:tcsanow tm)))

(defun cls ()
  "Clears the terminal"
  (format t "~A[H~@*~A[J" #\escape))

(defun format-seconds (seconds)
  "Transform seconds to a M:SS format."
  (let ((m (floor (/ seconds 60)))
         (s (mod seconds 60)))
    (format nil "~d:~2,'0d" m s)))

(defun count-up-print (&optional (n 0))
  "Prints and increments a number every second starting at 0"
  (cls)
  (echo-off)
  (format t "~10@A~%" (format-seconds n))
  (sleep 1)
  (count-up-print (+ n 1)))

(defun count-down-print (n)
  "Clears and prints the remaining seconds to timer completion"
  (when (not (= 0 n))
    (cls)
    (echo-off)
    (format t "~10@A~%" (format-seconds n))
    (sleep 1)
    (count-down-print (- n 1))))

(defun notification ()
  "Shows a notification using osascript."
  (sb-ext:run-program
    "/usr/bin/osascript"
    '("-e" "display notification \"Times up!\" with Title \"Timer\" sound name \"default\"")
    :output *standard-output*))

(defun to-seconds (str)
  "Takes time in the format MM:SS and transforms it to seconds"
  (let* ((time (reverse (cl-ppcre:split ":" str)))
          (params (length time)))
    (cond
      ((= 1 params) (parse-integer (car time)))
      ((= 2 params)
        (destructuring-bind (seconds minutes) time
          (+ (* (parse-integer minutes) 60)
            (parse-integer seconds))))
      (t (format t "You must specify a time in the format \"MM:SS\".~%") 0))))

(defmacro with-disabled-debugger (form)
  "Disables the debugger on C-c termination."
  `(handler-case ((lambda () , form))
      (sb-sys:interactive-interrupt ()
        (sb-ext:quit))))

(defun count-down (time)
  "Given a time counts down to 0 then presents a notification."
  (with-disabled-debugger
    (when (< 0 time)
      (count-down-print time)
      (cls)
      (notification))))

(defun track-time ()
  "Prints the accumulated time each second until interupted."
  (with-disabled-debugger
    (unwind-protect
      (count-up-print)
      (echo-on))))

(defun main (argv)
  "Expects the invocation to have one argument in either MM:SS or SS format."
  (if (= 1 (sb-sequence:length argv))
    (track-time)
    (count-down (to-seconds (cadr argv)))))
