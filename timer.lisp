(in-package #:timer)

(defun cls ()
  "Clears the terminal"
  (format t "~A[H~@*~A[J" #\escape))


(defun count-print (n)
  "Clears and prints the remaining seconds to timer completion"
  (when (not (= 0 n))
    (cls)
    (format t "Seconds remaining: ~d~%" n)
    (sleep 1)
    (count-print (- n 1))))


(defun notification ()
  "Shows a notification using osascript."
  (sb-ext:run-program
    "/usr/bin/osascript"
    '("-e" "display notification \"Times up!\" with Title \"Timer\" sound name \"default\"")
    :output *standard-output*))


(defun to-seconds (str)
  "Takes time in the format mm:ss and transforms it to seconds"
  (let* ((time (reverse (split ":" str)))
         (params (length time)))
    (cond
      ((= 1 params) (parse-integer (car time)))
      ((= 2 params)
        (destructuring-bind (seconds minutes) time
          (+ (* (parse-integer minutes) 60)
            (parse-integer seconds))))
      (t (format t "You must specify a time in the format \"MM:SS\".~%") 0))))


(defun main (argv)
  (let ((time (to-seconds (cadr argv))))
    (when (< 0 time)
      (count-print time)
      (cls)
      (notification))))
