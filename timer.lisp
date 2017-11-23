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
  "Show notification for user that time is up."
  (sb-ext:run-program
    (merge-pathnames "Projects/go/bin/noti" (user-homedir-pathname)) ;; Find better alternative
    '("-m" "Times up!")
    :output *standard-output*))


(defun to-seconds (str)
  "Takes time in the format mm:ss and transforms it to seconds"
  (destructuring-bind (seconds minutes) (reverse (split  ":" str))
    (+ (* (parse-integer minutes) 60)
      (parse-integer seconds))))


(defun main (argv)
  (let ((time (to-seconds (cadr argv))))
    (count-print time)
    (cls)
    (notification)))
