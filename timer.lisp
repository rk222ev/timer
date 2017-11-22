(in-package #:timer)

(defun split (delimiter str)
  (split-sequence delimiter str))


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
  (sb-ext:run-program
    "/Users/rpkn/Projects/go/bin/noti"
    '("-m" "Times up!")
    :output *standard-output*))


(defun to-seconds (str)
  (destructuring-bind (seconds minutes) (reverse (split-sequence  #\: str))
    (+ (* (parse-integer minutes) 60)
      (parse-integer seconds))))


(defun main (argv)
  (let ((time (to-seconds (cadr argv))))
    (count-print time)
    '(cls)
    '(notification)))
