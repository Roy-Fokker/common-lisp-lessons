(defpackage coin-toss
  (:use :cl)
  (:export main))
(in-package :coin-toss)

(defvar *toss-choices* '("heads" "tails")
  "Coin Toss result values")

(defun toss ()
  "toss the coin"
  (nth (random 2)
       *toss-choices*))

(defun read-choice ()
  "read player choice"
  (format t "Heads or Tails: ")
  (let ((choice
	  (string-downcase
	   (string-trim " " (read-line)))))
    (if (find choice
	      *toss-choices*
	      :test #'equal)
	choice
	(read-choice))))

(defun main ()
  "Main Starting point of Coin-Toss"
  (let ((player-choice (read-choice))
	(coin-value (toss)))
    (if (equal player-choice
	       coin-value)
	(format t "You win!~%")
	(format t "You loss! Coin flipped ~A.~%" coin-value))))


