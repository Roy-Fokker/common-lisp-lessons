(defpackage rock-paper-scissors
  (:use :cl)
  (:export game))
(in-package :rock-paper-scissors)

(defvar *rps-options* '("rock" "paper" "scissors")
  "options for rock paper scissors")

(defvar *rps-win-states*
  '(("paper" "rock") ("rock" "scissors") ("scissors" "paper"))
  "Victory conditions in rock paper scissors")

(defun read-player-choice (options)
  "Read player choice"
  (format t "Please enter either -> ~{\'~A\'~^, ~}: " options)
  (force-output)
  
  (let ((choice (string-downcase (string-trim " " (read-line)))))
    (if (member choice options :test #'equal)
	choice
	(read-player-choice options))))

(defun game ()
  "Rock Paper Scissors game"
  (let ((cpu-choice (nth (random (length *rps-options*) (make-random-state t)) *rps-options*))
	(player-choice (read-player-choice *rps-options*)))
    
    (format t "Player Choice: ~A~%CPU Choice: ~A~%" player-choice cpu-choice)
    
    (cond
      ((equal player-choice cpu-choice)
       (format t "It's a Draw!~%"))
      ((member `(,player-choice ,cpu-choice) *rps-win-states* :test #'equal)
       (format t "Player Wins!~%"))
      (t
       "Player looses!~%"))))
