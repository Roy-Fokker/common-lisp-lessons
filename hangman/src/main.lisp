(defpackage hangman
  (:use :cl)
  (:export game))
(in-package :hangman)

(defvar *dictionary*
  '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
  "Days of the week word dictionary")

(defun pick-word (dictionary)
  "Pick a random word from dictionary"
  (nth (random (length dictionary) (make-random-state t)) dictionary))

(defun print-status (lives guessed-letters scrambled-word)
  "Print status of the guesses"
  (format nil "Lives: ~A~%Letters: ~{~A~^, ~}~%Word: ~A" lives guessed-letters scrambled-word))

(defun scramble-word (word guessed-letters)
  "scramble word removing unguessed letters"
  (flet ((letter-or-underscore (letter)
	   (if (or (member letter guessed-letters)
		   (equal letter #\Space))
	       letter
	       #\_)))
    (coerce (mapcar #'letter-or-underscore
		    (coerce word 'list))
	    'string)))

(defun game-over-p (lives scrambled-word)
  "Is it game over?"
  (if (or (<= lives 0)
	  (eq nil (position #\_ scrambled-word)))
      t
      nil))

(defun read-letter (guessed-letters)
  "Read letter, player input"
  (format t "Please enter a letter: ")
  (force-output)
  (let ((user-input (string-downcase (string-trim " " (read-line)))))
    (cond ((= 0 (length user-input))
	   (read-letter guessed-letters))
	  ((member (char user-input 0) guessed-letters)
	   (read-letter guessed-letters))
	  (t
	   (char user-input 0)))))

(defun game (&key (word nil) (lives 10) (guessed-letters '()))
  "Game of hangman"
  (if (not word)
      ; If word not provided pick one from dictionary.
      (let ((word (string-downcase (pick-word *dictionary*))))
	(game :word word))
      ; else play game
      (let* ((scrambled-word (scramble-word word guessed-letters))
	     (game-over (game-over-p lives scrambled-word)))
	(format t "~A~%" (print-status lives guessed-letters scrambled-word))
	(if (not game-over)
	    (let ((letter (read-letter guessed-letters)))
	      (if (equal nil (position letter word))
		  (game :word word :lives (1- lives) :guessed-letters (append guessed-letters `(, letter)))
		  (game :word word :lives lives :guessed-letters (append guessed-letters `(, letter)))))
	    "Game Over!")
	)))
