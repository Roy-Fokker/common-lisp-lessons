(defpackage tic-tac-toe-clos
  (:use :cl)
  (:export game))
(in-package :tic-tac-toe-clos)

(defun display-board (board)
  "Display the game board"
  (dotimes (x 3)
    (dotimes (y 3)
      (if (= y 2)
	  (format t "~A~%"  (aref board x y))
	  (format t "~A | " (aref board x y)))))
  (force-output))

(defun update-board (board player)
  "update the board using player's input"
  (let ((coords (turn player board)))
    (setf (aref board (getf coords :x) (getf coords :y))
	  (icon player))))

(defun valid-position-p (board coords)
  "Is requested position is available"
  (eql '- (aref board (getf coords :x) (getf coords :y))))

(defun game-over-p (board)
  "Is game is over."
  (flet ((draw-p (board)
	   "Are any of the cells '-? if so return nil"
	   (let ((match-t nil))
	     (dotimes (x 3)
	       (dotimes (y 3)
		 (setq match-t (or match-t
				   (eql '-
					(aref board x y))))))
	     (not match-t)))
	 (cells-match-p (board cells)
	   "Do all the cell match? but are not '-"
	   (let ((match-t nil)
		 (match-on (aref board
				 (nth 0 (first cells))
				 (nth 1 (first cells)))))
	     (unless (eql match-on '-)
	       (setq match-t t)
	       (dolist (cell cells)
		 (setq match-t (and match-t
				    (eql match-on
					 (aref board
					       (nth 0 cell)
					       (nth 1 cell)))))))
	     match-t)))
    (cond
      ;; Check rows
      ((cells-match-p board '((0 0) (0 1) (0 2))) t)
      ((cells-match-p board '((1 0) (1 1) (1 2))) t)
      ((cells-match-p board '((2 0) (2 1) (2 2))) t)
      ;; Check columns
      ((cells-match-p board '((0 0) (1 0) (2 0))) t)
      ((cells-match-p board '((0 1) (1 1) (2 1))) t)
      ((cells-match-p board '((0 2) (1 2) (2 2))) t)
      ;; Check diagonals
      ((cells-match-p board '((0 0) (1 1) (2 2))) t)
      ((cells-match-p board '((0 2) (1 1) (2 0))) t)
      ;; Is Draw
      ((draw-p board) t)
      (t nil))))

(defclass player ()
  ((icon :initarg :icon :initform (error "Must provide an icon") :reader icon))
  (:documentation "Basic player data class"))

(defgeneric turn (player board)
  (:documentation "Executes a player's turn"))

(defclass human (player)
  ())

(defmethod turn ((player human) board)
  "Execute Human player's turn"
  (flet ((get-cell (coord)
	   "Get the cell coordinates from the human"
	   (format t "Please enter ~A: " coord)
	   (force-output)
	   (parse-integer (read-line) :junk-allowed t)))
    (do* ((x (get-cell "X") (get-cell "X"))
	  (y (get-cell "Y") (get-cell "Y"))
	  (coords `(:x ,x :y ,y) `(:x ,x :y ,y)))
	 ((and (member x '(0 1 2))
	       (member y '(0 1 2))
	       (valid-position-p board coords))
	  coords))))

(defclass cpu (player)
  ())

(defmethod turn ((player cpu) board)
  "Execute CPU player's turn"
  (do* ((x (random (array-dimension board 0)) (random (array-dimension board 0)))
	(y (random (array-dimension board 0)) (random (array-dimension board 0)))
	(coords `(:x ,x :y ,y) `(:x ,x :y ,y)))
       ((valid-position-p board coords)
	coords)))

(defun game ()
  "Game of tic tac toe using CLOS"
  (make-random-state t)

  (let ((board (make-array '(3 3) :initial-element '-))
	(human (make-instance 'human :icon 'x))
	(cpu (make-instance 'cpu :icon 'o)))
    (do ((turn-counter (1+ (random 2)) (1+ turn-counter)))
	((game-over-p board))
      
      (display-board board)

      (if (evenp turn-counter)
	  (progn
	    (format t "Player Turn ~%")
	    (update-board board human))
	  (progn
	    (format t "CPU Turn ~%")
	    (update-board board cpu))))

    (display-board board)
    (format t "Game Over!~%")
    (force-output)))
