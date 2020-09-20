(defpackage tic-tac-toe
  (:use :cl)
  (:export game))
(in-package :tic-tac-toe)

(defun display-board (board)
  "Display the game board"
  (dotimes (x 3)
    (dotimes (y 3)
      (if (= y 2)
	  (format t "~A~%"  (aref board x y))
	  (format t "~A | " (aref board x y)))))
  (force-output))

(defun update-board (board coords player)
  "Update the game board with player's move"
  (setf (aref board (getf coords :x) (getf coords :y))
	player))

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

(defun cpu-turn (board)
  "CPU places it's mark at"
  (let* ((rnd-state (make-random-state t))
	 (x (random (array-dimension board 0) rnd-state))
	 (y (random (array-dimension board 0) rnd-state))
	 (coords `(:x ,x :y ,y)))
    (if (valid-position-p board coords)
	coords
	(cpu-turn board))))

(defun player-turn (board)
  "Player places their mark at"
  (format t "Please enter X: ")
  (force-output)
  (let ((x (parse-integer (read-line) :junk-allowed t)))
    (unless (member x '(0 1 2))
      (player-turn board))
    
    (format t "Please enter Y: ")
    (force-output)
    (let ((y (parse-integer (read-line) :junk-allowed t)))
      (unless (member y '(0 1 2))
	(player-turn board))
      
      (let ((coords `(:x ,x :y ,y)))
	(if (valid-position-p board coords)
	    coords
	    (player-turn board))))))

(defun game ()
  "Game of tic tac toe."
  (let ((board (make-array '(3 3) :initial-element '-))
	(turn-counter (1+ (random 2 (make-random-state t)))))
    (do ()
	((game-over-p board))

      (display-board board)
      (if (evenp turn-counter)
	  (progn
	    (format t "Player Turn~%")
	    (let ((coords (player-turn board)))
	      (update-board board coords 'x)))

	  (progn
	    (format t "CPU Turn~%")
	    (let ((coords (cpu-turn board)))
	      (update-board board coords 'o))))

      (incf turn-counter))
    (display-board board)
    (format t "Game over!~%")))
