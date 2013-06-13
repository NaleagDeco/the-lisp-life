;;;; the-lisp-life.lisp

(in-package #:the-lisp-life)

;;; "the-lisp-life" goes here. Hacks and glory await!
(defun alivep (cell)
  (eql cell :alive))

(defun princ-cell (cell)
  (princ (cond ((alivep cell) "*")
	       (t "."))))

(defun princ-world (world)
  (loop for i below (array-dimension world 0)
     do (progn
	  (loop for j below (array-dimension world 1) do
	       (princ-cell (aref world i j)))
	  (terpri))))

(defun neighbours (world row col)
  (let ((rows (array-dimension world 0))
	(cols (array-dimension world 1))
	(nrows (list (1- row) row (1+ row)))
	(ncols (list (1- col) col (1+ col))))
    (loop for r in nrows nconc
	 (loop for c in ncols collect
	      (aref world (mod r rows) (mod c cols))))))

(defun tick-cell (world row col)
  (let ((cell (aref world row col))
	(live-adj (list-length (remove-if-not #'alivep
					      (neighbours world row col)))))
    (if (alivep cell)
	(cond ((or (= live-adj 2) (= live-adj 3)) :ALIVE)
	      (t :DEAD))		; Death by under/over-population
	(cond ((= live-adj 3) :ALIVE)	; Birth by reproduction
	      (t :DEAD)))))
