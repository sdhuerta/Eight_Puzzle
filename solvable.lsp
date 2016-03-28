#|
                  ***** SOLVABLE.LSP *****

The SOLVABLE function returns T if a given 8-puzzle position is solvable,
NIL otherwise.

Usage:    (solvable L)
          where L is a 9-element list such as (1 2 3 8 0 4 7 6 5)

Reference:  "Mathematical Games and Pastimes", p.79-85,
             A.P.Domoryad, Macmillan, 1964.

Written 03/88 by John M. Weiss, Ph.D.

Modifications:

  Modified to work for n-puzzle 

  Author: Allison Bodvig

  Algorithm reference: 
    https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html
|#

(defvar *flag*)
(defvar *inv*)
(defvar *loc*)

; original solvable function which works for 8-puzzle
(defun 8solvable (L)
"
  (8solvable L): Checks if 8puzzle is solvable. 

"

    (setf *flag* nil)                               ; global *flag*
    (mapcar #'(lambda (elem) (disorder elem L)) L)
    (eq *flag* (evenp (position 0 L)))
)

(defun disorder (elem L)
"
  (disorder elem L): Used to help see if 8-puzzle is solvable 

"
    (cond
        ((eq (car L) elem))
        ((> (car L) elem)
            (setf *flag* (not *flag*))
            (disorder elem (cdr L))
        )
        (t (disorder elem (cdr L)))
    )
)

(defun inversions (L)
"
  (inversions L): Counts the number of inversions in the puzzle.
  An inversion is when a tile in a puzzle has a smaller number 
  preceding it.  

"
  ; set inversions
  (setf *inv* 0)
  (let ((i 0) (j 0) (len 0) (index1) (index2))
    (setf len (length L))
    ; loop through puzzle
    (dotimes (i len)
      (setf index1 (nth i L))
      ; loop through puzzle again
      (dotimes (j len)
        (setf index2 (nth j L))
        ; checks for to make sure we are after the current index
        ; also makes sure we are not using the 'blank'
        (if (and (> j i) (and (not (= index1 0)) (not (= index2 0))))
          (progn 
            (if (< index2 index1)
              ; if less than, adds one to inversions
              (setf *inv* (1+ *inv*))
            )
          )
        )
      )
    )
  )
)

(defun findZero (L)
"
  (findZero L): Finds index of 0 in the puzzle.

"
  (let ((i 0) (index))
    (dotimes (i (length L))
      (setf index (nth i L))
      (if (= index 0)
        (setf *loc* i)
      )
    )
  )
)

(defun Nsolvable (L)
"
  (Nsolvable L): Checks if N-size puzzle is solvable. Returns true or nil.
"
  (setf *flag* nil)
  (setf *loc* nil)
  (let* ((len) (div))
    (setf len (sqrt (length L)))
    ; count inversions
    (inversions L)
    (cond
      ; odd number of inversions
      ((oddp len)
        (if (evenp *inv*)
          (setf *flag* t)
          (setf *flag* nil)
        )
      )
      ((evenp len)
        ; find location of blank
        (findZero L)
        ; divide blank by len
        (setf div (truncate (/ *loc* len)))

        (cond
          ((oddp div)
            (if (evenp *inv*)
              (setf *flag* t)
              (setf *flag* nil)
            )
          )

          ((evenp div)
            (if (oddp *inv*)
              (setf *flag* t)
              (setf *flag* nil)
            )
          )
        )
      )
    )
  )
)

(defun solvable (L)
"
  (solvable L): Checks puzzle size and calls appropiate solvable fucntion. 

"
  ; looks a lenght of puzzle and calls correct solvable function
  (if (= 9 (length L))
    (8solvable L)
    (Nsolvable L)
  )
)