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

#| 
  
  Author: Dr. John Weiss

  Checks if 8-puzzle is solvable or not

  
|#
(defun 8solvable (L)
"
  (8solvable L): Checks if 8puzzle is solvable. 

  L - puzzle to be checked

"

    (setf *flag* nil)                               ; global *flag*
    (mapcar #'(lambda (elem) (disorder elem L)) L)
    (eq *flag* (evenp (position 0 L)))
)


#| 
  
  Author: Dr John Weiss

|#
(defun disorder (elem L)
"
  (disorder elem L): Used to help see if 8-puzzle is solvable 

  elem - certain element of puzzle
  L - puzzle

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


#| 
  
  Author: Allison Bodvig

  Counts the number of inversions in the puzzle by looping through the list
  twice and seeing when a element in a puzzle has numbers that are less than 
  it in an index after the index of the nuber
  
|#
(defun inversions (L)
"
  (inversions L): Counts the number of inversions in the puzzle.
  An inversion is when a tile in a puzzle has a smaller number 
  preceding it.  

  L - puzzle to be checked

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


#| 
  
  Author: Allison Bodvig

  Finds the index of the zero element in the list 
  
|#
(defun findZero (L)
"
  (findZero L): Finds index of 0 in the puzzle.

  L - puzzle list

"
; loops through looking for 0
  (let ((i 0) (index))
    (dotimes (i (length L))
      (setf index (nth i L))
      (if (= index 0)
        ; sets value when its found
        (setf *loc* i)
      )
    )
  )
)


#| 
  
  Author: Allison Bodvig

  Returns true or nil if a given n-puzzle is solvable or not
  
|#
(defun Nsolvable (L)
"
  (Nsolvable L): Checks if N-size puzzle is solvable. Returns true or nil.

  L - puzzle list
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

        ; checks for odd or even row with 0 in it
        (cond
          ((oddp div)
            ; checks number of inversions
            (if (evenp *inv*)
              (setf *flag* t)
              (setf *flag* nil)
            )
          )
          ; checks for even row with 0
          ((evenp div)
            ; checks for odd number of inversions
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

#| 
  
  Author: Allison Bodvig

  Calls the correct solvable function based on the size of the puzzle. 
  
|#
(defun solvable (L)
"
  (solvable L): Checks puzzle size and calls appropiate solvable fucntion.

  L - puzzle list 

"
  ; looks a lenght of puzzle and calls correct solvable function
  (if (= 9 (length L))
    (8solvable L)
    (Nsolvable L)
  )
)