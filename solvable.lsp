#|
                  ***** SOLVABLE.LSP *****

The SOLVABLE function returns T if a given n-puzzle position is solvable,
NIL otherwise.

Usage:    (solvable L)
          where L is a n-element list


|#

(defvar *flag*)
(defvar *inv*)
(defvar *loc*)

; check for 0 in list - then ignore
(defun inversions (L)
  (setf *inv* 0)
  (let ((i 0) (j 0) (len 0) (index1) (index2))
    (setf len (length L))
    (dotimes (i len)
      (setf index1 (nth i L))
      (dotimes (j len)
        (setf index2 (nth j L))
        (if (and (> j i) (and (not (= index1 0)) (not (= index2 0))))
          (progn 
            (if (< index2 index1)
              (setf *inv* (1+ *inv*))
            )
          )
        )
      )
    )
  )
)

(defun findZero (L)
  (let ((i 0) (index))
    (dotimes (i (length L))
      (setf index (nth i L))
      (if (= index 0)
        (setf *loc* i)
      )
    )
  )
)

; solvable algorithm taken from:
; https://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html

(defun newSolvable (L)
  (setf *flag* nil)
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
  (write *flag*)
)