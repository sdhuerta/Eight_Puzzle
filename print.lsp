#|
 | Author: Christian Sieh
 | 
 | This helper function will go through all of our puzzle states and break them up
 | into list containing the states we want to print on each row. This will give us a list 
 | of rows where each row is a certain number of states. The number of states per row is
 | determined by the column width which is currently the number 12. The column width is an
 | arbitrary number that is divided by the size of the puzzle which will gives us the 
 | number of states per row. 
 |#
(defun rowState (puzzleStates puzzleSize)
  ;12 is an arbitrary number to represent column width. This is used to get the number of
  ;states per row.
  (let ((L '()) (rowStates '()) (statesPerRow (floor (/ 12 puzzleSize))))
    ;Go through each puzzle state
    (dotimes (i (length puzzleStates))
      ;Ignore when i is equal to 0 since 0 mod anything is 0 so we skip that case.
      ;If i mode states per row is equal to 0 then we need to create a new row
      (if (and (not(eq i 0)) (eq 0 (mod i statesPerRow)))
        (progn (push (reverse L) rowStates) (setf L nil) (push (nth i puzzleStates) L))
        (push (nth i puzzleStates) L)
      )
    )
    ;This push is just to push the final list onto rowStates
    (push (reverse L) rowStates)
    ;We need to reverse rowStates to get them in the correct order
    (setf rowStates (reverse rowStates)) 
  )
)

#|
 | Author: Christian Sieh
 |
 | This funciton gets each row from rowStates and prints out each row from each state
 | before then printing out the next row from each of those states. Once the row has
 | been printed it moves on to the next row.
 |#
(defun printPuzzle (name puzzleStates moveCount genNodes disNodes exNodes puzzleSize)  
  (let ((rowStates (rowState puzzleStates puzzleSize)) (j 0))
    ;Header information
    (format t "~a~%" name)
    (format t "---------------~%")
    (format t "Solution found in ~d moves~%" moveCount)
    (format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" genNodes disNodes exNodes)
    ;For each row
    (dolist (row rowStates)
      ;For each row in the state
      (dotimes (i puzzleSize)
        ;For each state in the row
        (dolist (state row)
          ;For each position in the state up to puzzleSize
          (do ((k j (1+ k)))
	       ((= k (+ j puzzleSize)))
              ;If the value is 0 ouput a space otherwise output the digit
              (if (eq 0 (nth k state))
                (format t "  ")
                (format t "~d " (nth k state))
              )
	  )
          (format t "     ")
        )
        (format t "~%")
        (setf j (+ j puzzleSize))
      )  
      (format t "~%")
      (setf j 0)
    )
  )
)

;Test puzzleState
; (setf puzzleStates '((2 8 3 1 6 0 7 5 4) (2 8 3 1 6 4 7 5 0) (2 8 3 1 6 4 7 0 5) (2 8 3 1 0 4 7 6 5) (2 0 3 1 8 4 7 6 5) (0 2 3 1 8 4 7 6 5) (1 2 3 0 8 4 7 6 5) (1 2 3 8 0 4 7 6 5)))

; ;Call Function
; (printPuzzle "BFS" puzzleStates 10 11 12 13 3)

