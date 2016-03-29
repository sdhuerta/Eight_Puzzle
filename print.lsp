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
  (let ((L '()) (rowStates '()) (statesPerRow (floor (/ 16 puzzleSize))))

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
(defun printPuzzle (type puzzleStates genNodes disNodes exNodes)  
  (let* ((puzzleSize (sqrt(length(car puzzleStates)))) (rowStates (rowState puzzleStates puzzleSize)) (j 0) 
        (moveCount (- (length puzzleStates) 1)) (midpoint (floor(/ puzzleSize 2))))

    ;Header information
    (cond
      ((eq type 'bfs) (format t "~a~%" "BFS Graph Search"))

      ((eq type 'idfs) (format t "~a~%" "DFID Graph Search"))

      ((eq type 'astar_hamming) (format t "~a~%" "A* Graph Search (Heuristic: Hamming)"))

      ((eq type 'astar_manhat) (format t "~a~%" "A* Graph Search (Heuristic: Manhattan)"))

      ((eq type 'astar_nilsson) (format t "~a~%" "A* Graph Search (Heuristic: Nilsson's Sequence Score)"))
    )

    (format t "---------------~%")
    (format t "Solution found in ~d moves~%" moveCount)
    (format t "~d nodes generated (~d distinct nodes), ~d nodes expanded~%~%" genNodes disNodes exNodes)
    ;For each row
    (dotimes (m (length rowStates))
      (setf row (nth m rowStates))
      ;For each row in the state
      (dotimes (i puzzleSize)
        ;For each state in the row
        (dotimes (n (length row))
	  (setf state (nth n row))
          ;For each position in the state up to puzzleSize
          (do ((k j (1+ k)))
	       ((= k (+ j puzzleSize)))
              ;If the value is 0 ouput a space otherwise output the digit
              (if (eq 0 (nth k state))
                (format t "  ")
                (format t "~d " (nth k state))
              )
	  )

	  ;if it's the midpoint of the puzzleSize and it's NOT the last state in the last row then 
	  ;write an arrow
          (if (and (eq midpoint i) (not (and (eq (- (length rowStates) 1) m) (eq (- (length row) 1) n))))
	    (format t " ->  ")
	    (format t "     ")
          )
        )
        (format t "~%")
        (setf j (+ j puzzleSize))
      )  
      (format t "~%")
      (setf j 0)
    )
  )
)
