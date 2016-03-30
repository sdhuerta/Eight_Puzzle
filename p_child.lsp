#| 
  p_child.lsp

  This file contains the functions to generate
  successors and to evaluate nodes to produce a 
  score for use in the A* graph search. The functions
  included are:
  
  	puzzle_children - generate successor states

  	hamming - calculate the Hamming distance score
  	manhattan - calculate the manhattan distance score
  	nilsson - calculate the Nilsson sequence score

|#


#|
	Function: puzzle_children

	Author: Steven Huerta
    
    Description: This function will take a list representation of a 
    n-size puzzle, and return a list of up to four 
    possible states that can be generated from its
    current state.

    Param: puzzle - a list of n^2 elements representing a slide puzzle

    Return: a list of successor states
|#
(defun puzzle_children (puzzle)
"
  (puzzle_children puzzle): Calculates the Nilsson Sequence Number.

  puzzle is a list of n^2 elements representing a slide puzzle. 

"
    (let*
        (
            ; Find the length of the puzzle
            (width (sqrt (list-length puzzle)))
            ; Let's find the 0 position
            (pos (position 0 puzzle))
            (localPos (mod pos width)) 
            ; Copy the list four times to represent the 
            ; possible successor states
            (movUp (copy-list puzzle))
            (movDn (copy-list puzzle))
            (movLf (copy-list puzzle))
            (movRt (copy-list puzzle))
            ; Define a container to return
            (children '())
        )

        ; "Let's test moves up and down"
        (when (< -1 (- pos width) )
            (rotatef (nth pos movUp) (nth (- pos width) movUp))
            (push movUp children)
        )

        (when (> (* width width) (+ pos width))
            (rotatef (nth  pos movDn) (nth (+ pos width) movDn))
            (push movDn children)
        )

        ; "Let's test moves left and right"
        (when (< -1 (1- localPos))
            (rotatef (nth pos movLf) (nth (1- pos) movLf))
            (push movLf children)
        )

        (when (> width (1+ localPos))
            (rotatef (nth pos movRt) (nth (1+ pos) movRt))
            (push movRt children)
        )

    children
    )
)

#|
	Function: hamming

	Author: Steven Huerta
    
    Description: This function will take a list representation of a 
    n-size puzzle, and return the Hamming score of the list.

    Param: puzzle - a list of n^2 elements representing a slide puzzle

    Return: hamming score of the puzzle
|#
; Calculate the hamming distance of the goal state
(defun hamming (puzzle)
"
  (hamming puzzle): Calculates the Hamming distance score.

  puzzle is a list of n^2 elements representing a slide puzzle. 

"	
    (let ((score 0 )) ; initialize score to 0
    	; over the length of the puzzle
        (dotimes (i (list-length puzzle))
            (cond (
            	; if tile is out of place add 1 to the score
                (not(eq (nth i puzzle) (nth i *goalState*))) 
                    (incf score))
                (t 0)
            )
        )
        score    
    )
)


#|
	Function: manhattan

	Author: Steven Huerta
    
    Description: This function will take a list representation of a 
    n-size puzzle, and return the manhattan score of the list.

    Param: puzzle - a list of n^2 elements representing a slide puzzle

    Return: manhattan score of the puzzle
|#
; Calculate the manhattan distance of the goal state
(defun manhattan (puzzle)
"
  (manhattan puzzle_list): Calculates the manhattan distance score.

  puzzle_list is a list of n^2 elements representing a slide puzzle. 

"
    (let  
        (
            (width (sqrt (list-length puzzle)))
            (total_dis 0)

        )
        ; over all the elements of this list
        (dolist (element puzzle)
            (let*
                (
                	; identify the positions of the value in the list
                	; and its distance from its correct location
                    (pos_goal (position element *goalState*))
                    (pos_elem (position element puzzle))

                    ; calculate the manhattan distance between the
                    ; element and its correct location
                    (dis (+ (abs (- (floor (/ pos_elem width)) 
                        (floor (/ pos_goal width))))
                        (abs (- (mod pos_elem width) (mod pos_goal width))))
                    )
                )
            
            	; add the distance calculated to the toal distance
                (setf total_dis (+ total_dis dis))
            )

        )
        total_dis   ; return the distance
    )
)


#|
	Function: nilsson

	Author: Steven Huerta
    
    Description:T his function will take a list representation of a 
    n-size puzzle, and return the manhattan score of the list.
    Score = manhattan(x) + 3 * sequence_scoring(x), x is the list of 
    elements.

    Param: puzzle - a list of n^2 elements representing a slide puzzle

    Return: nilsson score of the puzzle
|#
(defun nilsson (puzzle)
"
  (nilsson puzzle_list): Calculates the Nilsson Sequence Number.

  puzzle_list is a list of n^2 elements representing a slide puzzle. 

"	
    (let*
        (
            (length (list-length puzzle))
            (width (sqrt length))
            (score 0)
        )
        (cond ((eq length 9)
        	; Evaluate the Spiral Eight Puzzle
        	; for each position, check to see if the successor to 
        	; the evaluated tile is sequential, 2 follows 1, 
        	; 3 follows 2, etc.
       		; Each time a break in sequence is discovered, the
       		; score is incremented by 2
	    	(dotimes (pos length)
	    		(cond
	    			; evaluate positions 0 and 1
	    			((or (eq pos 0) (eq pos 1))
	    				(cond
	    					; if element is equal to 8, need to check
	    					; to see if the next in sequence is 1
	    					((eq 8 (nth pos puzzle))
	    						(cond ((not (eq 1 (nth (1+ pos) puzzle)))
	    							(setf score (+ score 2)))
	    						)
	    					)
	    					((not (eq (nth pos puzzle) 
	    						(1- (nth (1+ pos) puzzle)))) 
	    						(setf score (+ score 2))
	    					)
	    				)
	    			)
	    			; evaluate positions 2 and 5
	    			((or (eq pos 2) (eq pos 5))
	    				(cond
	    					; if element is equal to 8, need to check
	    					; to see if the next in sequence is 1	    					
	    					((eq 8 (nth pos puzzle))
	    						(cond ((not (eq 1 (nth (+ pos width) puzzle)))
	    							(setf score (+ score 2)))
	    						)
	    					)
	    					((not (eq (nth pos puzzle) 
	    						(1- (nth (+ pos width) puzzle))))
	    						(setf score (+ score 2))
	    					)
	    				)			
	    			)
	    			; evaluate positions 8 and 7
	    			((or (eq pos 8) (eq pos 7))
	    				(cond
	    					; if element is equal to 8, need to check
	    					; to see if the next in sequence is 1	    					
	    					((eq 8 (nth pos puzzle))
	    						(cond ((not (eq 1 (nth (1- pos) puzzle)))
	    							(setf score (+ score 2)))
	    						)
	    					)
	    					((not (eq (1+ (nth pos puzzle)) 
	    						(nth (1- pos) puzzle))) 
	    						(setf score (+ score 2))
	    					)
	    				)    				
	    			)
	    			; evaluate positions 3 and 6
	    			((or (eq pos 3) (eq pos 6))
	    				(cond
	    					; if element is equal to 8, need to check
	    					; to see if the next in sequence is 1	    					
	    					((eq 8 (nth pos puzzle))
	    						(cond ((not (eq 1 (nth (- pos width) puzzle)))
	    							(setf score (+ score 2)))
	    						)
	    					)
	    					((not (eq (1+ (nth pos puzzle)) 
	    						(nth (- pos width) puzzle)))
	    						(setf score (+ score 2))
	    					)
	    				) 				   				
	    			)
	    			; evaluate center
	    			; if the center is not equal to 0
	    			; , the score will be incremented by 1
	    			(t (cond 
	    				((not (eq (nth pos puzzle) 0))
	    				(setf score (+ score 1)))
	    				)			
	    			)
	  			))
	    	)
        ; Evaluate Puzzles w/ blank in last position
    	(t 
    		(dotimes (pos length)
    			(cond
    				; Check to see if the 0 is in the last
    				; position, if not score increase by 1
    				((eq pos (1- length))
    					(cond
    						((not (eq (nth pos puzzle) 0))
    							(setf score (+ score 1)))
    					)
    				)
    				; last element should be followed by 0,
    				; else increase score by 2
    				((eq (nth pos puzzle) (1- length))
    					(cond
    						((not (eq (nth (1+ pos) puzzle) 0))
    							(setf score (+ score 2)))
    					)
    				)
    				(t
    					; if each element is not followed by 
    					; its correct sequence successor, add
    					; 2 to the score
    					(cond
    						((not (eq (nth pos puzzle) (1- (nth (1+ pos) puzzle))))
    							(setf score (+ score 2))
    						)

    					)
    				)
    			)
    		)

    	)
    	)
    	; sum the manhattan score and the product of 3 times
    	; the score found from the sequence evaluation
        (+ (* 3 score) (manhattan puzzle))
    )
)
