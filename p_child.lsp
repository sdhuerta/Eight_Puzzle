#|
    This function will take a list representation of a 
    n-size puzzle, and return a list of up to four 
    possible states that can be generated from its
    current state.
|#
(defun puzzle_children (puzzle)
    (let*
        (
            ; "Find the length of the puzzle"
            (width (sqrt (list-length puzzle)))
            ;"Let's find the 0 position"
            (pos (position 0 puzzle))
            (localPos (mod pos width)) 
            ; "Define a container to return"
            (movUp (copy-list puzzle))
            (movDn (copy-list puzzle))
            (movLf (copy-list puzzle))
            (movRt (copy-list puzzle))
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


; Function to measure manhattan distance
(defun scoring (puzzle)
    (let ((score (+ (manhattan puzzle) (hamming puzzle)))) score)
)

; Calculate the hamming distance of the goal state
(defun hamming (puzzle)
    (let ((score 0 ))
        (dotimes (i (list-length puzzle))
            (cond (
                (not(eq (nth i puzzle) (nth i *goalState*))) 
                    (incf score))
                (t 0)
            )
        )
        score    
    )
)


; Calculate the manhattan distance of the goal state
(defun manhattan (puzzle)
    (let  
        (
            (width (sqrt (list-length puzzle)))
            (total_dis 0)

        )
        (dolist (element puzzle)
            (let*
                (
                    (pos_goal (position element *goalState*))
                    (pos_elem (position element puzzle))

                    (dis (+ (abs (- (floor (/ pos_elem width)) 
                        (floor (/ pos_goal width))))
                        (abs (- (mod pos_elem width) (mod pos_goal width))))
                    )
                )
            
                (setf total_dis (+ total_dis dis))
            )

        )
        total_dis   
    )
)

(defun nilsson (puzzle)
    (let*
        (
            (length (list-length puzzle))
            (width (sqrt length))
            (score 0)
        )
        (cond ((eq length 9)
        	; Evaluate the Spiral Eight Puzzle
	    	(dotimes (pos length)
	    		(cond
	    			((or (eq pos 0) (eq pos 1))
	    				(cond
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
	    			((or (eq pos 2) (eq pos 5))
	    				(cond
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
	    			((or (eq pos 8) (eq pos 7))
	    				(cond
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
	    			((or (eq pos 3) (eq pos 6))
	    				(cond
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
    				((eq pos (1- length))
    					(cond
    						((not (eq (nth pos puzzle) 0))
    							(setf score (+ score 1)))
    					)
    				)
    				((eq (nth pos puzzle) (1- length))
    					(cond
    						((not (eq (nth (1+ pos) puzzle) 0))
    							(setf score (+ score 2)))
    					)
    				)
    				(t
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
        (+ (* 3 score) (manhattan puzzle))
    )
)
