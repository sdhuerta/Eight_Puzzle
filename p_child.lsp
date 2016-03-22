
;---------------------------------------------------
; Author: Dr. John Weiss
; NODE Structure: stores the state and parent
(defstruct node state parent score)

; Author: Dr. John Weiss    
; Test if two nodes have the same state
(defun equal-states (n1 n2)
    (equal 
        (node-state n1) (node-state n2)
    )
)
;---------------------------------------------------


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


#|
    Author: Dr. John Weiss
    Function 
    Stuff
|#
; Breadth-first-search implements the OPEN list as a QUEUE of (state parent) nodes.
(defun bfs (start) (search_bfs_dfs start 'bfs))


#|
    Author: Dr. John Weiss
    Function 
    Stuff
|#
; Depth-first-search implements the OPEN list as a STACK of (state parent) nodes.
(defun dfs (start) (search_bfs_dfs start 'dfs))


#|
    Author: Dr. John Weiss
    Function 
    Stuff
|#
; Given a start state and a search type (BFS or DFS), return a path from the start to the goal.
(defun search_bfs_dfs (start type)
    (do*                                                    ; note use of sequential DO*
        (                                                   ; initialize local loop vars
            (curNode (make-node :state start :parent nil))  ; current node: (start nil)
            (OPEN (list curNode))                           ; OPEN list:    ((start nil))
            (CLOSED nil)                                    ; CLOSED list:  ( )
        )

        ; termination condition - return solution path when goal is found
        ; ((goal-state? (node-state curNode)) (build-solution curNode CLOSED))
        (eq )
        ; loop body
        (when (null OPEN) (return nil))             ; no solution

        ; get current node from OPEN, update OPEN and CLOSED
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
        (setf CLOSED (cons curNode CLOSED))

        ; add successors of current node to OPEN
        (dolist (child (puzzle_children (node-state curNode)))

            ; for each child node
            (setf child (make-node :state child :parent (node-state curNode) :score 0))

            ; if the node is not on OPEN or CLOSED
            (if (and (not (member child OPEN   :test #'equal-states))
                     (not (member child CLOSED :test #'equal-states)))

                ; add it to the OPEN list
                (cond

                    ; BFS - add to end of OPEN list (queue)
                    ((eq type 'bfs) (setf OPEN (append OPEN (list child))))

                    ; DFS - add to start of OPEN list (stack)
                    ((eq type 'dfs) (setf OPEN (cons child OPEN)))

                    ; error handling for incorrect usage
                    (t (format t "SEARCH: bad search type! ~s~%" type) (return nil))
                )
            )
        )
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
