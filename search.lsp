#|
                    ***** SEARCH.LSP *****

General-purpose exhaustive search routine includes both breadth-first
search, iterated depth first search, and three A* search methods using
different heuristics for evaluation. Uses graph search with OPEN and CLOSED
lists rather than tree search, to avoid cycles, with the exception of 
the IDFS search, which will not eliminate nodes that have been evaluated
before.   

BFS and IDFS not use heuristics to limit or guide search.

To solve a specific problem, the functions "generate-successors" and
"goal-state" must be defined. "Generate-successors" takes a state as its
argument and returns a list of child states. "Goal-state?" returns T if
its argument is a goal state, NIL otherwise.

In order to retrace a solution path, nodes are stored as (state parent)
pairs, where "state" is the current state and "parent" is the parent
state. Given a goal node, a solution path is generated by simply tracing
backwards through the parent states.

Author: John M. Weiss, Ph.D.
Written Spring 2016 for CSC447/547 AI class.

Modified by: Steven Huerta, Christian Sieh

|#

;--------------------------------------------------------------------------
; load file
(load 'p_child.lsp)
(load 'print.lsp)

; global variable for goal state of the puzzle
(defvar *goalState*)

; Node structure: 
(defstruct node 
    state  ; the list of integer values representing the puzzle
    parent ; the parent node
    score  ; the score of the list, useful for A*
    depth  ; the depth of the node, useful for A* and IDFS
)

; Test if two nodes have the same state.

#|
	Function: equal-states

	Author: Dr. John Weiss
    
    Description: Test if two nodes have the same state.

    Param: n1 - first node
    	   n2 - second node

|#
(defun equal-states (n1 n2)
"
  (equal-states n1 n2): test if lists are the same

  n1 - first node to compare

  n2 - second node to compare

" 
    (equal (node-state n1) (node-state n2))
)

;--------------------------------------------------------------------------


#|
	Function: bfs

	Author: Dr. John Weiss
    
    Description: Breadth-first-search implements
    			 the OPEN list as a QUEUE of (state parent) nodes.

    Param: start - a list of n^2 elements representing the intial
    			   state of the slide puzzle

|#
(defun bfs (start) 
"
  (bfs start): Search the graph using BFS

  start - list of elements representing initial state of the puzzle 

"
	(search_bfs_dfs start 'bfs)
)


; Depth-first-search implements the OPEN list as a STACK of (state parent) nodes.
#|
	Function: idfs

	Author: Dr. John Weiss

	Modified: Steven Huerta
    
    Description: Iterated depth-first-search 
    implements the OPEN list as a STACK of (state parent) nodes. 
    the idfs string will trigger iterative depth exploration
    of the graph. This implementation DOES NOT eliminate 
    duplicate puzzle states. 

    Param: start - a list of n^2 elements representing the intial
    			   state of the slide puzzle

|#
(defun idfs (start)
"
  (idfs start): Returns true if valid puzzle entries. 

"
	(search_bfs_dfs start 'idfs)
)

#|
	Function: astar_hamming

	Author: Steven Huerta
    
    Description: A* using hamming score to determine best node in list

    Param: start - a list of n^2 elements representing the intial
    			   state of the slide puzzle

|#
(defun astar_hamming (start) 
"
  (astar_hamming start): Returns true if valid puzzle entries. 

"
    (search_bfs_dfs start 'astar_hamming)
)


#|
	Function: astar_manhat

	Author: Steven Huerta

	Description: A* using manhattan score to determine best node in list

    Param: start - a list of n^2 elements representing the intial
    			   state of the slide puzzle

|#
(defun astar_manhat (start)
"
  (astar_manhat start): Returns true if valid puzzle entries. 

"
	(search_bfs_dfs start 'astar_manhat)
)


; A* using nilsson score to determine best node in list
#|
	Function: astar_nilsson

	Author: Steven Huerta
  
    Description: A* using nilsson score to determine best node in list

    Param: start - a list of n^2 elements representing the intial
    			   state of the slide puzzle

|#
(defun astar_nilsson (start)
"
  (astar_nilsson start): Returns true if valid puzzle entries. 

"
	(search_bfs_dfs start 'astar_nilsson)
)


#|
	Function: search_bfs_dfs

	Author: Dr. John Weiss

	Modified: Steven Huerta, Christian Sieh
    
    Description: This function is a catch all for the three
    types of search functions: BFS, IDFS, and A*. This function
    calls the other functions to generate children and search the 
    graph generated to find a goal state. After finding a goal state,
    the function will call a print function to display results on the
    screen

    Param: start - a list of n^2 elements representing the intial
    			   state of the slide puzzle
    		type - a string value that represents the type of search

|#
(defun search_bfs_dfs (start type)
"
  (search_bfs_dfs start type): Creates and explores a graph for a
  	solution to the puzzle.

  	start - list of elements representing initial state of the puzzle

  	type - string value representing search type (bfs, idfs 
  		   , astar_hamming, astar_manhat, or astar_nilsson)

"	
    (generateGoalState (1- (length start)))

    (do*                                                    ; note use of sequential DO*
        (                                                   ; initialize local loop vars
            (curNode (make-node :state start :parent nil :score 0 :depth 0))  ; current node: (start nil)
            (OPEN (list curNode))                           ; OPEN list:    ((start nil))
            (CLOSED nil)                                    ; CLOSED list:  ( )
            (depthLimit 0)   ; Keep track of the allowable depth
            (depthCount 0)   ; Track the size of the CLOSED list 
            (duplicateNodes 0)
		    (genNodes 0)
		    (exNodes 0)
		    (disNodes 0)
		)
	
        ; termination condition - return solution path when goal is found
        ((goal-state? (node-state curNode))
            (build-solution curNode CLOSED type genNodes disNodes exNodes))


        (cond 
        	; if the CLOSED list is empty
            ((null OPEN) (cond 
            	; and search is of type idfs
                ((eq type 'idfs)
                	; check that we aren't stuck and end if we are
                    (cond ((eq depthCount (list-length CLOSED)) (return nil)))
                    ; record the length of this closed list to check against the next
                    ; depth search
                    (setf depthCount (list-length CLOSED))

                    ; iterate the depth counter
                    (setf depthLimit (1+ depthLimit))

                    ; reset all the values to initial conditions
                    (setf curNode (make-node :state start :parent nil :score 0 :depth 0))  
                    (setf OPEN (list curNode))                          
                    (setf CLOSED nil)                  
                )
                ; if not idfs, then quit
                (t (return nil)))
            )
        )

        ; If an A* search
        (cond ((or (eq type 'astar_hamming) (eq type 'astar_manhat) 
        	(eq type 'astar_nilsson)) 
        	; sort the list ascending values
        	(sort OPEN #'< :key #'node-score)))

        ; get current node from OPEN, update OPEN and CLOSED
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
        (setf CLOSED (cons curNode CLOSED))

		;Since we are expanding the node we increment expanded nodes
		(incf  exNodes)

        ; add successors of current node to OPEN

        (if (or (not (eq type 'idfs)) (<= (node-depth curNode) depthLimit))

        (dolist (child (puzzle_children (node-state curNode)))

	        ;For every child we generate we increment generated nodes
	        (incf  genNodes)

	        ; for each child node
	        (setf child (make-node :state child :parent (node-state curNode) 
	        	:depth (1+ (node-depth curNode))))

	        ; if the node is not on OPEN or CLOSED

	        (if (or (and (eq type 'idfs) (< (node-depth curNode) depthLimit)) 
                (and (not (member child OPEN   :test #'equal-states))
	                 (not (member child CLOSED :test #'equal-states))
                ))

                     
	        
	        ; add it to the OPEN list and increment distinct nodes
	        (progn (incf disNodes)
			(cond		
	            ; BFS - add to end of OPEN list (queue)
	            ((eq type 'bfs) (setf OPEN (append OPEN (list child))))

	            ; IDFS - add to start of OPEN list (stack) up to the depth
	            ((eq type 'idfs) (setf OPEN (cons child OPEN)))

	           	; if A* type, let's check which A* type and proceed
	            ((or (eq type 'astar_hamming) (eq type 'astar_manhat) 
	            	(eq type 'astar_nilsson))
	            	(cond 
	            		; score the child with hamming
	            		((eq type 'astar_hamming)
	                    (setf (node-score child) (+ (node-depth child) 
	                    	(hamming (node-state child))))
	                    (setf OPEN (append OPEN (list child)))
	                   	)
	            		; score the child with manhattan
	            		((eq type 'astar_manhat)
	            		(setf (node-score child) (+ (node-depth child) 
	            			(manhattan (node-state child))))
	                    (setf OPEN (append OPEN (list child)))
	                    )
	                    ; score the child with nilsson
	                    ((eq type 'astar_nilsson)
	            		(setf (node-score child) (+ (node-depth child) 
	            			(nilsson (node-state child))))
	                    (setf OPEN (append OPEN (list child)))
	                    )
	                )
	            )

	            ; error handling for incorrect usage
	            (t (format t "SEARCH: bad search type! ~s~%" type) (return nil))
    		)))
    	))
    )
)



#|
	Function: build-solution

	Author: Dr. John Weiss

	Modified: Steven Huerta, Christian Sieh
    
    Description: Build-solution takes a state and a list of 
    (state parent) pairs and constructs the list of states 
    that led to the current state by tracing back through
    the parents to the start node (nil parent).

    Param:  node - final goal node
  	 		node-list - nodes from CLOSED list from graph search
  	 		genNodes - value of total nodes generated
  	 		disNodes - value of distinct nodes generated
  	 		exNodes - value of nodes expaned
|#
(defun build-solution (node node-list type genNodes disNodes exNodes)
"
  (build-solution node node-list type genNodes disNodes exNodes):
  	 Returns true if valid puzzle entries.

  	 node: final goal node
  	 node-list: nodes from CLOSED list from graph search
  	 genNodes: value of total nodes generated
  	 disNodes: value of distinct nodes generated
  	 exNodes: value of nodes expaned
"
    (let 
    	(
    		(finalPath)
    	)
        (do
            ((path (list (node-state node))))       ; local loop var
            ((null (node-parent node)) path)         ; termination condition

            ; find the parent of the current node
            (setf node (member-state (node-parent node) (- (node-depth node) 1) node-list))
        
            ; add it to the path
            (setf path (cons (node-state node) path))

            (setf finalPath path)
        )
	
        (printPuzzle type finalPath genNodes disNodes exNodes)
    )

)

#|
	Function: member-state

	Author: Dr. John Weiss

	Modified: Steven Huerta, Christian Sieh
    
    Description: Member-state looks for a node on the
    node-list with the same state.

    Param: 	state: puzzle state being searched for
		  	depth: depth of node to search for
		  	node-list: list of nodes to search

|#
(defun member-state (state depth node-list)
"
  (member-state state depth node-list):
  	Member-state looks for a node on the
    node-list with the same state.

  	 state: puzzle state being searched for
  	 depth: depth of node to search for
  	 node-list: list of nodes to search
"	
    (dolist (node node-list)
    	; find where the state and depth match the 
    	; parameters given
        (when (and (equal state (node-state node)) 
        	(equal depth (node-depth node))) (return node))
    )
)

#| 
  
  Author: Allison Bodvig

  Generates the goal-state of the puzzle given the size of the puzzle. The 
  goal state is spiral for the 8-puzzle and for the other puzzles, the goal-
  state is when the blank is in the lower right corner.
  
|#
(defun generateGoalState (puzSize)
"
  (generateGoalState puzSize): genetates the goal state based on puzzle size.

  puzSize - number of elements in the puzzle

"

    (setf *goalState* nil)
    (if (= puzSize 8)
        ; goal state for 8-puzzle
        (setq *goalState* '(1 2 3 8 0 4 7 6 5))
        (progn
            (push 0 *goalState*) 
            (loop for i from puzSize downto 1
                do (push i *goalState*)
            )
        )
    )

    ; (write *goalState*)
)

#|
	Function: goal-state?

	Author: Dr. John Weiss
    
    Description: Compare state to the goal state and 
    return true if they match. 

    Param: state - a list of n^2 elements representing the intial
    			   state of the slide puzzle

|#
(defun goal-state? (state)
"
  (goal-state? state): Returns true if goal-state was reached.
"
    (equal state *goalState*)
)
