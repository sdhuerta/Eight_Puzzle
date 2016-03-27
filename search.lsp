#|
                    ***** SEARCH.LSP *****

General-purpose exhaustive search routine includes both breadth-first
search and depth-first search. Uses graph search with OPEN and CLOSED
lists rather than tree search, to avoid cycles. Does not use heuristics
to limit or guide search.

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

Modifications:

|#

;--------------------------------------------------------------------------
; load file
(load 'p_child.lsp)
(load 'print.lsp)

; global variable for goal state of the puzzle
(defvar *goalState*)

; Node structure: stores state and parent.
(defstruct node 
    state 
    parent 
    score 
    depth
)

; Test if two nodes have the same state.
(defun equal-states (n1 n2) 
    (equal (node-state n1) (node-state n2))
)

;--------------------------------------------------------------------------

; Breadth-first-search implements the OPEN list as a QUEUE of (state parent) nodes.
(defun bfs (start) (search_bfs_dfs start 'bfs))

; Depth-first-search implements the OPEN list as a STACK of (state parent) nodes.
(defun idfs (start) (search_bfs_dfs start 'idfs))

(defun astar (start) (search_bfs_dfs start 'astar))

; Given a start state and a search type (BFS or DFS), return a path from the start to the goal.
(defun search_bfs_dfs (start type)
    (generateGoalState (1- (length start)))

    (do*                                                    ; note use of sequential DO*
        (                                                   ; initialize local loop vars
            (curNode (make-node :state start :parent nil :score 0 :depth 0))  ; current node: (start nil)
            (OPEN (list curNode))                           ; OPEN list:    ((start nil))
            (CLOSED nil)                                    ; CLOSED list:  ( )
            (depthLimit 0)
            (depthCount 0)                                  ; Track 
        )
	
        ; termination condition - return solution path when goal is found
        ((goal-state? (node-state curNode)) (build-solution curNode CLOSED OPEN type))

        (cond 
            ((null OPEN) (cond 
                ((eq type 'idfs)
                    ;(cond ((eq depthCount (list-length CLOSED)) (return nil)))
                    (setf depthCount (list-length CLOSED))
                    (incf depthLimit)
                    (setf curNode (make-node :state start :parent nil :score 0 :depth 0))  
                    (setf OPEN (list curNode))                           
                    (setf CLOSED nil)                     
                )

                (t (return nil)))
            )
        )

        ; Sort the remaining OPEN LIST, score ascending
        (cond ((eq type 'astar) (sort OPEN #'< :key #'node-score)))

        ; get current node from OPEN, update OPEN and CLOSED
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
        (setf CLOSED (cons curNode CLOSED))

        ; add successors of current node to OPEN
        (dolist (child (puzzle_children (node-state curNode)))

            ; for each child node
            (setf child (make-node :state child :parent (node-state curNode) :depth (incf (node-depth curNode))))

            ; if the node is not on OPEN or CLOSED
            (if (and (not (member child OPEN   :test #'equal-states))
                     (not (member child CLOSED :test #'equal-states)))

                ; add it to the OPEN list
                (cond

                    ; BFS - add to end of OPEN list (queue)
                    ((eq type 'bfs) (setf OPEN (append OPEN (list child))))

                    ; IDFS - add to start of OPEN list (stack)
                    ((eq type 'idfs)
                        (cond
                            ((< (node-depth curNode) depthLimit) (setf OPEN (cons child OPEN)))
                        )
                    )

                    ((eq type 'astar)
                        (setf (node-score child) (+ (node-score curNode) (scoring (node-state child))))
                        (setf OPEN (append OPEN (list child)))
                    )

                    ; error handling for incorrect usage
                    (t (format t "SEARCH: bad search type! ~s~%" type) (return nil))
                )
            )
        )
    )
)

;--------------------------------------------------------------------------

; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
(defun build-solution (node node-list OPEN type)
    (let ((finalPath) (genNodes (length node-list)) (exNodes (length OPEN))
         (disNodes (length(remove-duplicates node-list))))    
        (do
            ((path (list (node-state node))))       ; local loop var
            ((null (node-parent node)) path)         ; termination condition

            ; find the parent of the current node
            (setf node (member-state (node-parent node) node-list))

            ; add it to the path
            (setf path (cons (node-state node) path))
            ; (write path)
            ; (format t "~%")
            (setf finalPath path)
        )
	
        (printPuzzle type finalPath genNodes disNodes exNodes)
    )

)

; Member-state looks for a node on the node-list with the same state.
(defun member-state (state node-list)
    (dolist (node node-list)
        (when (equal state (node-state node)) (return node))
    )
)

; generates the goal state for the given puzzle size
(defun generateGoalState (puzSize)
"
  (generateGoalState puzSize): genetates the goal state based on puzzle size.

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

(defun goal-state? (state)
"
  (goal-state? state): Returns true if goal-state was reached.
"
    (equal state *goalState*)
)
