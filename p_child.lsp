#|
    This function will take a list representation of a 
    n-size puzzle, and return a list of up to four 
    possible states that can be generated from its
    current state.
|#

(defun puzzle_children (puzzle)
    (let
        (
            ; "Find the length of the puzzle"
            (width (sqrt (list-length puzzle)))
            ;"Let's find the 0 position"
            (pos (position 0 puzzle))
            (localPos (mod (1+ pos) width)) 
            ; "Define a container to return"
            (movUp (copy-list puzzle))
            (movDn (copy-list puzzle))
            (movLf (copy-list puzzle))
            (movRt (copy-list puzzle))
            (children '())
        )

        ; "Let's test moves up and down"
        (when (> -1 (- pos width) )
            (rotatef (nth pos movUp) (nth (- pos width) movUp))
            (push movUp children)
        )

        (when (< (width * width) (+ pos width))
            (rotatef (nth movDn pos movDn) (nth (+ pos width) movDn))
            (push movDn children)
        )

        ; "Let's test moves left and right"
        (when (> -1 (- localPos width))
            (rotatef (nth pos movLf) (nth (1- pos) movLf))
            (push movLf children)
        )

        (when (< width (+ localPos width))
            (rotatef (nth pos movRt) (nth (1+ pos) movRt))
            (push movRt children)
        )

    
    )
    children
)