; main file for 8puzzle program

; global variable for the puzzle
(setf *puzzle* nil)

; takes the arguments entered by the user and uses the appropiate action
(defun readArgs (L)

  (setf num (length L))
  ; (if (= 0 num) (write "No args listed"))
  (if (and (= 0 (mod (sqrt num) 1)) (> num 4))
      ; pushes input to list as integer
      (dolist (x L) (push x *puzzle*))
      ; default/error checking
      (t (format t"Invalid number of arguments. Exiting Program~%"))
  )
    ; reverse list so they are in the order that they were read in as
    (setf *puzzle* (reverse *puzzle*))
    ; (write *puzzle*)
)

; opens input file and stores the values
(defun openFile (filename)
  ; open file and check for valid file
  (setf fin (open filename :if-does-not-exist nil))
  (when (null fin) (return-from openFile (format t "Error: cannot open file ~a " filename)))
  ; read in the data from the file
  (do ((data (read fin nil) (read fin nil)))
    ((null data) (close fin))
    ; add to the global variable
    (push data *puzzle*)
  )  

  (setf *puzzle* (reverse *puzzle*))
)

; reads in the list entered by the user
(defun userInput ()

  ; print prompt to have user enter digits
  (princ "Enter puzzle in row-major order with each number seperated by white space (press enter when complete): " )
  ; read in everyting that was entered by the user
  (let ((vals) (num nil) (numList nil) (input (read-line)))
    (loop for x across input do (push (string x) vals))
    (dolist (y vals)
      (if (not (string= #\space y))
        (setf num (concatenate 'string y num))
        (progn (push (parse-integer num) numList) (setf num nil)) 
      )
    )
    ; push final number to the list
    (push (parse-integer num) numList)

    ; checks for correct number of inputs
    (if (not (and (= 0 (mod (sqrt (length numList)) 1)) (> (length numList) 4)))
      (progn (format t "Invalid number of puzzle elements. Please try again~%") (userInput))   
    )
    (setf *puzzle* numList)
  )
)

; check puzzle function to check for values in correct range
(defun checkPuzzle (puzSize)

  (dolist (x *puzzle*)
    ; (write x)
    (if (or (< x 0) (> x (- puzSize 1))) 
      (format t "Puzzle input incorrect. Enter numbers between 0 and N^2 - 1 (N is the number of rows in the puzzle). Exiting Program")
    )
  )
)

; function that is called in lisp to get puzzle
(defun 8puzzle (&rest puzzleInput)

  (if (= (length puzzleInput) 0)
    (userInput)
    (readArgs puzzleInput)
  )

  (checkPuzzle (length *puzzle*))
)

; check for one argument which should be filename
(if (= (length *args*) 1) 
  (openFile (car *args*))
  (format t "Command-line usage : clisp 8puzzle.lsp puzzlefile~%Usage inside CLISP: (8puzzle [puzzlelist])")
)