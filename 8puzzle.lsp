#| 
  8puzzle.lsp

  This gets the user input from either the command line or a call to the
  8puzzle function in lisp. It accepts input as a file or as user entered
  numbers. The numbers are put into a global list and checked to see if the 
  correct number of numbers was inputed and if they are valid numbers. 

  Class: CSC 447 - Artificial Intelligence
  Authors: 
  Due Date: March 27, 2016

|#

(load 'search.lsp)
(load 'solvable.lsp)

; global variable for the puzzle
(setf *puzzle* nil)

; takes the arguments entered by the user and uses the appropiate action
(defun readArgs (L)
"
  (readArgs L): Reads in the puzzle from the user and pushes to the *puzzle* list. Also checks for valid number of puzzle elements.

"
  (setf num (length L))
  (if (and (= 0 (mod (sqrt num) 1)) (> num 4))
      ; pushes input to list as integer
      (dolist (x L) (push x *puzzle*))
      ; default/error checking
      (format t"Invalid number of arguments. Exiting Program~%")
  )
    ; reverse list so they are in the order that they were read in as
    (setf *puzzle* (reverse *puzzle*))
    ; (write *puzzle*)
)

; opens input file and stores the values
(defun openFile (filename)

 "
  (openFile filename): Checks for valid file. Reads in file to *puzzle*
  variable.

"
  ; open file and check for valid file
  (setf fin (open filename :if-does-not-exist nil))
  (when (null fin) (return-from openFile (format t "Error: cannot open file ~a " filename)))
  ; read in the data from the file
  (do ((data (read fin nil) (read fin nil)))
    ((null data) (close fin))
    ; add to the global variable
    (push data *puzzle*)
  )  
  ; reverse puzzle so it is in the order that was entered
  (setf *puzzle* (reverse *puzzle*))
)

; reads in the list entered by the user
(defun userInput ()
"
  (userInput): Prompts user for puzzle. Checks for valid puzzle length and
  reads in puzzle to *puzzle* variable.

"
  ; print prompt to have user enter digits
  (princ "Enter puzzle in row-major order with each number seperated by white space (press enter when complete): " )
  ; read in everyting that was entered by the user
  (let ((vals) (num nil) (numList nil) (input (read-line)))
    (loop for x across input do (push (string x) vals))
    (dolist (y vals)
      ; checks for spaces and ignores thems
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
    ; set the list to puzzle
    (setf *puzzle* numList)
  )
)

; check puzzle function to check for values in correct range
(defun checkPuzzle (puzSize)
"
  (checkPuzzle puzSize): Returns true if valid puzzle entries. 

"

  (dolist (x *puzzle*)
    ; checks for number between 0 and N^2-1
    (if (or (< x 0) (> x (- puzSize 1))) 
      ; returns false
      (return-from checkPuzzle nil)
    )
  )
  ; returns true
  (return-from checkPuzzle t)
)

(defun startSearch ()
  "
  (startSearch): Checks for solvable puzzle and if it is,
  calls each search. 

"

  ; checks if puzzle is solvable
  (if (solvable *puzzle*)
    ; calls the searches
    ( progn 
      (bfs *puzzle*)
      (idfs *puzzle*)
      (astar *puzzle*) 
    )
    ; error message and exits program
    (format t "Puzzle is unsolvable. Exiting Program~%")
  )
)

; function that is called in lisp to get puzzle
(defun 8puzzle (&rest puzzleInput)
"
  (8puzzle puzzleInput): Makes call to proper function based on input
  entered by the user. 

"
  ; checks for no puzzle given in clisp and prompts for puzzle
  (if (= (length puzzleInput) 0)
    (userInput)
    ; reads in input from the user
    (readArgs puzzleInput)
  )

  ; checks for valid puzzle entered by user
  (if (checkPuzzle (length *puzzle*))
    ; calls to run searches
    (startSearch)
    (format t "Puzzle input incorrect. Enter numbers between 0 and N^2 - 1 (N is the number of rows in the puzzle). Exiting Program")
  )
)

; check for one argument which should be filename and calls to open file
(if (= (length *args*) 1) 
  (progn (openFile (car *args*)) (startSearch))
  (format t "Command-line usage : clisp 8puzzle.lsp puzzlefile~%Usage inside CLISP: (8puzzle [puzzlelist])")
)