#| 
  8puzzle.lsp

  This gets the user input from either the command line or a call to the
  8puzzle function in lisp. It accepts input as a file or as user entered
  numbers. The numbers are put into a global list and checked to see if the 
  correct number of numbers was inputed and if they are valid numbers. Then 
  the search functions are called to solve the puzzle.

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


(defun randomPuzzle (n)
"
  (randomPuzzle n): creates a random n-sized puzzle. The range is from 
  0 to n^2 - 1 with no repeats. 

"
  (let ((i 0) (elem) (puzSize) (lst))
    (setf lst nil)
    (setf puzSize (* n n)) 
    (dotimes (i puzSize)
      (setf elem (random puzSize))
      ; find returns nil if not in list
      (if (not (find elem lst))
        ; if not in list
        (push elem lst)
        ; else get new number
        (progn
          ; loop through adding 1 each time to find num not already in list 
          (loop while (eq nil (not (find elem lst))) do
            (if (= elem (1- puzSize))
              (setf elem 0)
              (setf elem (1+ elem))
            )
          )
          ; push to list
          (push elem lst)
        )
      )
    )
    ; checks if puzzle is solvable
    (if (not (solvable lst))
      (randomPuzzle n)
      ; sets the puzzle
      (setf *puzzle* lst)
    )
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
      (astar_hamming *puzzle*) ; Admissable Heuristic - hamming distance
      (astar_manhat *puzzle*) ; Admissable Heuristic - manhattan distance
      (astar_nilsson *puzzle*) ; Inadmissable Heuristic - Nilsson's sequence score

    )
    ; error message and exits program
    (format t "Puzzle is unsolvable. Exiting Program~%")
  )
)

; checks if user wants to enter puzzle manually or have a puzzle randomly generated
(defun puzzleType ()
"
  (puzzleType): Asks user to either enter a puzzle or give a row size to randomly generate a puzzle.
"
  ; prompt for entering puzzle or randomly generating puzzle based on num rows
  (princ "To manually enter a puzzle enter 1. To have a puzzle randomly 
    generated, enter 2: ")

  (let ((num) (rows))
    (setf num (read))

    (cond 
      ; checks if user entered 1
      ((= num 1 )
        (userInput)
      )

      ((= num 2)
        ; checks for 2 and asks for number of rows in puzzle
        (princ "Enter number of rows (please enter a number greater than 2): ")
        (setf rows (read))
        ; creates random puzzle
        (randomPuzzle rows)
        ; prints puzzle for user to see
        (format t "Randomly Generated Puzzle: ~a~%" *puzzle*)
      )
      ; error checking
      (t (progn (format t "Invalid Option. Please Try Again~%") (puzzleType)))
    )
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
    (puzzleType)
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
