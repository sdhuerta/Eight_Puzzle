; main file for 8puzzle program

; global variable for the puzzle
(setf puzzle nil)

; takes the arguments entered by the user and uses the appropiate action
(defun readArgs (L)

  (setf num (length L))
  ; (if (= 0 num) (write "No args listed"))
  (cond 
    ; check for no arguments listed
    ; ask for digits and read them in
    ((= num 0) (userInput))
    ; check for file name
    ; then read the file and store the digits in list called puzzle
    ((= num 1) (openFile (car L)))

    ; check for list
    ; store the digits
    ((and (= 0 (mod (sqrt num) 1)) (> num 4)) 
      ; pushes input to list as integer
      (dolist (x L) (push (parse-integer x) puzzle))
    )
    ; default/error checking
    (t (format t"Invalid number of arguments. Exiting Program~%"))
  )

    ; reverse list so they are in the order that they were read in as
    (setf puzzle (reverse puzzle))
    (write puzzle)
)

; opens input file and stores the values
(defun openFile (filename)
  ; open file and check for valid file
  (setf fin (open filename :if-does-not-exist nil))
  (when (null fin) (return-from fileio (format nil "Error: cannot open file ~a " filename)))
  ; read in the data from the file
  (do ((data (read fin nil) (read fin nil)))
    ((null data) (close fin))
    ; add to the global variable
    (push data puzzle)
  )  
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
        (progn (push num numList) (setf num nil)) 
      )
    )
    ; push final number to the list
    (push num numList)

    ; checks for correct number of inputs
    (if (not (and (= 0 (mod (sqrt (length numList)) 1)) (> (length numList) 4)))
      (progn (format t "Invalid number of puzzle elements. Please try again~%") (userInput))   
    )

    (setf puzzle (reverse numList))
  )

)

; check puzzle function to check for correct values ie range and integers
; start working on A* alogorithm to solve puzzle


; call the function to read the arguments
(readArgs *args*)