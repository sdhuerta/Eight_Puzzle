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
    ((= num 9) 
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
  (princ "Enter digits 0 - 8, seperated by white space, in any order (no repeats): " )

  ; read in 9 values
  (setf i 0)
  (dotimes (i 9 t)
    (setf input (read))
    (if (numberp input) 
      ; if true do this
      (push input puzzle) 
      ; else do this 
      ; progn allows more than one s-expression to berun
      (progn (format t "Invalid input, please try again.~%") (userInput))
      )
  ) 

  ; still need check for valid numbers
)

; call the function to read the arguments
(readArgs *args*)