;;;; AUXILIARY FUNCTIONS

(defun print_line (line)
    (if (not (null line))
        (progn
            (if (not (null (first line)))
                (format t "~d " (first line))
                (format t "~d " "_")
            )
            (print_line (rest line))
        )
        (fresh-line)
    )
)

(defun print_board (board)
    (if (not (null board))
        (progn
            (print_line  (first board))
            (print_board (rest board))
        )
    )
)


(defun set_pos (lst l c val)
    (setf (nth c (nth l lst)) val)
)


(defun get_pos (lst l c)
    (nth c (nth l lst))
)



;;;;;; 
; ALTERNATIVE TO COMPACT LEFT
;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This function collects a list with all the colums that are not null 
;;It is recursive and it appends those columns in fullcols
;;c is the current column the function is looking at. If it is empty, we pass. If it's not, we add.
;;
;;if we want to collect all nonempty columns: (collect_full_cols board '()' 0) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun collect_full_cols (board fullcols c) 
    (let
        ((lines (length board)) 
        (columns (length (first board))))
        (if (eq c columns)
            fullcols
            (if (not (null (get_pos board (- lines 1) c)))
                (progn
                    (collect_full_cols board (append fullcols (list c) ) (+ c 1))
                    
                )
                (collect_full_cols board fullcols (+ c 1)))
        )
    )
)


;;;;;;;;;;;;;;;;;
;;it move column number initCol to destCol.
;;in the end, initCol is full of nils
;;line is just an auxiliary variable to remember which line we are.
;; if we want to move colum 2 to position 1: (move_col_aux board 1 2 0)
;;;;;;;;;;;;;;;;;
(defun move_col_aux(board destCol initCol line)
    ;(progn
    ;(format t "~%")
    (let
        ((lines (length board)))
        (if (not (eq line lines))
            (progn 
                ;(format t "got it ~%")
                (set_pos board line destCol (get_pos board line initCol))
                (set_pos board line initCol nil)
                ;print_board board)
                (move_col_aux board destCol initCol (+ line 1))
            )
        )
    )
    ;)
)

;;;;;;;;;;;;
;;The as above, but cleaner and starting from line 0;;
;;;;;;;;;;
(defun move_col (board destCol initCol)
    (move_col_aux board destCol initCol 0)
)

;;;;;;;;;;;;
;;We iterate over the columns (recursively using currentCol to store the current one)
;;when we find a nil column we go to our list of full columns and swap it to our current empty column
;;we now remove the just transplanted full colum.
;;;;;;;;;;;;

(defun compact_board_horizontally_aux (board fullColsList currentCol)
    (if (null fullColsList)
        board
        (let
            ((lines (length board)))
            (progn
                (if (null (get_pos board (- lines 1) currentCol))
                    (progn
                        (move_col board currentCol (car fullColsList))
                    )
                )
                (compact_board_horizontally_aux board (cdr fullColsList) (+ currentCol 1))
            )
        )
    )
)


;;;;;;
;;The same as before, but pretty!
;;;;;;;;
( defun compact_board_horizontally (board)
    (compact_board_horizontally_aux board (collect_full_cols board '()' 0) 0)
)




(defvar b1 '((1 2 nil 3 3) (2 2 nil 1 3) (1 2 nil 2 2) (1 1 nil 1 1))
)

(defvar b2 '((1 2 nil nil nil 3 4 nil 5 ) (1 2 nil nil nil 3 4 nil 5 ) (1 2 nil nil nil 3 4 nil 5 ) (1 2 nil nil nil 3 4 nil 5 ))
)

(defvar b3'((1 2 nil nil nil 3 4 nil nil ) (1 2 nil nil nil 3 4 nil nil ) (1 2 nil nil nil 3 4 nil nil ) (1 2 nil nil nil 3 4 nil nil ))
)

(defvar b4'((nil 2 nil nil nil 3 4 nil nil ) (nil 2 nil nil nil 3 4 nil nil ) (nil 2 nil nil nil 3 4 nil nil ) (nil 2 nil nil nil 3 4 nil nil ))
)


(print_board (compact_board_horizontally b4))