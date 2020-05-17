;(in-package :user)


;************************************************************************
;*                  BOARD - AUXILIARY OPERATIONS                        *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Prints a board line, nill positions are displayed with a '_'
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Prints the board
;;
(defun print_board (board)
    (if (not (null board))
        (progn
            (print_line  (first board))
            (print_board (rest board))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set the content in the board at position (l, c) with val
;;
(defun set_pos (lst l c val)
    (setf (nth c (nth l lst)) val)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get the content in the board at position (l, c)
;;
(defun get_pos (lst l c)
    (nth c (nth l lst))
)

;************************************************************************
;*                   BOARD - GRAVITY DOWN OPERATIONS                    *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Recursively removes adjancent pieces of the same color from the board
;;
(defun do_action_tail (board l c) 
    (let(   (lines   (length board))
            (columns (length (first board)))
            (color (get_pos board l c))
            (pos_changed (list  (list l c))))
        (set_pos board l c nil)
        ;Propagate changes
        (if (< l (- lines 1)) ;are we before the down limit?
            (if (eq color (get_pos board (+ l 1) c )) 
                (setq pos_changed (append pos_changed (do_action_tail board (+ l 1) c)))
            )
        )
        (if (< 0 l) ;are we after the upper limit?
            (if (eq color (get_pos board (- l 1) c))
                (setq pos_changed (append pos_changed (do_action_tail board  (- l 1) c)))
            )
        )
        (if (< c (- columns 1)) ;are we befoe the right limit?
            (if (eq color (get_pos board l (+ c 1)))
                (setq pos_changed (append pos_changed (do_action_tail board l (+ c 1))))
            )
        )
        (if (< 0 c)
            (if (eq color (get_pos board l (- c 1)))
                (setq pos_changed (append pos_changed (do_action_tail board  l (- c 1))))
            )
        )
        pos_changed
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Shifts down all positions in a given column starting at pos_changed
;;
(defun bring_down_from (board pos_changed)
    (let(   (l (first pos_changed))
            (c (second pos_changed)))
        (if (< 0 l) ;recursively call this until we brought everything down
            (progn
                (set_pos board l c (get_pos board (- l 1) c))   
                (set_pos board (- l 1) c nil)
                (bring_down_from board (list (- l 1) c))
            )
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Applies down gravity to the board, i.e. all pieces that do not have
;; pieces bellow will fall
;;
(defun apply_gravity_down (board pos_changed)
    (sort pos_changed (lambda (pos1 pos2) (< (first pos1) (first pos2)))) ;ordena por linhas
    (if (not (null pos_changed))   
        (progn
            (bring_down_from    board (first pos_changed)) ;bring first down
            (apply_gravity_down board (rest  pos_changed)) ;bring the rest down
        )
    )
    board
)


;************************************************************************
;*                   BOARD - GRAVITY LEFT OPERATIONS                    *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This function collects a list with all the colums that are not null 
;; It is recursive and it appends those columns in fullcols
;; c is the current column the function is looking at. If it is empty, we pass. 
;; If it's not, we add.
;;
;; If we want to collect all nonempty columns: (collect_full_cols board '()' 0) 
;;
(defun collect_full_cols (board full_cols c) 
    (let(   (lines (length board)) 
            (columns (length (first board))))
        (if (eq c columns)
            full_cols
            (if (not (null (get_pos board (- lines 1) c)))
                (collect_full_cols board (append full_cols (list c) ) (+ c 1))
                (collect_full_cols board full_cols (+ c 1))
            )
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; It moves column number init_col to dest_col.
;; in the end, init_col is full of nils
;; line is just an auxiliary variable to remember which line we are.
;; if we want to move colum 2 to position 1: (move_col_aux board 1 2 0)
;;
(defun move_col_aux(board dest_col init_col line)
    (let(   (lines (length board)))
        (if (not (eq line lines))
            (progn 
                (set_pos board line dest_col (get_pos board line init_col))
                (set_pos board line init_col nil)
                (move_col_aux board dest_col init_col (+ line 1))
            )
        )
    )
    ;)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Moves the column numbered with init_col dest_col
;;
(defun move_col (board dest_col init_col)
    (move_col_aux board dest_col init_col 0)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; We iterate over the columns (recursively using current_col to store the current one)
;; when we find a nil column we go to our list of full columns and swap it to our current empty column
;; we now remove the just transplanted full colum.
;;
(defun apply_gravity_left_aux (board full_cols_list current_col)
    (if (null full_cols_list)
        board
        (let
            ((lines (length board)))
            (progn
                (if (null (get_pos board (- lines 1) current_col))
                    (move_col board current_col (car full_cols_list))
                )
                (apply_gravity_left_aux board (cdr full_cols_list) (+ current_col 1))
            )
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Applies left gravity to the board, i.e. compact all columns to the
;; left og the board
;;
(defun apply_gravity_left (board)
    (apply_gravity_left_aux board (collect_full_cols board '()' 0) 0)
)

;************************************************************************
;*                       BOARD - DO_ACTION                              *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Apply the action resulting of tapping position to the board
;;
(defun do_action (board position)
    (let (  (pos_changed (do_action_tail board (first position) (second position))))
        (apply_gravity_down board pos_changed)
        (apply_gravity_left board)
    )
    (fresh-line)
    (print_board board)
)



;************************************************************************
;*                            MAIN                                      *
;************************************************************************
;; 10x4 board with 3 colors
(defvar b1 '((2 1 3 2 3 3 2 3 3 3) 
             (1 3 2 2 1 3 3 2 2 2) 
             (1 3 1 3 2 2 2 1 2 1) 
             (1 3 3 3 1 3 1 1 1 3)))

;; 10x4 board with 5 colors 
(defvar b2 '((4 3 3 1 2 5 1 2 1 5) 
             (2 4 4 4 1 5 2 4 1 2)
             (5 2 4 1 4 5 1 2 5 4)
             (1 3 1 4 2 5 2 5 4 5)))

;; 15x10 board with 3 colors
(defvar b3 '((3 3 3 2 1 2 3 1 3 1)
             (1 1 2 3 3 1 1 1 3 1)
             (3 3 1 2 1 1 3 2 1 1)
             (3 3 2 3 3 1 3 3 2 2)
             (3 2 2 2 3 3 2 1 2 2)
             (3 1 2 2 2 2 1 2 1 3)
             (2 3 2 1 2 1 1 2 2 1)
             (2 2 3 1 1 1 3 2 1 3)
             (1 3 3 1 1 2 3 1 3 1) 
             (2 1 2 2 1 3 1 1 2 3)
             (2 1 1 3 3 3 1 2 3 1)
             (1 2 1 1 3 2 2 1 2 2)
             (2 1 3 2 1 2 1 3 2 3)
             (1 2 1 3 1 2 2 3 2 3)
             (3 3 1 2 3 1 1 2 3 1)))

;; 15x10 board with 5 colors
(defvar b4 '((5 1 1 1 2 1 4 2 1 2)
             (5 5 5 4 1 2 2 1 4 5)
             (5 5 3 5 5 3 1 5 4 3)
             (3 3 3 2 4 3 1 3 5 1)
             (5 3 4 2 2 2 2 1 3 1)
             (1 1 5 3 1 1 2 5 5 5)
             (4 2 5 1 4 5 4 1 1 1)
             (5 3 5 3 3 3 3 4 2 2)
             (2 3 3 2 5 4 3 4 4 4)
             (3 5 5 2 2 5 2 2 4 2)
             (1 4 2 3 2 4 5 5 4 2)
             (4 1 3 2 4 3 4 4 3 1)
             (3 1 3 4 4 1 5 1 5 4) 
             (1 3 1 5 2 4 4 3 3 2)
             (4 2 4 2 2 5 3 1 2 1)))

(do_action b1 '(1 0))
(print_board b1)
(terpri)

(do_action b2 '(0 1))
(print_board b2)
(terpri)

(do_action b3 '(12 9))
(print_board b3)
(terpri)

(do_action b4 '(14 3))
(print_board b4)
(terpri)
