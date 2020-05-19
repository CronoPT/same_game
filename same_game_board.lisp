;(in-package :user)

(load "procura.lisp") 



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
;; Prints the board + extra line
;;
(defun print_boardln (board)
    (print_board board)
    (terpri)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Prints a list of boards
;;
(defun print_list_board (list)
    (loop for board in list do
        (print_board board)
        (terpri)
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

;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create copy of board. Can't use copy-list because it will point to the 
;; same columns
;;
(defun copy_board (board)
    (let ((board2 (list 
              (copy-list (first board))))); copies the colum and puts it inside a list
          (loop for column in (cdr board) do
            (nconc board2 (list (copy-list column))))
          board2
    )
)

;************************************************************************
;*                  BOARD - CLUSTER OPERATIONS OPERATIONS               *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Recursively removes adjancent pieces of the same color, clusters, 
;; from the board
;;  
(defun remove_cluster (board l c) 
    (let(   (lines   (length board))
            (columns (length (first board)))
            (color (get_pos board l c)) 
            (pos_changed (list  (list l c))))
        (set_pos board l c nil)
        ;Propagate changes
        (if (< l (- lines 1)) ;are we before the down limit?
            (if (eq color (get_pos board (+ l 1) c))
                (setf pos_changed (append pos_changed (remove_cluster board (+ l 1) c)))
            )
        )
        (if (> l 0) ;are we begore the upper limit?
            (if (eq color (get_pos board (- l 1) c))
                (setf pos_changed (append pos_changed (remove_cluster board  (- l 1) c)))
            )
        )
        (if (< c (- columns 1)) ;are we before the right limit?
            (if (eq color (get_pos board l (+ c 1)))
                (setf pos_changed (append pos_changed (remove_cluster board l (+ c 1))))
            )
        )
        (if (> c 0) ;are we before the left limit?
            (if (eq color (get_pos board l (- c 1)))
                (setf pos_changed (append pos_changed (remove_cluster board  l (- c 1))))
            )
        )
        pos_changed
    )
)

;************************************************************************
;*                   BOARD - GRAVITY DOWN OPERATIONS                    *
;************************************************************************
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

(defun compare (pos1 pos2)
    (< (first pos1) (first pos2))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Applies down gravity to the board, i.e. all pieces that do not have
;; pieces bellow will fall
;;
(defun apply_gravity_down (board pos_changed)
    (setf pos_changed (sort pos_changed 'compare)) ;ordena por linhas
    (loop for position in pos_changed do
        (bring_down_from board position)
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
;*        BOARD - Generate Possible Actions Auxiliary Functions         *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This function will tell if we can play in position (l, c). In this
;; game we have to remove at least 2 pieces at a time
;;
(defun can_play_here (board l c)
    (let(   (lines   (length board))
            (columns (length (first board)))
            (color (get_pos board l c)))
        (if (< l (- lines 1)) ;are we before the down limit?
            (if (eq color (get_pos board (+ l 1) c))
                (return-from can_play_here T)))
        (if (> l 0) ;are we begore the upper limit?
            (if (eq color (get_pos board (- l 1) c))
                (return-from can_play_here T)))
        (if (< c (- columns 1)) ;are we before the right limit?
            (if (eq color (get_pos board l (+ c 1)))
                (return-from can_play_here T)))
        (if (> c 0) ;are we before the left limit?
            (if (eq color (get_pos board l (- c 1)))
                (return-from can_play_here T)))
        nil
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This function will return a list of positions. Each position
;; corresponds to a color cluster. 
;; The board_copy is a copy (it will be changed) of the board which actions
;; we want to find out.
;;
(defun add_action (l c board_copy possible_actions)
    (if (not (null (get_pos board_copy l c)))
        (if (can_play_here board_copy l c) 
            (progn
                (if (null possible_actions)
                    (setf possible_actions (append possible_actions (list (list l c)) ))
                    (nconc possible_actions (list (list l c)))
                )
                (remove_cluster board_copy l c)
            )
        )
    )
    possible_actions
)

;************************************************************************
;*                       BOARD - MAIN OPERATIONS                        *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A state in our problem corresponds to a board and the score of all 
;; the playes doen up until now
;;
(defstruct state
    board
    score
)

(defun print_state (state)
    (let ((board (state-board state))
          (score (state-score state)))
        (format t "Score:~d" score)
        (fresh-line)
        (print_boardln board)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Score a move that removed n pieces from the board
;;
(defun score_move (n)
    (* (- n 2) (- n 2))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Apply the action resulting of tapping position on the board and 
;; return the score corresponding to that move
;;
(defun do_action (board position)
    (let (  (pos_changed (remove_cluster board (first position) (second position))))
        (apply_gravity_down board pos_changed)
        (apply_gravity_left board)
        (score_move (list-length pos_changed))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Applies an action to the board in state and generates a new state
;; based on the move, computing the new board and the new score
;;
(defun do_action_clone (state position)
    (let* ((board (state-board state))
           (score (state-score state))
           (new_board (copy_board board))
           (new_score (+ (do_action new_board position) score)))
        
        (make-state :board new_board :score new_score)
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RETURNS a list with actions ((l,c) (l,c)...). One actions per cluster
;; ? shouldn't I just go until the first (l,0)==null in columns?
;;
(defun generate_possible_actions (board)
    (let  ((possible_actions '())
           (lines (length board))
           (columns (length (first board)))
           (board_clone (copy_board board))
          )
          (loop for l from 0 to (- lines 1) do 
            (loop for c from 0 to (- columns 1) do
              (setf possible_actions
                (add_action l c board_clone possible_actions)))
          )
          possible_actions
    )   
)

(defun generate_successors (state)
    (let ((successors '()) 
          (board (state-board state)))
      (dolist (action (generate_possible_actions  board))
        (if (null successors)
            (setf  successors (list (do_action_clone state action)))
            (nconc successors (list (do_action_clone state action)))
        )
      )
      successors
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ? Will we have to see the best position here in terms of score
;;
;; (defun is_it_goal (state)
;;     (let ((board (state-board state)))
;;         (loop for l in board do
;;         (loop for c in l do
;;             (if (not (null c))
;;                 (return-from is_it_goal nil)))))
;;     T
;; )
(defun is_it_goal (state)
    (> (state-score state) 150)
)


(defun h1 (state)
    (list-length (generate_possible_actions (state-board state)))
)

;************************************************************************
;*                            MAIN                                      *
;************************************************************************
;; 10x4 board with 3 colors
;; (defvar b1 '((2 1 3 2 3 3 2 3 3 3) 
;;              (1 3 2 2 1 3 3 2 2 2) 
;;              (1 3 1 3 2 2 2 1 2 1) 
;;              (1 3 3 3 1 3 1 1 1 3)))

;; ;; 10x4 board with 5 colors 
;; (defvar b2 '((4 3 3 1 2 5 1 2 1 5) 
;;              (2 4 4 4 1 5 2 4 1 2)
;;              (5 2 4 1 4 5 1 2 5 4)
;;              (1 3 1 4 2 5 2 5 4 5)))

;; 15x10 board with 3 colors
;; (defvar b3 '((3 3 3 2 1 2 3 1 3 1)
;;              (1 1 2 3 3 1 1 1 3 1)
;;              (3 3 1 2 1 1 3 2 1 1)
;;              (3 3 2 3 3 1 3 3 2 2)
;;              (3 2 2 2 3 3 2 1 2 2)
;;              (3 1 2 2 2 2 1 2 1 3)
;;              (2 3 2 1 2 1 1 2 2 1)
;;              (2 2 3 1 1 1 3 2 1 3)
;;              (1 3 3 1 1 2 3 1 3 1) 
;;              (2 1 2 2 1 3 1 1 2 3)
;;              (2 1 1 3 3 3 1 2 3 1)
;;              (1 2 1 1 3 2 2 1 2 2)
;;              (2 1 3 2 1 2 1 3 2 3)
;;              (1 2 1 3 1 2 2 3 2 3)
;;              (3 3 1 2 3 1 1 2 3 1)))

;; 15x10 board with 5 colors
(defvar boardinho '((5 1 1 1 2 1 4 2 1 2)
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

;; (do_action b1 '(1 0))
;; (print_board b1)
;; (terpri)
;; (do_action b1 '(3 1))
;; (print_board b1)
;; (terpri)
;; (do_action b1 '(3 1))
;; (print_board b1)
;; (terpri)
;; (do_action b1 '(3 1))
;; (print_board b1)
;; (terpri)
;; (do_action b1 '(3 1))
;; (print_board b1)
;; (terpri)
;; (do_action b1 '(3 1))
;; (print_board b1)
;; (terpri)
;; (do_action b1 '(3 1))
;; (print_board b1)
;; (terpri)
;; (do_action b1 '(3 1))
;; (print_board b1)
;; (terpri)
;; (do_action b1 '(3 1))
;; (print_board b1)
;; (terpri)

;; (do_action b2 '(0 1))
;; (print_board b2)
;; (terpri)

;; (do_action b3 '(12 9))
;; (print_board b3)
;; (terpri)

;; (do_action b4 '(14 3))
;; (print_board b4)
;; (terpri)




;! SOLVING PROBLEM WITH BFS EXAMPLE

;; (defvar boardinho'((1 2 2 3 3) 
;;                    (2 2 2 1 3) 
;;                    (1 2 2 2 2) 
;;                    (1 1 1 1 1)))
                   
(defvar initial_state (make-state :board boardinho :score 0))

(defvar problema (cria-problema initial_state '(generate_successors) :objectivo? #'is_it_goal :heuristica #'h1))

(print "SPAM BEGINS")
(defvar A (procura problema 'a*))
(terpri)
(print "RESULTS")
(terpri)
(loop for state in (first A) do
    (print_state state)
)

