(in-package :user)
(load "procura")

(defvar *nos-gerados*)
(defvar *nos-expandidos*)
(defvar *lisp-implementation*)

;************************************************************************
;*                       TIME AND MEMORY HELPERS                        *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up randomness in Lisp
;;
(setf *random-state* (make-random-state t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Limit execution time of our program in seconds
;;
(defvar *time_limit_seconds* 298)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Serves as reference to measure execution times of algorithms
;;
(defvar *initial_time* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Limit heap usage of our program in megabytes
;; ? The limit is actually 256 MB sould we leave this 2MB padding
;;
(defvar *memory_limit_megabytes* 150)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get the ammount of seconds elapsed since program started
;;
(defun get_elapsed_seconds ()
    (/ (get-internal-run-time) internal-time-units-per-second)
)

(defun determine_implementation() 
    (let(   (room_str (with-output-to-string (*standard-output*) (room nil)))
            (sbcl_first_word "Dynamic")
            (first_word nil)
            (string_word))
        
        (setf first_word (loop for char across room_str until (eql char #\space) 
                            collect char))
        
        (setf string_word (make-string (length first_word)))
        (loop for i from 0 to (- (length first_word) 1)  do
            (setf (aref string_word i) (nth i first_word)))

        (setf string_word (string-trim '(#\space #\newline) string_word))

        (if (string-equal string_word sbcl_first_word)
            1
            2
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; For steel bank common lisp
;;
(defun get_megabytes_used_sbcl ()
    (let ((room_str (with-output-to-string (*standard-output*) (room nil)))
          (mb_bytes 0))
        (loop for char across room_str until (eql char #\newline) do
            (when (digit-char-p char)
                (setf mb_bytes (+ (* 10 mb_bytes) (digit-char-p char))))
        )
        (floor mb_bytes 1000000)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; For Clisp
;;
(defun get_megabytes_used_clisp ()
    (let ((room_str (with-output-to-string (*standard-output*) (room nil)))
          (mb_bytes 0)
          (counter  0))
        (loop for char across room_str do
            (when (digit-char-p char)
                (setf mb_bytes (+ (* 10 mb_bytes) (digit-char-p char))))
            (when (eql char #\newline)
                (incf counter 1)
                (if (eq 3 counter)
                    (return-from get_megabytes_used_clisp (floor mb_bytes 1000000))
                    (setf mb_bytes 0)
                )
            )
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get used heap memory in megabytes - what i do is get the output
;; of room into a string and read the first number that appears there
;; which i think it's the number we want
;;
(defun get_megabytes_used ()
    (if (eql 1 *lisp-implementation*)
        (get_megabytes_used_sbcl)   ; 1 is for sbcl
        (get_megabytes_used_clisp)) ; 2 is for clisp
)


;****************************
;*       STRUCTURES                             
;****************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A different node that allows us to keep track of the depth of nodes
;;
(defstruct (node_depth (:include no))
    depth
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A different node that allows us to keep track of discrepancies of the
;; path of a given nodde
;;
(defstruct (node_discrepancy (:include no))
    discrepancies
    heuristic
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A state in our problem corresponds to a board and the score of all 
;; the plays done up until now as well as the move that originated the 
;; state
;;
(defstruct state
    board
    score
    move
)

;************************************************************************
;*                      STACK STRUCT - USED IN DFS                      *
;************************************************************************
(defstruct stack
    elements    
)

(defun stack_create ()
    (make-stack :elements '())
)

(defun stack_push (stack element)
    (if (listp element)
        (setf (stack-elements stack) (append element (stack-elements stack)))
        (setf (stack-elements stack) (append (list element) (stack-elements stack)))
    )
)

(defun stack_pop (stack)
    (let(   (poped (first (stack-elements stack))))
        (setf (stack-elements stack) (rest (stack-elements stack)))
        poped
    )
)

(defun stack_empty (stack)
    (= 0 (list-length (stack-elements stack)))
)

;************************************************************************
;*                  BOARD - AUXILIARY OPERATIONS                        *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set the content in the board at position (l, c) with val
;;
(defun set_pos (lst l c val)
    (setf (aref lst l c) val)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get the content in the board at position (l, c)
;;
(defun get_pos (lst l c)
    (aref lst l c)
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
;; If all the peaces of the same cluster were in a cluster together what
;; would be the total score of playing those clusters?
;;  
(defun max_score (board)
    (let*(  (ht (make-hash-table))
            (max_score 0)
            (shape   (array-dimensions board))
            (lines   (first  shape))
            (columns (second shape))
            (piece   nil))
        (loop for l from 0 to (- lines 1) do
            (loop for c from 0 to (- columns 1) do
                (setf piece (get_pos board l c))
                (when (numberp piece) 
                    (if (null (gethash piece ht) )
                        (setf (gethash piece ht) 1)
                        (incf (gethash piece ht) 1)))

            )
        )
        (loop for key being each hash-key of ht do
            (incf max_score (score_move (gethash key ht))))
        max_score
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Allows us to build an array board from a list board
;;
(defun list-to-2d-array (list)
    (make-array (list (length list)
                      (length (first list)))
                :initial-contents list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Prints the board
;;
(defun print_board (board)
    (when (not (null board))
        (let*(  (shape (array-dimensions board))
                (lines   (first  shape))
                (columns (second shape)))
            (loop for l from 0 to (- lines 1) do
                (loop for c from 0 to (- columns 1) do
                    (if (not (null (get_pos board l c)))
                        (format t "~d " (get_pos board l c))
                        (format t "~d " "_")))
                (fresh-line))
                    
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

;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create copy of board. Can't use copy-list because it will point to the 
;; same columns
;;
(defun copy_board (board)
    (let*(  (shape (array-dimensions board))
            (lines   (first  shape))
            (columns (second shape))
            (new_board (make-array shape)))
        (loop for l from 0 to (- lines 1) do
            (loop for c from 0 to (- columns 1) do
                (set_pos new_board l c (get_pos board l c))))
        new_board)
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
    (let*(  (shape (array-dimensions board))
            (lines   (first  shape))
            (columns (second shape))
            (pos_changed (list (list l c)))
            (color (get_pos board l c)))
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
    (let*(  (l (first  pos_changed))
            (c (second pos_changed)))
        (when (< 0 l) ;recursively call this until we brought everything down
            (set_pos board l c (get_pos board (- l 1) c))   
            (set_pos board (- l 1) c nil)
            (bring_down_from board (list (- l 1) c))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A useful function to sort positions by line
;; If we do not sort positions changed by line, our algorithm does not
;; work in specific situations
;;
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
    (let*(  (shape (array-dimensions board))
            (lines   (first  shape)) 
            (columns (second shape)))
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
    (let*(  (shape (array-dimensions board))
            (lines (first shape)))
        (if (not (eq line lines))
            (progn 
                (set_pos board line dest_col (get_pos board line init_col))
                (set_pos board line init_col nil)
                (move_col_aux board dest_col init_col (+ line 1))
            )
        )
    )
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
        (let*(  (shape (array-dimensions board))
                (lines (first shape)))
            (when (null (get_pos board (- lines 1) current_col))
                (move_col board current_col (car full_cols_list)))
            (apply_gravity_left_aux board (cdr full_cols_list) (+ current_col 1))
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
    (let*(  (shape (array-dimensions board))
            (lines   (first  shape))
            (columns (second shape))
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
        (when (can_play_here board_copy l c) 
            (if (null possible_actions)
                (setf possible_actions (append possible_actions (list (list l c)) ))
                (nconc possible_actions (list (list l c))))
            (remove_cluster board_copy l c)
        )
    )
    possible_actions
)

;************************************************************************
;*                       BOARD - MAIN OPERATIONS                        *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ? Will we have to see the best position here in terms of score
;; ? Or probably will end up returning always false
;;
(defun is_it_goal ()
    (>= (- (get_elapsed_seconds) *initial_time*) *time_limit_seconds*)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; We're not interested in minimizing the lenght of the solution, we're
;; interested in maximing the score, we will maximize cost and make the
;; cost a score
;;
(defun cost_same_game (state)
    (state-score state)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Prints to the screen the board and score of a given state
;;
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
        
        (make-state :board new_board :score new_score :move position)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RETURNS a list with actions ((l,c) (l,c)...). One actions per cluster
;; ? shouldn't I just go until the first (l,0)==null in columns?
;;
(defun generate_possible_actions (board)
    (let*(  (possible_actions '())
            (shape (array-dimensions board))
            (lines   (first  shape))
            (columns (second shape))
            (board_clone (copy_board board))
          )
        (loop for l from 0 to (- lines 1) do 
            (loop for c from 0 to (- columns 1) do
                (setf possible_actions (add_action l c board_clone possible_actions))))
        possible_actions
    )   
)

;************************************************************************
;*                          FILTROS - SUCCESSORS CUT                    *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Can the state ever reach the best_score?
;;
(defun give_filter_on_best_score (best_score)
    (lambda (state)
        (<= (+ (max_score (state-board state)) (state-score state)) best_score )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global variable so we can keep track of how many successores were cut
;;
(defvar *nodes_cut* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Every state that can't possibly be as good as best score (because a 
;; all the pieces that are there are not enough to reach that threshold)
;; gets cut
;;
(defun give_filter_function (filtro)
    (lambda (successors) 
        (let (  (prev successors) (after (remove-if filtro successors)))
            (if (not (eq (length prev) (length after)))
                (incf *nodes_cut* (- (length prev) (length  after))))
            after
        )    
    )
)

;; (defun give_filter_function (filtro)
;;     (lambda (successors) 
;;         successors
;;     )
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Filters successor list by a given criteria
;;
(defun give_filter_function_on_best_score (best_score)
    (give_filter_function (give_filter_on_best_score best_score))
)

;************************************************************************
;*                       GENERATE SUCCESSORS                            *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Redifiniton from procura.lisp so we can pass a filtering function 
;; for successors
;;
(defun problema-gera-sucessores (problema estado &optional (filtro #'identity))
  (let ((sucessores nil))
    (dolist (operador (problema-operadores problema))
      (setf sucessores
	(nconc (funcall operador estado)
	       sucessores)))
    (incf *nos-expandidos*)
    (incf *nos-gerados* (length sucessores))
    
    (funcall filtro sucessores)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A useful function to sort states
;;
(defun compare_states (state1 state2)
    (> (state-score state1) (state-score state2))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   THIS IS OUR OPERATOR THAT GENERATES BOARD AND IS USED IN 
;;   problema-gera-sucessores
;;
;; Computes the possible actions and applies them, generating new states,
;; this is the only operator we have in the whole problem
;; e.g. ( BOARD1, BOARD2, BOARD3...)
;;
(defun generate_successors (state)
    (if (>= (get_megabytes_used) *memory_limit_megabytes*)
        (return-from generate_successors '())
    )
    (let ((successors '()) 
          (board (state-board state)))
      (dolist (action (generate_possible_actions  board))
        (if (null successors)
            (setf  successors (list (do_action_clone state action)))
            (nconc successors (list (do_action_clone state action)))
        )
      )
      (sort successors 'compare_states)
    )
)

;************************************************************************
;*                             HEURISTICS                               *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tells how many pieces there are in the board still. It will be 
;; useful to compute heuristics
;;
(defun get_remaining_pieces (board)
    (let*(  (shape (array-dimensions board))
            (lines   (first  shape))
            (columns (second shape))
            (total   0))
        (loop for l from 0 to (- lines 1) do
            (loop for c from 0 to (- columns 1) do
                (when (null (get_pos board l c))
                    (incf total 1))))
        (- (* lines columns) total)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tells how many clusters there are in the board still. It will be 
;; useful to compute heuristics
;;
(defun get_remaining_clusters (board)
    (list-length (generate_possible_actions board))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; In this heristic the estimation of points we can still do by playing
;; in this state is the score you would get to play a cluster that has
;; the mean size of all the clusters in the board at the moment multi-
;; plied by the number of clusters remaining
;;
(defun h1 (state)
    (let* ((board (state-board state))
           (remaining_pieces   (get_remaining_pieces board))
           (remaining_clusters (get_remaining_clusters board)))
        (if (eq 0 remaining_clusters)
            0 ; no more plays, no more points
            (* remaining_clusters (score_move (/ remaining_pieces remaining_clusters)))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This heuristic is very similiar to h4, but instead of estimating -1 
;; point, it estimates less 1%. It seems do do better than the other.
;; Interesting for report?
;;
(defun h3 (state)
    (let* ((board (state-board state))
          (successors (generate_possible_actions board))
          (dummy_board (copy_board board))
          (expected_score 0))
         (dolist (action successors)
            (setf expected_score 
                  (+  expected_score
                      (score_move   
                        (* 0.99
                          (length
                            (remove_cluster dummy_board (first action) (second action)))))))
         )
         expected_score
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Expects we're going to get the current points if we got all clusters
;; 
(defun h4 (state)
    (let* ((board (state-board state))
          (successors (generate_possible_actions board))
          (dummy_board (copy_board board))
          (expected_score 0))
         (dolist (action successors)
            (setf expected_score 
                  (+  expected_score
                      (score_move   
                          (length
                            (remove_cluster dummy_board (first action) (second action))))))
         )
         (- expected_score 1) ;important. This is an incentive to actually explore the state and not only generate it
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;; Expects the score to be the value of the score of the 
;; biggest cluster
;;
(defun h5 (state)
    (let* ((board (state-board state))
          (successors (generate_possible_actions board))
          (dummy_board (copy_board board))
          (expected_score 0)
          (temp_store_score 0))
         (dolist (action successors)
            (setf temp_store_score 
                  (score_move  
                          (length
                            (remove_cluster dummy_board (first action) (second action)))))
            (if (< expected_score temp_store_score)
                (setf expected_score temp_store_score))   
                            
         )
         expected_score ;important. This is an incentive to actually explore the state and not only generate it
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Randomized Heuristic - it increases the size of each cluster uniformly
;;  from 0 to 100%. The idea is to prioritize the growth of clusters
;;  but avoiding getting stuck at a potential state with a large cluster,
;;  (this is the one which maximized the score of b3 at 2800 - but the score varies)
;; 
(defun h6 (state)
    (let* ((board (state-board state))
          (successors (generate_possible_actions board))
          (dummy_board (copy_board board))
          (expected_score 0)
          (temp_store_score 0))
         (dolist (action successors)
            (setf temp_store_score 
                  (score_move 
                        (* (+ 1 (random 1)) 
                          (length
                            (remove_cluster dummy_board (first action) (second action))))))
            (if (< expected_score temp_store_score)
                (setf expected_score temp_store_score))   
                            
         )
         expected_score  ;important. This is an incentive to actually explore the state and not only generate it
    )
)
 
;************************************************************************
;*                         SEARCH ALGORITHMS                            *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; From a list of states and their parent node, generates a list of nodes
;;
(defun generate_nodes (successores father)
    (mapcar (lambda (succ) 
                (make-node_depth 
                    :estado succ 
                    :pai father
                    :depth (+ 1 (node_depth-depth father)))
            ) successores)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; From a list of states and their parent node, generates a list of nodes
;;
(defun generate_nos (successores father)
    (mapcar (lambda (succ) 
                (make-no
                    :estado succ 
                    :pai father)
            ) successores)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; From a list of states and their parent node, generates a list of nodes
;;
(defun generate_nodes_discrepancy_max (successores father heuristic)
    (mapcar (lambda (succ) 
                (make-node_discrepancy
                    :estado succ 
                    :pai father
                    :discrepancies (node_discrepancy-discrepancies father)
                    :heuristic (funcall heuristic succ))
            ) successores)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; From a list of states and their parent node, generates a list of nodes
;; All discrepancies are incremented by one but then the best node is 
;; decremented in the function successores_non_max_discrepancy
;;
(defun generate_nodes_discrepancy_non_max (successores father heuristic)
    (mapcar (lambda (succ) 
                (make-node_discrepancy
                    :estado succ 
                    :pai father
                    :discrepancies (+ 1 (node_discrepancy-discrepancies father))
                    :heuristic (funcall heuristic succ))
            ) successores)
)

;****************************
;* AUX FUNCTIONS                           
;****************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Given two nodes, returns the best one in terms of score of its state
;; Useful to keep track of the best node
;;
(defun determine_best_node (node1 node2)
    (if (> (state-score (no-estado node1)) (state-score (no-estado node2)))
        node1
        node2
    )
)

;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! 
;;! OG COMMENT - Funcoes a utilizar para a procura em largura-primeiro.
;;! Now we want to maximise the score
;;!
(defun junta-ordenados (abertos nos-a*)
  "Junta os nos por ordem crescente do seu valor de f.  
   Estrategia A*."
  
  (flet ((maior (n1 n2)
	        (> n1 n2)))
        (merge 'list (sort nos-a* #'maior :key #'no-a*-f) abertos
	        #'maior :key #'no-a*-f))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; If the new solutions returned by DFS is not as good as the one before
;; two things can happen: 1) the time is up and the solution is partial
;; or 2) the DFS swept the search space twice and already returned the 
;; the best position possible, in both cases, we want to finish the IDS
;;
(defun new_best_than_old (new old)
    (> (state-score (car (last new))) (state-score (car (last old))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Comparing nodes by its heuristic value
;;
(defun compare_nodes (node1 node2)
    (> (node_discrepancy-heuristic node1) (node_discrepancy-heuristic node2))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; We have to reimplement this function so we can do evaluation of the
;; node in generation, not expantion
;;
(defun espaco-expande-no (espaco no best_node problema)
  "Expande o no recebido no espaco, actualizando a estrutura do
  espaco."
  ;; Comecamos por gerar todos os sucessores do estado correspondente
  ;; ao no recebido
    (let(   (sucessores (problema-gera-sucessores problema
                            (no-estado no) 
                            (give_filter_function_on_best_score (state-score (no-estado best_node)))))
            (nos-sucessores nil))
        ;; O no ja foi expandido, por isso passa para os expandidos
        (junta-no-expandido espaco no)
        
        ;; Finalmente, juntamos aos abertos os nos cujos estados ainda nao
        ;; existem no espaco (os nos mais recentes vao para o fim da
        ;; lista)
        (setf nos-sucessores (cria-nos-sucessores espaco no sucessores))
        (junta-nos-gerados espaco nos-sucessores)

        (if (not (null nos-sucessores))
            (determine_best_node best_node (nth 0 nos-sucessores))
            best_node
        )
    )             
)

;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;!
;;! Now we keep track of the best node so we can return it ata any time
;;? Maybe replacing best_node in expansion or generation is all the same,
;;? because if the node is trully the best then it will be the next one
;;? to be expanded
;;!
(defun procura-com-espaco (problema espaco)
  
  (let ((objectivo? (problema-objectivo? problema))
        (best_node nil)) ;! kepp track of the best node
    (loop
      
        ;; Quando nao temos mais nos e porque ja exploramos todo o
        ;; espaco e nao encontramos a solucao (nao existe)
        (when (espaco-vazio? espaco)
	        (return (da-caminho best_node))) ; ! when running out of states, return the best
      
        ;; Vamos considerar o no gerado mais antigo para termos uma
        ;; procura em largura primeiro
        (let ((proximo-no (espaco-proximo-no espaco)))

            ;; Se atingimos a solucao paramos e devolvemos os estados no
            ;; caminho 
	        (when (funcall objectivo?)
	            (return (da-caminho best_node))) ; ! return from best node
            (when (null best_node)
                (setf best_node proximo-no))

            ;; Caso contrario, devemos expandir o no
            (setf best_node (espaco-expande-no espaco proximo-no best_node problema)))))
)

;****************************
;* ALGORITHMS                         
;****************************
;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;?  MAYBE DO RECURSIVE W/ A depth_first_search(problam profundida-maxima melhor nó)
;;?  EM VEZ DA STACK
;;! Our implementation of DFS. Might not be as quick as the original,
;;! but at least it allows us to keep the search within the time 
;;! and memory limit, it is an iterative search, not recursive.
;;!
(defun profundidade-primeiro (problema profundidade-maxima)
    (let*(  (stack (stack_create))
            (closed '())
            (initial_node (make-node_depth :estado (problema-estado-inicial problema) 
                                           :pai nil
                                           :depth 0))
            (best_node initial_node)
            (next_node nil)
            (objetivo? (problema-objectivo? problema))
            (successor_nodes nil)
            (best_successor  nil))
        (stack_push stack initial_node)
        (loop
            (when (stack_empty stack)
                (return-from profundidade-primeiro (da-caminho best_node)))
            (setf next_node (stack_pop stack))
            (unless (or (member next_node closed) (> (node_depth-depth next_node) profundidade-maxima))
                (when (funcall objetivo?)
                    (return-from profundidade-primeiro (da-caminho best_node)))
                (setf successor_nodes (generate_nodes
                                        (problema-gera-sucessores problema
                                            (no-estado next_node) 
                                            (give_filter_function_on_best_score (state-score (no-estado best_node))))
                                        next_node))
                (when (not (null successor_nodes))
                    (setf best_successor (nth 0 successor_nodes))
                    (setf best_node (determine_best_node best_node best_successor))    
                )
                (stack_push stack successor_nodes)
                (if (null closed)
                    (setf closed (list next_node))
                    (push next_node (cdr (last closed))) ; add expanded node to closed
                )
            )
        )
    )
)



;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! 
;;! Ideally we would use 
;;!
(defun profundidade-iterativa (problema profundidade-maxima)
    (let(   (iteration 0)
            (best_solution nil)
            (current_solution nil)
            (objectivo? (problema-objectivo? problema)))
        (loop
            (when (or (funcall objectivo?) (> iteration profundidade-maxima))
                (return-from profundidade-iterativa best_solution))
            (setf current_solution (profundidade-primeiro problema iteration))
            (if (null best_solution)
                (setf best_solution current_solution)
                (if (new_best_than_old current_solution best_solution)
                    (setf best_solution current_solution)
                    (return-from profundidade-iterativa best_solution)
                )
            )
            (incf iteration 1)
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Iterative Deepening A*  
;;
(defun ida* (problema)                 
    (let* ((initial_node  (make-no 
                            :estado (problema-estado-inicial problema)
                            :pai nil ))
            (best_node initial_node)
            (heur (problema-heuristica problema))
            (objectivo? (problema-objectivo? problema)))
        (labels 
            ((prof (node score-min depth)
                  (let* 
                    ( (estado (no-estado node))
                      (current_score (+ (state-score estado) (funcall heur estado))));; f = g + h
                    (cond 
                        ((< current_score score-min) current_score)  ;return estimated cost.
                        ((funcall objectivo?) (return-from ida* (da-caminho best_node))) 
                        (t
                            (let(   (max-score 0) (result nil)
                                    (successor_nodes (generate_nos 
                                            (problema-gera-sucessores problema
                                                (no-estado node) 
                                                (give_filter_function_on_best_score (state-score (no-estado best_node))))
                                                node))
                                )
                                (loop for suc_node in successor_nodes do
                                    (setf result (prof suc_node score-min (+ depth 1)))
                                    (if (< max-score result ) (setf max-score result))
                                    (if (< (state-score (no-estado best_node)) (state-score (no-estado suc_node)))
                                        (setf best_node suc_node))
                                )
                                max-score
                            )
                        )
                    )
                ) 
            ))
            (let 
                ((score-min most-positive-fixnum))
                (loop
                    (let
                        ((solucao (prof initial_node score-min 0)))
                        (if (< solucao score-min)
                            (setf score-min solucao)
                            (return (da-caminho best_node))))))
        )
    )
)

;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! 
;;! Sends a random probe down the state tree
;;!
(defun send_probe (problema best_solution)
    (let*(  (next_node (cria-no (problema-estado-inicial problema) nil))
            (successores nil)
            (to_expand 0)
            (objectivo? (problema-objectivo? problema))
            (best_node nil))
        (when (null best_solution)
            (setf best_solution (list (problema-estado-inicial problema))))
        (loop
            (setf successores (problema-gera-sucessores problema
                                    (no-estado next_node) 
                                    (give_filter_function_on_best_score (state-score (car (last best_solution))))))
            (when (null best_node)
                (setf best_node (cria-no (nth 0 successores) next_node)))
            (when (not (null successores))
                (setf best_node (determine_best_node best_node (cria-no (nth 0 successores) next_node))))
            (when (or (null successores) (funcall objectivo?))
                (return-from send_probe (da-caminho best_node)))

            (setf to_expand (random (list-length successores)))
            (setf next_node (cria-no (nth to_expand successores) next_node))
        )
    )
)

;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! 
;;! The iterative sampling algorithm - it will never return before the 
;;! time limit, as long as there is time, it will constantly be
;;! sending probes down the tree in hopes of finding a better solution
;;!
(defun iterative_sampling_search (problema)
    (let(   (best_solution nil)
            (current_solution nil)
            (objectivo? (problema-objectivo? problema)))
        (loop
            (when (funcall objectivo?)
                (return-from iterative_sampling_search best_solution))
            (setf current_solution (send_probe problema best_solution))
            (if (null best_solution)
                (setf best_solution current_solution)
                (when (and (> 1 (length current_solution))
                            (new_best_than_old current_solution best_solution))
                    (setf best_solution current_solution))   
            )
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Expanding a node on a path where the limit for discrepancies has
;; already been reached - it will return only the best node according to
;; the heuristic
;; ? 2 passes is better than sorting and returning the first, right? 
;;
(defun successores_max_discrepancy (to_expand problema)
    (let*(  (heuristic (problema-heuristica problema))
            (max_heuristic_val 0)
            (successores (generate_nodes_discrepancy_max (problema-gera-sucessores problema
                                (no-estado to_expand)) 
                                to_expand heuristic))
            (best_node nil))

        (when (null successores)
            (return-from successores_max_discrepancy nil))
        (setf best_node (nth 0 successores))
        (loop for node in successores do
            (when (> (node_discrepancy-heuristic node) max_heuristic_val)
                (setf max_heuristic_val (node_discrepancy-heuristic node)))
        )
        (loop for node in successores do
            (when (eq (node_discrepancy-heuristic node) max_heuristic_val)
                (return (list node best_node)))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Expanding a node on a path where the limit for discrepancies has not
;; yet been reached - it will return all the successors but every one
;; except for the best, will have the field 'discrepancies' incremented
;;
(defun successores_non_max_discrepancy (to_expand problema best_node)
    (let*(  (heuristic (problema-heuristica problema))
            (successores (generate_nodes_discrepancy_non_max (problema-gera-sucessores problema
                                (no-estado to_expand) 
                                (give_filter_function_on_best_score (state-score (no-estado best_node)))) 
                                                             to_expand heuristic))
            (best_node nil))
        (when (null successores)
            (return-from successores_non_max_discrepancy nil))
        (setf best_node (nth 0 successores))
        (setf successores (sort successores 'compare_nodes))
        (decf (node_discrepancy-discrepancies (nth 0 successores)) 1) ;The best node did not have a discrepancy.
        (list successores best_node)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A comparison function to sort nodes by the score of their states
;;
(defun compare_nodes_score (node1 node2)
    (> (state-score (no-estado node1)) (state-score (no-estado node2)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A wierd version of Limited Discrepancy Search proposed by Harvey that
;; supports an arbitrary branching factor.
;;
(defun limited_discrepancy_search (problema)
    (let*(  (stack (stack_create))
            (initial_node (make-node_discrepancy :estado (problema-estado-inicial problema) 
                                                 :pai nil
                                                 :discrepancies 0))
            (best_node initial_node)
            (next_node nil)
            (objectivo? (problema-objectivo? problema))
            (allowed_discrepancies 0)
            (successor_nodes       nil)
            (best_node_next_gen    nil)
            (generation_result     nil))
        (stack_push stack initial_node)
        (loop
            (setf stack (stack_create))
            (stack_push stack initial_node)
            (loop
                (when (stack_empty stack)
                    (return))
                (when (funcall objectivo?)
	                (return-from limited_discrepancy_search (da-caminho best_node)))
                (setf next_node (stack_pop stack))
                (if (>= (node_discrepancy-discrepancies next_node) allowed_discrepancies)
                    (setf generation_result (successores_max_discrepancy next_node problema))
                    (setf generation_result (successores_non_max_discrepancy next_node problema best_node)))
                (setf successor_nodes    (first  generation_result))
                (setf best_node_next_gen (second generation_result))
                (when (not (null successor_nodes))
                    (stack_push stack successor_nodes)
                    (setf best_node (determine_best_node best_node best_node_next_gen)))
            )
            (incf allowed_discrepancies 1)
        )
    )
)


;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! 
;;! The same function as in procura.lisp but with our reimplementations
;;!
(defun procura (problema tipo-procura
		&key (profundidade-maxima most-positive-fixnum)
		     (espaco-em-arvore? nil))
  "Dado um problema e um tipo de procura devolve uma lista com: a
  solucao para o problema (a lista de estados desde o estado inicial
  ate' ao estado final), ou nil caso nao encontre a solucao; tempo
  gasto na procura (em internal-time-units); numero de nos expandidos;
  numero de nos gerados."

  (flet ((faz-a-procura (problema tipo-procura 
			 profundidade-maxima espaco-em-arvore?)

	   (cond ((string-equal tipo-procura "largura")
                (largura-primeiro problema 
                            :espaco-em-arvore? espaco-em-arvore?))
            ((string-equal tipo-procura "profundidade")
                (profundidade-primeiro problema profundidade-maxima))
            ((string-equal tipo-procura "profundidade-iterativa")
                (profundidade-iterativa problema profundidade-maxima))
            ((string-equal tipo-procura "a*")
                (a* problema :espaco-em-arvore? espaco-em-arvore?))
            ((string-equal tipo-procura "ida*")
                (ida* problema ))
            ((string-equal tipo-procura "iterative_sampling")
                (iterative_sampling_search problema))
            ((string-equal tipo-procura "limited_discrepancy")
                (limited_discrepancy_search problema)))))
        

    (let(   (*nos-gerados* 0)
	        (*nos-expandidos* 0)
            (*lisp-implementation*  (determine_implementation))
            (*initial_time* (get_elapsed_seconds))
	        (tempo-inicio (get-internal-run-time)))
        (let(   (solucao (faz-a-procura problema tipo-procura
				    profundidade-maxima
				    espaco-em-arvore?))
            )
	(list solucao 
	      (- (get-internal-run-time) tempo-inicio)
	      *nos-expandidos*
	      *nos-gerados*)))
    )
)

(defun resolve-same-game (board strategy)
    (let*(  (array_board (list-to-2d-array board))
            (initial_state (make-state :board array_board :score 0 :move nil))
            (problema (cria-problema initial_state '(generate_successors) 
                                                   :objectivo? #'is_it_goal
                                                   :custo #'cost_same_game
                                                   :heuristica #'h4))
            (solution nil)
        )

        (cond 
            ((string-equal strategy "melhor.abordagem")
                (setf solution (procura problema 'a*)))
            ((string-equal strategy "a*.melhor.heuristica")
                (setf solution (procura problema 'a*)))
            ((string-equal strategy "a*.melhor.heuristica.alternativa")
                (setf (problema-heuristica problema) #'h6)
                (setf solution (procura problema 'a*)))
            ((string-equal strategy "sondagem.iterativa")
                (setf solution (procura problema 'iterative_sampling)))
            ((string-equal strategy "abordagem.alternativa")
                (setf solution (procura problema 'limited_discrepancy)))
            ((string-equal strategy "profundidade.primeiro")
                (setf solution (procura problema 'profundidade)))
            ((string-equal strategy "largura.primeiro")
                (setf solution (procura problema 'largura)))
            ((string-equal strategy "profundidade.iterativa")
                (setf solution (procura problema 'profundidade-iterativa)))
            ((string-equal strategy "ida*.melhor.heuristica")
                (setf solution (procura problema 'ida*)))
            ((string-equal strategy "ida*.melhor.heuristica.alternativa")
                (setf (problema-heuristica problema) #'h6)
                (setf solution (procura problema 'ida*)))
        )

        (loop for state in (rest (first solution)) collect (state-move state))
    )
)
