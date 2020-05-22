;(in-package :user)

;; (load "procura.lisp")
(load "procura.lisp")

;************************************************************************
;*                       TIME AND MEMORY HELPERS                        *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Limit execution time of our program in seconds
;;
(defvar *time_limit_seconds* 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Limit heap usage of our program in megabytes
;; ? The limit is actually 256 MB sould we leave this 2MB padding
;;
(defvar *memory_limit_megabytes* 120)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get the ammount of seconds elapsed since program started
;;
(defun get_elapsed_seconds ()
    (/ (get-internal-run-time) internal-time-units-per-second)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get used heap memory in megabytes - what i do is get the output
;; of room into a string and read the first number that appears there
;; which i think it's the number we want
;;
(defun get_megabytes_used ()
    (let ((room_str (with-output-to-string (*standard-output*) (room nil)))
          (mb_bytes 0))
        (loop for char across room_str until (eql char #\newline) do
            (if (digit-char-p char)
                (setf mb_bytes (+ (* 10 mb_bytes) (digit-char-p char)))
            )
        )
        (floor mb_bytes 1000000)
    )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Computes the possible actions and applies them, generating new states,
;; this is the only operator we have in the whole problem
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
      successors
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ? Will we have to see the best position here in terms of score
;; ? Or probably will end up returning always false
;;
(defun is_it_goal ()
    (>= (get_elapsed_seconds) *time_limit_seconds*)
)

;; (defun is_it_goal (state)
;;     (let ((board (state-board state)))
;;         (loop for l in board do
;;         (loop for c in l do
;;             (if (not (null c))
;;                 (return-from is_it_goal nil)))))
;;     T
;; )

;; (defun is_it_goal (state)
;;     (> (state-score state) 150)
;; )



;************************************************************************
;*                             HEURISTICS                               *
;************************************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tells how many pieces there are in the board still. It will be 
;; useful to compute heuristics
;;
(defun get_remaining_pieces (board)
    (let((lines   (list-length board))
         (columns (list-length (first board)))
         (total   0))
        (loop for l from 0 to (- lines 1) do
            (loop for c from 0 to (- columns 1) until (not (null (get_pos board l c))) do
                (1+ total)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Just so we can use Uniform Cost Search ' might be good to prove the
;; value of the heuristics we wrote
;;
(defun h2 (state) 
    0
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
;;!for VASCO: test this and ask TO why this works. 
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
	        (when (>= (state-score (no-estado proximo-no)) (state-score (no-estado best_node)))
                (setf best_node proximo-no))     ; ! the expanded node is the best till know
            ;; Caso contrario, devemos expandir o no
            (espaco-expande-no espaco proximo-no))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A different node that allows us to keep track of the depth of nodes
;;
(defstruct (node_depth (:include no))
    depth
)

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

;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! 
;;! Our implementation of DFS. Might not be as quick as the original,
;;! but at least it allows us to keep the search within the time 
;;! and memory limit, it is an iterative search, not recursive
;;!
(defun depth_first_search (problema profundidade-maxima)
    (let*(  (stack (stack_create))
            (closed '())
            (initial_node (make-node_depth :estado (problema-estado-inicial problema) 
                                           :pai nil
                                           :depth 0))
            (best_node initial_node)
            (next_node nil)
            (objetivo? (problema-objectivo? problema)))
        (stack_push stack initial_node)
        (loop
            (when (stack_empty stack)
                (return-from depth_first_search (da-caminho best_node)))
            (setf next_node (stack_pop stack))
            (unless (or (member next_node closed) (> (node_depth-depth next_node) profundidade-maxima))
                (when (funcall objetivo?)
                    (return-from depth_first_search (da-caminho best_node)))
                (when (>= (state-score (no-estado next_node)) (state-score (no-estado best_node)))
                    (setf best_node next_node))

                (stack_push stack (generate_nodes (problema-gera-sucessores problema (no-estado next_node)) next_node))
                (if (null closed)
                    (setf closed (list next_node))
                    (push next_node (cdr (last closed))) ; add expanded node to closed
                )
            )
        )
    )
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

;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! 
;;! Ideally we would use 
;;!
(defun iterative_deepening_search (problema profundidade-maxima)
    (let(   (iteration 0)
            (best_solution nil)
            (current_solution nil)
            (objectivo? (problema-objectivo? problema)))
        (loop
            (when (or (funcall objectivo?) (> iteration profundidade-maxima))
                (return-from iterative_deepening_search best_solution))
            (setf current_solution (depth_first_search problema iteration))
            (if (null best_solution)
                (setf best_solution current_solution)
                (if (new_best_than_old current_solution best_solution)
                    (setf best_solution current_solution)
                    (return-from iterative_deepening_search best_solution)
                )
            )
            (incf iteration 1)
        )
    )
)

;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! 
;;! Sends a random probe down the state tree
;;!
(defun send_probe (problema)
    (let*(  (next_node (cria-no (problema-estado-inicial problema) nil))
            (successores nil)
            (to_expand 0)
            (objectivo? (problema-objectivo? problema)))
        (loop
            (setf successores (problema-gera-sucessores problema (no-estado next_node)))
            (when (or (null successores) (funcall objectivo?))
                (return-from send_probe (da-caminho next_node)))
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
            (setf current_solution (send_probe problema))
            (if (null best_solution)
                (setf best_solution current_solution)
                (when (new_best_than_old current_solution best_solution)
                    (setf best_solution current_solution))
            )
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
	   ;; Usamos cond em vez de case porque nao sabemos de que
	   ;; package veem os simbolos (o string-equal funciona com o
	   ;; symbol-name do simbolo e e' "case-insensitive")
	   
	   ;; Actualmente, apenas a procura em largura, o A* e o IDA*
	   ;; estao a aproveitar a informacao do espaco de estados ser
	   ;; uma arvore
	   (cond ((string-equal tipo-procura "largura")
                (largura-primeiro problema 
                            :espaco-em-arvore? espaco-em-arvore?))
            ((string-equal tipo-procura "profundidade")
                (depth_first_search problema profundidade-maxima))
            ((string-equal tipo-procura "profundidade-iterativa")
                (iterative_deepening_search problema profundidade-maxima))
            ((string-equal tipo-procura "a*")
                (a* problema :espaco-em-arvore? espaco-em-arvore?))
            ((string-equal tipo-procura "ida*")
                (ida* problema :espaco-em-arvore? espaco-em-arvore?))
            ((string-equal tipo-procura "iterative_sampling")
                (iterative_sampling_search problema)))))

    (let(   (*nos-gerados* 0)
	        (*nos-expandidos* 0)
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

;************************************************************************
;*                            MAIN                                      *
;************************************************************************
;; 10x4 board with 3 colors
 (defvar b1 '((2 1 3 2 3 3 2 3 3 3) 
              (1 3 2 2 1 3 3 2 2 2) 
              (1 3 1 3 2 2 2 1 2 1) 
              (1 3 3 3 1 3 1 1 1 3)))

;; ;; 10x4 board with 5 colors 
;; (defvar b2 '((4 3 3 1 2 5 1 2 1 5) 
;;              (2 4 4 4 1 5 2 4 1 2)
;;              (5 2 4 1 4 5 1 2 5 4)
;;              (1 3 1 4 2 5 2 5 4 5)))

;; 15x10 board with 3 colors
 (defvar b3'((3 3 3 2 1 2 3 1 3 1)
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
;; (defvar b4 '((5 1 1 1 2 1 4 2 1 2)
;;              (5 5 5 4 1 2 2 1 4 5)
;;              (5 5 3 5 5 3 1 5 4 3)
;;              (3 3 3 2 4 3 1 3 5 1)
;;              (5 3 4 2 2 2 2 1 3 1)
;;              (1 1 5 3 1 1 2 5 5 5)
;;              (4 2 5 1 4 5 4 1 1 1)
;;              (5 3 5 3 3 3 3 4 2 2)
;;              (2 3 3 2 5 4 3 4 4 4)
;;              (3 5 5 2 2 5 2 2 4 2)
;;              (1 4 2 3 2 4 5 5 4 2)
;;              (4 1 3 2 4 3 4 4 3 1)
;;              (3 1 3 4 4 1 5 1 5 4) 
;;              (1 3 1 5 2 4 4 3 3 2)
;;              (4 2 4 2 2 5 3 1 2 1)))

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



(defvar bt '((nil nil nil 1 1 1 1 nil nil nil)
            (nil nil nil 1 1 1 1 1   nil nil)
            (nil nil nil 2 1 1 1 1   nil nil)
            (1   1   1   1 1 2 1 1    1    3)
            (1   1   1   2 1 1 1 1    1    1)
            (1   1   1   3 1 1 1 1    1    3)
            (1   1   3   2 3 1 1 1    3    1)))

(defvar tt (make-state :board bt :score 0))
(print (h4  tt))






;! SOLVING PROBLEM WITH BFS EXAMPLE

(defvar boardinho'((1 2 2 3 3) 
                   (2 2 2 1 3) 
                   (1 2 2 2 2) 
                   (1 1 1 1 1)))
                   
(defvar initial_state (make-state :board b3 :score 0))

(defvar problema (cria-problema initial_state '(generate_successors) 
                    :objectivo? #'is_it_goal
                    :custo #'cost_same_game
                    :heuristica #'h6))

(print "SPAM BEGINS")
(time (defvar A (procura problema 'a*)))
(terpri)
(print "RESULTS")
(terpri)
(loop for state in (first A) do
    (print_state state))
(terpri)
(format t "Expanded  nodes: ~d" (third  A))
(terpri)
(format t "Generated nodes: ~d" (fourth A))
(terpri)
(format t "Elapsed seconds ~f" (get_elapsed_seconds))




