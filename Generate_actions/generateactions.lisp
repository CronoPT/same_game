;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   POSSIBLE_ACTIONS
;   
;

(defun generate_actions (board) ;shouldn't I just go until the first (l,0)==null in columns?
    (let  ((possible_actions '())
           (lines (length board))
           (columns (length (first board)))
           (checked_actions (copy-list board))
          )
          (loop for l from 0 to (- lines 1) do 
            (loop for c from 0 to (- columns 1) do
              (setf possible_actions
                (add_action board l c checked_actions possible_actions)))
          )
          possible_actions
    )   
)

;;;;;;;;;;;;;;;;;
; This function will add a pos = (l,c) to possible_actions 
; And will invalidate all other pos that belong to the same cluster
; It does this by using a copy of the board - check_actions and 
; putting every single pos belonging to the cluster at nil
;
; If we try to add a pos on the board that is null on checked_actions, nothing happens
;
(defun add_action (board l c checked_actions possible_actions)
    (setf p '(a c))
    (if (not (null (get_pos checked_actions l c)))
        (progn
          (if (null possible_actions )
            (setf possible_actions (append possible_actions (list (list l c)) ))
            (nconc possible_actions (list (list l c))))
          (mark_check_actions checked_actions (do_action_tail board l c))
          (set_pos board l c nil)
        ))
    possible_actions
)

;;;;;
; This is the function that gets the checked_actions board and a list of positons
; of a cluster and sets it all to null
(defun mark_check_actions (checked_actions cluster_positions)
    (loop for pos in cluster_positions do
      (set_pos checked_actions (first pos) (second pos) nil)))

