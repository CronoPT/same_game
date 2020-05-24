;************************************************************************
;*                                 IDA*                                 *
;************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A different node that allows us to keep track of the depth of nodes
;;
(defstruct (node_f (:include no))
    f
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; From a list of states and their parent node, generates a list of nodes
;;
(defun generate_nodes_f (successores father heuristic)
    (mapcar (lambda (succ)
                (let ((heuristic_val (funcall heuristic succ)))
                    (make-node_f :estado succ 
                                 :pai father
                                 :f (+ heuristic_val (state-score succ)))
                ) 
            ) successores)
)

;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;?  MAYBE DO RECURSIVE W/ A depth_first_search(problam profundida-maxima melhor nó)
;;?  EM VEZ DA STACK
;;! Our implementation of DFS. Might not be as quick as the original,
;;! but at least it allows us to keep the search within the time 
;;! and memory limit, it is an iterative search, not recursive.
;;!
(defun limited_a* (problema min_score)
    (let*(  (stack (stack_create))
            (closed '())
            (heuristic (problema-heuristica problema))
            (initial_node (make-node_f :estado (problema-estado-inicial problema) 
                                       :pai nil
                                       :f (funcall heuristic (problema-estado-inicial problema))))
            (best_node initial_node)
            (next_node nil)
            (objetivo? (problema-objectivo? problema))
            (new_limit 0))
        (stack_push stack initial_node)
        (loop
            (when (stack_empty stack)
                (return-from limited_a* (list (da-caminho best_node) new_limit)))
            (setf next_node (stack_pop stack))
            (if (or (member next_node closed) (< (node_f-f next_node) min_score))
                ;;! Lets re-check the new limitn
                (when (> (node_f-f next_node) new_limit)
                    (setf new_limit (node_f-f next_node)))
                (progn
                    (when (funcall objetivo?)
                        (return-from limited_a* (list (da-caminho best_node) new_limit)))
                    (when (>= (state-score (no-estado next_node)) (state-score (no-estado best_node)))
                        (setf best_node next_node))

                    (stack_push stack (generate_nodes_f (problema-gera-sucessores problema (no-estado next_node)) next_node heuristic))
                    (if (null closed)
                        (setf closed (list next_node))
                        (push next_node (cdr (last closed))) ; add expanded node to closed
                    )
                )
            )
        )
    )
)
;;! The maximum that does not reach the minimum

;;!;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;! 
;;! Ideally we would use 
;;!
(defun iterative_deepening_a_vasco* (problema)
    (let(   (limit most-positive-fixnum)
            (best_solution nil)
            (current_solution nil)
            (objectivo? (problema-objectivo? problema))
            (a*_return nil))
        (loop
            (when (or (funcall objectivo?))
                (return-from iterative_deepening_a_vasco* best_solution))

            (setf a*_return (limited_a* problema limit))
            (setf current_solution (first a*_return))
            (setf limit (second a*_return))
            (if (null best_solution)
                (setf best_solution current_solution)
                (if (new_best_than_old current_solution best_solution)
                    (setf best_solution current_solution)
                    (return-from iterative_deepening_a_vasco* best_solution)
                )
            )
        )
    )
)