(load "same_game_board.lisp")

(defun resolve-same-game-for-test (board strategy)
    (let*(  (initial_state (make-state :board board :score 0 :move nil))
            (problema (cria-problema initial_state '(generate_successors) 
                                                   :objectivo? #'is_it_goal
                                                   :custo #'cost_same_game
                                                   :heuristica #'h6))
            (solution nil)
        )

        (cond 
            ((string-equal strategy "melhor.abordagem")
                (format t "What the hell am I doing here? Cause I'm a creep...."))
            ((string-equal strategy "a*.melhor.heuristica")
                (setf solution (procura problema 'a*)))
            ((string-equal strategy "a*.melhor.heuristica.alternativa")
                (setf (problema-heuristica problema) #'h5)
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
            ((string-equal strategy "a*.melhor.heuristica.alternativa")
                (setf (problema-heuristica problema) #'h5)
                (setf solution (procura problema 'ida*)))
        )

        solution
    )
)

(defvar *approaches* '("a*.melhor.heuristica" "a*.melhor.heuristica.alternativa"
                       "sondagem.iterativa" "abordagem.alternativa" "profundidade.primeiro"
                       "largura.primeiro" "profundidade.iterativa" "ida*.melhor.heuristica"
                       "a*.melhor.heuristica.alternativa"))

(defvar *shapes* '((5 5) (10 10) (15 15) (30 30)))

(defvar *tries_per_shape* 3)

(defvar *colors* 3)

(defun run_for_results ()
    (let(   (mean_expanded  0)
            (mean_generated 0)
            (mean_score     0)
            (result         nil)
            (board          nil)
            (score_now      0)
            (expanded_now   0)
            (generated_now  0))
        
        (loop for shape in *shapes* do
            (loop for approach in *approaches* do
                (setf mean_expanded  0)
                (setf mean_generated 0)
                (setf mean_score     0)
                (loop for i from 1 to *tries_per_shape* do
                    (setf board  (generate_random_board (first shape) (second shape) *colors*))
                    (setf result (resolve-same-game-for-test board approach))
                    (setf score_now (state-score (first (last (first result)))))
                    (setf expanded_now  (third result))
                    (setf generated_now (fourth result))
                    (incf mean_score score_now)
                    (incf mean_expanded  expanded_now)
                    (incf mean_generated generated_now)
                )
                (setf mean_expanded  (/ mean_expanded  *tries_per_shape*))
                (setf mean_generated (/ mean_generated *tries_per_shape*))
                (setf mean_score     (/ mean_score     *tries_per_shape*))
                (terpri)
                (format t "Strategy: ~A" approach)
                (terpri)
                (format t "Shape: ~dx~d" (first shape) (second shape))
                (terpri)
                (format t "Mean Expanded Nodes --> ~f" mean_expanded)
                (terpri)
                (format t "Mean Generated Nodes -> ~f" mean_generated)
                (terpri)
                (format t "Mean Score -----------> ~f" mean_score)
                (terpri)
            )
        )
    )
)

(run_for_results)