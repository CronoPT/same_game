(load "G005")

;; 10x4 board with 3 colors
 (defvar b1 '((2 1 3 2 3 3 2 3 3 3) 
              (1 3 2 2 1 3 3 2 2 2) 
              (1 3 1 3 2 2 2 1 2 1) 
              (1 3 3 3 1 3 1 1 1 3)))

;; ;; 10x4 board with 5 colors 
(defvar b2 '((4 3 3 1 2 5 1 2 1 5) 
             (2 4 4 4 1 5 2 4 1 2)
             (5 2 4 1 4 5 1 2 5 4)
             (1 3 1 4 2 5 2 5 4 5)))

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

;; 15x20 board with 5 colors
(defvar b5 '((1 1 1 1 1 1 1 1 1 1 5 1 1 1 2 1 4 2 1 2)
             (1 1 1 1 1 1 1 1 1 1 5 3 4 2 2 2 2 1 3 1)
             (1 1 1 1 1 1 1 1 1 1 5 5 3 5 5 3 1 5 4 3)
             (1 1 1 1 1 1 1 1 1 1 5 3 4 2 2 2 2 1 3 1)
             (1 1 1 1 1 1 1 1 1 1 3 5 5 2 2 5 2 2 4 2)
             (1 1 1 1 1 1 1 1 1 1 1 1 5 3 1 1 2 5 5 5)
             (1 1 1 1 1 1 1 1 1 1 4 2 4 2 2 5 3 1 2 1)
             (1 1 1 1 1 1 1 1 1 1 4 3 1 3 5 1 5 3 4 2)
             (2 3 3 2 5 4 3 4 4 4 3 5 5 2 2 5 2 2 4 2)
             (3 5 5 2 2 5 2 2 4 2 5 3 5 3 3 3 3 4 2 2)
             (1 4 2 3 2 4 5 5 4 2 4 3 3 2 4 2 4 2 2 5)
             (4 1 3 2 4 3 4 4 3 1 1 1 1 1 1 2 3 4 5 3)
             (4 2 4 2 2 5 3 1 2 1 1 1 1 1 1 1 5 1 5 4) 
             (1 3 1 5 2 4 4 3 3 1 1 1 1 1 1 5 3 1 2 1)
             (4 2 4 2 2 5 3 1 2 1 1 1 1 1 1 3 3 4 2 2)))

(defvar b6 '((1 2 3 1 2 2 2 3 1 2) 
             (1 1 2 3 3 2 2 1 2 3) 
             (2 3 3 1 1 3 2 2 1 3) 
             (3 2 3 2 1 2 2 1 2 1) 
             (3 2 1 1 3 3 1 1 3 1) 
             (1 2 3 1 3 1 3 1 2 3) 
             (3 3 2 3 2 2 3 3 2 3) 
             (1 2 3 1 3 1 2 2 3 1) 
             (1 3 3 3 1 1 1 1 3 1) 
             (1 1 3 1 3 3 2 3 3 1) 
             (2 3 2 3 2 2 2 3 3 3) 
             (2 3 1 1 2 2 2 3 2 2) 
             (3 2 1 2 3 3 2 1 1 1) 
             (2 2 3 2 3 3 3 3 3 1) 
             (1 2 3 1 3 1 1 3 2 1))) 

(defvar boardinho '((1 2 2 3 3) 
                    (2 2 2 1 3) 
                    (1 2 2 2 2) 
                    (1 1 1 1 1)))

(print (resolve-same-game  b5 'abordagem.alternativa))

;; (print (resolve-same-game b3 'abordagem.alternativa))

;; (defvar board (list-to-2d-array b4))

;; (trace remove_cluster)
;; (trace apply_gravity_down)
;; (trace apply_gravity_left)
;; (do_action board '(1 0))
;; (print_board board)

;; (defvar initial_state (make-state :board board :score 0 :move nil))

;; (defvar problema (cria-problema initial_state '(generate_successors) 
;;                     :objectivo? #'is_it_goal
;;                     :custo #'cost_same_game
;;                     :heuristica #'h4))

;; (print "SPAM BEGINS")
;; (time (defvar A (procura problema 'a*)))
;; (terpri)
;; (print "RESULTS")
;; (terpri)
;; (loop for state in (first A) do
;;     (print_state state))
;; (terpri)
;; (format t "Expanded  nodes: ~d" (third  A))
;; (terpri)
;; (format t "Generated nodes: ~d" (fourth A))
;; (terpri)
;; (format t "Elapsed seconds ~f" (get_elapsed_seconds))
;; (terpri)
;; (format t "Branches pruned ~d" *nodes_cut*)

;; (if (eql 1 *lisp-implementation*)
;;     (format t "~d" (get_megabytes_used_sbcl))   ; 1 is for sbcl
;;     (format t "~d" (get_megabytes_used_clisp))) ; 2 is for clisp
;; (fresh-line)
;; (format t "Gerados: ~d" *nos-gerados*)
;; (fresh-line)