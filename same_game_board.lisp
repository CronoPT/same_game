;(in-package :user)

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
    (setf (nth l (nth c lst)) val)
)

(defun get_pos (lst l c)
    (nth l (nth c lst))
)

(defun do_action_tail (board l c)
    (let ((lines   (length board))
            (columns (length (first board)))
            (color (get_pos board l c))
            (pos_changed (list  (list l c))))
        (set_pos board l c nil)
        (if (<= l (+ lines 1))
            (if (eq color (get_pos board (+ l 1) c ))
                (setq pos_changed (append pos_changed (do_action_tail board (+ l 1) c)))
            )
        )
        (if (>= (- l 1) 0)
            (if (eq color (get_pos board (- l 1) c))
                (setq pos_changed (append pos_changed (do_action_tail board  (- l 1) c)))
            )
        )
        (if (<= c (+ columns 1))
            (if (eq color (get_pos board l (+ c 1)))
                (setq pos_changed (append pos_changed (do_action_tail board l (+ c 1))))
            )
        )
        (if (>= (- c 1) 0)
            (if (eq color (get_pos board l (- c 1)))
                (setq pos_changed (append pos_changed (do_action_tail board  l (- c 1))))
            )
        )
        pos_changed
    )
)

(defun apply_gravity_down (board pos_changed)

)

(defun apply_gravity_left (board)

)

(defun do_action (board position)
    (let ((pos_changed (do_action_tail board (first position) (second position))))
        (apply_gravity_down board pos_changed)
        (apply_gravity_left board)
    )
    (fresh-line)
    (print_board board)
)

;; (defun possible_action (board position)
;;     (let ((lines   (length board))
;;             (columns (length (first board)))
;;             (l (first  position))
;;             (c (second position))
;;             (color (get_pos board l c))
;;             ((get_pos board l c) nil)
;;             (adj_same_color 0))
;;         (if (<= l (+ lines 1))
;;             (if (eq color (get_pos board (+ l 1) c ))
;;                 (+ adj_same_color 1)
;;             )
;;         )
;;         (if (>= 0 (- lines 1))
;;             (if (eq color (get_pos board (- l 1) c))
;;                 (+ adj_same_color 1)
;;             )
;;         )
;;         (if (<= c (+ columns 1))
;;             (if (eq color (get_pos board l (+ c 1)))
;;                 (+ adj_same_color 1)
;;             )
;;         )
;;         (if (>= 0 (- columns 1))
;;             (if (eq color (get_pos board l (- c 1)))
;;                 (+ adj_same_color 1)
;;             )
;;         )
;;         (>= adj_same_color 2)
;;     )
;; )

(do_action '((1 2 2 3 3) (2 2 2 1 3) (1 2 2 2 2) (1 1 1 1 1)) '(1 0))
