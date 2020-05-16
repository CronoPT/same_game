;(in-package :user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AUXILIARY FUNCTIONS FOR BOARD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ALL PIECES THAT WILL GET REMOVED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun do_action_tail (board l c) 
    (let 
        ( 
            (lines   (length board))
            (columns (length (first board)))
            (color (get_pos board l c))
            (pos_changed (list  (list l c)))
        )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FUNCTIONS TO BRING DOWN ELEMENTS
;      pos_changed -> (l,c)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bring_down_from (board pos_changed)
    (let ((l (first pos_changed))
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

(defun apply_gravity_down (board pos_changed)
    (sort pos_changed (lambda (pos1 pos2) (< (first pos1) (first pos2)))) ;ordena por linhas
    (if (not (null pos_changed))   
        (progn
            (bring_down_from    board (first pos_changed)) ;bring first down
            (apply_gravity_down board (rest pos_changed))  ;bring the rest down
        )
    )
    board
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bring_column_foward (board mov_col)
    (let ((lines (length board)) 
        (columns (length (first board))))
        (loop for c from mov_col to (- columns 1) doing
            (loop for l from 0 to (- lines 1) doing
                (if (< c (- columns 1))
                    (progn 
                        (set_pos board l c (get_pos board l (+ c 1)))
                        (set_pos board l (+ c 1) nil)
                    )
                )
            )
        )
    )
)

(defun apply_gravity_left (board)
    (let* ((lines (length board)) 
        (columns (length (first board)))
        (null_cols (loop for c from (- columns 1) downto 0 
                        when (null (get_pos board (- lines 1) c)) collect c))) ;collects all columns whose values in the bottom are nil
        (loop for c in null_cols do
            (bring_column_foward board c)
        )
        ;(print_board board)
    )
)










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;(defvar boardinho '((1 2 2 3 3) (2 2 2 1 3) (1 2 2 2 2) (1 1 1 1 1))
;)


;(print_board boardinho)

;(format t "hey1~%")

;(do_action boardinho '(1 1))

;
;(format t "~%")

;(do_action boardinho '(1 4))





(defvar b1 '((1 2 nil 3 3) (2 2 nil 1 3) (1 2 nil 2 2) (1 1 nil 1 1))
)

(defvar b2 '((1 2 nil nil nil 3 4 nil 5 ) (1 2 nil nil nil 3 4 nil 5 ) (1 2 nil nil nil 3 4 nil 5 ) (1 2 nil nil nil 3 4 nil 5 ))
)

(defvar b3'((1 2 nil nil nil 3 4 nil nil ) (1 2 nil nil nil 3 4 nil nil ) (1 2 nil nil nil 3 4 nil nil ) (1 2 nil nil nil 3 4 nil nil ))
)

(defvar b4'((nil 2 nil nil nil 3 4 nil nil ) (nil 2 nil nil nil 3 4 nil nil ) (nil 2 nil nil nil 3 4 nil nil ) (nil 2 nil nil nil 3 4 nil nil ))
)


;(print "hello")
(print (apply_gravity_left b4))