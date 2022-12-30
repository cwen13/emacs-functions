;;
;; This is a file with some functions for line and chunks of text movement
;; I have these installed in my .emacs file too
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move a single line                                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun move-line-up (&optional lines)
  "This is to move a line up or down a single line"
  (interactive)
  (or lines (setq lines 0))
  (progn (beginning-of-line)
	 (kill-region (point) (line-end-position))
	 (delete-backward-char 1)
	 (beginning-of-line (- 1 lines))
	 (if (= lines 0)
	     (forward-line 0)
	 (forward-line 1))
 	 (yank)
	 (insert "\n")
	 (forward-line -1)))

(defun move-line-down (&optional lines)
  "This is to move a line up or down a single line"
  (interactive)
  (progn (beginning-of-line)
	 (kill-region (point) (line-end-position))
	 (delete-char 1)
	 (beginning-of-line lines)
	 (forward-line 1)
	 (yank)
	 (insert "\n")
	 (forward-line -1)))


(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move a region of lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun move-region-up ()
  "This is to move a region up "

)
