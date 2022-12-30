;;
;; This is a file contianing some functions used to parse file names at a ervious job
;; This was mainly don in my free time as a way tohelp expand my knowledg of elisp
;;
;;

(defun ends-with-number (filename)
   "Does the filename given end with a number?"
   (interactive "FFilename: ")
   (let* ((fname (file-name-sans-extension filename))
	  (last-ch (substring fname -1)))
      (if (or (> (string-to-number last-ch) 0)
	      (string= last-ch "0"))
	    t
	 nil)))

(defun is-revision-file (filename)
   "Is the given file a revision one?
Does it end in the form 'RX' or 'RXX'
where X is a number?"
   (interactive "FFilename: ")
   (if (ends-with-number filename)
	 (let* ((fname (file-name-sans-extension filename))
		(last-two (substring fname -2))
		(rest-fname (substring fname nil (- (length fname) 2))))
	    (if (or (and (> (string-to-number last-two) 0)
			 (string= "R" (substring rest-fname -1)))
		    (string= "R" (substring last-two nil 1)))
		  t
	      nil))))


(defun is-padded (filename)
  "Is the given file already padded?"
  (interactive "FFilename: ")
  (if (is-revision-file filename)
      (let ((rev-num (substring (file-name-sans-extension filename)
				(- (length (file-name-sans-extension filename)) 3))))
	(if (string= "R" (substring rev-num 0 1))
	    t
	  nil))))
		     

(defun pad-number (Snumber &optional padding)
   "Given a number in the form of a string determine
if it needs to be padded. And padd if nessecary.
If number is to big to pad return nil"
   (interactive "SNumber: ")
   (or padding (setq padding 2))
   (if (> (length Snumber) padding)
	 nil
      (if (= (length Snumber) padding)
	    Snumber
	 (while (< (length Snumber) padding)
	    (setq Snumber (concat "0" Snumber)))
	 Snumber)))



(defun pad-revision-number-of-filename (filename)
   "Pad the numbers at the end of a file name
that has a revision number in the format 'RX'
and skip those with out or those already in 
the form 'RXX'"
   (interactive "FFilename: ")
   (if (and (ends-with-number filename)
	    (is-revision-file filename))
	 (let* ((fname (file-name-sans-extension filename))
		(RevPoint (string-match "R[[:digit:]]$" fname))
		(Rnumber (substring fname RevPoint))
		(name (substring fname nil RevPoint))
		(Snumber ""))
	    (if (< (length Rnumber) 3)
		  (progn (setq Snumber (substring Rnumber -1))
			 (setq Snumber (pad-number Snumber))
			 (setq Rnumber (concat "R" Snumber)))
	       )
	    (setq fname (concat name
				Rnumber
				"."
				(file-name-extension filename)))
	    fname)))

(defun pad-revision-numbers-of-files-in-directory (directory)
  "Replace single revisoin numbered files with 
a double digit revisino padded with 0s"
  (interactive "DDirectory: ")
  (cd directory)
  (let ((file-list (directory-files directory))
	filename)
    (while file-list
      (if (and (ends-with-number (car file-list))
	       (is-revision-file (car file-list))
	       (not (is-padded (car file-list))))
	  (progn (setq fname (pad-revision-number-of-filename (car file-list)))
		 (if (file-exists-p fname)
		     (setq fname (concat (file-name-sans-extension fname)
					 "A."
					 (file-name-extension fname))))
		   (rename-file (car file-list) fname)))
      (setq file-list (cdr file-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;; Old fileplay.el

(defun replace-space (name &optional sep rep)
   "Take a name and remove the space and replace it with a _."
   (interactive "sFilename to be fixed: ")
   (or rep (setq rep "_"))
   (or sep (setq sep split-string-default-separators))
   (let ((split-name (split-string name sep))
	 new-name)
      (while split-name
	 (if (< 1 (length split-name))
	       (setq new-name (concat new-name
				      (car split-name)
				      rep))
	    (setq new-name (concat new-name
				   (car split-name))))
	 (setq split-name (cdr split-name)))
      new-name))

(defun contains-space (str)
   "See if a STR contains a space"
   (interactive "sString: ")
   (if (or (> (length (split-string str)) 1)
	   (string= (substring str 0 1) " ")
	   (string= (substring str -1) " "))
	 t
      nil))

(defun replace-spaces-in-filenames (directory)
   "Replace spaces in filenames with `_'.
 Must be same buffer you are active in."
   (interactive "DDirectory name: ")
   (cd directory)
   (let ((files-list (directory-files directory))
	 (fname ""))
      (while files-list
	 (if (contains-space (car files-list))
	       (progn (setq fname (replace-space (car files-list)))
		      (rename-file (car files-list) fname)))
	 (setq files-list (cdr files-list)))))

;;;;;;;;;;
;; Combine the two major operations into one

(defun replace-spaces-and-pad-revision-number-in-filename (directory)
  "Both replace spaces with '_' and pad revision number in filenames"
  (interactive "DDirectory: ")
  (cd directory)
  ;; if file is a revison file
  ;; then attempt padding
  ;; then replace spaces
  (let ((files-list (directory-files directory))
        (fname ""))
    (while files-list
      (if (and (contains-space (car files-list))
	       (ends-with-number (car files-list))
	       (is-revision-file (car files-list)))
	  ;; Need a check is the newly created file name exists and if it does
	  ;; either check if they contain the same info or add a letter
	  (progn (setq fname (pad-revision-number-of-filename (car files-list)))
		 (setq fname (replace-space fname))
		 (rename-file (car files-list) fname t)
		 (setq files-list (cdr files-list)))
	(setq files-list (cdr files-list))))))

(defun replace-spaces-and-pad-revision-number-in-filename (directory)
  "Both replace spaces with '_' and pad revision number in filenames"
  (interactive "DDirectory: ")
  (cd directory)
  ;; if file is a revison file
  ;; then attempt padding
  ;; then replace spaces
  (let ((files-list (directory-files directory))
        (fname ""))
    (while files-list
      (if (and (contains-space (car files-list))
	       (ends-with-number (car files-list))
	       (is-revision-file (car files-list)))
	  ;; Need a check is the newly created file name exists and if it does
	  ;; either check if they contain the same info or add a letter
	  (progn (setq fname (pad-revision-number-of-filename (car files-list)))
		 (setq fname (replace-space fname))
		 (rename-file (car files-list) fname t)
		 (setq files-list (cdr files-list)))
	(setq files-list (cdr files-list))))))


