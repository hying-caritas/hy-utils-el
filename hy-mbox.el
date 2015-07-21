;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defconst +hy-mbox-mail-begin+ "^From ")

(define-derived-mode hy-mbox-mode text-mode "MBOX"
  "Huang Ying's mbox mode")

(cl-defun hy-mbox-beginning-of-mail (&optional n)
  (interactive "P")
  (unless n
    (setf n 1))
  (decf n)
  (if (or (looking-at +hy-mbox-mail-begin+)
	  (re-search-backward +hy-mbox-mail-begin+ nil 'end)
	  (when (re-search-forward +hy-mbox-mail-begin+ nil 'end)
	    (beginning-of-line)
	    t))
      (or (= n 0)
	  (let ((re-search (if (> n 0) #'re-search-forward #'re-search-backward))
		(count (abs n)))
	    (if (> n 0) (end-of-line))
	    (cl-loop
	     for i from 1 upto count
	     for rep = (funcall re-search +hy-mbox-mail-begin+ nil 'end)
	     while rep
	     finally (when rep
		       (beginning-of-line)
		       (cl-return rep)))))))

(cl-defun hy-mbox-mail-beginning-position (&optional n)
  (save-excursion
    (when (hy-mbox-beginning-of-mail n)
      (point))))

(cl-defmacro hy-mbox-for-each-mail (&body body)
  (hy-with-gensyms (next-mail-pos)
    `(hy-save-reset-buffer
      (goto-char (point-min))
      (hy-mbox-beginning-of-mail)
      (cl-loop
       for ,next-mail-pos = (hy-mbox-mail-beginning-position 2)
       do (save-excursion
	    (save-restriction
	      (narrow-to-region (point) (or ,next-mail-pos (point-max)))
	      ,@body))
       while ,next-mail-pos
       do (goto-char ,next-mail-pos)))))

;;; For debug only
(cl-defun hy-mbox-each-mail-head ()
  (interactive)
  (let ((heads nil))
    (hy-mbox-for-each-mail
     (message "mail %d - %d" (point-min) (point-max))
     (push (buffer-substring (point-min) (line-end-position)) heads))
    (message "%s" (print heads))))

(cl-defun hy-mbox-next-mail-position ()
  (interactive)
  (message "%d" (or (hy-mbox-mail-beginning-position 2) (point-max))))

(provide 'hy-mbox)
