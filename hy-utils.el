;; -*- lexical-binding: t -*-

(require 'cl-lib)

(cl-defmacro hy-with-gensyms ((&rest syms) &body body)
  `(let ,(mapcar (lambda (sym) `(,sym (cl-gensym))) syms)
     ,@body))

(defconst +hy-macro-expand-buffer+ "*hy macro expand*")

(cl-defun hy-macroexpand-last-sexp (&optional arg)
  (interactive "P")
  (let* ((debug-on-error t)
	 (sexp (preceding-sexp))
	 (expanded (if arg
		       (macroexpand-all sexp)
		     (macroexpand sexp))))
    (with-current-buffer (get-buffer-create +hy-macro-expand-buffer+)
      (erase-buffer)
      (emacs-lisp-mode)
      (insert (pp expanded))
      (goto-char 1)
      (display-buffer (current-buffer)))))

(defconst +hy-to-strip-chars+ " \t\r\n\f\v")

(cl-defun hy-string-rstrip (str)
  (cl-loop
   for n from (1- (length str)) downto 0
   while (cl-find (aref str n) +hy-to-strip-chars+)
   finally (cl-return (substring str 0 (1+ n)))))

(cl-defun hy-rstrip ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (cl-find (char-before) +hy-to-strip-chars+)
      (delete-char -1))))

(cl-defmacro hy-save-reset-buffer-state (&body body)
  `(save-excursion
     (save-restriction
       (widen)
       (goto-char 1)
       ,@body)))

(cl-defmacro hy-with-temp-buffer-let (var &body body)
  `(with-temp-buffer
     (let ((,var (current-buffer)))
       ,@body)))

(cl-defmacro hy-with-generate-new-buffer (name &body body)
  `(with-current-buffer (generate-new-buffer ,name)
     ,@body))

(cl-defmacro hy-with-generate-new-buffer-let ((var name) &body body)
  `(with-generate-new-buffer ,name
     (let ((,var (current-buffer)))
       ,@body)))

(cl-defun hy-delete-whole-line ()
  (let ((line-begin (line-beginning-position)))
    (beginning-of-line 2)
    (delete-region line-begin (point))))

(provide 'hy-utils)
