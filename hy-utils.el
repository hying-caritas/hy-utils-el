;; -*- lexical-binding: t -*-

(require 'cl-lib)

(cl-defmacro hy-with-gensyms ((&rest syms) &body body)
  `(let ,(mapcar (lambda (sym) `(,sym (cl-gensym))) syms)
     ,@body))

(defconst +hy-macro-expand-buffer+ "*hy macro expand*")

(cl-defun hy-macroexpand-last-sexp (&optional arg)
  (interactive "P")
  (let* ((debug-on-error t)
	 (sexp (elisp--preceding-sexp))
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

(cl-defun hy-maplist2 (function list &rest more-lists)
  (cl-labels ((maplist2 (flist result)
	       (if (cl-notany #'null flist)
		   (maplist2 (cl-mapcar #'cddr flist)
			     (cons (apply function flist) result))
		 result)))
    (nreverse (maplist2 (cons list more-lists) nil))))

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

(cl-defun hy-setenvs (names values)
  (cl-loop
   for name in names
   for val in values
   do (setenv name val)))

(cl-defmacro hy-with-env ((&rest raw-name-value-list) &body body)
  (hy-with-gensyms
   (name-value-list names values old-values)
   `(let* ((,name-value-list (list ,@raw-name-value-list))
	   (,names (hy-maplist2 #'car ,name-value-list))
	   (,values (hy-maplist2 #'car (cdr ,name-value-list)))
	   (,old-values (cl-mapcar #'getenv ,names)))
      (unwind-protect
	  (progn
	    (hy-setenvs ,names (append ,values '(nil)))
	    ,@body)
	(hy-setenvs ,names ,old-values)))))

(cl-defun hy-add-to-alist (key val alist &key (test #'equal))
  (cons (cons key val) (cl-remove key alist :test test :key #'car)))

(cl-defmacro hy-add-to-alistf (key val alist &key (test '#'equal))
  (gv-letplace (getter setter) alist
    (funcall setter `(hy-add-to-alist ,key ,val ,getter :test ,test))))

(provide 'hy-utils)
