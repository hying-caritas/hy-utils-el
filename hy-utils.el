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
