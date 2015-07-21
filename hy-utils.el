;; -*- lexical-binding: t -*-

(require 'cl-lib)

(cl-defmacro hy-with-gensyms ((&rest syms) &body body)
  `(let ,(mapcar (lambda (sym) `(,sym (cl-gensym))) syms)
     ,@body))

(cl-defmacro hy-save-reset-buffer (&body body)
  `(save-excursion
     (save-restriction
       (widen)
       (goto-char 1)
       ,@body)))

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

(provide 'hy-utils)
