;; copied from http://xahlee.org/emacs/elisp_syntax_coloring.html
(require 'font-lock)

(defvar jade-function-regexp "[A-Za-z]")
(defvar jade-tab-width 2)

(defun jade-debug (string &rest args)
  "Prints a debug message"
  (apply 'message (append (list string) args)))


(defun jade-indent-line ()
  "Indents current line"
  (interactive)
  (if (= (point) (point-at-bol))
      (insert-tab)
    (save-excursion
      (let ((prev-indent 0) (cur-indent 0))
        ;; Figure out the indentation of the previous line
        (setq prev-indent)

        ;; Figure out the current line's indentation
        (setq cur-indent (current-indentation))

        ;; Shift one column to the left
        (beginning-of-line)
        (insert-tab)

        (when (= (point-at-bol) (point))
          (forward-char 2))


        ;; We're too far, remove all indentation.
        ;;         (when (> (- (current-indentation) prev-indent) coffee-tab-width)
        ;;           (backward-to-indentation 0)
        ;;           (delete-region (point-at-bol) (point)))
        ))))

(setq jade-font-lock-keywords
      `((,"!!!\\( \\(default\\|5\\|transitional\\)\\)?" . font-lock-constant-face)
        (,"#\\(\\w\\|_\\|-\\)*" . font-lock-type-face)
        (,"\\.[A-Za-z0-9\-\_]*" . font-lock-function-name-face)
        (,"^ *\\<\\([A-Za-z0-9]*\\)\\>" . font-lock-keyword-face)))


;; mode declaration
(define-derived-mode jade-mode fundamental-mode
  "Jade"
  "Major mode for editing jade node.js templates"
  ;; ...
  (kill-all-local-variables)
  (setq tab-width 2)
  ;; code for syntax highlighting
  (setq major-mode 'jade-mode)


  (setq font-lock-defaults '(jade-font-lock-keywords))
  (setq mode-name "Jade")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'jade-indent-line)
  ;; no tabs
  (setq indent-tabs-mode nil))

(provide 'jade-mode)

(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
