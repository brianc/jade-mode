;; copied from http://xahlee.org/emacs/elisp_syntax_coloring.html
(require 'font-lock)

(defvar jade-tab-width 2)

(defun jade-debug (string &rest args)
  "Prints a debug message"
  (apply 'message (append (list string) args)))


(defun jade-indent-line ()
  "Indents current line")

(defun jade-indent-line ()
  "Indents the line."
  (interactive)
  (jade-debug "TODO: implement this"))

(setq jade-font-lock-keywords
      `((,"!!!\\( \\(default\\|5\\|transitional\\)\\)?" 0 font-lock-constant-face) ;; doctype
        (,"#\\(\\w\\|_\\|-\\)*" . font-lock-type-face) ;; id
        (,"\\(?:^[ {2,}]+\\(?:[a-z0-9_:\\-]*\\)\\)?\\(#[A-Za-z0-9\-\_]*[^ ]\\)" 1 font-lock-type-face) ;; class name
        (,"\\(?:^[ {2,}]+\\(?:[a-z0-9_:\\-]*\\)\\)?\\(\\.[A-Za-z0-9\-\_]*\\)" 1 font-lock-function-name-face) ;; class name
        (,"^[ {2,}]+[a-z0-9_:\\-]*" 0 font-lock-comment-face)))

;; mode declaration
(define-derived-mode jade-mode fundamental-mode
  "Jade"
  "Major mode for editing jade node.js templates"
  (kill-all-local-variables)
  (setq tab-width 2)

  (setq mode-name "Jade")
  (setq major-mode 'jade-mode)

  ;; highlight syntax
  (setq font-lock-defaults '(jade-font-lock-keywords))

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'jade-indent-line)
  ;; no tabs
  (setq indent-tabs-mode nil))

(provide 'jade-mode)

(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
