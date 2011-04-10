;; copied from http://xahlee.org/emacs/elisp_syntax_coloring.html
(require 'font-lock)

(defun jade-debug (string &rest args)
  "Prints a debug message"
  (apply 'message (append (list string) args)))

(defmacro jade-line-as-string ()
  "Returns the current line as a string."
  `(buffer-substring (point-at-bol) (point-at-eol)))


(defun jade-empty-line-p ()
  "If line is empty or not."
  (= (point-at-eol) (point-at-bol)))

(defun jade-blank-line-p ()
  "If line contains only spaces."
  (string-match-p "^[ ]*$" (jade-line-as-string)))

(setq jade-font-lock-keywords
      `((,"!!!\\( \\(default\\|5\\|transitional\\)\\)?" 0 font-lock-constant-face) ;; doctype
        (,"#\\(\\w\\|_\\|-\\)*" . font-lock-type-face) ;; id
        (,"\\(?:^[ {2,}]+\\(?:[a-z0-9_:\\-]*\\)\\)?\\(#[A-Za-z0-9\-\_]*[^ ]\\)" 1 font-lock-type-face) ;; class name
        (,"\\(?:^[ {2,}]+\\(?:[a-z0-9_:\\-]*\\)\\)?\\(\\.[A-Za-z0-9\-\_]*\\)" 1 font-lock-function-name-face) ;; class name
        (,"^[ {2,}]+[a-z0-9_:\\-]*" 0 font-lock-comment-face)))

(defun jade-region-for-sexp ()
  "Selects the current sexp as the region"
  (interactive)
  (beginning-of-line)
  (let ((ci (current-indentation)))
    (push-mark nil nil t)
    (while (> (jade-next-line-indent) ci)
      (next-line)
      (end-of-line))))

(defvar jade-mode-map (make-sparse-keymap))
;;defer to sws-mode
;;(define-key jade-mode-map [S-tab] 'jade-unindent-line)

;; mode declaration
(define-derived-mode jade-mode sws-mode
  "Jade"
  "Major mode for editing jade node.js templates"
  (kill-all-local-variables)
  (setq tab-width 2)

  (setq mode-name "Jade")
  (setq major-mode 'jade-mode)

  ;; default tab width
  (setq sws-tab-width 2)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'sws-indent-line)
  (make-local-variable 'indent-region-function)

  (setq indent-region-function 'sws-indent-region)


  ;; keymap
  (use-local-map jade-mode-map)

  ;; highlight syntax
  (setq font-lock-defaults '(jade-font-lock-keywords)))

(provide 'jade-mode)

(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
