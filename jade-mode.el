;;; jade-mode.el --- Major mode for editing .jade files
;;;
;;; URL: https://github.com/brianc/jade-mode
;;; Author: Brian M. Carlson and other contributors
;;; Package-Requires: ((sws-mode "0"))
;;;
;;; copied from http://xahlee.org/emacs/elisp_syntax_coloring.html
(require 'font-lock)
(require 'sws-mode)

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
      `((,"!!!\\( ?[A-Za-z0-9\-\_]*\\)?" 0 font-lock-comment-face) ;; doctype
        (,"#\\(\\w\\|_\\|-\\)*" . font-lock-variable-name-face) ;; id
        (,"\\(?:^[ {2,}]+\\(?:[a-z0-9_:\\-]*\\)\\)?\\(#[A-Za-z0-9\-\_]*[^ ]\\)" 1 font-lock-variable-name-face) ;; id
        (,"\\(?:^[ {2,}]+\\(?:[a-z0-9_:\\-]*\\)\\)?\\(\\.[A-Za-z0-9\-\_]*\\)" 1 font-lock-type-face) ;; class name
        (,"^[ {2,}]+[a-z0-9_:\\-]*" 0 font-lock-function-name-face))) ;; tag name

;; syntax table
(defvar jade-syntax-table nil "Syntax table for `jade-mode'.")
(setq jade-syntax-table
      (let ((syn-table (make-syntax-table)))

        (modify-syntax-entry ?\/ ". 12b" syn-table)
        (modify-syntax-entry ?\n "> b" syn-table)

        syn-table))

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
;;;###autoload
(define-derived-mode jade-mode sws-mode
  "Jade"
  "Major mode for editing jade node.js templates"
  :syntax-table jade-syntax-table

  (setq tab-width 2)

  (setq mode-name "Jade")
  (setq major-mode 'jade-mode)

  ;; keymap
  (use-local-map jade-mode-map)

  ;; highlight syntax
  (setq font-lock-defaults '(jade-font-lock-keywords)))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(provide 'jade-mode)
;;; jade-mode.el ends here
