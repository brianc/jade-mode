;; copied from http://xahlee.org/emacs/elisp_syntax_coloring.html
(require 'font-lock)

(defvar jade-tab-width 2)

(defun jade-debug (string &rest args)
  "Prints a debug message"
  (apply 'message (append (list string) args)))

(defmacro jade-line-as-string ()
  "Returns the current line as a string."
  `(buffer-substring (point-at-bol) (point-at-eol)))


(defun jade-indent-line ()
  "Indents current line")

(defun jade-previous-indentation ()
  "Gets indentation for previous line"
  (save-excursion
    (previous-line)
    (current-indentation)))

(defun jade-should-indent-p ()
  "Whether or not line should be indented."
  ;; should only indent if previous line is indented at most one less
  (> (jade-previous-indentation) (- (current-indentation) 1)))

(defun jade-empty-line-p ()
  "If line is empty or not."
  (= (point-at-eol) (point-at-bol)))

(defun jade-blank-line-p ()
  "If line contains only spaces."
  (string-match-p "^[ ]*$" (jade-line-as-string)))

(defun jade-indent-line ()
  "Indents the line."
  (interactive)

  ;; indent straight to end on empty line
  (if (jade-empty-line-p)
      (indent-to (jade-previous-indentation))
    ;; otherwise indent if nesting is correct
    (save-excursion
      (if (jade-should-indent-p)
          (let ((ci (current-indentation)))
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to (+ jade-tab-width ci)))
        ;; if cannot indent, reset indentation
        (progn
          (beginning-of-line)
          (delete-horizontal-space)))))
  ;; move point to end of line on empty lines to make tabbing
  ;; more obvious
  (if (jade-blank-line-p)
      (move-end-of-line 1)))

(defun jade-max-indent ()
  "Max indents previous line."
  (indent-to (+ jade-tab-width (jade-previous-indentation))))

(defun jade-unindent-line ()
  "Unindents the current line"
  (interactive)
  (let ((ci (current-indentation)))
    (if (= ci 0)
        ;; no indentation, set to max
        (jade-max-indent)
      (progn
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to (- ci jade-tab-width))))))

(setq jade-font-lock-keywords
      `((,"!!!\\( \\(default\\|5\\|transitional\\)\\)?" 0 font-lock-constant-face) ;; doctype
        (,"#\\(\\w\\|_\\|-\\)*" . font-lock-type-face) ;; id
        (,"\\(?:^[ {2,}]+\\(?:[a-z0-9_:\\-]*\\)\\)?\\(#[A-Za-z0-9\-\_]*[^ ]\\)" 1 font-lock-type-face) ;; class name
        (,"\\(?:^[ {2,}]+\\(?:[a-z0-9_:\\-]*\\)\\)?\\(\\.[A-Za-z0-9\-\_]*\\)" 1 font-lock-function-name-face) ;; class name
        (,"^[ {2,}]+[a-z0-9_:\\-]*" 0 font-lock-comment-face)))

(defun jade-next-line-indent ()
  "Gets indentation level for next line."
  (save-excursion
    (next-line)
    (current-indentation)))

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
(define-key jade-mode-map [S-tab] 'jade-unindent-line)

;; mode declaration
(define-derived-mode jade-mode fundamental-mode
  "Jade"
  "Major mode for editing jade node.js templates"
  (kill-all-local-variables)
  (setq tab-width 2)

  (setq mode-name "Jade")
  (setq major-mode 'jade-mode)

  ;; keymap
  (use-local-map jade-mode-map)

  ;; highlight syntax
  (setq font-lock-defaults '(jade-font-lock-keywords))

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'jade-indent-line)
  ;; no tabs
  (setq indent-tabs-mode nil))

(provide 'jade-mode)

(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
