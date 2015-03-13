;;; jade-mode.el --- Major mode for editing .jade files
;;;
;;; URL: https://github.com/brianc/jade-mode
;;; Author: Brian M. Carlson and other contributors
;;; inspired by http://xahlee.org/emacs/elisp_syntax_coloring.html
(require 'font-lock)

(defun jade-debug (string &rest args)
  "Prints a debug message"
  (apply 'message (append (list string) args)))

(defmacro jade-line-as-string ()
  "Returns the current line as a string."
  `(buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun jade-empty-line-p ()
  "If line is empty or not."
  (= (point-at-eol) (point-at-bol)))

(defun jade-blank-line-p ()
  "Returns t when line contains only whitespace chars, nil otherwise."
  (string-match-p "^\\s-*$" (jade-line-as-string)))

;; command to comment/uncomment text
(defun jade-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let (
        (comment-start "//") (comment-end "")
        )
    (comment-dwim arg)))

(defconst jade-keywords
  (eval-when-compile
    (regexp-opt
     '("if" "else" "for" "in" "each" "case" "when" "default" "block" "extends"
       "block append" "block prepend" "append" "prepend"
       "include" "yield" "mixin") 'words))
  "Jade keywords.")

(defvar jade-double-quote-string-re "[\"]\\(\\\\.\\|[^\"\n]\\)*[\"]"
  "Regexp used to match a double-quoted string literal")

(defvar jade-single-quote-string-re "[']\\(\\\\.\\|[^'\n]\\)*[']"
  "Regexp used to match a single-quoted string literal")

(defvar jade-font-lock-keywords
  `(
    ;; higlight string literals on lines beginning with an equals sign
    ;; TODO improve this to play nice with attribute assignments in
    ;; parentheses following tags
    (,(concat "^\\s-*"
              "=")
              (,(concat jade-single-quote-string-re "\\|" jade-double-quote-string-re)
               nil
               nil
               (0 font-lock-string-face)))

    (,"!!!\\|doctype\\( ?[A-Za-z0-9\-\_]*\\)?" 0 font-lock-comment-face) ;; doctype
    (,jade-keywords . font-lock-keyword-face) ;; keywords
    (,"#\\(\\w\\|_\\|-\\)*" . font-lock-variable-name-face) ;; id
    (,"\\(?:^[ {2,}]*\\(?:[a-z0-9_:\\-]*\\)\\)?\\(#[A-Za-z0-9\-\_]*[^ ]\\)" 1 font-lock-variable-name-face) ;; id
    (,"\\(?:^[ {2,}]*\\(?:[a-z0-9_:\\-]*\\)\\)?\\(\\.[A-Za-z0-9\-\_]*\\)" 1 font-lock-type-face) ;; class name
    (,"^[ {2,}]*[a-z0-9_:\\-]*" 0 font-lock-function-name-face) ;; tag name
    (,"^\\s-*\\(//.*\\)" 1 font-lock-comment-face t) ;; jade block comments

    ;; remove highlighting from lines opening with a pipe `|'
    ;; e.g. | keywords like for should not be highlighted here
    ;;      | I'm not supposed to highlight single quotes either
    (,(concat "^\\s-*"
              "\\("
              "|"
              ".*"
              "\\)") 1 nil t)))

;; syntax table
(defvar jade-syntax-table
  (let ((syn-table (make-syntax-table)))
    syn-table)
  "Syntax table for `jade-mode'.")

(defun jade-region-for-sexp ()
  "Selects the current sexp as the region"
  (interactive)
  (beginning-of-line)
  (let ((ci (current-indentation)))
    (push-mark nil nil t)
    (while (> (jade-next-line-indent) ci)
      (next-line)
      (end-of-line))))

(defun jade-indent ()
  "Indent current region or line.
Calls `jade-indent-region' with an active region or `jade-indent-line'
without."
  (interactive)
  (if (region-active-p)
      (jade-indent-region

       ;; use beginning of line at region-beginning
       (save-excursion
         (goto-char (region-beginning))
         (line-beginning-position))

       ;; use end of line at region-end
       (save-excursion
         (goto-char (region-end))
         (line-end-position)))
    (jade-indent-line)))

(defun jade-indent-line ()
  "Indent current line of jade code.
If the cursor is left of the current indentation, then the first call
will simply jump to the current indent. Subsequent calls will indent
the current line by `jade-tab-width' until current indentation is
nested one tab-width deeper than its parent tag. At that point, an
additional call will reset indentation to column 0."
  (interactive)
  (let ((left-of-indent (>= (current-column) (current-indentation)))
        (indent (jade-calculate-indent-target)))
    (if left-of-indent

        ;; if cursor is at or beyond current indent, indent normally
        (indent-line-to indent)

      ;; if cursor is trailing current indent, first indentation should
      ;; jump to the current indentation column (subsequent calls
      ;; will indent normally)
      (indent-line-to (current-indentation)))))

(defun jade-indent-region (start end)
  "Indent active region according to indentation of region's first
line relative to its parent. Keep region active after command
terminates (to facilitate subsequent indentations of the same region)"

  (interactive "r")
  (save-excursion

    ;; go to start of region so we can find out its target indent
    (goto-char start)

    ;; keep region active after command
    (let* ((deactivate-mark)

           ;; find indent target for first line
           (first-line-indent-target (jade-calculate-indent-target))

           ;; use current-indentation to turn target indent into
           ;; a relative indent to apply to each line in region
           (first-line-relative-indent
            (- first-line-indent-target (current-indentation))))

      ;; apply relative indent
      (indent-rigidly start end first-line-relative-indent))))

(defun jade-calculate-indent-target ()
  "Return the column to which the current line should be indented."
  (let ((max-indent (+ (jade-previous-line-indentation) jade-tab-width)))
    (if (>= (current-indentation) max-indent) ;; if at max indentation
        0
      (+ (current-indentation) jade-tab-width))))

(defun jade-unindent ()
  "Unindent active region or current line."
  (interactive)
  (if (region-active-p)
      (jade-unindent-region

       ;; use beginning of line at region-beginning
       (save-excursion
         (goto-char (region-beginning))
         (line-beginning-position))

       ;; use end of line at region-end
       (save-excursion
         (goto-char (region-end))
         (line-end-position)))

    ;; when no region is active
    (jade-unindent-line)
    ))

(defun jade-unindent-line ()
  "Unindent line under point by `jade-tab-width'.
Calling when `current-indentation' is 0 will have no effect."
  (indent-line-to
   (max
    (- (current-indentation) jade-tab-width)
    0)))

(defun jade-unindent-region (start end)
  "Unindent active region by `jade-tab-width'.
Follows indentation behavior of `indent-rigidly'."

  (interactive "r")
  (let (deactivate-mark)
    (indent-rigidly start end (- jade-tab-width))))

(defun jade-previous-line-indentation ()
  "Get the indentation of the previous (non-blank) line (from point)."
  (interactive)
  (save-excursion

    ;; move up to the nearest non-blank line (or buffer start)
    (while (progn ;; progn used to get do...while control flow
             (forward-line -1)
             (and (jade-blank-line-p) (not (= (point-at-bol) (point-min))))))
    (let ((prev-line-indent (current-indentation)))
      prev-line-indent)))

(defun jade-newline-and-indent ()
  "Insert newline and indent to parent's indentation level."
  (interactive)
  (newline)
  (indent-line-to (max (jade-previous-line-indentation) 0))
  )

(defvar jade-mode-map (make-sparse-keymap))

;; mode declaration
;;;###autoload
(define-derived-mode jade-mode fundamental-mode
  "Jade"
  "Major mode for editing jade node.js templates"
  :syntax-table jade-syntax-table

  ;; turn off electric indent mode for jade buffers (by default, at least)
  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode 0))
  (setq mode-name "Jade")
  (setq major-mode 'jade-mode)

  ;; comment syntax
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "//\\s-*")

  (setq-default jade-tab-width 2)
  (setq-local indent-line-function 'jade-indent-line)
  (set (make-local-variable 'indent-region-function) 'jade-indent-region)
  (setq-local indent-tabs-mode nil)

  ;; keymap
  (use-local-map jade-mode-map)

  ;; modify the keymap
  (define-key jade-mode-map [remap comment-dwim] 'jade-comment-dwim)
  (define-key jade-mode-map [tab] 'jade-indent)
  (define-key jade-mode-map [backtab] 'jade-unindent)
  (define-key jade-mode-map (kbd "RET") 'jade-newline-and-indent)

  ;; highlight syntax
  (setq font-lock-defaults '(jade-font-lock-keywords)))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))

(provide 'jade-mode)
;;; jade-mode.el ends here
