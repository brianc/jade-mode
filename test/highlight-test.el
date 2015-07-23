(ert-deftest jade-mode-command-should-be-bound ()
  (with-temp-buffer
    (should (fboundp 'jade-mode))
    (should (null (jade-mode)))))

(ert-deftest jade-mode-highlight-doctype ()
  (with-temp-buffer

    ;; interesting - if you omit the trailing newline in the string,
    ;; `font-lock-fontify-buffer' will freak out and fail with
    ;; end-of-buffer
    (insert "doctype html\nhtml content\n\n")
    (jade-mode)

    ;; temp buffers require explict fontification
    (font-lock-fontify-buffer)

    ;; face at char 1 should be `font-lock-comment-face'
    (should (eq
             (get-text-property 1  'face)
             'font-lock-comment-face))
    (goto-char 1)

    ;; face shouldn't change (from `font-lock-comment-face') b/t char
    ;; 1 and eol
    (should (=
             (next-single-property-change (point) 'face)
             (point-at-eol)))))
