(ert-deftest jade-mode-command-should-be-bound ()
  (with-temp-buffer
    (should (fboundp 'jade-mode))
    (should (null (jade-mode)))))

(ert-deftest jade-mode-highlight-doctype ()
  (jade-test-with-temp-buffer-pt-min
      "doctype html\n\n"
    (should (eq (get-text-property 1 'face)
                'font-lock-comment-face))
    (should (eq
             (next-single-property-change (point) 'face)
             (point-at-eol)))))

(ert-deftest jade-mode-highlights-in-isolation ()
  (jade-test-highlight-one-word "mixin" 'font-lock-keyword-face 2)

  (jade-test-highlight-one-word "#container" 'font-lock-variable-name-face 2)
  (jade-test-highlight-one-word ".class" 'font-lock-type-face 2)
  (jade-test-highlight-one-word "// this is a comment" 'font-lock-comment-face 2)
  (jade-test-highlight-one-word "//- this is a comment" 'font-lock-comment-face 2)
  (jade-test-highlight-one-word "-// this is a comment" 'font-lock-comment-face 2)
  (jade-test-highlight-one-word "head" 'font-lock-function-name-face 0)
  (jade-test-highlight-one-word "body" 'font-lock-function-name-face 2)
  (jade-test-highlight-one-word "doctype html" 'font-lock-comment-face 0))
