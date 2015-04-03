(require 'ert)
(require 'jade-mode)

(ert-deftest jade-mode-get-line-indentations ()
  (with-temp-buffer
    (insert "doctype html\nhtml\n  body\n    div\n      p content inside body\n")
    (jade-mode)

    ;; go to <div> line (at d of div)
    (goto-char 30)
    (should (looking-at "d"))
    (should (= (current-indentation) 4))
    (should (= (jade-next-line-indentation) 6))
    (should (= (jade-previous-line-indentation) 2))

    (beginning-of-line)
    (should (looking-at "^"))
    (should (= (current-indentation) 4))
    (should (= (jade-next-line-indentation) 6))
    (should (= (jade-previous-line-indentation) 2))

    (end-of-line)
    (should (looking-at "$"))
    (should (= (current-indentation) 4))
    (should (= (jade-next-line-indentation) 6))
    (should (= (jade-previous-line-indentation) 2))))
