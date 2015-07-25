;;; test-helper.el --- macros and functions for testing jade-mode.el based on python-mode.el ert test-suite

;; Copyright (C) 2015

;; Author:  <matt@Akshobya>

;; test highlighting based on python mode https://github.com/jkitchin/jmax/blob/master/python-mode/test/py-ert-tests.el

(defmacro jade-test-with-temp-buffer-pt-max (contents &rest body)
  "Creates a temp buffer in `jade-mode' inserting CONTENTS. BODY is code to be executed within temp buffer. Point is placed at end-of-buffer"
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (jade-mode)
     (insert ,contents)
     (font-lock-fontify-buffer)
     ,@body))

(defmacro jade-test-with-temp-buffer-pt-min (contents &rest body)
  "Creates a temp buffer in `jade-mode' inserting CONTENTS. BODY is code to be executed within temp buffer. Point is placed at beginning-of-buffer"
  (declare (indent 1))
  `(jade-test-with-temp-buffer-pt-max
     ,contents
     ,@(cons '(goto-char (point-min)) body)))

(defun jade-test-go-to (string)
  (and (eq (point) (point-max)) (goto-char (point-min)))
  (search-forward string nil t 1))

(defun jade-test--show-tested-text (text form)
  form)

(defmacro jade-test-highlight-one-word (word face n)
  `(jade-test-with-temp-buffer-pt-min
       ,(concat (s-repeat n "\t") word "\n\n")
     (print (buffer-string))
     (should (eq (jade-test--show-tested-text
                  ,word
                  (get-text-property
                   ,(+ 1 n) 'face))
                 ,face))
     (goto-char ,(+ 1 n))
     (should (eq
              (next-single-property-change (point) 'face)
              (point-at-eol)))))




(byte-to-string 108)
