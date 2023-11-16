(require 'ert)
(require 'ert-x)
(require 'eiffel-ts-mode)

(defmacro eiffel-with-temp-buffer (contents &rest body)
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,contents)
     (eiffel-ts-mode)
     ,@body))

(defun eiffel-should-indent (content column)
  "Assert indentation COLUMN on the last line of CONTENT."
  (eiffel-with-temp-buffer (eiffel-test-string content)
    (indent-according-to-mode)
    (should (= (current-indentation) column))))

(defun eiffel-should-indent-buffer (expected content)
  "Assert that CONTENT turns into EXPECTED after the buffer is re-indented."
  (eiffel-with-temp-buffer (eiffel-test-string content)
    (indent-region (point-min) (point-max))
    (should (string= (eiffel-test-string expected) (buffer-string)))))

(defun eiffel-test-string (s &rest args)
  (apply 'format (replace-regexp-in-string "^[ \t]*|" "" s) args))

(ert-deftest eiffel-indent-class-feature-body ()
  (eiffel-should-indent-buffer
   "class A
   |feature
   |  x
   |    do
   |    end
   |end"
   "  class A
   |      feature
   | x
   |do
   |end
   |end"))

(ert-deftest eiffel-class-is-0-indent ()
  "It should indent class to beginning of line"
  (eiffel-should-indent "  class A" 0))

(ert-deftest eiffel-class-feature-after-class-is-0-indent ()
  "It should indent 'feature' to beginning of line after class"
  (eiffel-should-indent "class A\n    feature" 0))

(ert-deftest eiffel-class-feature-identifier-is-0-indent ()
  "It should indent feature new_feature to 2"
  (eiffel-should-indent
   "class A
   |feature
   |x"
   2))

(ert-deftest eiffel-class-feature-identifier-do-is-4-indent ()
  "It should indent feature new_feature do to 4"
  (eiffel-should-indent
   "class A
   |feature
   |  x
   |do"
   4))

(ert-deftest eiffel-class-feature-identifier-do-end-is-4-indent ()
  "It should indent feature new_feature do end to 4"
  (eiffel-should-indent
   "class A
   |feature
   |  x
   |    do
   |end"
   4))

(ert-deftest eiffel-conditional-indent ()
  "It should indent conditional"
  (eiffel-should-indent-buffer
   "class A
   |feature
   |  x
   |    do
   |      if a then
   |        x := 1
   |      elseif b then
   |        x := 2
   |      else
   |        x := 3
   |      end"
   "class A
   |feature
   |x
   |do
   |if a then
   |x := 1
   |elseif b then
   |x := 2
   |else
   |x := 3
   |end"))
