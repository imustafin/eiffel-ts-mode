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
   |	x
   |		do
   |		end
   |end"
   "	class A
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
   |	x
   |		do
   |end"
   4))

(ert-deftest eiffel-conditional-indent ()
  "It should indent conditional"
  (eiffel-should-indent-buffer
   "class A
   |feature
   |	x
   |		do
   |			if a then
   |				x := 1
   |			elseif b then
   |				x := 2
   |			else
   |				x := 3
   |			end"
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

(ert-deftest eiffel-class-notes-indent ()
  "It should indent class' notes"
  (eiffel-should-indent-buffer
   "note
   |	a: 1
   |class A
   |end"
   "note
   |a: 1
   |class A
   |end"))

(ert-deftest eiffel-class-classname-indent ()
  "It should indent class' class_name"
  (eiffel-should-indent-buffer
   "class
   |	A"
   "class
   |A"))

(ert-deftest eiffel-class-create-indent ()
  "It should indent class' create"
  (eiffel-should-indent-buffer
   "class A
   |create
   |	f"
   "class A
   |create
   |f"))

(ert-deftest eiffel-locals-indent ()
  "It should indent feature locals"
  (eiffel-should-indent-buffer
   "class A
   |feature
   |	x
   |		local
   |			a: A"
   "class A
   |feature
   |x
   |local
   |a: A"))

(ert-deftest eiffel-feature-header-comment-indent ()
  "It should indent feature header comments"
  (eiffel-should-indent-buffer
   "class A
   |feature
   |	x
   |			-- header"
   "class A
   |feature
   |x
   |-- header"))

(ert-deftest eiffel-feature-precondition-indent ()
  "It should indent feature precondition"
  (eiffel-should-indent-buffer
   "class A
   |feature
   |	x
   |		require
   |			True
   |		deferred
   |		ensure
   |			False
   |		end"
   "class A
   |feature
   |x
   |require
   |True
   |deferred
   |ensure
   |False
   |end"))

(ert-deftest eiffel-loop-indent ()
  "It should indent loop"
  (eiffel-should-indent-buffer
   "class A
   |feature
   |	x
   |		do
   |			from
   |				i := 1
   |			invariant
   |				True
   |			until
   |				i = 10
   |			loop
   |				i := i + 1
   |			variant
   |				10 - i
   |			end"
   "class A
   |feature
   |x
   |do
   |from
   |i := 1
   |invariant
   |True
   |until
   |i = 10
   |loop
   |i := i + 1
   |variant
   |10 - i
   |end"))
