(require 'treesit)

(require 'prog-mode)

;;;###autoload
(define-derived-mode eiffel-ts-mode prog-mode "Eiffel[ts]"
  "Major mode for editing Eiffel with tree-sitter."

  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'eiffel)
    (treesit-parser-create 'eiffel)
    (eiffel-ts-setup)))

(defvar eiffel-ts-font-lock-rules
  '(
    :language eiffel
    :override t
    :feature eiffel-highlight
    ([["class" "frozen" "feature" "end" "do" "alias" "convert"
       "invariant" "across" "as" "loop" "check"
       "if" "attached" "then" "else" "elseif"
       "note" "local" "create" "require" "ensure"
       "from" "variant" "until" "not" "and" "and then" "or" "or else" "xor"
       ]]
     @font-lock-keyword-face

     [["(" ")" "[" "]" "<<" ">>"]] @font-lock-bracket-face

     [["," ":"]] @font-lock-delimiter-face

     [[(unary) ":=" (binary_caret) (binary_mul_div) (binary_plus_minus)
       (binary_comparison) (binary_and) (binary_or) (binary_implies)
       (comparison)]]
     @font-lock-operator-face

     [(boolean_constant) (current) (result) (void) (current)] @font-lock-constant-face

     (header_comment) @font-lock-doc-face
     (comment) @font-lock-comment-face
     (class_name) @font-lock-type-face

     [(verbatim_string) (basic_manifest_string)] @font-lock-string-face

     [(integer_constant) (real_constant)] @font-lock-number-face

     (extended_feature_name (identifier) @font-lock-function-name-face)

     (entity_declaration_group (identifier) @font-lock-variable-name-face)

     ;; Highlight the modified value in calls:
     ;; x := 1  ;  a.b.c := 1
     ;; ^              ^
     (assignment (call (_) @font-lock-variable-name-face :anchor))
    )))

(defvar eiffel-ts-indent-rules
  `((eiffel
     ((node-is "class_declaration") parent 0)
     ((parent-is "notes") parent 2)

     ((match "class_name" "class_declaration") parent 2)
     ((node-is "header_comment") parent 4)
     ((node-is "comment") parent 2)
     ((parent-is "class_declaration") parent 0)

     ((parent-is "creation_clause") parent 2)

     ((node-is "feature_clause") parent 0)
     ((node-is "feature_declaration") parent 2)
     ((node-is "feature_body") parent 0)
     ((parent-is "feature_body") parent 0)
     ((node-is "attribute_or_routine") parent 2)
     ((parent-is "attribute_or_routine") parent 0)
     ((node-is "end") parent 0)

     ((parent-is "postcondition") parent 2)
     ((parent-is "precondition") parent 2)

     ((node-is "entity_declaration_list") parent 2)
     ((parent-is "entity_declaration_list") parent 0)

     ((parent-is "then_part") parent 0)
     ((node-is "instruction") parent 2)
     ((node-is "invariant") parent 0)
     ((parent-is "invariant") parent 2)
     ((node-is "exit_condition") parent 0)
     ((parent-is "exit_condition") parent 2)
     ((node-is "loop_body") parent 0)
     ((node-is "variant") parent 0)
     ((parent-is "variant") parent 2)

     ((and no-node (parent-is "attribute_or_routine")) parent 2)
     ((and no-node (parent-is "source_file")) parent 2)
     ((and no-node (parent-is "routine_mark")) parent 2)
     (no-node prev-sibling 0)
     )))

(defun eiffel-ts-setup ()
  "Setup treesit for eiffel-ts-mode."

  (setq-local treesit-font-lock-feature-list '((identifier-simple eiffel-highlight)))

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     eiffel-ts-font-lock-rules))

  (setq-local treesit-simple-indent-rules eiffel-ts-indent-rules)

  (setq treesit--indent-verbose t)

  (treesit-major-mode-setup))
