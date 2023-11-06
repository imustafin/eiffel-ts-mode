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
    ((comment) @font-lock-comment-face
     (comment_start) @font-lock-comment-delimiter-face

     (boolean_constant) @font-lock-constant-face
     (current) @font-lock-constant-face
     (result) @font-lock-constant-face

     (character_constant) @font-lock-string-face
     (integer_constant) @font-lock-number-face
     (real_constant) @font-lock-number-face
     (manifest_string) @font-lock-string-face

     (class_declaration "class" @font-lock-keyword-face
                        (class_name) @font-lock-type-face
                        "end" @font-lock-keyword-face)

     (feature_clause "feature" @font-lock-keyword-face)

     (feature_declaration [(identifier) @font-lock-function-name-face
                           (formal_arguments ["(" ")"] @font-lock-bracket-face)
                           (attribute_or_routine ["end"] @font-lock-keyword-face)])

     (routine_mark "do" @font-lock-keyword-face)


     (variable (identifier) @font-lock-variable-use-face)

     (assignment ":=" @font-lock-operator-face)

     (entity_declaration_group
      (identifier_list [(identifier) @font-lock-variable-name-face
                        "," @font-lock-delimiter-face])
      ":" @font-lock-delimiter-face
      ((type) @font-lock-type-face))
    )))


(defvar eiffel-ts-indent-rules
  `((eiffel
     ((node-is "features") parent 0)
     ((node-is "feature_declaration") parent 2)
     ((node-is "attribute_or_routine") parent 2)
     ((node-is "end") parent 0)

     ((node-is "compound") parent 2)
     ((node-is "instruction") parent 0)

     ((and no-node (parent-is "attribute_or_routine")) parent 2)
     ((and no-node (parent-is "source_file")) parent 2)
     ((and no-node (parent-is "routine_mark")) parent 2)
     (no-node parent-bol 0)

     )))

(defun eiffel-ts-setup ()
  "Setup treesit for eiffel-ts-mode."

  (setq-local treesit-font-lock-feature-list '((eiffel-highlight)))

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     eiffel-ts-font-lock-rules))

  (setq-local treesit-simple-indent-rules eiffel-ts-indent-rules)

  (setq treesit--indent-verbose t)

  (treesit-major-mode-setup))
