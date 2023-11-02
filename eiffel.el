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
    :feature comment
    ((comment) @font-lock-comment-face)

    :language eiffel
    :override t
    :feature keyword
    ((class_declaration "class" @font-lock-keyword-face
                        (class_name) @font-lock-type-face
                        "end" @font-lock-keyword-face))

    :language eiffel
    :override t
    :feature keyword
    ((feature_clause "feature" @font-lock-keyword-face))

    :language eiffel
    :override t
    :feature keyword
    ((feature_declaration [(identifier) @font-lock-function-name-face
                           (formal_arguments ["(" ")"] @font-lock-bracket-face)
                           (attribute_or_routine ["end"] @font-lock-keyword-face)]))

    :language eiffel
    :override t
    :feature keyword
    ((routine_mark "do" @font-lock-keyword-face))


    :language eiffel
    :override t
    :feature keyword
    ((boolean_constant ["True" "False"] @font-lock-constant-face))

    :language eiffel
    :override t
    :feature keyword
    ((variable (identifier) @font-lock-variable-use-face))

    :language eiffel
    :override t
    :feature keyword
    ((assignment ":=" @font-lock-operator-face))

    :language eiffel
    :override t
    :feature keyword
    ((entity_declaration_group
      (identifier_list [(identifier) @font-lock-variable-name-face
                        "," @font-lock-delimiter-face])
      ":" @font-lock-delimiter-face
      ((type) @font-lock-type-face)))
    ))

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

  (setq-local treesit-font-lock-feature-list
              '((comment keyword)))

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     eiffel-ts-font-lock-rules))

  (setq-local treesit-simple-indent-rules eiffel-ts-indent-rules)

  (setq treesit--indent-verbose t)

  (treesit-major-mode-setup))
