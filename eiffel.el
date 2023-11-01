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
    ))

(defun eiffel-ts-setup ()
  "Setup treesit for eiffel-ts-mode."

  (setq-local treesit-font-lock-feature-list
              '((comment keyword)))

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     eiffel-ts-font-lock-rules))

  ; (setq-local treesit-simple-indent-rules eiffel-ts-indent-rules)

  (treesit-major-mode-setup))
