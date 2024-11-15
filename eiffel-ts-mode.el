;;; eiffel-ts-mode.el --- Major mode for editing Eiffel using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Ilgiz Mustafin

;; Author: Ilgiz Mustafin
;; Version: 0.1
;; Keywords: eiffel languages tree-sitter
;; URL: https://github.com/imustafin/eiffel-ts-mode
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This file defines eiffel-ts-mode which is a major mode for editing
;; Eiffel files.

;;; License:


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'treesit)

(require 'prog-mode)


(defvar eiffel-ts-mode-syntax-table
	(let ((table (make-syntax-table)))
		(modify-syntax-entry ?\' "\"" table)
		(modify-syntax-entry ?\" "\"" table)

		(modify-syntax-entry ?% "\\" table)

		(modify-syntax-entry ?- ". 12" table)
		(modify-syntax-entry ?\n ">  " table)

		(modify-syntax-entry ?⟳ "(⟲" table)
		(modify-syntax-entry ?⟲ ")⟳" table)
		table)
	"Syntax table to use in `eiffel-ts-mode'.")

(defvar eiffel-ts-font-lock-rules
	'(
		:language eiffel
		:override t
		:feature eiffel-highlight
		([["class" "frozen" "feature" "end" "do" "alias" "convert"
			 "invariant" "across" "as" "loop" "check"
			 "if" "attached" "then" "else" "elseif"
			 "note" "local" "create" "require" "ensure"
			 "from" "variant" "until" "and" "and then" "or" "or else" "xor"
			 "deferred" "inherit" "redefine" "undefine"
			 "detachable" "old" "∀" "∃" "⟳" "⟲" "¦" "all" "some"
			 "implies" "once" (unary_not) "attribute" "agent" "like" "export" "all"
			 "rename" "inspect" "when" "Precursor" "obsolete"
			 "debug" "rescue" "assign" (retry)
			 "external" "separate"
			 ]]
		 @font-lock-keyword-face


		 [["(" ")" "[" "]" "<<" ">>"]] @font-lock-bracket-face

		 [["," ":"]] @font-lock-delimiter-face

		 [[(unary) ":=" (binary_caret) (binary_mul_div) (binary_plus_minus)
			 (binary_comparison) (binary_and) (binary_or) (binary_implies)
			 (comparison)]]
		 @font-lock-operator-face

		 (at_cursor ["@" (identifier)] @font-lock-negation-char-face)

		 [(boolean_constant) (current) (result) (void) (current)] @font-lock-constant-face

		 (header_comment) @font-lock-doc-face
		 (comment) @font-lock-comment-face
		 (class_name) @font-lock-type-face

		 (anchored (call (_) @font-lock-variable-use-face :anchor))

		 (call_agent [(agent_target) @font-lock-property-use-face
									(agent_unqualified (identifier) @font-lock-function-call-face)])

		 (special_text) @font-lock-escape-face
		 [(verbatim_string) (basic_manifest_string) (character_constant)] @font-lock-string-face

		 [(integer_constant) (real_constant)] @font-lock-number-face

		 (extended_feature_name (identifier) @font-lock-function-name-face)
		 (assigner_mark (identifier) @font-lock-property-use-face)

		 (iteration (identifier) @font-lock-variable-name-face)
		 (quantifier_loop (identifier) @font-lock-variable-name-face)
		 (loop (identifier) @font-lock-variable-name-face)

		 (object_test (identifier) @font-lock-variable-name-face)

		 (entity_declaration_group (identifier) @font-lock-variable-name-face)

		 (redefine (identifier) @font-lock-property-use-face)
		 (undefine (identifier) @font-lock-property-use-face)
		 (creation_clause (identifier) @font-lock-property-use-face)
		 (rename_pair (identifier) @font-lock-property-use-face)
		 (new_export_item (identifier) @font-lock-property-use-face)

		 (assigner_mark (identifier) @font-lock-property-use-face)

		 ;; Highlight the modified value in calls:
		 ;; x := 1  ;  a.b.c := 1
		 ;; ^              ^
		 (assignment (call (_) @font-lock-variable-name-face :anchor)))

		:language eiffel
		:override t
		:feature check-then-warning
		((check ("check" @font-lock-warning-face
						 _ :?
						 "then" @font-lock-warning-face
						 _ :*
						 "end" @font-lock-warning-face)))))

(defvar eiffel-ts-indent-rules
	`((eiffel
		 ((n-p-gp "header_comment" "ERROR" nil) parent 6)
		 ((node-is "header_comment") parent 4)
		 ((parent-is "source_file") column-0 0)
		 ((node-is "feature_declaration") parent 2)
		 ((parent-is "feature_declaration") parent 2)

		 ((node-is "end") parent-bol 0)

		 ((parent-is "creation_clause") parent 2)

		 ((and (parent-is "class_declaration") (node-is "class_name")) parent 2)

		 ((and (parent-is "ERROR") (node-is "routine_mark")) parent 4)
		 ((and (parent-is "ERROR") (node-is "new_feature")) parent 2)
		 ((and (parent-is "ERROR") (node-is "feature_body")) parent 4)
		 ((and (parent-is "ERROR") (node-is "local_declarations")) parent 4)
		 ((and (parent-is "ERROR") (node-is "class_declaration")) parent 2)
		 ((and (parent-is "ERROR") (node-is "precondition")) parent 4)
		 ((and (parent-is "ERROR") (node-is "postcondition")) parent 4)

		 ((parent-is "local_declarations") parent 2)
		 ((parent-is "postcondition") parent 2)
		 ((parent-is "precondition") parent 2)

		 ((parent-is "internal") parent 2)
		 ((and no-node (parent-is "attribute_or_routine")) parent 2)
		 ((and no-node (parent-is "internal")) parent 2)

		 ((and no-node (parent-is "conditional")) parent 2)

		 ((parent-is "initialization") parent 2)
		 ((node-is "invariant") prev-sibling 0)
		 ((parent-is "invariant") parent 2)
		 ((node-is "variant") parent 0)
		 ((parent-is "variant") parent 2)
		 ((node-is "exit_condition") parent 0)
		 ((parent-is "exit_condition") parent 2)
		 ((node-is "loop_body") parent 0)
		 ((parent-is "loop") parent-bol 2)
		 ((parent-is "iteration") parent 2)

		 ((parent-is "notes") parent 2)


		 ((and (node-is "comment")
					 (or (parent-is "then_part") (parent-is "else_part")))
			grand-parent 4)

		 ((parent-is "then_part_expression") parent-bol 2)
		 ((n-p-gp "else" "conditional_expression" nil) parent-bol 0)
		 ((or (parent-is "then_part") (parent-is "else_part")) grand-parent 2)

		 ((parent-is "check") parent 2)

		 ((and no-node (parent-is "class_declaration")) parent 2)
		 ((parent-is "class_declaration") parent 0)

		 ((node-is ")") parent-bol 0)
		 ((node-is "]") parent-bol 0)
		 ((parent-is "parenthesized") parent-bol 2)
		 ((parent-is "expression") parent-bol 2)
		 ((node-is "tuple_parameter_list") parent-bol 2)
		 ((n-p-gp nil nil "tuple_parameter_list") parent-bol 2)

		 ((parent-is "inheritance") parent 2)
		 ((parent-is "parent") parent 2)
		 ((n-p-gp nil nil "feature_adaptation") parent 2)

		 ((n-p-gp nil "actuals" nil) parent-bol 2)
		 ((n-p-gp "." "call" nil) parent-bol 2)

		 ((parent-is "verbatim_string_closer") parent-bol 0)
		 ((parent-is "verbatim_string_content") no-indent)

		 ((parent-is "local_declarations") grand-parent 2)
		 ((parent-is "formal_arguments") parent-bol 2)

		 ((parent-is "actual_generics") parent-bol 2)

		 ((parent-is "ERROR") prev-line 0)
		 (catch-all parent-bol 0))))

(defvar eiffel-ts-mode-s-p-query
	(when (treesit-available-p)
		(treesit-query-compile 'eiffel
													 '((manifest_array) @manifest_array))))

(defun eiffel-ts-mode-syntax-propertize (beg end)
	"Add text properties using tree-sitter in between positions BEG and END."
	(let ((captures (treesit-query-capture 'eiffel eiffel-ts-mode-s-p-query beg end)))
		(pcase-dolist (`(,name . ,node) captures)
			(pcase-exhaustive name
				('manifest_array
				 (put-text-property (treesit-node-start node) (1+ (treesit-node-start node))
														'syntax-table (string-to-syntax "(>"))
				 (put-text-property (1- (treesit-node-end node)) (treesit-node-end node)
														'syntax-table (string-to-syntax ")<")))
				('special_text
				 (put-text-property (1+ (treesit-node-start node)) (treesit-node-end node)
														'syntax-table (string-to-syntax "."))
				 (put-text-property (treesit-node-start node) (1+ (treesit-node-start node))
														'syntax-table (string-to-syntax "\\")))))))

(defun eiffel-ts-mode-at-indentation-p (&optional point)
	"Is POINT at good indentation location?"
	(save-excursion
		(unless point (setq point (point)))
		(forward-line 0)
		(skip-chars-forward " \t")
		(eq (point) point)))

(defconst eiffel-ts-mode-indent-keywords
	'("then" "else" "elseif" "rescue" "feature" "require" "ensure" "do"
		"deferred" "invariant" "from" "until" "loop" "note" "create"
		"variant")
	"When typing these keywords the line is reindented.")

(defun eiffel-ts-mode-electric-indent-p (char)
	"After typing CHAR, should the line be reindented?"
	(cond
	 ((and (>= char ?a) (<= char ?z))
		(let ((pt (point)))
			(save-excursion
				(skip-chars-backward "[:alpha:]")
				(and (eiffel-ts-mode-at-indentation-p)
						 (looking-at (regexp-opt (cons "end" eiffel-ts-mode-indent-keywords)))
						 ;; Outdent after typing a keyword
						 (or (eq (match-end 0) pt)
								 ;; Reindent if it wasn't a keyword after all
								 (eq (match-end 0) (1- pt)))))))))

(defun eiffel-ts-defun-name-function (node)
	"Extract defun name for different types of nodes."
	(pcase (treesit-node-type node)
		("feature_declaration" (treesit-node-text (treesit-search-subtree node "extended_feature_name")))
		("class_declaration" (treesit-node-text (treesit-search-subtree node "class_name")))
		(_ nil)))

(defun eiffel-ts-setup ()
	"Setup treesit for `eiffel-ts-mode'."

	(setq-local treesit-font-lock-feature-list
							'((eiffel-highlight check-then-warning)))

	(setq-local treesit-font-lock-settings
							(apply #'treesit-font-lock-rules
										 eiffel-ts-font-lock-rules))

	(setq-local treesit-simple-indent-rules eiffel-ts-indent-rules)

	(setq treesit--indent-verbose t)

	(setq syntax-propertize-function #'eiffel-ts-mode-syntax-propertize)

	(setq treesit-defun-type-regexp "feature_declaration")
	(setq treesit-defun-name-function #'eiffel-ts-defun-name-function)
	(setq treesit-simple-imenu-settings
				`(("Feature" "extended_feature_name" nil treesit-node-text)
					("Class" "class_declaration" nil eiffel-ts-defun-name-function)))

	(treesit-major-mode-setup))

(defun eiffel-ts-insert-exists ()
	"Insert ∃."
	(interactive)
	(insert-char ?∃))

(defun eiffel-ts-insert-forall ()
	"Insert ∀."
	(interactive)
	(insert-char ?∀))

(defun eiffel-ts-insert-bar ()
	"Insert ¦ (used for ∃ and ∀)."
	(interactive)
	(insert-char ?¦))

(defun eiffel-ts-insert-loop-from ()
	"Insert ⟳."
	(interactive)
	(insert-char ?⟳))

(defun eiffel-ts-insert-loop-to()
	"Insert ⟲."
	(interactive)
	(insert-char ?⟲))

(defvar-keymap eiffel-ts-mode-map
	:doc "Keymap used in eiffel-ts-mode"
	:parent prog-mode-map
	"C-c C-e" #'eiffel-ts-insert-exists
	"C-c C-a" #'eiffel-ts-insert-forall
	"C-c C-b" #'eiffel-ts-insert-bar
	"C-c C-f" #'eiffel-ts-insert-loop-from
	"C-c C-g" #'eiffel-ts-insert-loop-to)

;;;###autoload
(define-derived-mode eiffel-ts-mode prog-mode "Eiffel[ts]"
	"Major mode for editing Eiffel with tree-sitter."
	:syntax-table eiffel-ts-mode-syntax-table

	(setq-local font-lock-defaults nil)
	(when (treesit-ready-p 'eiffel)
		(treesit-parser-create 'eiffel)

		(add-hook 'electric-indent-functions #'eiffel-ts-mode-electric-indent-p nil 'local)

		(setq-default indent-tabs-mode t)
		(setq-default tab-width 2)

		(setq-local comment-start "--")
		(setq-local comment-end "")
		(setq-local comment-start-skip "-- *")
		(eiffel-ts-setup)))

;;;###autoload
(if (treesit-ready-p 'eiffel t)
		(add-to-list 'auto-mode-alist '("\\.e\\'" . eiffel-ts-mode)))

(provide 'eiffel-ts-mode)
;;; eiffel-ts-mode.el ends here
