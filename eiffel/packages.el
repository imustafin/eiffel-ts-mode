(defconst eiffel-packages
  '(eiffel-ts-mode smartparens company))

(defun eiffel/init-eiffel-ts-mode ()
	(use-package eiffel-ts-mode
    :mode (("\\.e\\'" . eiffel-ts-mode))
		:defer t))

(defun eiffel/pre-init-smartparens ()
	(spacemacs|use-package-add-hook smartparens
		:post-config
		(sp-with-modes 'eiffel-ts-mode
			(sp-local-pair "if" "end"
										 :when '(("SPC"))
										 :actions '(insert navigate)
										 :post-handlers '("| then\n[i]"))
			(sp-local-pair "do" "end"
										 :when '(("RET"))
										 :actions '(insert navigate)
										 :post-handlers '("||\n[i]"))
			(sp-local-pair "check" "end"
										 :when '(("SPC"))
										 :actions '(insert navigate))
			(sp-local-pair "<<" ">>"
										 :when '(("SPC"))
										 :actions '(insert navigate)
										 :post-handlers '("| "))
			(sp-local-pair "loop" "end"
										 :when '(("RET"))
										 :actions '(insert navigate)
										 :post-handlers '("||\n[i]")))))

(defun eiffel/post-init-company ()
	(spacemacs|add-company-backends
		:modes eiffel-ts-mode))
