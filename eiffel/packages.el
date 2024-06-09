;;; packages.el --- eiffel layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Ilgiz Mustafin <ilgimustafin@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
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

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `eiffel-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `eiffel/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `eiffel/pre-init-PACKAGE' and/or
;;   `eiffel/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

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
