;;; daml-mode.el --- Emacs mode for Digital Asset's DAML
;; https://discuss.daml.com/t/daml-syntax-highlighting-on-emacs/3174

;;; Commentary:

;;; Code:
(require 'haskell-mode)
(require 'haskell-indent)
(require 'cl-lib)

(defcustom daml-font-lock-keywords
  '("template" "ensure" "daml" "observer" "signatory" "agreement" "controller" "can" "nonconsuming" "return" "with" "mwith" "choice")
  "Identifiers treated as reserved keywords in DAML and are not keywords for Haskell."
  :group 'daml
  :type '(repeat string))

(set
 'haskell-font-lock-keywords
 (cl-remove-duplicates (append haskell-font-lock-keywords daml-font-lock-keywords)))

(defun daml-font-lock-keywords ()
  "Generate font lock keywords for DAML. Mostly haskell, with some DAML specific syntax."
  (append (haskell-font-lock-keywords)
          `(("\\<\\(submit\\|submitMustFail\\|fetch\\|exercise\\|create\\|test\\)\\>"
             0 font-lock-function-name-face)
            ("\\<\\(m?with\\)\\>"
             0 font-lock-builtin-face))))

(define-derived-mode daml-mode haskell-mode "DAML" "Major mode for DAML."
  (setq-local font-lock-defaults
              '((daml-font-lock-keywords)
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . haskell-syntactic-face-function)
                ;; Get help from font-lock-syntactic-keywords.
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props . (composition haskell-type))))
  (haskell-indentation-mode -1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.daml\\'" . daml-mode))


(provide 'daml-mode)
;;; daml-mode.el ends here
