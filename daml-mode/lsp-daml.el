;;; lsp-daml.el --- Emacs mode for Digital Asset Modelling Language
;; Copyright © 2016 Digital Asset.

;; https://discuss.daml.com/t/daml-syntax-highlighting-on-emacs/3174/6

;; Version: 1.1

;;; Commentary:

;; lsp-mode shim for language server embedded in Daml CLI

;;; Code:

(require 'lsp-ui)
(require 'lsp-mode)
(require 'daml-mode)

(defcustom lsp-daml-home "~/.daml"
  "Specify the path to Daml SDK."
  :type '(list string)
  :group 'lsp-daml)

(defcustom lsp-daml-extra-arguments nil
  "Specify extra argments to 'daml damlc ide'"
  :type '(list string)
  :group 'lsp-daml)

(defgroup lsp-daml nil
  "Customization group for lsp-daml."
  :group 'tools)

(defun daml-ls--suggest-project-root ()
  (and (memq major-mode '(daml-mode))
       (when-let (dir (locate-dominating-file default-directory "daml.yaml"))
         (expand-file-name dir))))

(add-to-list 'lsp-language-id-configuration '(daml-mode . "daml"))

;;;###autoload
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (append (list (concat lsp-daml-home "/bin/daml") "damlc" "ide") lsp-daml-extra-arguments))
                  :major-modes '(daml-mode)
                  :priority -1
                  :multi-root nil
                  :server-id 'daml-ls
                  ;; :ignore-messages '(".*keepAlive.*" ".*validations.*")
                  ;; :ignore-regexps '(".*keepAlive.*" ".*validations.*")
                  :notification-handlers
                  (lsp-ht
                   ("daml/keepAlive" 'ignore)
                   ("daml/workspace/validations" 'ignore)
                   ("window/progress/start" 'ignore)
                   ("window/progress/report" 'ignore)
                   ("window/progress/done" 'ignore)
                   ("$/progress" 'ignore))))

(provide 'lsp-daml)
;;; lsp-daml.el ends here
