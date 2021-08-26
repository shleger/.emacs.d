;;https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
;; list the packages you want
(setq package-list '(lsp-haskell lv lsp-mode lsp-ui google-translate yasnippet flymake exec-path-from-shell neotree sbt-mode ensime anaconda-mode company-anaconda meghanada))
;;-no-in-stable-melpamelpa:  auto-complete go-autocomplete

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; disable tutorial loading buffer
(setq inhibit-startup-screen t)

;;only enable zsh terminal in emacs shell
(setenv "ESHELL" (expand-file-name "~/.emacs.d/bin/eshell"))

(require 'package)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
;;        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(
;;	("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 50)))


;;https://www.reddit.com/r/emacs/comments/apr9b2/bad_signature_from_gnu_elpa_signing_agent/

(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;logview set time zone for logview
;(set-variable "datetime-timezone" "Europe/Moscow")
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(datetime-timezone "Europe/Moscow")
 '(display-battery-mode t)
 '(display-time-mode t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(logview-additional-submodes
   '(("apix"
      (format . "LEVEL TIMESTAMP")
      (levels . "SLF4J")
      (timestamp)
      (aliases))))
 '(logview-additional-timestamp-formats
   '(("xxx"
      (regexp . "[0-9]{4}-[01][0-9]-[0-3][0-9][012][0-9]:[0-5][0-9]:[0-9]{8}")
      (aliases))))
 '(lsp-haskell-server-path
   "/home/saa/.config/Code - OSS/User/globalStorage/haskell.haskell/haskell-language-server-0.7.0-linux-8.8.4")
 '(mouse-wheel-tilt-scroll t)
 '(package-check-signature nil)
 '(package-selected-packages
   '(lsp-ui helm-lsp lsp-treemacs ## lsp-haskell lv lsp-mode vyper-mode virtualenvwrapper jedi flycheck yafolding vimish-fold magit elisp-format logview vlf intero haskell-mode elpy google-translate json-mode exec-path-from-shell list-packages-ext company-go go-autocomplete auto-complete))
 '(show-paren-mode t))

(windmove-default-keybindings 'meta) ;; alt+ arrows moves coursor
(save-place-mode 1) ;; save last opened position

; ask before open large files
(require 'vlf-setup) ;;--no-before-install-stable-melpa


;; Enable mouse support
;;
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)
(global-set-key (kbd "M-o")  'mode-line-other-buffer)

;;transpose-frames - https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)


;; backup save dir
(defun make-backup-file-name (FILE)                                             
  (let ((dirname (concat "~/.emacs.d/backups/"                                    
                         (format-time-string "%y/%m/%d/"))))                    
    (if (not (file-exists-p dirname))                                           
        (make-directory dirname t))                                             
    (concat dirname (file-name-nondirectory FILE))))




;;for-git
(add-to-list 'load-path "~/.emacs.d/diff-hl/")
(load "diff-hl.el")
(load "diff-hl-amend.el")
(load "diff-hl-dired.el")
(load "diff-hl-flydiff.el")
(load "diff-hl-margin.el")

(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(diff-hl-margin-mode)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :background "green"))))
 '(diff-file-header ((t (:background "color-17" :weight bold))))
 '(diff-function ((t (:inherit diff-header :background "color-241"))))
 '(diff-header ((t (:background "color-17"))))
 '(diff-hunk-header ((t (:inherit diff-header :background "color-241"))))
 '(diff-removed ((t (:inherit diff-changed :background "red"))))
 '(go-guru-hl-identified-face ((t (:background "SkyBlue"))))
 '(highlight ((t (:background "darkseagreen2" :foreground "cyan"))))
 '(neo-file-link-face ((t :inherit default)))
 '(region ((t (:background "goldenrod" :distant-foreground "gtk_selection_fg_color")))))

(require 'company)
(require 'flycheck)
(require 'yasnippet)
;;(require 'multi-compile) ;;;;--no in stable melpa:
(require 'go-eldoc)
(require 'company-go)
(require 'company-anaconda)
;;(require 'go-autocomplete) 
(require 'auto-complete-config) 
(require 'neotree)

(require 'meghanada)
(require 'company-meghanada)
(require 'flycheck-meghanada)

(require 'google-translate)
(require 'google-translate-smooth-ui)

;; (add-hook 'haskell-mode-hook 'intero-mode)


;; (defun intero-repl-run-main-on-save()
;;  (when (eq major-mode 'haskell-mode)
;;    (intero-repl-load-main)
;;  )
;; )
;; ;;run haskell app in intero-mode on after-save
;; (defun intero-repl-load-main (&optional prompt-options)
;;   (let ((file (intero-path-for-ghci (intero-buffer-file-name))))
;;     (intero-with-repl-buffer prompt-options
;;       (comint-simple-send
;;          (get-buffer-process (current-buffer))
;;          ":set prompt \"\\n\"")
;;           (comint-simple-send (get-buffer-process (current-buffer)) ":reload")
;; 	  (comint-simple-send (get-buffer-process (current-buffer)) ":main")
;;           (comint-simple-send (get-buffer-process (current-buffer)) ":set prompt \"\\4 \""))))

;; (add-hook 'after-save-hook 'intero-repl-run-main-on-save)

(require 'lsp)
(require 'lsp-haskell)
;;Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)


(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))


;install godoctor before
;go get github.com/godoctor/godoctor
;go install github.com/godoctor/godoctor
(require 'godoctor)


;;no-in-stable-melpa:
(ac-config-default)

;;resently opned files https://www.emacswiki.org/emacs/RecentFiles
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;audio bell off      https://www.emacswiki.org/emacs/AlarmBell
(setq visible-bell 1) 

;;neoTree
(setq neo-theme 'ascii)
(setq neo-smart-open t)
(global-set-key [f8] 'neotree-toggle)


;;translate hot-key for google translate
(global-set-key (kbd "M-t") 'google-translate-smooth-translate)
(setq google-translate-translation-directions-alist '(("en" . "ru")))

;;lsp
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;;python
;;(add-hook 'python-mode-hook 'anaconda-mode)
;;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;;use IPython
;;(setq python-shell-interpreter "ipython")

(elpy-enable) ;; - python mode enable 
(setq elpy-rpc-python-command "python3")
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (setq comint-move-point-for-output t)))


;;build and test
(defun my-go-mode-hook ()
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)


;; INSERT PAIR BRACKET
(electric-pair-mode)

(add-to-list 'exec-path "/usr/local/bin:/opt/anaconda/anaconda2/bin")
