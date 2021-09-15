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

;disable toolbar and menu and scrollabar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;https://www.emacswiki.org/emacs/SmoothScrolling
(good-scroll-mode 1)
;; Hide right scroll bar
(set-face-foreground 'vertical-border (face-background 'default)) 
(set-face-background 'fringe (face-background 'default)) 
(set-face-foreground 'fringe (face-background 'default))
;;automatically enable that mode in all programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


;Relaxing the rather conservative garbage collector can also speed up startup times:
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))


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
 '(custom-safe-themes
   '("333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" default))
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
   '(treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil use-package all-the-icons-dired doom-themes web-mode tide graphql-mode yaml-mode all-the-icons good-scroll minimap ranger helm-lsp lsp-treemacs lv lsp-mode vyper-mode virtualenvwrapper jedi yafolding vimish-fold magit elisp-format logview vlf elpy google-translate json-mode exec-path-from-shell list-packages-ext))
 '(show-paren-mode t))

(windmove-default-keybindings 'meta) ;; alt+ arrows moves coursor
(save-place-mode 1) ;; save last opened position

; ask before open large files
(require 'vlf-setup) ;;--no-before-install-stable-melpa

;;(load-theme 'doom-zenburn t)
(load-theme 'doom-tomorrow-day)
;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)



;;REMOVE replaced by good scroll 
;; Enable mouse support
;;
;; (unless window-system
;;   (require 'mouse)
;;   (xterm-mouse-mode t)
;;   (global-set-key [mouse-4] (lambda ()
;;                               (interactive)
;;                               (scroll-down 1)))
;;   (global-set-key [mouse-5] (lambda ()
;;                               (interactive)
;;                               (scroll-up 1)))
;;   (defun track-mouse (e))
;;   (setq mouse-sel-mode t)
;; )


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
 '(highlight ((t (:background "gold" :foreground "black"))))
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

;;REMOVE java
;; (require 'meghanada)
;; (require 'company-meghanada)
;; (require 'flycheck-meghanada)

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

;; minimap on the right
(require 'minimap)
(setq minimap-window-location 'right)
(minimap-mode 1)


(require 'lsp)
(require 'lsp-haskell)
;;Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)


;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             ;; meghanada-mode on
;;             (meghanada-mode t)
;;             (flycheck-mode +1)
;;             (setq c-basic-offset 2)
;;             ;; use code format
;;             (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
;; (cond
;;    ((eq system-type 'windows-nt)
;;     (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
;;     (setq meghanada-maven-path "mvn.cmd"))
;;    (t
;;     (setq meghanada-java-path "java")
;;     (setq meghanada-maven-path "mvn")))


;;install godoctor before
;;go get github.com/godoctor/godoctor
;;go install github.com/godoctor/godoctor
;;(require 'godoctor)

;;return to previos buffer
(global-set-key (kbd "M-o")  'mode-line-other-buffer)


;;start ranger - file manager
(global-set-key [C-escape] 'ranger)
;;add icons for ranger view
;;(add-hook 'dired-mode-hook 'all-the-icons-dired-mode) ;; after loading treemacs icons showed 


;;scroll by line
(global-set-key (quote [C-S-up]) (quote scroll-down-line))
(global-set-key (quote [C-S-down]) (quote scroll-up-line))
(global-set-key [next] #'good-scroll-up-full-screen)
(global-set-key [prior] #'good-scroll-down-full-screen)

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
;;(setq neo-theme 'ascii)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-smart-open t)
(global-set-key [f8] 'neotree-toggle)
;;https://github.com/domtronn/all-the-icons.el
;;+https://github.com/jaypei/emacs-neotree
;;(require 'all-the-icons)
(setq neo-vc-integration '(face))


;;tide (typescript+tsx+js) - https://github.com/ananthakumaran/tide
;;TypeScript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;;TSX
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)


;;JSX
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
;;(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
;;(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(tsx-tide)))



;;translate hot-key for google translate
(global-set-key (kbd "M-t") 'google-translate-smooth-translate)
(setq google-translate-translation-directions-alist '(("en" . "ru")))

;;golang
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


(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               nil
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-width                           35
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))
