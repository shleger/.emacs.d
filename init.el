;;https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
;; list the packages you want
(setq package-list '(lsp-haskell lv lsp-mode lsp-ui google-translate yasnippet flymake exec-path-from-shell neotree sbt-mode ensime anaconda-mode company-anaconda meghanada))

;;-no-in-stable-melpamelpa:  auto-complete go-autocomplete TODO del

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


;; In Emacs 24.4, you will also be able to save on frame focus loss thanks to the new focus hooks:
;; (add-hook 'focus-out-hook 'save-buffer) (to save the active buffer) or
;; (add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
;; (to save all open buffers).
;; https://emacs.stackexchange.com/a/266
(add-hook 'focus-out-hook 'save-buffer)

;; toggle-truncate-lines by default
(set-default 'truncate-lines t)


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
   '("e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" default))
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
 '(lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper")
 '(mouse-wheel-tilt-scroll t)
 '(package-check-signature nil)
 '(package-selected-packages
   '(terraform-mode journalctl-mode ox-reveal protobuf-mode helm helm-bibtex org-ref minions flycheck-pos-tip deft org-roam ormolu dockerfile-mode solidity-mode which-key shackle jenkinsfile-mode rustic plantuml-mode org-download alert org-alert lsp-java diff-hl treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile projectile treemacs-evil use-package all-the-icons-dired doom-themes web-mode tide graphql-mode yaml-mode all-the-icons good-scroll minimap ranger lsp-treemacs lv lsp-mode vyper-mode virtualenvwrapper jedi yafolding vimish-fold magit elisp-format logview vlf elpy google-translate json-mode exec-path-from-shell list-packages-ext))
 '(show-paren-mode t))

(windmove-default-keybindings 'meta) ;; alt+ arrows moves coursor
(save-place-mode 1) ;; save last opened position
(setq org-support-shift-select 'always) ;; enable shift+arrows in org mode for selection

;;https://rejeep.github.io/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; m (mark the buffer you want to keep)
;; t (toggle marks)
;; D (kill all marked buffers)
;;local: ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-w") 'kill-this-buffer)

;; Ensure ibuffer opens with point at the current buffer's entry.
(defadvice ibuffer
  (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name."
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;; https://stackoverflow.com/a/51593874
(defun my/kill-all-buffers-except-toolbox ()
  "Kill all buffers except current one and toolkit (*Messages*, *scratch*). Close other windows."
  (interactive)
  (mapc 'kill-buffer (remove-if
                       (lambda (x)
                         (or
                           (eq x (current-buffer))
                           (member (buffer-name x) '("*Messages*" "*scratch*"))))
                       (buffer-list)))
  ;; TODO add kill open tabs
  (delete-other-windows))


(use-package awesome-tab
  :load-path "~/.emacs.d/awesome-tab"
  :init
  (defun my-awesome-tab-hide-tab (x)
   (let ((name (format "%s" x)))
    (or
     ;; Hide tab if current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Hide sdcv tab.
     (string-prefix-p "*sdcv" name)

     ;;Rust cargo output
     (string-prefix-p "*cargo" name)

     ;;IBuffers selection
     (string-prefix-p "*Ibuffer" name)

     ;; Hide tab if current buffer is helm buffer.
     ;; (string-prefix-p "*helm" name) ;; need press any cay for showing helm content

     ;; Hide tab if current buffer is flycheck buffer.
     (string-prefix-p "*flycheck" name)

     ;; Hide blacklist if emacs version < 27 (use header-line).
     (and (eq awesome-tab-display-line 'header-line)
          (or (string-prefix-p "*Compile-Log*" name)
              (string-prefix-p "*Flycheck" name)))
     (and (string-prefix-p "magit" name)
               (not (file-name-extension name)))
     (and (string-prefix-p "*vc" name)
               (not (file-name-extension name)))

     )))
  (defvar awesome-tab-hide-tab-function 'my-awesome-tab-hide-tab)

  :config
  (awesome-tab-mode t)
  (my-awesome-tab-hide-tab t)
  (setq awesome-tab-height 120))

(global-set-key (kbd "C-M-<left>")   'awesome-tab-backward)
(global-set-key (kbd "C-M-<right>")   'awesome-tab-forward)
(global-set-key (kbd "C-M-<up>")   'awesome-tab-forward-group)
(global-set-key (kbd "C-M-<down>")   'awesome-tab-backward-group)


(use-package aweshell
  :load-path "~/.emacs.d/aweshell"
  :config
  (global-set-key (kbd "C-M-<tab>")   'aweshell-dedicated-toggle)
  (setq aweshell-auto-suggestion-p nil))

;;from https://sachachua.com/dotemacs/index.html
(fset 'yes-or-no-p 'y-or-n-p)

;;Modify the syntax table yourself and tell Emacs you want
;;it to treat the _ character as a word constituent, like so:
(modify-syntax-entry ?_ "w")


(use-package helm
  :ensure t
  :init
   (helm-mode 1)
  :config
  (progn
 ;;   (require 'helm-config)
    (require 'helm-for-files)
    (setq helm-candidate-number-limit 100)
    (setq helm-completing-read-handlers-alist
          '((describe-function)
            (consult-bookmark)
            (org-refile-get-location)
            (consult-outline)
            (consult-line)
            (org-olpath-completing-read)
            (consult-mark)
            (org-refile)
            (consult-multi-occur)
            (describe-variable)
            (execute-extended-command)
            (consult-yank)))
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t))
   (setq helm-M-x-fuzzy-match t)
   (awesome-tab-build-helm-source)

   (define-key helm-map (kbd "C-<tab>") ;;https://stackoverflow.com/a/27652821
     (lambda ()
      (interactive)
       (helm-move-selection-common :where 'edge :direction 'previous)
       (helm-move-selection-common :where 'line :direction 'next)
       (helm-move-selection-common :where 'line :direction 'next)
       (helm-execute-persistent-action)))
   (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
   (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
   (define-key helm-map (kbd "C-z") #'helm-select-action)

  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x c o" . helm-occur)
         ("C-f" . helm-find) 
         ("M-y" . helm-show-kill-ring) ;SC
         ("C-x r b" . helm-filtered-bookmarks)
         ;; :map org-mode-map
	       ;; ("<tab>" .helm-execute-persistent-action )
	       ;; ("C-S-z" . helm-select-action)
	 ))



;;local
(global-set-key (kbd "C-d") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-S-f") 'grep-find)
(global-set-key (kbd "C-/") 'comment-line) 
(global-set-key (kbd "<escape>") 'keyboard-quit) ;
(global-set-key (kbd "C-<delete>") 'kill-whitespace-or-word)
(global-set-key (kbd "M-<SPC>") 'company-complete)

;;magit
(global-set-key (kbd "C-x g") 'magit-status)

;;projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "s-`") 'projectile-command-map)
(add-hook 'prog-mode-hook #'projectile-mode) ;; why here #



;;plantuml-mode (binary installed -> /usr/bin/plantuml from arch community repo)
(setq plantuml-executable-path "/usr/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)

; ask before open large files
(require 'vlf-setup) ;;--no-before-install-stable-melpa




;; (defun saa/load-timed-theme2 (dawn)  (message dawn))
;; (saa/load-timed-theme2 "fff")

;;(run-at-time 2 3600 'saa/load-timed-theme)
(defun saa/load-timed-theme (light dawn dark dusk)
  "Dark theme for night and day with timer"

;;day | night theme
;;"(string-to-number (format-time-string "%H" (current-time)))) --with string variant"
;; (if (> 0 1) '"else?") =>  return nil
;;Test: (cond ((< 19 7 ) "b")   ((> 6 7 ) "b")   (t "w"))
(cond ((< dusk  (nth 2 (decode-time))) (load-theme dark)) 
      ((> dawn   (nth 2 (decode-time))) (load-theme dark))       
      (t (load-theme light)))

(let ((secondsPart ":00") (oneDayInSeconds 86400 )) 


(run-at-time (concatenate 'string (number-to-string dusk) secondsPart) oneDayInSeconds 'load-theme dark) 
(run-at-time (concatenate 'string (number-to-string dawn) secondsPart) oneDayInSeconds 'load-theme light) )) 

(saa/load-timed-theme 'doom-tomorrow-day 6 'doom-zenburn 19)



;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)


;;java  lang server (auto install eclipse EDT)
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
;;(add-hook 'java-mode-hook 'lsp-jt-lens-mode) -- TODO how enable hook in progn mode?


;;org-mode additions
(use-package org-alert
  :ensure t)
(setq alert-default-style 'libnotify)
(org-alert-enable) ;;run timer for check schedulers and deadlines

;;insert screenshots (with xclip )
(require 'org-download)
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))


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
(defun my-make-backup-file-name (FILE)                                             
  (let ((dirname (concat "~/.emacs.d/backups/"                                    
                         (format-time-string "%y/%m/%d/"))))                    
    (if (not (file-exists-p dirname))                                           
        (make-directory dirname t))                                             
    (concat dirname (file-name-nondirectory FILE))))
(setq make-backup-file-name-function 'my-make-backup-file-name)


;; https://github.com/tarsius/minions
;; hide minor modes list 
(use-package minions
  :config (minions-mode 1))



;;findout wich face under coursor
;; (defun what-face (pos)
;;     (interactive "d")
;;         (let ((face (or (get-char-property (point) 'read-face-name)
;;             (get-char-property (point) 'face))))
;;     (if face (message "Face: %s" face) (message "No face at %d" pos))))

(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(diff-hl-margin-mode)

(global-diff-hl-show-hunk-mouse-mode)

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
 '(diff-removed ((t (:inherit diff-changed :extend t :background "#f4978e" :foreground "black"))))
 '(go-guru-hl-identified-face ((t (:background "SkyBlue"))))
 '(highlight ((t (:background "gold" :foreground "black"))))
 '(neo-file-link-face ((t :inherit default)))
 '(region ((t (:background "goldenrod" :distant-foreground "gtk_selection_fg_color")))))

(use-package flycheck :ensure)
;;=====TODO rm, used in use-package at the and of this file
;;(require 'company)
;;(require 'flycheck)
;;(require 'yasnippet)
;;https://github.com/joaotavora/yasnippet -- enable minor mode
;;(yas-reload-all)
;;(add-hook 'prog-mode-hook #'yas-minor-mode)
;;(require 'multi-compile) ;;;;--no in stable melpa:
;;=====TODO rm until


(require 'go-eldoc)
(require 'company-go)
(require 'company-anaconda)
;;(require 'go-autocomplete)  TODO del
;;(require 'auto-complete-config) TODO del
(require 'neotree)


(require 'google-translate)
(require 'google-translate-smooth-ui)

;;errors when work with rustic
;; minimap on the right
;;(require 'minimap)
;;(setq minimap-window-location 'right)
;;(minimap-mode 1)
;; FIXME Invalid face attribute :foreground nil


;;return to previos buffer
(global-set-key (kbd "C-<tab>")  'mode-line-other-buffer)


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
  (setq create-lockfiles nil) ;; https://stackoverflow.com/questions/62567370/reactjs-local-server-crashes-after-editing-file-in-emacs-even-without-saving
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;;https://dev.to/viglioni/how-i-set-up-my-emacs-for-typescript-3eeh -- typescript-mode
;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(require 'web-mode)


;;TSX
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)


;;JSX
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


;;https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-haskell.el
(use-package lsp-haskell)
(use-package haskell-mode
  :hook
  ((haskell-mode . (lambda ()
                     (lsp)
		     (interactive-haskell-mode)
                     (my-haskell-mode-hook)
                     (company-mode)
                     (haskell-collapse-mode))))
  :config

  ;; https://stackoverflow.com/a/71212327
  (defun my-haskell-load-and-run ()
    "Loads and runs the current Haskell file main function."
    (interactive)
    (haskell-process-load-file)
    (haskell-interactive-mode-run-expr "main"))

  ;; ;; format and organize imports before save
  ;; (defun lsp-haskell-save-hooks()
  ;;   (add-hook 'before-save-hook  #'lsp-format-buffer t t)
  ;;   (add-hook 'before-save-hook  #'lsp-organize-imports t t))
  ;;   (add-hook 'haskell-mode-hook #'lsp-haskell-save-hooks)

  (defun my-haskell-mode-hook ()
    "Hook for `haskell-mode'."
    (define-key haskell-mode-map (kbd "M-/") 'lsp-find-references) ;; find usage
    (define-key haskell-mode-map (kbd "S-<f2>") 'lsp-rename) ;; rename

    (define-key interactive-haskell-mode-map  (kbd "C-c C-c") 'my-haskell-load-and-run ) ;; run
    ;OR
    ;; (defun my-haskell-mode-hook ()
    ;;   (local-set-key (kbd "C-c c") 'my-haskell-load-and-run)) 
    ;; (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

    ;; TODO 
    ;; (set (make-local-variable 'company-backends)
    ;;      '((company-capf company-files :with company-yasnippet)
    ;;        (company-dabbrev-code company-dabbrev)))

    ))

;; https://discuss.daml.com/t/daml-syntax-highlighting-on-emacs/3174
(use-package daml-mode
  :load-path "daml-mode/"
  :mode "\\.daml\\'"
  :bind (([remap haskell-mode-jump-to-def-or-tag] . lsp-find-definition)
	 :map daml-mode-map
         ("C-c e" . flycheck-list-errors)
         ("M-n"   . flycheck-next-error)
         ("M-p"   . flycheck-previous-error)
         ("<tab>" . haskell-indent-cycle))
  :config
  (require 'lsp-daml)
  ;; (setq lsp-daml-extra-arguments '("--scenarios" "no" "--telemetry" "--ghc-option" "-XMonoLocalBinds" "--ghc-option" "-W" "--ghc-option" "-Wno-unused-binds" "--ghc-option" "-Wno-unused-matches" "--RTS" "--jobs" "16" "+RTS" "-N16" "-H10G" "-M12G" "-A20M" "-RTS"))
  ;; OR ("--RTS" "+RTS" "-M6G" "-N") -- ;;https://discuss.daml.com/t/daml-on-vim/465/3
  
  )

(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t
	;; lsp-auto-guess-root t
	;; lsp-log-io t
	;; lsp-server-trace "verbose"
	)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode  . lsp)

	 ;; (haskell-mode . lsp)
         ;; (haskell-mode . interactive-haskell-mode)
	 ;; (haskell-literate-mode . lsp)


;;	 (java-mode . lsp)
;;	 (typescript-mode . lsp)
;;	 (json-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; enable formatter
(use-package ormolu
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :bind
  (:map haskell-mode-map
    ("C-c f" . ormolu-format-buffer)))

;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key :config (which-key-mode))


(use-package lsp-ui
  :requires lsp-mode flycheck
  :config (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-sideline-ignore-duplicates t
		lsp-ui-sideline-show-diagnostics t
		lsp-ui-sideline-show-code-actions t
		lsp-ui-sideline-update-mode 'line
                lsp-ui-sideline-toggle-symbol-info t

	        lsp-ui-flycheck-enable t
                lsp-ui-flycheck-list-position 'right
                lsp-ui-flycheck-live-reporting t
		
		lsp-ui-peek-enable t
	        lsp-ui-peek-fontify 'always
	        lsp-ui-peak-always-show t
                lsp-ui-peek-list-width 60
                lsp-ui-peek-peek-height 20

		;; lsp-ui-doc-delay 5
                ;; lsp-ui-doc-position 'bottom
                ;; lsp-ui-doc-alignment 'frame
                ;; lsp-ui-doc-header nil
                ;; lsp-ui-doc-include-signature t
                ;; lsp-ui-doc-use-childframe t
		lsp-ui-doc-show-with-cursor nil

		lsp-ui-imenu-enable t
        	lsp-ui-imenu-kind-position 'top)
  :bind
  (:map lsp-mode-map
	("C-c m" . lsp-ui-imenu)
	("C-c s" . lsp-ui-sideline-mode))
	
  :commands lsp-ui-mode)

(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals


;;golang
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

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
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
        ("<right>"   . treemacs-RET-action)
        ("<left>"    . treemacs-COLLAPSE-action))
  )

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

(use-package lsp-treemacs)
(lsp-treemacs-sync-mode 1)

;; rustic - https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/#rust-analyzer
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package yaml-mode
  :ensure t
  :mode (".yaml$")
  :hook
  (yaml-mode . yaml-mode-outline-hook)

  :init
    (defun yaml-outline-level ()
    "Return the outline level based on the indentation, hardcoded at 2 spaces."
    (s-count-matches "[ ]\\{2\\}" (match-string 0)))
    
  (defun yaml-mode-outline-hook ()
    (outline-minor-mode)
    (define-key yaml-mode-map (kbd "TAB") 'outline-toggle-children)
    (define-key yaml-mode-map (kbd "<backtab>") 'outline-show-all)
    ;;    (setq outline-regexp "^\\([ ]\\{2\\}\\)*\\([-] \\)?\\([\"][^\"]*[\"]\\|[a-zA-Z0-9_-]*\\): *\\([>|]\\|&[a-zA-Z0-9_-]*\\)?$")
    (setq outline-regexp ;;https://github.com/yoshiki/yaml-mode/issues/25#issuecomment-797940023
      (rx
       (seq
	bol
	(group (zero-or-more "  ")
	       (or (group
		    (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
			     (seq "'" (*? (not (in "'" "\n"))) "'")
			     (*? (not (in ":" "\n"))))
			 ":"
			 (?? (seq
			      (*? " ")
			      (or (seq "&" (one-or-more nonl))
				  (seq ">-")
				  (seq "|"))
			      eol))))
		   (group (seq
			   "- "
			   (+ (not (in ":" "\n")))
			   ":"
			   (+ nonl)
			   eol)))))))
    (setq outline-level 'yaml-outline-level))
  )

(use-package hideshow
  :bind (("C-\\" . hs-toggle-hiding)
         ("C-|" . hs-show-all))
  :init (add-hook #'prog-mode-hook #'hs-minor-mode)
  :diminish hs-minor-mode
  :config
  (setq hs-special-modes-alist
        (mapcar 'purecopy
                '((c-mode "{" "}" "/[*/]" nil nil)
                  (c++-mode "{" "}" "/[*/]" nil nil)
                  (java-mode "{" "}" "/[*/]" nil nil)
                  (rust-mode "{" "}" "/[*/]" nil nil)
                  (go-mode "{" "}" "/[*/]" nil nil)
                  (js-mode "{" "}" "/[*/]" nil)
                  (json-mode "{" "}" "/[*/]" nil)
                  (javascript-mode  "{" "}" "/[*/]" nil)))))

;;init- https://gist.github.com/rksm/8c07d9ccc9e15adf752d3dd73dd9a61e
(use-package shackle
  :ensure
  :diminish
  :init
  (shackle-mode)
  :custom
  (setq helm-display-function 'pop-to-buffer) 
  (shackle-rules '((compilation-mode :noselect t)
		   ("\\*Apropos\\|Help\\|Occur\\|tide-references\\*" :regexp t :same t :select t :inhibit-window-quit t)
		   ("\\*magit" :regexp t :same t :select t)
		   ("\\*shell.*" :regexp t :same t :select t)
		   ("\\*PowerShell.*" :regexp t :same t :select t)
;;		   ("\\*Cargo.*" :regexp t :other t :select nil) ;; strage output to treemacs window
		   ("*Messages*" :select nil :other t)
		   ("*Proced*" :select t :same t)
		   ("*Buffer List*" :select t :same t)
		   ("\\*Pp Eval" :regexp t :same nil :select t :other t)
		   ("*Messages*" :same nil :other t :select t :inhibit-window-quit t)

;;		   slime
		   ("*slime-source*" :select nil :same nil :other t)
		   ("*slime-description*" :select nil :other t :inhibit-window-quit t)
		   ("\\*slime-repl" :regexp t :same nil :select nil :other t)
		   ("\\*sldb" :regexp t :other t :inhibit-window-quit t :select t)
		   ("\\*slime-compilation" :regexp t :same nil :select nil :other t)
		   ("*slime-scratch*" :same nil :select t :other t)

;;		   ert
		   ("*ert*" :select nil :same nil :other t)

		   ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)
		   

;;		   clojure
		   ("*sesman CIDER browser*" :inhibit-window-quit t :select t :same t)
		   ("\\*cider-repl" :regexp t :same nil :other t)))
  (shackle-default-rule nil))

;; Save history 
(use-package savehist
  :init
  (savehist-mode))

;;;;;;;;;;;;;;;;;;;

;; https://mstempl.netlify.app/post/beautify-org-mode/
(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("◉" "◆" "▶" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "☯" ))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

;; TODO del?
;;https://gist.github.com/Lukewh/47b200f0af5a632205f0fbec48669647
;; (use-package expand-region
;;   :ensure t
;;   :bind (("C-=" . er/expand-region)
;; 	 ("C--" . er/contract-region)))


;;https://stackoverflow.com/a/17959258
(defun kill-whitespace-or-word ()
  (interactive)
  (if (looking-at "[ \t\n]")
      (let ((p (point)))
        (re-search-forward "[^ \t\n]" nil :no-error)
        (backward-char)
        (kill-region p (point)))
    (kill-word 1)))

(setq org-image-actual-width nil) ;; remove setup default image width, now coud be customised with:  #+attr_org: :width 50[px|%]
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)) ;; scale latex previews

;;https://systemcrafters.net/build-a-second-brain-in-emacs/keep-a-journal/
(setq my-org-roam-dir "~/my/org/RoamNotes")
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  
  (setq org-hide-emphasis-markers t) ;; remove asterics from *bold* and other type formatting in org-mode
  (defun bolding ()      (interactive) (org-emphasize ?*)) ;; ?* char parameter for bold
  (defun emphasizeing () (interactive) (org-emphasize ))

  ;;render plant uml in org-mode
  (setq org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

  
  :custom
  (org-roam-directory my-org-roam-dir)
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%I:%M %p>: %?"
       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         ("C-b" . bolding )  ;; hotkey for bold formatting
         ("C-S-b" . emphasizeing )  ;; hotkey for formatting with variants
	 ("C-S-<left>" . org-do-promote)
	 ("C-S-<right>" . org-do-demote)
	 ("C-S-<up>" . org-move-subtree-up)
	 ("C-S-<down>" . org-move-subtree-down)
         
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (org-roam-bibtex-mode +1)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

;; https://www.orgroam.com/manual.html#Full_002dtext-search-with-Deft
(use-package deft
  :after org
  :bind
  ("C-c n s" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory my-org-roam-dir))

;; reveal.js presentation mode for org
(use-package ox-reveal)


(setq bib-files-directory "~/my/org/RoamNotes/references/bibtex-file-1.bib")
(setq pdf-files-directory "~/my/org/RoamNotes/pdfs")
(setq notes-files-directory "~/my/org/RoamNotes/notes")


(use-package helm-bibtex
  :config
  (setq bibtex-completion-bibliography bib-files-directory
        bibtex-completion-library-path pdf-files-directory
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-path notes-files-directory)

  ;; Símbolos usados para indicar que tiene PDF y notas
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-additional-search-fields '(journal booktitle))


  (setq bibtex-completion-notes-extension ".org")
  (setq bibtex-completion-pdf-extension '(".pdf" ".djvu" ".txt"))

  ;; Para abrir URL/DOIs
  (setq bibtex-completion-browser-function
        (lambda (url _) (start-process "firefox" "*firefox*" "firefox" url)))


  (helm-add-action-to-source
   "Open annotated PDF (if present)" 'helm-bibtex-open-annotated-pdf
   helm-source-bibtex 1)

  :bind
  (("<menu>" . helm-command-prefix) ;; <menu> --right click mouse on keyboard
   ("C-c n b" . orb-note-actions) ;; org-roam-bibtex-mode
   :map helm-command-map
   ( "b" . helm-bibtex)
   ( "B" . helm-bibtex-with-local-bibliography)
   ( "n" . helm-bibtex-with-notes)
   ( "<menu>" . helm-resume)))

(use-package org-ref
  :after (org-roam-bibtex)
  :init
  (setq org-ref-default-bibliography bib-files-directory)
  (setq org-ref-completion-library 'org-ref-helm-cite)
  (setq org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)
  (setq org-ref-default-citation-link "citep"))

(use-package org-roam-bibtex
  :after (org-roam)
  :config
  ;(require 'org-ref)
  ;; (setq orb-note-actions-interface 'hydra)
  (setq orb-note-actions-interface 'helm)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file" "date")
        orb-process-file-keyword t
        orb-insert-interface 'helm-bibtex 
        orb-file-field-extensions '("pdf")))

;; C-c C-c l: markdown-live-preview-mode > *eww* buffer: on .md files
;;https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))



;; search from selected string in buffer
;; https://www.reddit.com/r/emacs/comments/2amn1v/comment/cixq7zx/?utm_source=share&utm_medium=web2x&context=3
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))


;; configure for aspell checker
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--camel-case")) ;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
