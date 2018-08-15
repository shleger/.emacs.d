;;https://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
;; list the packages you want
(setq package-list '( google-translate go-mode go-eldoc company company-go yasnippet go-rename multi-compile flycheck gotest go-scratch go-direx exec-path-from-shell go-guru godoctor auto-complete go-autocomplete neotree sbt-mode ensime anaconda-mode company-anaconda meghanada))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)



; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(package-selected-packages
   (quote
    (google-translate json-mode exec-path-from-shell list-packages-ext go-autocomplete auto-complete company-go))))
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
 '(neo-file-link-face ((t :inherit default))))


;;(require 'package)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(require 'company)
(require 'flycheck)
(require 'yasnippet)
(require 'multi-compile)
(require 'go-eldoc)
(require 'company-go)
(require 'company-anaconda)
(require 'go-autocomplete)
(require 'auto-complete-config)
(require 'neotree)

(require 'meghanada)
(require 'company-meghanada)
(require 'flycheck-meghanada)

(require 'google-translate)
(require 'google-translate-smooth-ui)

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


(ac-config-default)

;;neoTree
(setq neo-theme 'ascii)
(setq neo-smart-open t)
(global-set-key [f8] 'neotree-toggle)


;;translate hot-key for google translate
(global-set-key (kbd "M-t") 'google-translate-smooth-translate)
(setq google-translate-translation-directions-alist '(("en" . "ru")))


;;autosave
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)


;;python
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;;use IPython
(setq python-shell-interpreter "ipython2")


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

;; HOOKS
(add-hook 'go-mode-hook 'my-go-mode-hook)                  
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)    ; Very slow on Windows ((

;; Call Gofmt before saving
(add-hook 'before-save-hook 'gofmt-before-save)
(setq-default gofmt-command "goimports")
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
(add-hook 'go-mode-hook 'yas-minor-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(setq multi-compile-alist '(
    (go-mode . (
("go-build" "go build -v"
   (locate-dominating-file buffer-file-name ".git"))
("go-build-and-run" "go build -v && echo 'build finish' && eval ./${PWD##*/}"
   (multi-compile-locate-file-dir ".git"))))
    ))


(add-to-list 'exec-path "/usr/local/bin:/opt/anaconda/anaconda2/bin")
