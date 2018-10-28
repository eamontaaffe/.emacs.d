(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

;; No splash screen
(setq inhibit-startup-message t)

;; Window
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(height . 300))
(add-to-list 'default-frame-alist '(width . 80))

;; Set up package management with `use-package`

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;; Sane defaults ;;;;

;; Allow pasting from system clipboard
(setq x-select-enable-clipboard t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Tabs should be two spaces
(setq-default tab-width 2)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Beginning of line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
		'smarter-move-beginning-of-line)

;; UTF-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;;; Backups ;;;;

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)

;; Turn off bell

(setq ring-bell-function 'ignore)

;; Automatically reload files that have changed
(global-auto-revert-mode t)

;;;; Package stuff ;;;;

(use-package helm
  :config
  (helm-mode 1)
  (require 'helm-config)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-c h" . helm-mini)
         ("C-x C-b" . helm-buffers-list))
  :ensure t)

(use-package helm-ls-git
  :bind (("C-c g" . helm-ls-git-ls)
         ("C-x C-d" . helm-browse-project)
         ("M-y" . helm-show-kill-ring))
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package helm-ag
  :config
  (add-to-list 'exec-path "/usr/local/bin/")
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  :config
  (projectile-global-mode t)
  (helm-projectile-on)
  :bind (("C-c p ." . helm-projectile-find-file-dwim)))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :ensure t)

(use-package reason-mode
  :ensure t)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :ensure t)

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (setq js2-basic-offset 2)
  :ensure t)

(use-package doom-themes
  :config
  (load-theme 'doom-molokai t)
  :ensure t)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :ensure t)

(use-package toc-org
  :config
  (add-hook 'org-mode-hook 'toc-org-enable)
  :ensure t)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :ensure t)

;; Typescript

(use-package typescript-mode
  :ensure t)

(use-package tide
  :ensure t)

;; Terraform

(use-package terraform-mode
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq typescript-indent-offset 4))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Haskell

(use-package intero
  :config
  (intero-global-mode 1)
  :ensure t)

;; Org mode

(global-set-key "\C-ca" 'org-agenda)

(setq org-time-clocksum-use-fractional 1)

(setq org-agenda-files '("~/org"))

(use-package ox-pandoc
  :ensure t)

(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-duration-format (quote h:mm))

;;; Org capture setup

(setq org-default-notes-file (concat org-directory "/capture.org"))

(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("c" "Cadmus task" entry
         (file+headline "~/org/cadmus.org" "Tasks")
         "** TODO %?\n %i" :prepend t)

        ("k" "Cadmus question" entry
         (file+headline "~/org/cadmus.org" "Tasks")
         "** QUESTION %?\n %i" :prepend t)
        
        ("h" "Home task" entry (file+headline "~/org/home.org" "Tasks")
         "** TASK %?\n %i" :prepend t)))

(define-key global-map "\C-cj" 'org-clock-jump-to-current-clock)

;; Graphql

(use-package graphql-mode
  :ensure t)

;; Markdown

(use-package markdown-mode
  :ensure t)

;; Idris

(use-package idris-mode
  :ensure t)

;; Elm

(use-package elm-mode
  :ensure t)

;; Column enforcer

(use-package column-enforce-mode
  :config
  (80-column-rule)
  :ensure t)

;; Docker mode

(use-package dockerfile-mode
  :ensure t)

;; Latex mode

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize))
  :ensure t)

(use-package auctex
  :requires exec-path-from-shell
  :defer t
  :init
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t)
  :ensure t)

;; Dash documentation
(use-package helm-dash
  :config
  (setq helm-dash-docsets-path "/Users/eamon/Library/Application Support/Dash/DocSets/")
  :ensure t)

;; Custom functions

(defun frame-default ()
  (interactive)
  (if (window-system)
      (set-frame-size (selected-frame) 80 100)))

(defun frame-double ()
  (interactive)
  (if (window-system)
      (set-frame-size (selected-frame) 163 100)))  

;; Generated stuff (don't edit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#191C25" "#C16069" "#A2BF8A" "#ECCC87" "#80A0C2" "#B58DAE" "#86C0D1" "#F0F4FC"])
 '(custom-safe-themes
   (quote
    ("f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "8dce5b23232d0a490f16d62112d3abff6babeef86ae3853241a85856f9b0a6e7" "d97baf5a34c87b05508739505cad03438cde8efa2a0d350c7773f2a8bc26a50d" "099c44618d7660548701d4f495a8c23a85103bc7b87fec33c9db4cd099a4adaf" default)))
 '(fci-rule-color "#4C566A")
 '(jdee-db-active-breakpoint-face-colors (cons "#10151C" "#5EC4FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#10151C" "#8BD49C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#10151C" "#384551"))
 '(markdown-command "/usr/local/bin/pandoc")
 '(package-selected-packages
   (quote
    (zeal-at-point helm-dash doom-themes json-mode auctex exec-path-from-shell terraform-mode tide typescript-mode dockerfile-mode rjsx-mode column-enforce-mode intero interleave elm-mode idris-mode ox-pandoc ox-md markdown-mode undo-tree graphql-mode toc-org rainbow-delimiters web-mode exec-path helm-ag basic-theme white-theme twilight-bright-theme reason-mode multiple-cursors magit helm-projectile helm projectile)))
 '(safe-local-variable-values
   (quote
    ((intero-targets "resources:lib" "resources:exe:resources-exe" "resources:test:resources-test"))))
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-color-map
   (quote
    ((20 . "#ab4642")
     (50 . "#dc9656")
     (80 . "#f7ca88")
     (110 . "#a1b56c")
     (140 . "#86c1b9")
     (170 . "#7cafc2")
     (200 . "#ab4642")
     (230 . "#a16046")
     (260 . "#181818")
     (290 . "#282828")
     (320 . "#383838")
     (350 . "#585858"))))
 '(vc-annotate-very-old-color "#585858"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
