;; No splash screen

(setq inhibit-startup-message t)

;; Window

(defvar default-window-height 40)

(if (display-graphic-p)
    (progn
      (toggle-scroll-bar -1)
      (tool-bar-mode -1)))

(menu-bar-mode -1)

(setq default-directory "~/")

;; Package

(require 'package)

(setq package-enable-at-startup
      nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Add to load path

(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages"))

;; Custom file

(setq custom-file "~/.emacs.d/custom.el")

(if (file-exists-p custom-file)
    (load custom-file))

;; Bootstrap use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Allow pasting from system clipboard

(setq x-select-enable-clipboard t)

;; Show keystrokes in progress

(setq echo-keystrokes 0.1)

;; Answering just 'y' or 'n' will do

(defalias 'yes-or-no-p 'y-or-n-p)

;; Spaces instead of tabs

(setq-default indent-tabs-mode nil)

;; Tabs should be two spaces

(setq-default tab-width 2)

;; Remove text in active region if inserting text

(delete-selection-mode 1)

;; Disable minimise (I keep accidentally pressing it)

(global-unset-key (kbd "C-z"))

;; Custom functions

(defun frame-default ()
  (interactive)
  (if (window-system)
      (set-frame-size (selected-frame) 80 default-window-height)))

(defun frame-double-wide ()
  (interactive)
  (set-frame-width (selected-frame) 163))

(defun frame-single-wide ()
  (interactive)
  (set-frame-width (selected-frame) 80))

(defun font-large ()
  (interactive)
  (set-face-attribute 'default nil :height 150))

(defun font-medium ()
  (interactive)
  (set-face-attribute 'default nil :height 120))

(defun font-small ()
  (interactive)
  (set-face-attribute 'default nil :height 105))

;; Mac setup

(if (string-equal system-type "darwin")
    (font-large))

;; Helm

(use-package helm
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f". helm-find-files)
  ("M-y" . helm-show-kill-ring)
  :config
  (helm-mode 1)
  :ensure t)

;; Crux (mostly for to replace smarter beginning of line)

(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-c k" . crux-kill-other-buffers)
   ("C-c I" . crux-find-user-init-file))
  :ensure t)

;; UTF-8

(prefer-coding-system 'utf-8)

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Backups

(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)

;; Turn off bell

(setq ring-bell-function 'ignore)

;; Automatically reload files that have changed

(global-auto-revert-mode t)

;; Show parens (bracket matching)

(show-paren-mode 1)

;; Magit

(use-package magit
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package forge
  :after magit
  :ensure t)

;; Multiple cursors

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :ensure t)

;; Undo tree

(require 'undo-tree)

(global-undo-tree-mode)

(setq undo-tree-visualizer-diff t)

;; Clojure

(use-package clojure-mode
  :ensure t)

;; Cider

(use-package cider
  :config
  (setq cider-clojure-cli-global-options "-A:dev")
  :ensure t)

;; Markdown

(use-package markdown-mode
  :ensure t)

;; Highlight Indentation

(use-package highlight-indentation
  :config
  (add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
  :ensure t)

;; 80 column rule

(use-package column-enforce-mode
  :config
  (global-column-enforce-mode t)
  :ensure t)

;; Term paste

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;; Projectile

(use-package ag
  :ensure t)

(use-package projectile
  :requires ag
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p x t") 'projectile-run-term)
  (define-key projectile-mode-map (kbd "C-c p x e") 'projectile-run-eshell)
  (projectile-mode +1)
  :bind
  (("C-c p p" . projectile-switch-project))
  :ensure t)

;; Beige theme

(use-package spacemacs-theme
  :disabled
  :defer t
  :init
  (load-theme 'spacemacs-light t)
  :ensure t)

;; Light theme

(use-package twilight-bright-theme
  :disabled
  :ensure t)

;; Dark theme

(use-package twilight-anti-bright-theme
  :disabled
  :ensure t)

;; Yaml mode

(use-package yaml-mode
  :ensure t)

;; Exec path from shell (mac only)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Org mode

(use-package org
  :config
  (custom-set-variables
   '(org-export-backends (quote (md gfm beamer))))

  ;; Organise some directories
  (setq org-directory "~/org")
  (setq org-agenda-files (list (concat org-directory "/notes.org")))

  ;; Record the finish timestamp of tasks
  (setq org-log-done 'time)

  ;; Align tags immediately after title
  (setq org-tags-column 0)

  ;; Setup the org task states
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" "CANCELLED")))

  ;; Setup capture
  (setq org-default-notes-file
        (concat org-directory "/refile.org"))

  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :ensure t)

(use-package babel
  :config
  (add-hook 'org-mode-hook 'visual-line-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (sql . t)
     (ditaa . t)
     (restclient . t)
     (dot . t)))
  :after org
  :ensure t)

;; Move lines

(require 'move-lines)

;; Binds to:
;; - Move lines up (M-p or M-<up>)
;; - Move lines down (M-n or M-<down>)

(move-lines-binding)

;; Shell

(setq explicit-shell-file-name
      "/bin/zsh")

;; Typescript

(use-package typescript-mode
  :custom
  (typescript-indent-level 2)
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))
  :ensure t)

;; Javascript (JSON)

(setq js-indent-level 2)

;; Haskell

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode))

;; Clojure

(require 'ob-clojure)

(setq org-babel-clojure-backend 'cider)

(use-package cider
  :ensure t)

;; Elixir

(use-package elixir-mode
  :ensure t)

;; Paredit

;; (use-package paredit
;;   :config
;;   (add-hook 'clojure-mode-hook #'paredit-mode)
;;   :ensure t)

;; Python

(use-package elpy
  :init
  (elpy-enable)
  (setq python-indent-offset 4)
  :ensure t)

;; Added by emacs

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
