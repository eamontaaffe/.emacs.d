;; No splash screen

(setq inhibit-startup-message t)

;; Window

(defvar default-window-height 40)

(menu-bar-mode -1)

(toggle-scroll-bar -1)

(tool-bar-mode -1)

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

;; Bootstrap `use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

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

;; Magit

(use-package magit
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

(use-package undo-tree
  :disabled
  :config
  (global-undo-tree-mode)
  :ensure t)

;; Clojure

(use-package clojure-mode
  :ensure t)

;; Cider

(use-package cider
  :disabled
  :pin melpa-stable
  :ensure t)

;; Markdown

(use-package markdown-mode
  :ensure t)

;; Visualise Git Diffs

(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode)
  :ensure t)

;; Highlight Indentation

(use-package highlight-indentation
  :config
  (add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
  :ensure t)

;; 80 column rule

(use-package whitespace
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail trailing))
  (add-hook 'prog-mode-hook 'whitespace-mode)
  :ensure t)

;; Scala

(use-package ensime
  :init
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  (setq ensime-startup-notification nil)
  :ensure t)

;; Term paste

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;; Ivy

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  :bind
  (("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume))
  :ensure t)

;; Swiper

(use-package swiper
  :bind
  (("\C-s" . swiper))
  :requires ivy
  :ensure t)

;; Counsel

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("C-S-o" . counsel-rhythmbox)
   ("M-y" . counsel-yank-pop))
  :requires ivy
  :ensure t)

;; Projectile

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :bind
  (("C-c p p" . projectile-switch-project))
  :ensure t)

;; Counsel projectile extension

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  :ensure t)

;; Light theme

(use-package twilight-bright-theme
  :disabled
  :ensure t)

;; Dark theme

(use-package twilight-anti-bright-theme
  :ensure t)

;; Centered window layout

(use-package olivetti
  :init
  (setq olivetti-body-width 80)
  :ensure t)

;; Yaml mode

(use-package yaml-mode
  :ensure t)

;; Digdag

(add-to-list 'auto-mode-alist
             '("\\.dig\\'" . yaml-mode))

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
   '(org-export-backends (quote (md gfm))))
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
     (racket . t)
     (dot . t)))
  :after org
  :ensure t)

(use-package ox-gfm
  :after org
  :ensure t)

;; Artist mode

(eval-after-load "artist"
  '(define-key artist-mode-map
     [(down-mouse-3)]
     'artist-mouse-choose-operation))

;; Racket

(use-package racket-mode
  :ensure t)

(require 'ob-racket)
(put 'narrow-to-region 'disabled nil)

(put 'downcase-region 'disabled nil)

;; Move lines

(require 'move-lines)

;; Binds to:
;; - Move lines up (M-p or M-<up>)
;; - Move lines down (M-n or M-<down>)

(move-lines-binding)
