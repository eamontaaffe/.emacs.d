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

;; MacOS switch meta key
;;
;; This is specifically for the railwaycat distribution of emacs. There's more
;; info here: https://gist.github.com/railwaycat/3498096

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

;; Disable warning for cl package

(setq byte-compile-warnings '(cl-functions))

;; Package

(require 'package)

(setq package-enable-at-startup
      nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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

;; Show column number in the mode line

(setq column-number-mode t)

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

;; Fill column default

(setq-default fill-column 80)

(set-face-attribute 'fill-column-indicator nil :foreground "grey90")

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

(global-set-key (kbd "C-x C-y") 'capitalize-region)

;; Mac setup

(if (string-equal system-type "darwin")
    (font-large))

;; Counsel

(use-package counsel
  :config (counsel-mode)
  :ensure t)

(use-package ivy
  :after counsel
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind (("C-c C-r" . ivy-resume))
  :config (ivy-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; Avy: jump to things

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2))
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

(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.emacs.d.backups/"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

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

;; (use-package forge
;;   :after magit
;;   :ensure t)

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
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2))
  :ensure t)

;; Cider

(use-package cider
  :config
  (setq nrepl-use-ssh-fallback-for-remote-hosts t)
  :ensure t)

;; Markdown

(use-package markdown-mode
  :ensure t)

;; Highlight Indentation

(use-package highlight-indentation
  :init
  (setq yaml-block-literal-electric-alist nil)
  (setq yaml-indent-offset 2)
  :config
  (add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
  :ensure t)


;; Term paste

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;; Projectile

(use-package ag
  :ensure t)

(use-package projectile
  :requires ag
  :init
  (setq projectile-completion-system 'ido)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :ensure t)

(use-package counsel-projectile
  :config (counsel-projectile-mode)
  :ensure t) ;; Hello

;; Beige theme

(use-package spacemacs-theme
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
  :defer t
  :init
  (load-theme 'twilight-anti-bright t)
  :ensure t)

;; Yaml mode

(use-package yaml-mode
  :hook ((yaml-mode . display-fill-column-indicator-mode))
  :init
  (setq fill-column 80)
  :ensure t)

;; Exec path from shell (mac only)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; Org mode

(use-package org
  :config
  (custom-set-variables
   '(org-export-backends (quote (md gfm beamer))))

  ;; Organise some directories
  (setq org-directory "~/.org")
  (setq org-agenda-files (list (concat org-directory "/refile.org")))

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

  ;; Indent mode
  (add-hook 'org-mode-hook 'org-indent-mode)

  ;; Auto fill mode
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; Don't format durations with day
  (setq org-duration-format 'h:mm)

  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :ensure t)

(use-package ob-http
  :ensure t)

(use-package babel
  :config
  (add-hook 'org-mode-hook 'visual-line-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (sql . t)
     (ditaa . t)
     (dot . t)
     (http . t)
     (js . t)))
  :after org ob-http ob-json
  :ensure t)

;; Move lines

;; (require 'move-lines)

;; Binds to:
;; - Move lines up (M-p or M-<up>)
;; - Move lines down (M-n or M-<down>)

;; (move-lines-binding)

;; Shell

(setq explicit-shell-file-name
      "/bin/zsh")

;; Flycheck

(use-package flycheck
  :ensure t)

;; Typescript

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))
  :hook ((typescript-mode . display-fill-column-indicator-mode))
  :init
  (setq fill-column 80)
  :ensure t)

;; Javascript (JSON)

(setq js-indent-level 4)

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
;;   (add-hook 'racket-mode-hook #'paredit-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;;   :ensure t)

;; Python

;; (use-package elpy
;;   :init
;;   (elpy-enable)
;;   (setq python-indent-offset 4)
;;   :ensure t)

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i")

(add-hook 'python-mode-hook #'display-fill-column-indicator-mode)

;; Company (code completion)

(use-package company
  :init
  (global-company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  :ensure t)

;; Browse Kill Ring

(use-package browse-kill-ring
  :config
  (global-set-key (kbd "M-y") 'browse-kill-ring)
  :ensure t)

;; Dired

(defun dired-mode-setup ()
  "To be run as hook for `dired-mode'. Displays simplified file view
   by default."
  (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook 'dired-mode-setup)

;; Docker

(use-package dockerfile-mode
  :ensure t)

;; Go

(use-package go-mode
  :ensure t)

(use-package go-scratch
  :ensure t)

;; String inflection

(use-package string-inflection
  :bind
  (("C-c C-q C-j" . string-inflection-lower-camelcase))
  :ensure t)

;; Common Lisp

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl")
  :ensure t)

;; Plant UML

(use-package plantuml-mode
  :init
  (setq plantuml-jar-path "/usr/local/lib/plantuml/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  :ensure t)

;; Racket

;; (use-package racket-mode
;;   :init
;;   (setq racket-program "/Applications/Racket v8.0/bin/racket")
;;   :ensure t)

(use-package geiser-racket
  :init
  (setq geiser-active-implementations '(racket))
  :ensure t)

;; Fill region

(global-set-key (kbd "C-c C-f") 'fill-region)

;; Colourful Dired Mode

(use-package diredfl
  :commands diredfl-global-mode
  :hook (dired-mode . diredfl-mode))

;; SQL

(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432)))

(add-hook
 'sql-mode-hook
 (lambda ()
   (setq fill-column 80)
   (display-fill-column-indicator-mode)))

;; Rust

(use-package rust-mode
  :bind
  (("C-c C-c" . rust-compile)
   ("C-c C-t" . rust-test))
  :hook ((rust-mode . display-fill-column-indicator-mode))
  :init
  (setq fill-column 80)
  :ensure t)

;; Language Server

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-rust-server "rls")
  :hook ((rust-mode . lsp))
  :commands lsp
  :ensure t)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :ensure t)

;; Copilot

(use-package copilot
  :hook ((prog-mode-hook . copilot-mode))
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind (("C-c x l" . copilot-accept-completion-by-line)
         ("C-c x w" . copilot-accept-completion-by-word)
         ("C-c x a" . copilot-accept-completion))
  :init
  (add-to-list 'copilot-major-mode-alist '("python" . "python"))
  (add-to-list 'copilot-major-mode-alist '("yaml" . "yaml"))
  :ensure t)

;; Evil Mode (Vim mode)

;; (use-package evil
;;   :config
;;   (evil-mode 1)
;;   :ensure t)

;; (use-package key-chord
;;   :init
;;   (setq key-chord-two-keys-delay 1)
;;   :config
;;   (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
;;   (key-chord-mode 1)
;;   :ensure t)

;; Added by emacs

(put 'narrow-to-region 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
