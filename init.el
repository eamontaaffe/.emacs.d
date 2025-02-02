;; No splash screen

(setq inhibit-startup-message t)

;; Window

;; (defvar default-window-height 40)

(if (display-graphic-p)
    (progn
      (toggle-scroll-bar -1)
      (tool-bar-mode -1)))

(menu-bar-mode -1)

(setq default-directory "~/")

;; Straight
;;
;; straight.el replaces package.el which the main difference being that it is
;; git repository based rather than using tarballs. This means that you can
;; inspect source code from dependencies and make changes locally.

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

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

(setq-default tab-width 4)

;; Remove text in active region if inserting text

(delete-selection-mode 1)

;; Disable minimise (I keep accidentally pressing it)

(global-unset-key (kbd "C-z"))

;; Default fill column

(setq-default fill-column 80)

;; Keep backup files in a central location

(setq backup-directory-alist
          `(("." . ,(concat user-emacs-directory "backups"))))

(setq backup-by-copying t)

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

;; Mac setup

(if (string-equal system-type "darwin")
    (font-large))

;; Counsel

(use-package counsel
  :config (counsel-mode))

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

;; Crux (mostly for to replace smarter beginning of line)

(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-c k" . crux-kill-other-buffers)
   ("C-c I" . crux-find-user-init-file)))

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
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; Multiple cursors

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

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
    (context 2)))

;; Cider

(use-package cider
  :config
  (setq nrepl-use-ssh-fallback-for-remote-hosts t))

;; Markdown

(use-package markdown-mode)

;; Highlight Indentation

(use-package highlight-indentation
  :config
  (add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode))


;; Term paste

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;; Projectile

(use-package ag)

(use-package projectile
  :requires ag
  :init
  (setq projectile-completion-system 'ido)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :config (counsel-projectile-mode)) ;; Hello

;; Theme

(use-package spacemacs-theme
  :straight (:host github :repo "nashamri/spacemacs-theme")
  :init
  (load-theme 'spacemacs-light))

;; Yaml mode

(use-package yaml-mode)

;; Exec path from shell (mac only)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
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

  ;; Indent mode
  (add-hook 'org-mode-hook 'org-indent-mode)

  ;; Auto fill mode
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; Don't format durations with day
  (setq org-duration-format 'h:mm)

  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)))

(use-package babel
  :config
  (add-hook 'org-mode-hook 'visual-line-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (sql . t)
     (ditaa . t)
     (dot . t)))
  :after org)

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
  :init (global-flycheck-mode))

(use-package flycheck-rust
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Typescript

;; (use-package typescript-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode)))

;; Javascript (JSON)

(setq js-indent-level 4)

;; Web Mode

(add-to-list
 'auto-mode-alist '("\\.\\(astro\\|ts\\|tsx\\|js\\|jsx\\|json\\|html\\)\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-indentation nil))

(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Haskell

(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode))

;; Clojure

(require 'ob-clojure)

(setq org-babel-clojure-backend 'cider)

(use-package cider)

;; Elixir

(use-package elixir-mode)

;; Paredit

;; (use-package paredit
;;   :config
;;   (add-hook 'clojure-mode-hook #'paredit-mode)
;;   (add-hook 'racket-mode-hook #'paredit-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

;; Python

;; (setq python-shell-interpreter "python3")

(setq python-shell-interpreter "rye")
(setq python-shell-interpreter-args "run ipython")

(use-package poetry
  :hook
  ((elpy-mode-hook . poetry-tracking-mode)))

(use-package elpy
  :init
  (elpy-enable)
  (setq python-indent-offset 4))

;; Company (code completion)

(use-package company
  :init
  (global-company-mode)
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  :bind
  (:map company-active-map
    ("C-n". company-select-next)
    ("C-p". company-select-previous)
    ("M-<". company-select-first)
    ("M->". company-select-last))
  :hook
  ((cider-repl-mode . company-mode)
   (cider-mode . company-mode)
   (go-mode . company-mode)
   (rust-mode . company-mode)))

;; Yasnippet

(use-package yasnippet
  :init
  (yas-global-mode))

;; Browse Kill Ring

(use-package browse-kill-ring
  :config
  (global-set-key (kbd "M-y") 'browse-kill-ring))

;; Dired

(defun dired-mode-setup ()
  "To be run as hook for `dired-mode'. Displays simplified file view
   by default."
  (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook 'dired-mode-setup)

;; Docker

(use-package dockerfile-mode)

;; Go

(use-package go-mode
  :hook
  ((go-mode . display-fill-column-indicator-mode)
   (go-mode . lsp-ui-sideline-mode)))

(use-package go-scratch)

;; String inflection

(use-package string-inflection
  :bind
  (("C-c C-q C-j" . string-inflection-lower-camelcase)))

;; Common Lisp

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl"))

;; Plant UML

(use-package plantuml-mode
  :init
  (setq plantuml-executable-path "/opt/local/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable))

;; Racket

;; (use-package racket-mode
;;   :init
;;   (setq racket-program "/Applications/Racket v8.0/bin/racket")
;;   :ensure t)

(use-package geiser-racket
  :init
  (setq geiser-active-implementations '(racket)))

;; Fill region

(global-set-key (kbd "C-c C-f") 'fill-region)

;; SQL

(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432)))

;; Rust

(use-package rust-mode
  :bind
  (("C-c C-c" . rust-compile)
   ("C-c C-t" . rust-test))
  :hook ((rust-mode . display-fill-column-indicator-mode))
  :ensure t)

;; Language Server - Eglot

(use-package eglot
  :hook ((python-mode . eglot-ensure) (rust-mode . eglot-ensure))
  :bind (("C-c C-l a" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer"
                :initializationOptions
                (:check (:allTargets :json-false)
                 :cargo (:target "thumbv8m.main-none-eabihf")))))
  :ensure t)

;; Key Chord
;;
;; Used by evil mode to map jj to escape

(use-package key-chord
  :straight (:host github :repo "emacsorphanage/key-chord")
  :init
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-mode 1))

;; Evil
;;
;; Install Evil but default to emacs mode in most major modes.

(use-package evil
  :requires key-chord
  :init
  (setq evil-default-state 'emacs)
  :config
  (evil-set-initial-state 'prog-mode 'normal)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (evil-set-initial-state 'term-mode 'emacs)
  ;; (evil-mode 1)
  )

;; Switch Option for meta


(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-pass-command-to-system nil)

;; Copilot

(use-package copilot
 :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
 :hook ((prog-mode . copilot-mode))
 :config
  (add-to-list 'copilot-indentation-alist '(org-mode . 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode . 2))
  ;; :bind
  ;; (("C-f" . copilot-accept-completion)
  ;;  ("M-f" . copilot-accept-completion-by-word)
  ;;  ("C-e" . copilot-accept-completion-by-line)
  ;;  ("M-n" . copilot-next-completion)
  ;;  ("M-p" . copilot-previous-completion))
  :ensure t)


;; Added by emacs

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(put 'downcase-region 'disabled nil)
