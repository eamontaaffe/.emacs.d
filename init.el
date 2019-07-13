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
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

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

;; Remap C-a to `smarter-move-beginning-of-line'

(global-set-key [remap move-beginning-of-line]
		'smarter-move-beginning-of-line)

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
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1))

;; Multiple cursors

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :ensure t)

;; Undo tree

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :ensure t)

;; Clojure

(use-package clojure-mode
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
  (add-hook 'prog-mode-hook 'whitespace-mode))

;; Scala

(use-package ensime
  :init
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
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
   ("C-S-o" . counsel-rhythmbox))
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
  :bind
  (("C-c p s s" . counsel-projectile-ag))
  :ensure t)

;; Light theme

(use-package twilight-bright-theme
  :disabled
  :ensure t)

;; Dark theme

(use-package twilight-anti-bright-theme
  :ensure t)

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

