(setq user-emacs-directory "~/emacs_d/")
(setq default-directory "~/")

;; Use UTF-8 everywhere
(set-language-environment    "UTF-8")
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

;; Load files smaller than 50MB without warning
(setq large-file-warning-threshold (* 50 1024 1024))

;; Store customize settings in their own file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Store private settings (e.g. email address) in private.el
(setq private-file (concat user-emacs-directory "private.el"))
(load-file private-file)

;; Put backup files somewhere else
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Spaces, not tabs
(setq-default indent-tabs-mode nil)

;; Visual settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      ring-bell-function 'ignore
      display-time-default-load-average nil
      use-dialog-box nil
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      )

(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
       (abbreviate-file-name (buffer-file-name))
       "%b"))))

(pixel-scroll-precision-mode)

;; Set up package management. I like the use-package macro (which is
;; part of emacs 29) and am trying to use regular package archives
;; rather than something like straight / elpaca.
(require 'package)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("ORG"          . "https://orgmode.org/elpa/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("GNU ELPA"     . 20)
        ("MELPA"        . 15)
        ("ORG"          . 10)
        ("MELPA Stable" . 5)
        ("nongnu"       . 0)))
(package-initialize)

;; Basic package installations (for things that require no customisation)
(use-package diminish)
(use-package git-link)
(use-package pdf-tools)
(use-package lua-mode)
(use-package rspec-mode)
(use-package yaml-mode)
(use-package virtual-auto-fill)
(use-package hledger-mode)
(use-package ripgrep)

;; https://github.com/larstvei/dot-emacs?tab=readme-ov-file#key-bindings
(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

;; MacOS keys
(setq mac-command-modifier       'meta
      mac-right-command-modifier 'meta
      mac-option-modifier        nil
      mac-right-option-modifier  nil)

;; Auto-reverting files
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode t)

;; Tidy-up whitespace everywhere
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Typography / font settings
(set-frame-font "Roboto Mono 20" nil t)

;; I prefer a light theme, but if I use a dark theme it's
;; modus-vivendi
(use-package emacs
  :config
  (load-theme 'modus-operandi))

;; Save history in minibuffers between sessions
(use-package savehist
  :init
  (savehist-mode))

;; Save recent files between sessions
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

;; Vertico
(use-package vertico
  :init
  (vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Consult
(use-package consult
  :bind (("C-c r" . consult-ripgrep)
         ("C-x b" . consult-buffer) ; orig. switch-to-buffer
         ("M-y" . consult-yank-pop) ; orig. yank-pop
         ("C-s" . consult-line))
  )

;; Orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Which key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1))

;; Editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Working with markdown
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'virtual-auto-fill-mode)
  )

;; Magit
(use-package magit
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (:map custom-bindings-map ("<f10>" . magit-status)))

;; Highlight uncommitted changes
(use-package diff-hl
  :config
  (global-diff-hl-mode 1))

;; Projects
(use-package projectile
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  )

;; Terminals
;; https://github.com/larstvei/dot-emacs?tab=readme-ov-file#vterm
(use-package vterm
  :defer t
  :preface
  (defvar vterms nil)

  (defun toggle-vterm (&optional n)
    (interactive)
    (setq vterms (seq-filter 'buffer-live-p vterms))
    (let ((default-directory (or (vc-root-dir) default-directory)))
     (cond ((numberp n) (push (vterm n) vterms))
           ((null vterms) (push (vterm 1) vterms))
           ((seq-contains-p vterms (current-buffer))
            (switch-to-buffer (car (seq-difference (buffer-list) vterms))))
           (t (switch-to-buffer (car (seq-intersection (buffer-list) vterms)))))))

  :bind (:map custom-bindings-map
              ("<f1>" . toggle-vterm)
              ("M-1" . (lambda () (interactive) (toggle-vterm 1)))
              ("M-2" . (lambda () (interactive) (toggle-vterm 2)))
              ("M-3" . (lambda () (interactive) (toggle-vterm 3)))
              ("M-4" . (lambda () (interactive) (toggle-vterm 4)))
              ("M-5" . (lambda () (interactive) (toggle-vterm 5)))
              ("M-6" . (lambda () (interactive) (toggle-vterm 6)))
              ("M-7" . (lambda () (interactive) (toggle-vterm 7)))
              ("M-8" . (lambda () (interactive) (toggle-vterm 8)))
              ("M-9" . (lambda () (interactive) (toggle-vterm 9))))

  :config
  ;; Don't query about killing vterm buffers, just kill it
  (defadvice vterm (after kill-with-no-query nil activate)
    (set-process-query-on-exit-flag (get-buffer-process ad-return-value) nil)))

;; Treesitter
(setq treesit-language-source-alist
      '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        ))

;; Working with Ruby code
(use-package ruby-ts-mode
    :mode "\\.rb\\'"
    :mode "Rakefile\\'"
    :mode "Gemfile\\'"
    :hook (ruby-ts-mode . subword-mode)
    :custom
    (ruby-indent-level 2)
    (ruby-indent-tabs-mode nil))

(use-package rbtagger
  :config
  (add-hook 'ruby-ts-mode (rbtagger-mode)))

;; Working with React / JSX code

;; Some projects use prettier. Rather than enable this globally,
;; instead I add a .dir-locals file in the project route with
;; something like
;;
;;    ((jtsx-jsx-mode . ((eval . (prettier-mode t)))))
;;
;; in it
(use-package prettier)

;; treesitter mode for JSX files
(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode))
  :commands jtsx-install-treesit-language
  :custom
  (js-indent-level 2)
  (jtsx-switch-indent-offset 0)
  (jtsx-indent-statement-block-regarding-standalone-parent nil)
  (jtsx-jsx-element-move-allow-step-out t)
  (jtsx-enable-jsx-electric-closing-element t)
  (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  (jtsx-enable-jsx-element-tags-auto-sync nil)
  (jtsx-enable-all-syntax-highlighting-features t)
  )

;; My functions
(defun my/find-logseq-daily-file ()
  "Find logseq daily file."
  (interactive)
  (find-file (format-time-string "~/Dropbox/notes/journals/%Y_%m_%d.md")))

;; Keybindings
(use-package emacs
  :bind (:map custom-bindings-map
              ("C-;" . backward-kill-word)
              ("C-c c" . comment-line)
              ("C-c o" . occur)
              ("M-/" . hippie-expand)
              ("C-<help>" . beginning-of-buffer) ;; magicforce keyboard
              ("C-<delete>" . end-of-buffer) ;; magicforce keyboard
              ("<f2>" . dired)
              ("<f9>" . my/find-logseq-daily-file)
              ))

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  :init-value t
  :keymap custom-bindings-map)

;; Set the frame size to something sensible
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 80))
           (add-to-list 'default-frame-alist (cons 'width 60)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)