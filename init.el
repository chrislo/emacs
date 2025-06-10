(setq user-emacs-directory "~/.emacs.d/")
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

;; Only look in one place for authinfo
(setq auth-sources '("~/.authinfo.gpg"))

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
(use-package lorem-ipsum)

(load-file (concat user-emacs-directory "my.el"))

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

;; Enable abbreviations in text mode
;; I use these to expand co-authors initials into Co-authored-by lines in git commit messages.
;;
;; In ~/.emacs.d/abbrev_defs I have something like:
;;   (define-abbrev-table 'text-mode-abbrev-table
;;    '(("ae" "Co-authored-by: Alice Example <alice@example.com>" nil :count 0)))
;;
(add-hook 'text-mode-hook 'abbrev-mode)

;; Typography / font settings
(defun my/set-face-font (face family)
  (set-face-attribute
   face nil
   :family family :weight 'regular :width 'expanded :height 200))

(my/set-face-font 'default "Iosevka")
(my/set-face-font 'fixed-pitch "Iosevka")
(my/set-face-font 'variable-pitch "Iosevka Aile")

(use-package ef-themes
  :config
  (load-theme 'ef-dream))

(use-package spacious-padding
  :init
  (spacious-padding-mode 1))

;; Encryption
(setq epg-gpg-program "gpg2")

;; Save history in minibuffers between sessions
(use-package savehist
  :init
  (savehist-mode))

;; Save recent files between sessions
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

;; Vertico
;; Display minibuffer candidates vertically, and immediately.
(use-package vertico
  :init
  (vertico-mode))

;; Configure vertico-directory extension.
;; It's awkward to work with files and paths in the minibuffer
;; character-by-character. This package (which comes with vertico, but
;; is not enabled by default) makes it quicker to delete
;; subdirectories etc.
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
;; Show previews of search candidates.
(use-package consult
  :bind (("C-c r" . consult-ripgrep)
         ("C-x b" . consult-buffer) ; orig. switch-to-buffer
         ("M-y" . consult-yank-pop) ; orig. yank-pop
         ("C-s" . consult-line))
  )

;; Orderless
;; More relaxed completion in the minibuffer
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia
;; Display helpful hints next to completion candidates.
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Autocomplete
;;
;; I haven't used autocomplete much before, I've always found it to be
;; a bit too noisy. But corfu keeps out of the way unless it's
;; requested by pressing <TAB>. By default it will also complete from
;; the current TAGS file, and I've added language keyword and filename
;; completion too. Let's see if I can get used to it.
(use-package corfu
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode)
)

(use-package emacs
  :custom
  ;; if there are less than 3 candidates, just cycle through them.
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  )

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  )

;; Which key
;; Helpful hints when partial key bindings are invoked.
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1))

;; Editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Working with markdown
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  )

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'virtual-auto-fill-mode)
  )

;; Magit
(use-package magit
  :bind (:map custom-bindings-map ("<f10>" . magit-status)))

(use-package forge
  :after magit)

;; Projects
(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (define-key projectile-command-map (kbd "a")
    (lambda ()
      (interactive)
      (let ((buf-name (format "*gptel: %s*" (projectile-project-name))))
        (if (string= (buffer-name) buf-name)
            (switch-to-buffer (other-buffer))
          (if (get-buffer buf-name)
              (switch-to-buffer buf-name)
            (gptel buf-name))))))
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

;; Install https://github.com/Shopify/ruby-lsp
(use-package eglot
  :hook ((ruby-mode . eglot-ensure)
         (ruby-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  ;; disable pop-up doc buffers
  (setq eglot-managed-mode-hook (list (lambda () (eldoc-mode -1))))
  )

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

;; gptel
(use-package gptel
  :commands (gptel gptel-send)
  :bind (("C-c C-<return>" . gptel-menu)
         ("C-c <return>" . gptel-send))
  :config
  (setq gptel-backend (gptel-make-anthropic "Claude"
    :stream t
    :key (auth-source-pick-first-password :host "api.anthropic.com")))
  (setq-default gptel-model 'claude-3-5-sonnet-20241022)
  )

;; Dired
(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (use-package dired-x
    :ensure nil
    :demand
    :config
    (setq dired-omit-files (concat dired-omit-files "\\|^.DS_STORE$"))
    :hook
    (dired-mode . dired-omit-mode))
  )

;; Supercollider
;; Install Quark following instructions at https://github.com/supercollider/scel
(add-to-list 'load-path "/Users/chris/Library/Application Support/SuperCollider/downloaded-quarks/scel/el")
(require 'sclang)
(setq exec-path (append exec-path '("/Applications/SuperCollider.app/Contents/MacOS/")))


;; Denote
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/org/notes/"))
  (denote-rename-buffer-mode 1))

(use-package denote-journal
  :ensure t
  :commands ( denote-journal-new-entry
              denote-journal-new-or-existing-entry
              denote-journal-link-or-create-entry )
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  (setq denote-journal-directory nil)
  (setq denote-journal-keyword "journal")
  (setq denote-journal-title-format 'day-date-month-year))

;; Keybindings
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(use-package emacs
  :bind (:map custom-bindings-map
              ("C-;" . backward-kill-word)
              ("C-c c" . comment-line)
              ("C-c o" . occur)
              ("M-/" . hippie-expand)
              ("M-." . xref-find-definitions)
              ("C-<help>" . beginning-of-buffer) ;; magicforce keyboard
              ("C-<delete>" . end-of-buffer) ;; magicforce keyboard
              ("<f2>" . dired)
              ("<f9>" . my/toggle-journal)
              ("M-<f9>" . my/toggle-todo)
              ("<f12>" . my/cycle-font-height)
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
    (setq initial-frame-alist '((top . 100) (left . 100)))
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
