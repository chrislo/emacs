(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d" default))
 '(package-selected-packages
   '(prettier editorconfig denote cape corfu gptel rbtagger jtsx markdown-mode which-key marginalia orderless vertico-directory vertico consult vterm projectile diff-hl magit doom-themes exec-path-from-shell))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (shell-command-to-string
              (concat "bundle exec htmlbeautifier -b1 " buffer-file-name)))
           nil t)
     (eval prettier-mode t)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
