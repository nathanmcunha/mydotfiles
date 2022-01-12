;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Nathan Cunha"
      user-mail-address "nathanmartins@outlook.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)
(setq doom-themes-treemacs-theme 'doom-colors)
;;If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq x-select-enable-clipboard-manager nil)

(global-auto-revert-mode t)
(setq auto-revert-check-vc-info t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! treemacs
  (setq lsp-treemacs-sync-mode 1))

(after! java-mode (set-company-backend! 'java-mode
                    (company-capf :with company-yasnippet)))
(after! js2-mode
  (set-company-backend! 'js2-mode 'company-tide 'company-yasnippet))

(use-package! tree-sitter)

(global-tree-sitter-mode)
(tree-sitter-require 'java)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(require 'lsp-java-boot)

(global-undo-tree-mode)
(setq treemacs-load-theme 'doom-colors)

(after! undo-tree
  (setq undo-tree-auto-save-history nil))

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(use-package! consult-flycheck)
(use-package! consult-lsp)
(use-package! consult-yasnippet)
(use-package! affe)
(consult-lsp-marginalia-mode)

(use-package! whitespace-cleanup-mode)
