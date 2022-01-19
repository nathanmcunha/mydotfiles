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
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'kaolin-bubblegum)
;;(setq doom-themes-treemacs-theme 'kaolin-treemacs-theme)
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

(use-package! tree-sitter)

(global-tree-sitter-mode)
(tree-sitter-require 'java)
(global-tree-sitter-mode)
(add-hook! 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(require 'lsp-java-boot)

(global-undo-tree-mode)
(setq treemacs-load-theme 'kaolin-treemacs-theme)

(after! undo-tree
  (setq undo-tree-auto-save-history nil))

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(use-package! consult-flycheck)
(use-package! consult-lsp)
(use-package! consult-yasnippet)
(use-package! consult-company)
(use-package! affe)
(consult-lsp-marginalia-mode)

(use-package! whitespace-cleanup-mode)


;; Configure the display per command.
;; Use a buffer with indices for imenu
;; and a flat (Ido-like) menu for M-x.
(setq vertico-multiform-commands
      '((consult-imenu buffer indexed)
        (execute-extended-command unobtrusive)))

;; Configure the display per completion category.
;; Use the grid display for files and a buffer
;; for the consult-grep commands.
(setq vertico-multiform-categories
      '((file grid)
        (consult-grep buffer)))
;; Change the default sorting function
(setq vertico-multiform-commands
      '((describe-symbol (vertico-sort-function . vertico-sort-alpha))))

(setq vertico-multiform-categories
      '((symbol (vertico-sort-function . vertico-sort-alpha))
        (file (vertico-sort-function . sort-directories-first))))

;;Configure the buffer display and the buffer display action
(setq vertico-multiform-categories
      '((consult-grep
         buffer
         (vertico-buffer-display-action . (display-buffer-same-window)))))

(setq +format-with-lsp nil)
(define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)

(use-package! kaolin-themes
  :config
  (load-theme 'kaolin-eclipse t)
  (kaolin-treemacs-theme))

;; vertico, orderlss, consult, marginalia, embark
(use-package! vertico
  :init
  (vertico-mode +1))

(use-package! orderless
  :init
  (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion)))))
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package! marginalia
  :init (marginalia-mode))

(setq prefix-help-command #'embark-prefix-help-command)
(define-key company-mode-map [remap completion-at-point] #'consult-company)


(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(map! :leader
      (:prefix ("l". "list-neisan-custom"))
      :desc "Consult-Flycheck-List-Erro"
      "l e" #'consult-flycheck)

(map! :leader
      :desc "Debug java aplication"
      "d j" #'dap-java-debug)

(map! "D" #'dap-ui-breakpoint-delete)
