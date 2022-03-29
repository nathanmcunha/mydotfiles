;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Nathan Cunha"
      user-mail-address "nathanmartins@outlook.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 14)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 24))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq doom-theme 'doom-dracula)
(setq doom-themes-treemacs-theme 'doom-colors)
(setq display-line-numbers-type 'relative)
(setq x-select-enable-clipboard-manager nil)
(setq global-auto-revert-mode t)
(setq auto-revert-check-vc-info t)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org-files/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(use-package! consult-flycheck)
(use-package! consult-lsp)
(use-package! consult-yasnippet)
(use-package! consult-company)
(use-package! embark)
(use-package! embark-consult)


(use-package! marginalia
  :config
  (setq marginalia-max-relative-age 0)
  (setq marginalia-align 'right)
  :init
  (setq marginalia-mode t))

(setq! prefix-help-command #'embark-prefix-help-command)

(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)


(setq +format-with-lsp t)
(define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
(define-key company-mode-map [remap completion-at-point] #'consult-company)

;; (after! consult
;;   (set-face-attribute 'consult-file nil :inherit 'consult-buffer)
;;   (setq (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) ";"))

;;( use-package! orderless
;;   :init
;;   (setq completion-styles '(orderless)
;;           completion-category-defaults nil
;;           completion-category-overrides '((file (styles partial-completion)))))
;; ;
                                        ; Example configuration for Consult
(use-package! consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file))

(use-package! embark-consult
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
 (embark-collect-mode . consult-preview-at-point-mode))

(setq
 indent-tabs-mode t
 tab-width 2 c-basic-offset 2)


;;(setq lsp-java-format-settings-url "file://home/nathanmartins/.doom.d/java-settings/eclipse-java-google-style.xml")
(setq lsp-java-format-settings-url (lsp--path-to-uri "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"))
(setq lsp-java-format-settings-profile "GoogleStyle")

;; (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")

(setq lsp-java-completion-guess-method-arguments t)
(setq lsp-java-format-comments-enabled t)
(setq lsp-treemacs-sync-mode 1)
(setq lsp-lens-enable t)
(setq lsp-ui-sideline-enable t)
(setq lsp-modeline-code-actions-enable t)
(setq lsp-signature-auto-activate t)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)

(require 'lsp-java-boot)
;; to enable the lenses
(add-hook! 'lsp-mode-hook (setq lsp-lens-mode t))
(add-hook! 'java-mode-hook (setq lsp-java-boot-lens-mode t))

;;(add-hook! 'java-mode-hook lsp-ui-imenu t)

(use-package! ht)
;; (use-package! lsp-sonarlint)
;; (require 'lsp-sonarlint-java)
;; (
;r setq lsp-sonarlint-java-enabled t)
;;(setq lsp-sonarlint-java-analyzer-path "/home/nathanmartins/.config/JetBrains/IdeaIC2021.3/sonarlint/storage/6162692d726577617264732d73657276696365/sonar-java-plugin-7.8.1.28740.jar")

(use-package! treemacs
  :config
  (set treemacs-indent-guide-style 'block))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook! 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

(map! :leader
      (:prefix ("l". "list-neisan-custom"))
      :desc "Consult-Flycheck-List-Erro"
      "l e" #'consult-flycheck)

(map! :leader
      (:prefix ("d". "Debug Utils"))
      :desc "Debug java aplication"
      "d j" #'dap-java-debug)

(map! :leader
      :desc "Dap Hydra"
      "d h" #'dap-hydra)

(map! :leader
      (:prefix ("c"))
      :desc "Consult-File-Symbols"
      "l f s" #'consult-lsp-file-symbols)


(+global-word-wrap-mode +1)
(setq doom-modeline-indent-info t)
(setq doom-modeline-display-default-persp-name t)
(setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
(setq doom-modeline-vcs-max-length 16)
(setq doom-modeline-display-default-persp-name t)

(setq centaur-tabs-set-bar 'over)
(beacon-mode 1)
(setq lsp-log-io t)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-java-organize-imports)
  (add-hook 'before-save-hook #'lsp-execute-code-action-by-kind "source.generate.finalModifiers"))

(add-hook 'java-mode #'lsp-go-install-save-hooks)

(which-func-mode 1)

(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'prog-mode-hook #'evil-vimish-fold-mode)
(add-hook 'prog-mode-hook #'marginalia-mode)
(add-hook 'prog-mode-hook #'consult-lsp-marginalia-mode)
(setq rainbow-identifiers-mode t)
(setq-hook! 'java-mode smartparens-mode t)

;;(add-hook! 'java-mode-hook  lsp-ui-imenu 'lsp-ui)
(setq lsp-ui-imenu-auto-refresh t)
(use-package! consult-lsp)
(setq consult-lsp-marginalia-mode t)
(use-package! ecukes)

(use-package! org-roam
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory "~/Documents/org-files/")
  (setq org-roam-completion-everywhere t))

(add-hook 'org-mode #'org-sticky-header-mode)

(map!
 :after org-mode
 :map org-mode-map
 ("C-SPC" #'completion-at-point))

(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat "file:" (buffer-file-name) "::" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(map! :leader
      (:prefix ("y"))
      :desc "Neisan Utils"
      " f n" #'copy-current-line-position-to-clipboard)

(setq eros-eval-result-prefix "⟹ ")

(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)

   (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(after! centaur-tabs
  (centaur-tabs-mode -1)
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "o"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above
        centaur-tabs-gray-out-icons 'buffer)
  ;;(centaur-tabs-change-fonts "P22 Underground Book" 160)
)
;; (setq x-underline-at-descent-line t)
;; Enable vertico
;;(add-to-list 'load-path (format "%sstraight/build-%s/vertico/extensions/" (file-truename doom-local-dir) emacs-version))

;; (use-package! vertico
;;   :init
;;   (vertico-mode)
;;   :custom
;;   (vertico-buffer-display-action '(display-buffer-in-side-window  . (side .top)))
;;   (setq vertico-buffer-mode t)
;; )

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
