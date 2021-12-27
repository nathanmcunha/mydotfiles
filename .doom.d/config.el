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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq x-select-enable-clipboard-manager nil)

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
(after! magit
  (setq magit-diff-refine-hunk 'all))

(after! treemacs
  (setq lsp-treemacs-sync-mode 1))

(after! java-mode (set-company-backend! 'java-mode
                    (company-capf :with company-yasnippet)))
(after! js2-mode
  (set-company-backend! 'js2-mode 'company-tide 'company-yasnippet))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(require 'lsp-java-boot)
(after! ivy-hydra
 (defhydra hydra-ivy (:hint nil :color pink)
   "
^ ^ ^ ^ ^ ^ | ^Call^      ^ ^  | ^Cancel^ | ^Options^ | Action _w_/_s_/_a_: %-14s(ivy-action-name)
^-^-^-^-^-^-+-^-^---------^-^--+-^-^------+-^-^-------+-^^^^^^^^^^^^^^^^^^^^^^^^^^^^^---------------------------
^ ^ _k_ ^ ^ | _f_ollow occ_U_r | _i_nsert | _c_: calling %-5s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
_h_ ^+^ _l_ | _d_one      ^ ^  | _o_ops   | _M_: matcher %-5s(ivy--matcher-desc)^^^^^^^^^^^^ _T_runcate: %-11`truncate-lines
^ ^ _j_ ^ ^ | _g_o        ^ ^  | ^ ^      | _<_/_>_: shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _D_efinition of this menu
"
   ;; arrows
   ("h" ivy-beginning-of-buffer)
   ("j" ivy-next-line)
   ("k" ivy-previous-line)
   ("l" ivy-end-of-buffer)
   ;; mark
   ("m" ivy-mark)
   ("u" ivy-unmark)
   ("DEL" ivy-unmark-backward)
   ("t" ivy-toggle-marks)
   ;; actions
   ("o" keyboard-escape-quit :exit t)
   ("r" ivy-dispatching-done :exit t)
   ("C-g" keyboard-escape-quit :exit t)
   ("i" nil)
   ("C-o" nil)
   ("f" ivy-alt-done :exit nil)
   ("C-j" ivy-alt-done :exit nil)
   ("d" ivy-done :exit t)
   ("g" ivy-call)
   ("C-m" ivy-done :exit t)
   ("c" ivy-toggle-calling)
   ("M" ivy-rotate-preferred-builders)
   (">" ivy-minibuffer-grow)
   ("<" ivy-minibuffer-shrink)
   ("w" ivy-prev-action)
   ("s" ivy-next-action)
   ("a" (let ((ivy-read-action-function #'ivy-read-action-by-key))
          (ivy-read-action)))
   ("T" (setq truncate-lines (not truncate-lines)))
   ("C" ivy-toggle-case-fold)
   ("U" ivy-occur :exit t)
   ("D" (ivy-exit-with-action
         (lambda (_) (find-function 'hydra-ivy/body)))
    :exit t)))
