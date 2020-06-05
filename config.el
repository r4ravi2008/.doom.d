;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Ravi"
      user-mail-address "r4ravi2008@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; test
(setq doom-font (font-spec :family "Monospace" :size 14)
      doom-variable-pitch-font (font-spec :family "Chalkboard"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-acario-dark)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/gtd/")
(setq org-agenda-files (directory-files-recursively "~/gtd/" "\.org$"))

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

(setq delete-by-moving-to-trash t)
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq doom-localleader-key ",")

;; i spell config to avoid spell check on code buffers
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))
(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(global-auto-revert-mode t)

(setq python-shell-interpreter "python3")
(use-package pyvenv
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

;; For change evil cursor in tty mode
(unless (display-graphic-p)
        (require 'evil-terminal-cursor-changer)
        (evil-terminal-cursor-changer-activate) ; or (etcc-on)
        )


;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  ;; :hook (python-mode . (lambda ()
                         ;; (require 'lsp-python-ms)
                         ;; (lsp)))
  :config (setq lsp-prefer-flymake nil))



;; add .epub files to run in nov-mode
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; the underscore to be recognized as word character
(modify-syntax-entry ?_ "w")
;; For language specific below examples can be found:
;; For python
;; (add-hook! 'python-mode-hook (modify-syntax-entry ?_ "w"))
;; For ruby
;; (add-hook! 'ruby-mode-hook (modify-syntax-entry ?_ "w"))
;; For Javascript
;; (add-hook! 'js2-mode-hook (modify-syntax-entry ?_ "w"))

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                                           :height 1.0))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))
(setq flymd-browser-open-function 'my-flymd-browser-function)

;; ~/.doom.d/config.el
(use-package! org-roam
   :commands (org-roam-insert org-roam-find-file org-roam org-roam-show-graph)
   :init
   (setq org-roam-directory "~/gtd")
   (map! :leader
         :prefix "n"
         :desc "Org-Roam-Insert" "i" #'org-roam-insert
         :desc "Org-Roam-Find"   "/" #'org-roam-find-file
         :desc "Org-Roam-Show-Graph" "g" #'org-roam-show-graph
         :desc "Org-Roam-Buffer" "r" #'org-roam)
   :config
(org-roam-mode +1)
(require 'org-roam-protocol))
(setq org-roam-link-title-format "R:%s")
(setq org-roam-index-file "index.org")

(defun my-org-protocol-focus-advice (orig &rest args)
  (x-focus-frame nil)
  (apply orig args))

(advice-add 'org-roam-protocol-open-ref :around
            #'my-org-protocol-focus-advice)
(advice-add 'org-roam-protocol-open-file :around
            #'my-org-protocol-focus-advice)

(setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+ROAM_KEY: ${ref}
#+TITLE: ${title}

- source :: ${ref}"
           :unnarrowed t)))

(setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :unnarrowed t)
          ("p" "private" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))

(use-package company-org-roam
  :after org-roam company org
  :config
  (push 'company-org-roam company-backends))


;; git forge stuff
(with-eval-after-load 'forge
 (push '("github.intuit.com" "github.intuit.com/api/v3"
        "github.intuit.com" forge-github-repository)
        forge-alist)
 (setq epa-pinentry-mo 'loopback)
)

(after! lsp-ui
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover t))
;; Make underscore part of word in all buffers
(modify-syntax-entry ?_ "w")

(setq doom-scratch-buffer-major-mode 'org-mode)

;; Deft settings
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/gtd")
(setq deft-recursive t)

;; disable copying to system clipboard
;; (setq select-enable-clipboard nil)

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(menu-bar-mode -1)

;; configure org-crypt

(use-package org-crypt
  :after org-mode
  :config
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key 615C4989B85D9D399C7D6D5A76A741FD7BD5DECC)
  (setq org-tags-exclude-from-inheritance '("crypt")))

;; (server-start)
;; (require 'org-protocol)
;; (require 'org-roam-protocol)
