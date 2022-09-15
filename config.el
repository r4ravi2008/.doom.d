;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ravi"
      user-mail-address "r4ravi2008@gmail.com")


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
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/gtd/")
(setq org-agenda-files (directory-files-recursively "~/gtd/" "\.org$"))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq delete-by-moving-to-trash t)


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

(setq doom-localleader-key ",")

(global-auto-revert-mode t)

(use-package pyvenv
  :diminish
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

;; the underscore to be recognized as word character
(modify-syntax-entry ?_ "w")
;; For language specific below examples can be found:
;; For python
;; (add-hook! 'python-mode-hook (modify-syntax-entry ?_ "w"))
;; For ruby
;; (add-hook! 'ruby-mode-hook (modify-syntax-entry ?_ "w"))
;; For Javascript
;; (add-hook! 'js2-mode-hook (modify-syntax-entry ?_ "w"))

;; org-roam stuff
(use-package! org-roam
   :init
   (setq org-roam-directory "~/gtd"))

(setq org-roam-completion-everywhere t)

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

(setq doom-scratch-buffer-major-mode 'org-mode)

;; configure org-crypt
(use-package org-crypt
  :after org-mode
  :config
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key 615C4989B85D9D399C7D6D5A76A741FD7BD5DECC)
  (setq org-tags-exclude-from-inheritance '("crypt")))


;; configur rest client
(use-package restclient
  :after org-mode)
(use-package ob-restclient.el
  :after org-mode
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

;; create custom binding for inserting current date and time without user prompt
(defun insert-timestamp-custom ()
  "Insert current timestamp at point"
  (interactive)
  (insert (format-time-string (org-time-stamp-format 'long 'inactive)
                                (org-current-effective-time))))
(map! :leader
      (:prefix-map("z" . "custom-bindings")
        :desc "Insert current timestamp" "i" 'insert-timestamp-custom))

(map! :leader
      (:prefix-map("z" . "custom-bindings")
        :desc "leetcode-try" "t" 'leetcode-try))

(defun leetcode-and-view-problem()
  (interactive)
  (leetcode)
)

(map! :leader
      (:prefix-map("z" . "custom-bindings")
        :desc "leetcode-view-problem" "" 'leetcode-and-view-problem))

(defun eshell-here ()
      "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
      (interactive)
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             (height (/ (window-total-height) 3))
             (name   (car (last (split-string parent "/" t)))))
        (split-window-vertically (- height))
        (other-window 1)
        (eshell "new")
        (rename-buffer (concat "*eshell: " name "*"))

        (insert (concat "ls"))
        (eshell-send-input)))
(map! :leader
      (:prefix-map("z" . "custom-bindings")
        :desc "open eshell-here" "z" 'eshell-here))

;; (use-package! nroam
;;   :after org-roam
;;   :config
;;   (add-hook! 'org-mode-hook #'nroam-setup-maybe))

(defun org-insert-clipboard-image ()
  "Insert clipboard image using pngpaste command. Need pngpaste command installed as a prereq" (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_files/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  (shell-command (concat "pngpaste " filename))
    ; insert into file if correctly taken
  (if (file-exists-p filename)
    (insert
    (concat "#+CAPTION: "
        (file-name-base filename)
        "\n#+ATTR_ORG: :width 800\n[[file:" filename "]]\n\n"))
  )
  (org-display-inline-images)
)

;; https://emacs.stackexchange.com/questions/17990/speeding-up-company-mode
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)


(map! :gv "C-j" 'drag-stuff-down)
(map! :gv "C-k" 'drag-stuff-up)

(map! :gv "C-;" 'evil-avy-goto-char)

(map! :gv "C-'" 'swiper-avy)

(setq org-startup-with-inline-images t)
(setq org-startup-folded nil)

;; scala setup
(setq lsp-verify-signature nil)

(setq leetcode-prefer-language "java")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/leetcode")

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            yaml-mode
            nxml-mode
            xml-mode))
;;(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
;;                   :major-modes '(nix-mode)
;;                   :server-id 'nix))

;; (add-hook! 'nix-mode-hook 'nixpkgs-fmt-buffer)

;; (exec-path-from-shell-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; (add-hook 'prog-mode-hook 'copilot-mode)

;; (customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))

; complete by copilot first, then company-mode
;; (defun my-tab ()
;;   (interactive)
;;   (or (copilot-accept-completion)
;;       (company-indent-or-complete-common nil)))

;; ;
;; modify company-mode behaviors
;; (with-eval-after-load 'company
  ; disable inline previews
  ;; (delq 'company-preview-if-just-one-frontend company-frontends)
  ; enable tab completion
  ;; (define-key company-mode-map (kbd "<tab>") 'my-tab)
  ;; (define-key company-mode-map (kbd "TAB") 'my-tab)
  ;; (define-key company-active-map (kbd "<tab>") 'my-tab)
  ;; (define-key company-active-map (kbd "TAB") 'my-tab))
