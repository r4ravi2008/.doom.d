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
   :config
   (org-roam-mode +1)
   (require 'org-roam-protocol))

(setq org-roam-link-title-format "%s")
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

(use-package company-org-roam
  :after org-roam company org
  :config
  (push 'company-org-roam company-backends))

(setq org-roam-completion-ignore-case t)

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


;; configur rest client
;;
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
       (:prefix ("t" . "current timestamp")
        :desc "Insert current timestamp" "." 'insert-timestamp-custom)))


;; Use prettier for node projects
(defun zdx/use-prettier-if-in-node-modules ()
  "Enable prettier-js-mode iff prettier was found installed locally in project"
  (interactive)
  (let* ((file-name (or (buffer-file-name) default-directory))
         (root (locate-dominating-file file-name "node_modules"))
         (prettier (and root
                        (expand-file-name "node_modules/prettier/bin-prettier.js" root))))
    (if (and prettier (file-executable-p prettier))
        (progn
          (message "Found local prettier executable at %s. Enabling prettier-js-mode" prettier)
          (setq prettier-js-command prettier)
          (make-variable-buffer-local 'prettier-js-command)
          (prettier-js-mode)
          (message "Disabling aggressive-indent-mode in favour of prettier")
          (aggressive-indent-mode -1))
      (progn
        (message "Prettier not found in %s. Not enabling prettier-js-mode" root)
        (message "Falling back to aggressive-indent-mode")
        (aggressive-indent-mode 1)))))

(map! :leader
      (:prefix-map("z" . "custom-bindings")
        :desc "Insert current timestamp" "e" 'eshell))

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
        :desc "Insert current timestamp" "z" 'eshell-here))

(use-package! nroam
  :after org-roam
  :config
  (add-hook! 'org-mode-hook #'nroam-setup-maybe))

;; Lets you copy file link for org - mode for inserting screenshots
(defun kym/dired-copy-images-links ()
  "Works only in dired-mode, put in kill-ring,
ready to be yanked in some other org-mode file,
the links of marked image files using file-name-base as #+CAPTION.
If no file marked then do it on all images files of directory.
No file is moved nor copied anywhere.
This is intended to be used with org-redisplay-inline-images."
  (interactive)
  (if (derived-mode-p 'dired-mode)                           ; if we are in dired-mode
      (let* ((marked-files (dired-get-marked-files))         ; get marked file list
             (number-marked-files                            ; store number of marked files
              (string-to-number                              ; as a number
               (dired-number-of-marked-files))))             ; for later reference
        (when (= number-marked-files 0)                      ; if none marked then
          (dired-toggle-marks)                               ; mark all files
          (setq marked-files (dired-get-marked-files)))      ; get marked file list
        (message "Files marked for copy")                    ; info message
        (dired-number-of-marked-files)                       ; marked files info
        (kill-new "\n")                                      ; start with a newline
        (dolist (marked-file marked-files)                   ; walk the marked files list
          (when (org-file-image-p marked-file)               ; only on image files
            (kill-append                                     ; append image to kill-ring
             (concat "#+CAPTION: "                           ; as caption,
                     (file-name-base marked-file)            ; use file-name-base
                     "\n#+ATTR_ORG: :width 200\n[[file:" marked-file "]]\n\n") nil))) ; link to marked-file
        (when (= number-marked-files 0)                      ; if none were marked then
          (dired-toggle-marks)))                             ; unmark all
    (message "Error: Does not work outside dired-mode")      ; can't work not in dired-mode
    (ding)))

(defun org-take-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
               [[file:functors.org_imgs/20210520_013256_FGvj0Z.png]]   "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
    (insert (concat "[[file:" filename "]]"))))

(defun org-insert-clipboard-image ()
  "Insert clipboard image using pngpaste command. Need pngpaste command installed as a prereq"
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  (shell-command (concat "pngpaste " filename))
    ; insert into file if correctly taken
  (if (file-exists-p filename)
    (insert
    (concat "#+CAPTION: "
        (file-name-base filename)
        "\n#+ATTR_ORG: :width 200\n[[file:" filename "]]\n\n"))
  )
)
