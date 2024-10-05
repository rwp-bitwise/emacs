;; [[file:init.org::*General configuration][General configuration:1]]
;;; package init.el --- emacs init and config

;;; Code:
(setq max-lisp-eval-depth 2048
      custom-safe-themes t
      epa-pinentry-mode 'loopback)

(require 'package)

(add-to-list 'package-archives
           '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives
;;            '("gnu-elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
           '("non-gnu-elpa" . "https://elpa.nongnu.org/nongnu/packages") t)
(add-to-list 'package-archives
           '("melpa" . "https://stable.melpa.org/packages/") t)

(recentf-mode) ;; keep track of recently opened files, useful for consult
(global-visual-line-mode)
;; General configuration:1 ends here

(use-package lsp-mode
  :ensure t)

(use-package eglot
  :ensure t
  :bind
  (:map eglot-mode-map
        ("C-c s" . eglot-find-declaration)))


(use-package gptel
  :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package ac-ispell
  :ensure t)

(use-package flyspell
  :ensure t
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :config
  (setq ispell-extra-args '(":--sug-mode=ultra"))
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-wrapper)))

(use-package consult
  :ensure t
  :bind
  ("M-s M-b" . consult-buffer)
  ("M-s M-g" . consult-grep)
  ("M-s M-o" . consult-outline))

(use-package consult-dir
  :ensure t)

(use-package orderless
  :ensure t
  :init
  (icomplete-mode)
  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles '(orderless-literal)))

(use-package denote
  :ensure t
  :custom (denote-directory "~/iClouddrive/Notes/notes"))

(use-package ob-cypher
  :ensure t)

(use-package s
  :ensure t)

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)

  :bind
  (:map company-active-map
        ("<tab>" . company-completion-selection))

  :config
  (setq company-minimum-prefix-length 2)  ; Set this to adjust the minimum prefix length triggering auto-completion
  (setq company-tooltip-align-annotations t)  ; Align annotations to the right
  (setq company-idle-delay 0.2))  ; Adjust this to control the delay before showing suggestions

;; (add-hook 'eglot-managed-mode-hook (lambda ()
;;                                    (add-to-list 'company-backends
;;                                                 '(company-capf :with company-yasnippet))))

(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi))

(setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (lua "https://github.com/Azganoth/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (r "https://github.com/r-lib/tree-sitter-r")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (use-package jedi
    :ensure t
    :config
    (setq jedi:complete-on-dot t)
    (add-hook 'python-mode-hook 'jedi:setup))

  (use-package flycheck-rust
    :ensure t)

  (use-package cc-mode
    :ensure t
    :hook
    (c-mode . display-line-numbers-mode)
    (c++-mode . display-line-numbers-mode))

(use-package rustic
  :ensure t
  :mode (("\\.org$" . org-mode))
  :init
  (setq display-line-numbers-mode nil
        yas-minor-mode nil
        rustic-lsp-client 'eglot))

(use-package pyvenv
  :ensure t
  :init
  (pyvenv-mode t)
  (setq pyvenv-env-name "~/python_venv"
        python-shell-completion-native-enable nil
        python-shell-native-complete nil)
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "~/.venv/bin/python3"))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package python
  :ensure nil
  :mode (("\\.py$" . python-ts-mode))
  :defer t
  :init
  (setq ;;python-shell-interpreter "~/python_venv/bin/python3"
   ;;python-python-command "~/python_venv/bin/python3"
   indent-tabs-mode nil
   python-indent-offset 2)

  (pyvenv-activate "~/.venv")
  :hook
  (python-ts-mode . display-line-numbers-mode)
  (python-ts-mode . eglot-ensure)
  (python-ts-mode . company-mode)
  (python-ts-mode . yas-minor-mode))

(use-package magit
  :defer t
  :ensure t
  :hook
  (git-commit-turn-on-fylspell)
  (git-commit-turn-on-auto-fill)
  (git-commit-mode . ac-ispell-ac-setup)
  (after-save . magit-after-save-refresh-status))

(use-package osx-clipboard
  :ensure t
  :defer t
  :if (eq system-type 'darwin))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/snippet-mode"
                           "~/.emacs.d/elpa/yasnippet-snippets-1.0/snippets"))
  (yas-global-mode)

  :bind
  (:map yas-minor-mode-map
        ("C-c x" . yas-expand))) ;; This is to work around conflict of key bindings with company

(use-package yasnippet-snippets
  :ensure t)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package modus-themes
  :ensure t
  :init
;;   (setq modus-themes-mode-line '(moody accented borderless))
   (load-theme 'modus-vivendi-deuteranopia))

(defun org-completion-at-point ()
  (let ((element (org-element-at-point)))
    (when (member (org-element-property :language element)
		  '("emacs-lisp" "elisp"))
      (funcall #'elisp-completion-at-point))))
;;
;; Org mode settings
;;
(add-hook 'completion-at-point-functions 'org-completion-at-point nil t)
(add-hook 'org-mode-hook (lambda ()
                           (add-hook 'completion-at-point-functions 'org-completion-at-point t)))

(use-package org-bullets
  :ensure t)

(use-package org
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-log-done 'time
        org-hide-leading-stars t
        org-startup-indented t
        org-hide-emphasis-markers t
        org-src-tab-acts-natively t)
        ;;company-backends '(company-dabbrev))
  :hook
  (org-mode . flyspell-mode)

  (org-mode . yas-minor-mode)
  (org-mode . company-mode)
  (org-mode . visual-line-mode)
  :bind (:map org-mode-map
              ("C-c i" . org-id-get-create)))

  (use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :after org)

(use-package org-mime
  :ensure t)

;;This is a test
(use-package org-auto-tangle
  :ensure t
  :hook
  (org-mode . org-auto-tangle-mode))


(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq user-mail-address "rwplace@gmail.com"
      send-mail-function 'smtpmail-send-it
      sendmail-program "/opt/homebrew/bin/msmtp"
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      smtpmail-auth-credentials "~/.authinfo.gpg"
      smtpmail-stream-type 'starttls
      mu4e-maildir "~/Mail"
      mu4e-mu-binary "/opt/homebrew/bin/mu"
      mu4e-compose-dont=reply-to-self t
      mu4e-use-fancy-chars t
      mu4e-change-filenames-when-moving t
      mu4e-get-mail-command "mbsync --all"
      ;;mu4e-update-interval 300
      ;;mu4e-index-cleanup nil
      ;;mu4e-index-lazy-check t
      mu4e-index-update-error-warning nil
      )

;; Show emails as plain text, if possible
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "gmail-rwplace"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "rwplace@gmail.com")
                (user-full-name . "Rob Place")
                (mu4e-sent-folder . "/Gmail/Sent")
                (mu4e-drafts-folder . "/Gmail/Drafts")
                (mu4e-refile-folder . "/Gmail/All Mail")))
       (make-mu4e-context
        :name "alldyn"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/Alldyn" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "robert.place@alldyn.com")
                (user-full-name . "Rob Place")
                (mu4e-sent-folder . "/Alldyn/Sent")
                (mu4e-drafts-folder . "/Alldyn/Drafts")
                (mu4e-refile-folder . "/Alldyn/All Mail")))
       (make-mu4e-context
        :name "icloud"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/icloud" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "rwplace@mac.com")
                (user-full-name . "Rob Place")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 4)
 '(ac-ispell-requires 4)
 '(custom-safe-themes
   '("a1c18db2838b593fba371cb2623abd8f7644a7811ac53c6530eebdf8b9a25a8d" "603a831e0f2e466480cdc633ba37a0b1ae3c3e9a4e90183833bc4def3421a961" default))
 '(org-agenda-files
   '("~/iCloudDrive/Notes/fiserv/ctlm/fiserv.bmc.notes.org" "/Users/rplace/iCloudDrive/Notes/fiserv/ad-cleanup/fiserv.db.project.org"))
 ;; '(package-archives
 ;;   '(("gnu" . "https://elpa.gnu.org/packages/")
 ;;     ("melpa-stable" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(cyberpunk-theme dracula-theme org-bullets mu4e-views mu4easy adaptive-wrap yasnippet-snippets company-c-headers corfu-candidate-overlay corfu-prescient corfu vterm  flycheck-pyre flycheck-irony irony elpy ac-ispell git osx-clipboard org-notebook alect-themes haskell-mode company-irony))
 '(show-trailing-whitespace t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "yellow" :font "Sans Serif" :height 1.75 :underline nil))))
 '(org-done ((t (:foreground "#00ff00" :weight bold))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Sans Serif" :height 1.5))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Sans Serif" :height 1.25))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Sans Serif" :height 1.1))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Sans Serif" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Sans Serif"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Sans Serif"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Sans Serif"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Sans Serif"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(eglot-highlight-symbol-face ((t (:inherit bold :background "light green" :foreground "dark blue"))))
 '(mode-line ((t :background "#8b3626" :foreground "#90ee90" :box "#8b0000")))
 '(mode-line-inactive ((t :background "#008b8b" :foreground "#969696" :box "#ff34b3"))))

;;; Org values for variable pitch fonts, only works when a window-system is enabled
;;(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

(when window-system
  (let* ((variable-tuple
          (cond ;;((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ;;((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline
          `(:inherit default :weight bold :foreground ,base-font-color))) ;

(custom-theme-set-faces
 'user
 `(org-level-8 ((t (,@headline ,@variable-tuple))))
 `(org-level-7 ((t (,@headline ,@variable-tuple))))
 `(org-level-6 ((t (,@headline ,@variable-tuple))))
 `(org-level-5 ((t (,@headline ,@variable-tuple))))
 `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
 `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
 `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5 :foreground "SeaGreen3"))))
 `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75 :foreground "chartreuse3"))))
 `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
) ;; close out window system check

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (C . t)))

;;(global-flycheck-mode)
;;(global-company-mode)

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'python-ts-hook 'eglot-ensure)
;;(add-hook 'rust-mode 'eglot-ensure)

(with-eval-after-load 'eglot
(add-to-list 'eglot-server-programs
             '(c-mode . ("clangd"))))


;;(add-hook 'newsticker-start-hook

(setq newsticker-url-list
  '(("slashdot" "https://rss.slashdot.org/Slashdot/slashdotMain" nil nil nil)
   ("emacs" "https://www.reddit.com/r/emacs/.rss" nil nil nil)
   ("programming" "https://www.reddit.com/r/programming/.rss" nil nil nil)
   ("cpp" "https://www.reddit.com/r/cpp/.rss" nil nil nil)
   ("rust" "https://www.reddit.com/r/rust/.rss" nil nil nil)
   ("BaltimoreCounty" "https://www.reddit.com/r/BaltimoreCounty/.rss" nil nil nil)))

;; (setq lsp-auto-guess-root nil)

(set-face-attribute 'default nil :height 160) ;; Default to 16 point font for this old guy

(defun set-frame-size-according-to-resolution ()
  "Set the default frame size based on display resolution.
Shamelessly borrowed from Bryan Oakley."
  (interactive)
  (if window-system
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 220))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 200)
                                      (frame-char-height)))))))

(set-frame-size-according-to-resolution)

;;(global-visual-line-mode t)
(global-hl-line-mode)
(let ((shell-file-name "/bin/sh")) (shell)) ;; speeds up rendering when tail valouminous amounts of data

(setq column-number-mode t)
(tool-bar-mode -1)
(display-battery-mode)
(display-time-mode)
(desktop-save-mode)

;; Make it easy to turn off spell check
(global-set-key (kbd "C-c f") 'flyspell-toggle )

;; Key binding to split the window horizontally and automatically
;; turn on follow-mode to handle long files
(global-set-key (kbd "C-x C-t") (lambda ()
                                  (interactive)
                                  (split-window-horizontally)
                                  (follow-mode)))

;; Allow for directionally selecting visible buffers
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c n") 'newsticker-show-news)

;; ;; native tab completion
;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)

(setq windmove-wrap-around t)
(setq display-buffer-alist nil)
(setq display-buffer-alist '(
                             ("\\*Occur\\*"
                              (display-buffer-in-side-window)
                              (display-buffer-reuse-mode-window
                               display-buffer-below-selected)
                              (window-height . fit-window-to-buffer)
                              (dedicated . t)
                              (side . right))

                             ("\\*Python\\*"
                              (display-buffer-in-side-window)
                              (display-buffer-reuse-mode-window
                               display-buffer-below-selected)
                              (window-height . fit-window-to-buffer)
                              (dedicated . t)
                              (side . right))
                             ))
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)

(cond
 ((eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq osx-clipboard-mode +1)))

(cond
 ((eq system-type 'linux)
  (setq x-alt-keysym 'meta)))

(if (boundp 'server)
    (message "Emacs server is running")
  (message "Starting server")
  (server-start))

;;; init.el ends here
