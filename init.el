;;; Code:
(setq max-lisp-eval-depth 2048)
(require 'package)
(setq custom-safe-themes t)
(setq epa-pinentry-mode 'loopback)

  ;;; Package deffinitions
;; first, declare repositories
;; Makesure libtool, libtool-bin, and cmake are installed
;; pip install virtualenv pylint if they doesn't already exist
;; ;; Declare packages
(defvar my-packages
  '(adaptive-wrap
    org
    osx-clipboard
    expand-region
    helm
    magit
    markdown-mode
    wrap-region
    yaml-mode
    company
    yasnippet
    yasnippet-snippets
    vterm
    adaptive-wrap
    dracula-theme
    corfu
    elpy
    org-bullets
    org-ai
    ivy
    flycheck
    flyspell
    flyspell-correct-ivy
    flycheck
    flycheck-pyre
    flycheck-irony
    irony
    modus-themes
    ac-ispell
    auto-virtualenv
    py-snippets
    python-mode
    pylint
    pyvenv
    jedi
    json-mode))

;; Make sure package list is up to date
(when (not package-archive-contents)
  (package-refresh-contents))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (when (require pkg nil 'noerror)
      (package-install pkg))))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package modus-themes
  :init
  (setq modus-themes-mode-line '(moody accented borderless))
  (load-theme 'modus-vivendi-deuteranopia))

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

(use-package jedi
  :ensure t
  :config
  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook 'jedi:setup))

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode))

(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package cc-mode
  :ensure t
  :hook
  (c-mode . display-line-numbers-mode))

(use-package pyvenv
  :ensure t
  :init
  (pyvenv-mode t)
  (setq pyvenv-env-name "~/python_venv"
        python-shell-interpreter "~/python_venv/bin/python3")
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "~/python_venv/bin/python3"))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package pylint
  :init
  (setq flycheck-python-pylint-executable "~/python_venv/bin/pylint"
        flycheck-pylintrc "~/.pylintrc"))

(use-package python-mode
  :ensure t
  :mode (("\\.py$" . python-mode))
  :defer t
  :init
  (setq python-python-command "~/python_venv/bin/python3"
        indent-tabs-mode nil
        python-indent-offset 2
        elpy-enable t
        tab-width 2)
  (pyvenv-activate "~/python_venv")
  :hook
  (python-mode . display-line-numbers-mode)
  (python-mode . jedi-mode)
  (python-mode . yas-minor-mode)) 

(use-package elpy
  :ensure t
  :init
  (setq elpy-eldoc-show-current-function nil))

(use-package flycheck
  :init
  (setq flycheck-flake8rc "~/.flake8"))

(use-package magit
  :ensure t
  :hook
  ;;(git-commit-turn-on-fylspell)
  (git-commit-turn-on-auto-fill)
  (git-commit-mode . ac-ispell-ac-setup)
  (after-save . magit-after-save-refresh-status))

(use-package osx-clipboard
  :ensure t
  :defer t
  :if (eq system-type 'darwin))

;;(use-package dracula-theme
 ;; :ensure t
 ;; :init
  ;;(load-theme 'dracula t)
  ;; Mode lines from the dracula theme are a bit tough for me to read
  ;;(set-face-attribute 'mode-line nil
  ;;                    :background "#8b3626"
  ;;                    :foreground "#90ee90"
  ;;                    :box "#8b0000")
  ;;(set-face-attribute 'mode-line-inactive nil
  ;;                    :background "#ff1493"
  ;;                    :foreground "#2e8b57"
  ;;                    :box "#ff34b3"))

;;
;; Completion with pop-ups
;;
(use-package corfu
  :custom
  (corfu-xdauto t)
  (corfu-auto-delay 0.0)
  (corfu-quit-at-boundary 'seperator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)

  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-seperator)
              ("RET"   . nil)
              ("TAB"   . corfu-next)
              ("S-TAB" . corfu-previous)
              ("S-<return>" . corfu-insert))
  :init
  :config
  (global-corfu-mode))

(use-package yasnippet
  :config
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/snippet-mode"
                           "~/.emacs.d/elpa/yasnippet-snippets-1.0/snippets/python-mode"))
  :config
  (yas-reload-all)
  :commands
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

;;
;; Org mode settings
;;
(use-package org
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-log-done 'time
        org-hide-leading-stars t
        org-startup-indented t
        org-hide-emphasis-markers t)
  (setq-local company-backends '(company-dabbrev))
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

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 2)  ; Set this to adjust the minimum prefix length triggering auto-completion
  (setq company-tooltip-align-annotations t)  ; Align annotations to the right
  (setq company-idle-delay 0.1))  ; Adjust this to control the delay before showing suggestions

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(use-package mu4e
  :ensure nil
  :config
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
        ))
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
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(cyberpunk-theme dracula-theme org-bullets mu4e-views mu4easy adaptive-wrap yasnippet-snippets company-c-headers corfu-candidate-overlay corfu-prescient corfu vterm flycheck-pycheckers flycheck-pyre flycheck-irony irony elpy ac-ispell git osx-clipboard org-notebook alect-themes haskell-mode company-irony))
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
 '(org-document-title ((t (:inherit default :weight bold :foreground "yellow" :font "Lucida Grande" :height 2.0 :underline nil))))
 '(org-done ((t (:foreground "#00ff00" :weight bold))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;;; Org values for variable pitch fonts, only works when a window-system is enabled
;;(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

(when window-system
  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
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
 '((python . t)))

(global-flycheck-mode)
(global-company-mode)

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

;;(setenv "PYTHONPATH" "/the/python/path")

(defun set-frame-size-according-to-resolution ()
  "Set the default frame size based on display resolution.
Shamelessly bottowed from Bryan Oakley."
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

(visual-line-mode t)
(global-visual-line-mode +1)
(global-hl-line-mode)
;;(setq-default tab-width )

(setq column-number-mode t
  indent-line-function 'insert-tab)
(tool-bar-mode -1)
(display-battery-mode)
(desktop-save-mode)

(global-set-key (kbd "C-c f") 'flyspell-toggle ) ;; Make it easy to turn off spell check

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
