;;; Code:

(require 'package)

;;; Package deffinitions
;; first, declare repositories
;; Makesure libtool, libtool-bin, and cmake are installed
;; pip install virtualenv if it doesn't already exist
;; ;; Declare packages
(defvar my-packages
  '(adaptive-wrap
    org
    osx-clipboard
    alect-themes
    expand-region
    helm
    jinja2-mode
    magit
    markdown-mode
    paredit
    wrap-region
    yaml-mode
    company
    company-jedi
    yasnippet
    yasnippet-snippets
    vterm
    adaptive-wrap
    dracula-theme
    corfu
    elpy
    org-bullets
    ivy
    flycheck
    flyspell
    flyspell-correct-ivy
    flycheck-pycheckers
    flycheck-pyre
    flycheck-irony
    irony
    ac-ispell
    auto-virtualenv
    py-snippets
    python-mode
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

(use-package flycheck
  :commands
  (global-flycheck-mode))

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

(use-package python-mode
  :ensure t
  :init
  (setq python-python-command "/Library/Frameworks/Python.framework/Versions/Current/bin/python3")
  (python-mode))

(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-backends "jedi")
  :commands
  (elpy-enable))

(use-package osx-clipboard
  :ensure t
  :defer t
  :if (eq system-type 'darwin))

(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t))

(use-package use-package-ensure-system-package
  :ensure t)


(use-package magit
  :ensure t
  :hook
  (git-commit-turn-on-flyspell)
  (git-commit-turn-on-auto-fill)
  (git-commit-mode . ac-ispell-ac-setup)
  (after-save . magit-after-save-refresh-status))


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
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/snippet-mode"))
  :hook
  (org-mode . yas-minor-mode)
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

  :hook
  (org-mode . flyspell-mode)
  (org-mode . yas-minor-mode)
  (org-mode . visual-line-mode))

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :after org)

(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode))

(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi)
  :hook
  (python-mode jedi:setup))




(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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
     ("melpa" . "https://melpa.org/packages/")))
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
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

(custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5 :foreground "royal blue"))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75 :foreground "red"))))
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
)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;;(global-flycheck-mode)
(global-company-mode)

 (eval-after-load "auto-complete"
   '(progn
      (ac-ispell-setup)))

 ;; (add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
 (add-hook 'python-mode-hook
           (lambda () (setq indent-tabs-mode t)))

;;; Python specific stuff
 (add-hook 'python-mode-hook
           (lambda ()
             (setq indent-tabs-mode t)
             (setq tab-width 2)
             (setq python-indent-offset 2)))

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
(setq-default tab-width 2)

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

(elpy-enable)

;;; init.el ends here
