
(require 'package)


;;; Code:

(use-package use-package-ensure-system-package
      :ensure t)

;; Makesure libtool, libtool-bin, and cmake are installed
;; Declare packages
(defvar my-packages
      '(
				adaptive-wrap
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
        yasnippet
        vterm
        org-bullets
        adaptive-wrap
        json-mode))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 4)
 '(ac-ispell-requires 4)
 '(org-agenda-files
  '("~/iCloudDrive/Notes/fiserv/ctlm/fiserv.bmc.notes.org" "/Users/rplace/iCloudDrive/Notes/fiserv/ad-cleanup/fiserv.db.project.org"))
 '(package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
  '(org-bullets mu4e-views mu4easy adaptive-wrap yasnippet-snippets company-c-headers corfu-candidate-overlay corfu-prescient corfu vterm flycheck-pycheckers flycheck-pyre flycheck-irony irony elpy ac-ispell git osx-clipboard org-notebook alect-themes haskell-mode company-irony))
 '(show-trailing-whitespace t))
;(package-initialize)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "yellow" :font "Lucida Grande" :height 2.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#d5d2be" :font "Lucida Grande")))))

; first, declare repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

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
    :init)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/snippet-mode"))
  (yas-global-mode 1))


  
;;
;; Org mode settings
;;
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org
  :config
  (setq
      org-log-done 'time
			org-hide-leading-stars t
      org-startup-indented t))
;;			org-indent-mode t))

(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

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

;;(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(global-flycheck-mode)
(global-company-mode)
(global-corfu-mode)

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
(add-hook 'python-mode-hook
          (lambda () (setq indent-tabs-mode t)))
;;(setq tab-width 2)

;;
;; General look and feel
;;
(visual-line-mode t)
(load-theme 'alect-dark t)
(tool-bar-mode -1)
(osx-clipboard-mode +1)
;;(adaptive-wrap-prefix-mode)
(global-visual-line-mode +1)


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

;;;(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(server-start)
(desktop-save-mode 1)

;;; init.el ends here

