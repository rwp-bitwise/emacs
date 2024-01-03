(require 'package)

; first, declare repositories
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package corfu
  :custom
    (corfu-auto t)
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


(adaptive-wrap-prefix-mode)
(global-visual-line-mode +1)

(global-flycheck-mode)
(global-company-mode)
(global-corfu-mode)

;; Declare packages
(setq my-packages
      '(cider
        projectile
        clojure-mode
        expand-region
        helm
        jinja2-mode
        magit
        markdown-mode
        paredit
        wrap-region
        yaml-mode
	      elpy
        vterm
        yasnippet
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
   '(adaptive-wrap yasnippet-snippets company-c-headers corfu-candidate-overlay corfu-prescient corfu vterm flycheck-pycheckers flycheck-pyre flycheck-irony irony elpy ac-ispell git osx-clipboard org-notebook alect-themes haskell-mode company-irony))
 '(show-trailing-whitespace t))
;(package-initialize)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Completion words longer than 4 characters


(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setup)

(add-hook 'python-mode-hook
  (lambda () (setq indent-tabs-mode t)))  
(setq tab-width 2)

;;
;; General look and feel
;;
(visual-line-mode t)
(load-theme 'alect-dark t)
(tool-bar-mode -1)
(osx-clipboard-mode +1)

 
;;
;; Org mode settings
;;
(use-package org
  :config
  (setq org-agenda-start-wirh-log-mode t
	org-log-done 'time
	org-hide-leading-stars t
  org-startup-folded t
  org-indent-mode t))

;;
;; Function to fold all DONE items
;;
(defun my/org-get-folded-state ()
    (cond
        ((not (or (org-at-item-p) (org-at-heading-p)))
            'not-at-node)
        ((org-before-first-heading-p)
            'not-at-node)
        (t
            (let (eoh eol eos has-children children-skipped struct)
                ;; First, determine end of headline (EOH), end of subtree or item
                ;; (EOS), and if item or heading has children (HAS-CHILDREN).
                (save-excursion
                    (if (org-at-item-p)
                        (progn
                            (beginning-of-line)
                            (setq struct (org-list-struct))
                            (setq eoh (point-at-eol))
                            (setq eos (org-list-get-item-end-before-blank (point) struct))
                            (setq has-children (org-list-has-child-p (point) struct)))
                        (org-back-to-heading)
                        (setq eoh (save-excursion (outline-end-of-heading) (point)))
                        (setq eos (save-excursion (org-end-of-subtree t t)
                                      (when (bolp) (backward-char)) (point)))
                        (setq has-children
                            (or (save-excursion
                                    (let ((level (funcall outline-level)))
                                        (outline-next-heading)
                                        (and (org-at-heading-p t)
                                            (> (funcall outline-level) level))))
                                (save-excursion
                                    (org-list-search-forward (org-item-beginning-re) eos t)))))
                    ;; Determine end invisible part of buffer (EOL)
                    (beginning-of-line 2)
                    (while (and (not (eobp)) ;; this is like `next-line'
                               (get-char-property (1- (point)) 'invisible))
                        (goto-char (next-single-char-property-change (point) 'invisible))
                        (and (eolp) (beginning-of-line 2)))
                    (setq eol (point)))
                (cond
                    ((= eos eoh)
                        'empty-node)
                    ((or (>= eol eos)
                         (not (string-match "\\S-" (buffer-substring eol eos))))
                        'folded)
                    (t
                        'not-folded))))))

(defun my/org-tree-can-fold-p ()
    (not (member (my/org-get-folded-state) (list 'folded 'empty-node))))

(defun my/org-cycle-until-folded ()
    (while (my/org-tree-can-fold-p)
        (org-cycle)))

(defun my/org-hide-done-entries-in-range (start end)
    (save-excursion
        (goto-char end)
        (while (and (outline-previous-heading) (> (point) start))
            (when (org-entry-is-done-p)
                (my/org-cycle-until-folded)))))

(defun my/org-hide-done-entries-in-region (start end)
    (interactive "r")
    (my/org-hide-done-entries-in-range start end))

(defun my/org-hide-done-entries-in-buffer ()
    (interactive)
    (my/org-hide-done-entries-in-range (point-min) (point-max)))


;;
;; Set default frame size based on display resolution
;; Shamelessly bottowed from Bryan Oakley
;;
(defun set-frame-size-according-to-resolution ()
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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)


