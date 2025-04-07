(defconst emacs-start-time (float-time))
(defun print-time-since-init (loc)
  (let* ((now (float-time))
         (elapsed (- now emacs-start-time)))
    (message "Init file %s: time=%.4f sec" loc elapsed)))

(message "Starting emacs... early-init.el")

(setq load-prefer-newer t)

;; Resizing the Emacs frame can be an expensive part of changing the
;; font. Inhibit this to reduce startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)
