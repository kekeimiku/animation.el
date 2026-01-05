;;; buffer-transition.el --- Buffer switching animations -*- lexical-binding: t -*-

(defvar buffer-transition-type 0
  "Type of transition animation.
  0 - Fade
  1 - Push from left
  2 - Push from right")

(defvar buffer-transition-duration 0.3
  "Duration of transition animation in seconds.")

(defun buffer-transition-animate (&rest _)
  "Apply transition animation when switching buffers."
  (when (and (display-graphic-p)
             (functionp 'buffer-transition))
    (buffer-transition buffer-transition-type buffer-transition-duration)))

;;;###autoload
(define-minor-mode buffer-transition-mode
  "Toggle buffer switching animations."
  :global t
  (if buffer-transition-mode
      (progn
        (unless (functionp 'buffer-transition)
          (load "buffer-transition-core"))
        (advice-add 'switch-to-buffer :after #'buffer-transition-animate)
        (advice-add 'set-window-buffer :after #'buffer-transition-animate))
    (advice-remove 'switch-to-buffer #'buffer-transition-animate)
    (advice-remove 'set-window-buffer #'buffer-transition-animate)))

(provide 'buffer-transition)
