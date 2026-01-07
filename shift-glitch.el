;;; shift-glitch.el --- RGB shift glitch effect for Emacs -*- lexical-binding: t -*-

(defun shift-glitch-start ()
  "Start the RGB shift glitch effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'shift-glitch-render))
    (shift-glitch-render)
    (message "RGB shift glitch effect started")))

(defun shift-glitch-stop ()
  "Stop the RGB shift glitch effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'shift-glitch-cleanup))
    (shift-glitch-cleanup)
    (message "RGB shift glitch effect stopped")))

;;;###autoload
(define-minor-mode shift-glitch-mode
  "Toggle RGB shift glitch effect for Emacs window."
  :global t
  (if shift-glitch-mode
      (progn
        (unless (functionp 'shift-glitch-render)
          (load "shift-glitch-core"))
        (shift-glitch-start))
    (shift-glitch-stop)))

(provide 'shift-glitch)
