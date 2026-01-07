;;; glitch-effect.el --- Glitch effect for Emacs window -*- lexical-binding: t -*-

(defun glitch-effect-start ()
  "Start the glitch effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'glitch-effect-render))
    (glitch-effect-render)
    (message "Glitch effect started")))

(defun glitch-effect-stop ()
  "Stop the glitch effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'glitch-effect-cleanup))
    (glitch-effect-cleanup)
    (message "Glitch effect stopped")))

;;;###autoload
(define-minor-mode glitch-effect-mode
  "Toggle glitch effect for Emacs window."
  :global t
  (if glitch-effect-mode
      (progn
        (unless (functionp 'glitch-effect-render)
          (load "glitch-effect-core"))
        (glitch-effect-start))
    (glitch-effect-stop)))

(provide 'glitch-effect)
