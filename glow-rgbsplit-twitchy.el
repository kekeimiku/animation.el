;;; glow-rgbsplit-twitchy.el --- RGB split with glow effect -*- lexical-binding: t -*-

(defun glow-rgbsplit-start ()
  "Start the RGB split glow effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'glow-rgbsplit-render))
    (glow-rgbsplit-render)
    (message "RGB split glow effect started")))

(defun glow-rgbsplit-stop ()
  "Stop the RGB split glow effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'glow-rgbsplit-cleanup))
    (glow-rgbsplit-cleanup)
    (message "RGB split glow effect stopped")))

;;;###autoload
(define-minor-mode glow-rgbsplit-mode
  "Toggle RGB split with glow effect."
  :global t
  (if glow-rgbsplit-mode
      (progn
        (unless (functionp 'glow-rgbsplit-render)
          (load "glow-rgbsplit-twitchy-core"))
        (glow-rgbsplit-start))
    (glow-rgbsplit-stop)))

(provide 'glow-rgbsplit-twitchy)
;;; glow-rgbsplit-twitchy.el ends here
