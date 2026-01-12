;;; text-glow.el --- Text glow effect -*- lexical-binding: t -*-

(defun text-glow-start ()
  "Start the text glow effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'text-glow-render))
    (text-glow-render)
    (redraw-display)
    (message "Text glow effect started")))

(defun text-glow-stop ()
  "Stop the text glow effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'text-glow-cleanup))
    (text-glow-cleanup)
    (redraw-display)
    (message "Text glow effect stopped")))

;;;###autoload
(define-minor-mode text-glow-mode
  "Toggle text glow effect."
  :global t
  (if text-glow-mode
      (progn
        (unless (functionp 'text-glow-render)
          (load "text-glow-core"))
        (text-glow-start))
    (text-glow-stop)))

(provide 'text-glow)
