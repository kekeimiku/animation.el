;;; text-rainbow.el --- Text rainbow -*- lexical-binding: t -*-

(defun text-rainbow-start ()
  "Start the text rainbow effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'text-rainbow-render))
    (text-rainbow-render)
    (redraw-display)
    (message "Text rainbow effect started")))

(defun text-rainbow-stop ()
  "Stop the text rainbow effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'text-rainbow-cleanup))
    (text-rainbow-cleanup)
    (redraw-display)
    (message "Text rainbow effect stopped")))

;;;###autoload
(define-minor-mode text-rainbow-mode
  "Toggle text rainbow effect."
  :global t
  (if text-rainbow-mode
      (progn
        (unless (functionp 'text-rainbow-render)
          (load "text-rainbow-core"))
        (text-rainbow-start))
    (text-rainbow-stop)))

(provide 'text-rainbow)
