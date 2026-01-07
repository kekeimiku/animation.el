;;; glow-text.el --- Golden spiral glow effect for text -*- lexical-binding: t -*-

(defun glow-text-start ()
  "Start the text glow effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'glow-text-render))
    (glow-text-render)
    (message "Text glow effect started")))

(defun glow-text-stop ()
  "Stop the text glow effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'glow-text-cleanup))
    (glow-text-cleanup)
    (message "Text glow effect stopped")))

;;;###autoload
(define-minor-mode glow-text-mode
  "Toggle golden spiral glow effect for text."
  :global t
  (if glow-text-mode
      (progn
        (unless (functionp 'glow-text-render)
          (load "glow-text-core"))
        (glow-text-start))
    (glow-text-stop)))

(provide 'glow-text)
