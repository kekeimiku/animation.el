;;; bloom.el --- Golden spiral bloom effect for text -*- lexical-binding: t -*-

(defun bloom-start ()
  "Start the text bloom effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'bloom-render))
    (bloom-render)
    (message "Text bloom effect started")))

(defun bloom-stop ()
  "Stop the text bloom effect."
  (interactive)
  (when (and (display-graphic-p)
             (functionp 'bloom-cleanup))
    (bloom-cleanup)
    (message "Text bloom effect stopped")))

;;;###autoload
(define-minor-mode bloom-mode
  "Toggle golden spiral bloom effect for text."
  :global t
  (if bloom-mode
      (progn
        (unless (functionp 'bloom-render)
          (load "bloom-core"))
        (bloom-start))
    (bloom-stop)))

(provide 'bloom)
