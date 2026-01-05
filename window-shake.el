;;; window-shake.el --- Window shake effect on typing -*- lexical-binding: t -*-

(defvar window-shake-commands
  '(self-insert-command newline delete-char delete-backward-char backward-delete-char-untabify)
  "Commands that trigger window shake effect.")

(defvar window-shake-intensity 5
  "Shake intensity in pixels.")

(defvar window-shake-duration 0.3
  "Shake duration in seconds.")

(defun window-shake-animate ()
  "Trigger window shake animation."
  (when (and (display-graphic-p)
             (functionp 'window-shake))
    (window-shake window-shake-intensity window-shake-duration)))

(defun window-shake-on-type ()
  "Check if current command should trigger shake effect."
  (when (memq this-command window-shake-commands)
    (window-shake-animate)))

;;;###autoload
(define-minor-mode window-shake-mode
  "Toggle window shake effect on typing."
  :global t
  (if window-shake-mode
      (progn
        (unless (functionp 'window-shake)
          (load "window-shake-core"))
        (add-hook 'post-command-hook #'window-shake-on-type))
    (remove-hook 'post-command-hook #'window-shake-on-type)))

(provide 'window-shake)
