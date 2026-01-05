;;; particle-cursor.el --- Particle effects for cursor -*- lexical-binding: t -*-

(defvar particle-cursor-commands
  '(self-insert-command newline delete-char delete-backward-char backward-delete-char-untabify)
  "Commands that trigger particle effects.")

(defun animation ()
  (when (and (display-graphic-p)
             (pos-visible-in-window-p))
    (let* ((pos (posn-at-point))
           (x-y (posn-x-y pos))
           (edges (window-inside-pixel-edges))
           (frame-char-width (frame-char-width))
           (frame-char-height (frame-char-height))
           (x (+ (car x-y) (nth 0 edges)))
           (y (+ (cdr x-y) (nth 1 edges)))
           (width frame-char-width)
           (height frame-char-height)
           (color (or (frame-parameter nil 'cursor-color) "red")))
      (when (functionp 'particle-cursor)
        (apply #'particle-cursor
               x y width height
               (color-name-to-rgb color))))))

(defun particle-on-type ()
  (when (memq this-command particle-cursor-commands)
    (animation)))

;;;###autoload
(define-minor-mode particle-cursor-mode
  "Toggle cursor particle effects."
  :global t
  (if particle-cursor-mode
      (progn
        (unless (functionp 'particle-cursor)
          (load "particle-cursor-core"))
        (add-hook 'post-command-hook #'particle-on-type))
    (remove-hook 'post-command-hook #'particle-on-type)))

(provide 'particle-cursor)
