;;; bracket-lightning.el --- Lightning effect between matching brackets -*- lexical-binding: t -*-

(require 'paren)

(defun bracket-lightning--trigger (&rest _)
  "Show lightning between show-paren overlays."
  (when (and (display-graphic-p)
             (overlayp show-paren--overlay)
             (overlay-buffer show-paren--overlay)
             (overlayp show-paren--overlay-1)
             (overlay-buffer show-paren--overlay-1))
    (let* ((pos1 (overlay-start show-paren--overlay))
           (pos2 (overlay-start show-paren--overlay-1))
           (xy1 (posn-x-y (posn-at-point pos1)))
           (xy2 (posn-x-y (posn-at-point pos2))))
      (when (and xy1 xy2)
        (let* ((edges (window-inside-pixel-edges))
               (cw (frame-char-width))
               (ch (frame-char-height))
               (x1 (+ (car xy1) (nth 0 edges) (/ cw 2)))
               (y1 (+ (cdr xy1) (nth 1 edges) (/ ch 2)))
               (x2 (+ (car xy2) (nth 0 edges) (/ cw 2)))
               (y2 (+ (cdr xy2) (nth 1 edges) (/ ch 2)))
               (color (face-foreground 'show-paren-match nil t))
               (rgb (color-name-to-rgb (or color "#FFFF00"))))
          (apply #'draw-lightning x1 y1 x2 y2 rgb))))))

;;;###autoload
(define-minor-mode bracket-lightning-mode
  "Show lightning between matching brackets."
  :global t
  (if bracket-lightning-mode
      (progn
        (unless show-paren-mode (show-paren-mode 1))
        (unless (functionp 'bracket-lightning)
          (load "bracket-lightning-core"))
        (advice-add 'show-paren-function :after #'bracket-lightning--trigger))
    (advice-remove 'show-paren-function #'bracket-lightning--trigger)))

(provide 'bracket-lightning)

