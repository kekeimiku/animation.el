;;; ripple-click.el --- Ripple effect on mouse click -*- lexical-binding: t -*-

(defun ripple-click--handle-mouse-event (event)
  (when (and (display-graphic-p)
             (mouse-event-p event))
    (let* ((position (event-start event))
           (window (posn-window position))
           (pos-x-y (posn-x-y position))
           (edges (window-inside-pixel-edges window))
           (x (+ (car pos-x-y) (nth 0 edges)))
           (y (+ (cdr pos-x-y) (nth 1 edges)))
           (color (or (face-foreground 'cursor nil t)
                      (face-background 'cursor nil t)
                      "#007ACC")))
      (when (functionp 'ripple-click)
        (apply #'ripple-click
               x y
               (color-name-to-rgb color))))))

(defun ripple-click--check-mouse-click ()
  "Check if last command was a mouse click and show ripple."
  (let ((keys (this-command-keys-vector)))
    (when (and (> (length keys) 0)
               (vectorp keys))
      (let ((last-event (aref keys (1- (length keys)))))
        (when (mouse-event-p last-event)
          (ripple-click--handle-mouse-event last-event))))))

;;;###autoload
(define-minor-mode ripple-click-mode
  "Toggle ripple effect on mouse clicks."
  :global t
  (if ripple-click-mode
      (progn
        (unless (functionp 'ripple-click)
          (load "ripple-click-core"))
        (add-hook 'post-command-hook #'ripple-click--check-mouse-click))
    (remove-hook 'post-command-hook #'ripple-click--check-mouse-click)))

(provide 'ripple-click)
