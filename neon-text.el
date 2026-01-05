;;; neon-text.el --- Neon glow effect for text -*- lexical-binding: t -*-

(defvar neon-text--rainbow-colors
  '("#FF0000" "#FF7F00" "#FFFF00" "#00FF00" "#0000FF" "#4B0082" "#9400D3")
  "Rainbow colors: Red, Orange, Yellow, Green, Blue, Indigo, Violet.")

(defvar neon-text--overlays nil
  "List of overlays with neon effect.")

(defvar neon-text--color-offset 0
  "Current color offset for animation.")

(defun neon-text--get-rainbow-color (index)
  "Get rainbow color for INDEX with current animation offset."
  (let* ((colors neon-text--rainbow-colors)
         (color-count (length colors))
         (color-index (mod (+ index neon-text--color-offset) color-count)))
    (nth color-index colors)))

(defun neon-text--create-overlay (beg end color)
  "Create a neon effect overlay from BEG to END with COLOR."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'face `(:foreground ,color :weight bold))
    (overlay-put ov 'neon-text t)
    (push ov neon-text--overlays)
    ov))

(defun neon-text--remove-all-overlays ()
  "Remove all neon text overlays."
  (dolist (ov neon-text--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq neon-text--overlays nil))

(defun neon-text--apply-to-buffer ()
  "Apply rainbow neon effect to all text in current buffer."
  (neon-text--remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (let ((char-index 0))
      (while (not (eobp))
        (unless (looking-at-p "[[:space:]\n]")
          (let ((char-end (min (1+ (point)) (point-max))))
            (neon-text--create-overlay (point) char-end 
                                       (neon-text--get-rainbow-color char-index))
            (setq char-index (1+ char-index))))
        (forward-char 1)))))

(defun neon-text--update-display ()
  "Update neon effect display using native rendering."
  (when (and (display-graphic-p)
             (functionp 'neon-text-render))
    (neon-text-render)))

(defvar neon-text--timer nil
  "Timer for neon text glow animation.")

(defun neon-text--animate-colors ()
  "Animate rainbow colors by shifting the offset."
  (setq neon-text--color-offset (mod (1+ neon-text--color-offset) 
                                     (length neon-text--rainbow-colors)))
  (neon-text--apply-to-buffer)
  (neon-text--update-display))

(defun neon-text--start-animation ()
  "Start neon glow animation."
  (when neon-text--timer
    (cancel-timer neon-text--timer))
  (setq neon-text--timer
        (run-with-timer 0 0.15 #'neon-text--animate-colors)))

(defun neon-text--stop-animation ()
  "Stop neon glow animation."
  (when neon-text--timer
    (cancel-timer neon-text--timer)
    (setq neon-text--timer nil))
  (when (functionp 'neon-text-stop)
    (neon-text-stop))
  (setq neon-text--color-offset 0))

;;;###autoload
(define-minor-mode neon-text-mode
  "Toggle neon glow effect for text."
  :global t
  (if neon-text-mode
      (progn
        (unless (functionp 'neon-text-render)
          (load "neon-text-core"))
        (neon-text--apply-to-buffer)
        (neon-text--start-animation)
        (add-hook 'after-change-functions #'neon-text--on-change nil t)
        (add-hook 'window-scroll-functions #'neon-text--on-scroll nil t))
    (neon-text--stop-animation)
    (neon-text--remove-all-overlays)
    (remove-hook 'after-change-functions #'neon-text--on-change t)
    (remove-hook 'window-scroll-functions #'neon-text--on-scroll t)))

(defun neon-text--on-change (_beg _end _len)
  "Handle buffer changes."
  (when neon-text-mode
    (neon-text--apply-to-buffer)))

(defun neon-text--on-scroll (_window _start)
  "Handle window scrolling."
  (when neon-text-mode
    (neon-text--update-display)))

(provide 'neon-text)
