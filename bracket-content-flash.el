;;; bracket-content-flash.el --- Flash content between matching brackets -*- lexical-binding: t -*-

(require 'cl-lib)

(defgroup bracket-content-flash nil
  "Flash the content inside matching delimiters at point."
  :group 'convenience)

(defface bracket-content-flash-face
  '((t :inherit region))
  "Face used to flash content between matching brackets."
  :group 'bracket-content-flash)

(defcustom bracket-content-flash-duration 0.7
  "Total duration (in seconds) of the flash effect."
  :type 'number
  :group 'bracket-content-flash)

(defcustom bracket-content-flash-max-length 2000
  "Maximum number of characters to flash.

This avoids flashing extremely large regions (e.g. huge lists)."
  :type 'integer
  :group 'bracket-content-flash)

(defcustom bracket-content-flash-include-angle-brackets t
  "Whether to treat `<` and `>` as a bracket pair for detection.

This may produce false positives in languages where `<`/`>` are commonly used
as comparison operators; disable it if that becomes distracting."
  :type 'boolean
  :group 'bracket-content-flash)

(defcustom bracket-content-flash-idle-delay 0.0
  "Seconds to wait before checking for a flash after a command.

This mode is implemented via `post-command-hook'. To avoid adding latency to
cursor movement, the expensive bracket-range computation can be deferred until
Emacs is idle. Set this to 0 for immediate flashing."
  :type 'number
  :group 'bracket-content-flash)

(defvar-local bracket-content-flash--last-range nil
  "Last flashed range as (BEG . END) for this buffer.")

(defvar-local bracket-content-flash--last-open nil
  "Last opening delimiter position that triggered a flash in this buffer.")

(defvar-local bracket-content-flash--last-point nil
  "Last point observed by `bracket-content-flash--post-command'.")

(defvar-local bracket-content-flash--defer-timer nil
  "Idle timer used to defer `bracket-content-flash--maybe-flash'.")

(defvar-local bracket-content-flash--defer-point nil
  "Point captured when scheduling `bracket-content-flash--defer-timer'.")

(defvar-local bracket-content-flash--syntax-cache nil
  "Cached syntax tables for this buffer.

Stored as (ORIGINAL . MODIFIED-WITH-ANGLES).")

(defun bracket-content-flash--cancel-defer-timer ()
  (when (timerp bracket-content-flash--defer-timer)
    (cancel-timer bracket-content-flash--defer-timer))
  (setq bracket-content-flash--defer-timer nil
        bracket-content-flash--defer-point nil))

(defun bracket-content-flash--cleanup ()
  "Remove timers for current buffer."
  (bracket-content-flash--cancel-defer-timer))

(defun bracket-content-flash--before-open-delimiter-p ()
  (with-syntax-table (bracket-content-flash--syntax-table)
    (let ((ch (char-after)))
      (and ch (eq (char-syntax ch) ?\()))))

(defun bracket-content-flash--schedule (pos)
  (bracket-content-flash--cancel-defer-timer)
  (setq bracket-content-flash--defer-point pos)
  (let ((buf (current-buffer)))
    (setq bracket-content-flash--defer-timer
          (run-with-idle-timer
           bracket-content-flash-idle-delay nil
           (lambda ()
             (when (and (buffer-live-p buf)
                        (eq buf (window-buffer (selected-window))))
               (with-current-buffer buf
                 (when (and bracket-content-flash-mode
                            (integerp bracket-content-flash--defer-point)
                            (= (point) bracket-content-flash--defer-point))
                   (bracket-content-flash--maybe-flash)))))))))

(defun bracket-content-flash--syntax-table ()
  "Return the syntax table to use for delimiter detection."
  (if (not bracket-content-flash-include-angle-brackets)
      (syntax-table)
    (let* ((orig (syntax-table))
           (cached bracket-content-flash--syntax-cache)
           (cached-orig (car-safe cached))
           (cached-mod (cdr-safe cached)))
      (if (and (eq orig cached-orig)
               (char-table-p cached-mod)
               (eq (char-table-subtype cached-mod) 'syntax-table))
          cached-mod
        (let ((st (copy-syntax-table orig)))
          (dolist (entry '((?< . "(>") (?> . ")<")))
            (modify-syntax-entry (car entry) (cdr entry) st))
          (setq bracket-content-flash--syntax-cache (cons orig st))
          st)))))

(defun bracket-content-flash--current-range ()
  "Return the range to flash when point is before an opening delimiter.

This mode flashes the content *inside* the matching pair when point is
immediately before an opening delimiter character (i.e. `char-after' is an open
delimiter in the current syntax table)."
  (with-syntax-table (bracket-content-flash--syntax-table)
    (let ((open (point))
          (ch (char-after)))
      (when (and ch (eq (char-syntax ch) ?\()))
        (let ((ppss (syntax-ppss open)))
          (unless (or (nth 3 ppss) (nth 4 ppss)) ; string/comment
            (let ((after-close (ignore-errors (scan-sexps open 1))))
              (when (and (integerp after-close) (> after-close (1+ open)))
                (let* ((beg (1+ open))
                       (end (1- after-close)))
                  (when (and (< beg end)
                             (<= (- end beg) bracket-content-flash-max-length))
                    (cons beg end))))))))))

(defun bracket-content-flash--flash (beg end)
  (when (and (display-graphic-p)
             (functionp 'bracket-content-flash-draw-rect))
    (let* ((face-bg (face-attribute 'bracket-content-flash-face :background nil t))
           (default-bg (face-attribute 'region :background nil t))
           (color (if (and (stringp face-bg) (not (string-prefix-p "unspecified" face-bg)))
                      face-bg
                    (if (and (stringp default-bg) (not (string-prefix-p "unspecified" default-bg)))
                        default-bg
                      "#FFFF00"))) ; Fallback yellow
           (rgb (color-name-to-rgb color))
           (edges (window-inside-pixel-edges))
           (win-left (nth 0 edges))
           (win-top (nth 1 edges)))
      
      (save-excursion
        (goto-char beg)
        (let ((current-line (line-number-at-pos))
              (end-line (line-number-at-pos end)))
          (while (<= current-line end-line)
            (let* ((line-end-pos (min end (line-end-position)))
                   (p1 (posn-at-point (point)))
                   (p2 (posn-at-point line-end-pos)))
              (when (and p1 p2)
                (let* ((x1 (+ (car (posn-x-y p1)) win-left))
                       (y1 (+ (cdr (posn-x-y p1)) win-top))
                       (x2 (+ (car (posn-x-y p2)) win-left))
                       (h (line-pixel-height))
                       (w (- x2 x1)))
                  (when (> w 0)
                    (apply 'bracket-content-flash-draw-rect
                           (list x1 y1 w h
                                 (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)
                                 bracket-content-flash-duration))))))
            (forward-line 1)
            (setq current-line (1+ current-line))))))))

(defun bracket-content-flash--maybe-flash ()
  (let ((range (bracket-content-flash--current-range)))
    (if (not range)
        (setq bracket-content-flash--last-open nil
              bracket-content-flash--last-range nil)
      (let ((open (point)))
        (unless (eql open bracket-content-flash--last-open)
          (setq bracket-content-flash--last-open open
                bracket-content-flash--last-range range)
          (bracket-content-flash--flash (car range) (cdr range)))))))

(defun bracket-content-flash--post-command ()
  (unless (minibufferp)
    (let ((pos (point)))
      (when (or (not (integerp bracket-content-flash--last-point))
                (/= pos bracket-content-flash--last-point))
        (setq bracket-content-flash--last-point pos)
        (if (not (bracket-content-flash--before-open-delimiter-p))
            (progn
              (setq bracket-content-flash--last-open nil
                    bracket-content-flash--last-range nil)
              (bracket-content-flash--cancel-defer-timer))
          ;; This is the only time we do any real work. The user asked for
          ;; "point is right before an opener" semantics, so make it immediate
          ;; by default.
          (if (<= bracket-content-flash-idle-delay 0)
              (bracket-content-flash--maybe-flash)
            (bracket-content-flash--schedule pos)))))))

;;;###autoload
(define-minor-mode bracket-content-flash-mode
  "Flash content inside matching brackets before an opening delimiter.

This uses the current buffer's syntax table to detect bracket pairs.
Angle brackets `<>` are supported by default via
`bracket-content-flash-include-angle-brackets'."
  :global t
  (if bracket-content-flash-mode
      (progn
        (unless (functionp 'bracket-content-flash-draw-rect)
          (load "bracket-content-flash-core" t t))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (setq bracket-content-flash--last-point (point))))
        (add-hook 'post-command-hook #'bracket-content-flash--post-command))
    (remove-hook 'post-command-hook #'bracket-content-flash--post-command)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (bracket-content-flash--cleanup)
        (setq bracket-content-flash--last-range nil
              bracket-content-flash--last-open nil
              bracket-content-flash--last-point nil)))))

(provide 'bracket-content-flash)
