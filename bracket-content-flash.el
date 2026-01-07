;;; bracket-content-flash.el --- Flash content between matching brackets -*- lexical-binding: t -*-

(require 'cl-lib)

(defgroup bracket-content-flash nil
  "Flash the content between matching brackets when point enters them."
  :group 'convenience)

(defface bracket-content-flash-face
  '((t :inherit highlight))
  "Face used to flash content between matching brackets."
  :group 'bracket-content-flash)

(defcustom bracket-content-flash-duration 0.35
  "Total duration (in seconds) of the flash effect."
  :type 'number
  :group 'bracket-content-flash)

(defcustom bracket-content-flash-blinks 2
  "How many times to blink during a flash.

0 means do not blink (just show once for `bracket-content-flash-duration')."
  :type 'integer
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

(defvar-local bracket-content-flash--last-range nil
  "Last flashed range as (BEG . END) for this buffer.")

(defvar-local bracket-content-flash--overlay nil
  "Overlay used for flashing.")

(defvar-local bracket-content-flash--timer nil
  "Timer used for blinking/removing the overlay.")

(defvar-local bracket-content-flash--blink-step 0
  "Internal step counter for blink animation.")

(defvar-local bracket-content-flash--syntax-cache nil
  "Cached syntax tables for this buffer.

Stored as (ORIGINAL . MODIFIED-WITH-ANGLES).")

(defun bracket-content-flash--cleanup ()
  "Remove overlay/timers for current buffer."
  (when (timerp bracket-content-flash--timer)
    (cancel-timer bracket-content-flash--timer))
  (setq bracket-content-flash--timer nil
        bracket-content-flash--blink-step 0)
  (when (overlayp bracket-content-flash--overlay)
    (delete-overlay bracket-content-flash--overlay))
  (setq bracket-content-flash--overlay nil))

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
          (modify-syntax-entry ?< "(>" st))
          (modify-syntax-entry ?> ")<" st)
          (setq bracket-content-flash--syntax-cache (cons orig st))
          st)))))

(defun bracket-content-flash--current-range ()
  "Return the range to flash for the innermost enclosing bracket pair.

The returned value is (BEG . END) or nil if point is not between a pair."
  (with-syntax-table (bracket-content-flash--syntax-table)
    (let ((ppss (syntax-ppss)))
      (when (and (not (nth 3 ppss))     ; string
                 (not (nth 4 ppss)))    ; comment
        (let ((open (nth 1 ppss)))
          (when open
            (let ((close (ignore-errors (scan-sexps open 1))))
              (when (and (integerp close)
                         (> close (1+ open))
                         (> (point) open)
                         (< (point) close))
                (let* ((inner-beg (1+ open))
                       (inner-end (1- close))
                       (beg (if (> inner-end inner-beg) inner-beg open))
                       (end (if (> inner-end inner-beg) inner-end close)))
                  (when (and (< beg end)
                             (<= (- end beg) bracket-content-flash-max-length))
                    (cons beg end)))))))))))

(defun bracket-content-flash--apply-face (face)
  (when (overlayp bracket-content-flash--overlay)
    (overlay-put bracket-content-flash--overlay 'face face)))

(defun bracket-content-flash--blink-tick (total-steps)
  (setq bracket-content-flash--blink-step (1+ bracket-content-flash--blink-step))
  (cond
   ((>= bracket-content-flash--blink-step total-steps)
    (bracket-content-flash--cleanup))
   ((cl-oddp bracket-content-flash--blink-step)
    (bracket-content-flash--apply-face nil))
   (t
    (bracket-content-flash--apply-face 'bracket-content-flash-face))))

(defun bracket-content-flash--flash (beg end)
  (bracket-content-flash--cleanup)
  (setq bracket-content-flash--overlay (make-overlay beg end nil t nil))
  (overlay-put bracket-content-flash--overlay 'priority 1000)
  (overlay-put bracket-content-flash--overlay 'evaporate t)
  (bracket-content-flash--apply-face 'bracket-content-flash-face)
  (let ((blinks (max 0 bracket-content-flash-blinks))
        (buf (current-buffer)))
    (if (<= blinks 0)
        (setq bracket-content-flash--timer
              (run-with-timer bracket-content-flash-duration nil
                              (lambda ()
                                (when (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (bracket-content-flash--cleanup)))))))
      (let* ((steps (* blinks 2))
             (interval (max 0.01 (/ (max 0.01 bracket-content-flash-duration) steps))))
        (setq bracket-content-flash--blink-step 0)
        (setq bracket-content-flash--timer
              (run-with-timer interval interval
                              (lambda ()
                                (when (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (bracket-content-flash--blink-tick steps)))))))))))

(defun bracket-content-flash--post-command ()
  (unless (minibufferp)
    (let ((range (bracket-content-flash--current-range)))
      (when (or (and range (not (equal range bracket-content-flash--last-range)))
                (and (null range) bracket-content-flash--last-range))
        (setq bracket-content-flash--last-range range)
        (when range
          (bracket-content-flash--flash (car range) (cdr range)))))))

;;;###autoload
(define-minor-mode bracket-content-flash-mode
  "Flash content between matching brackets when point enters them.

This uses the current buffer's syntax table to detect bracket pairs.
Angle brackets `<>` are supported by default via
`bracket-content-flash-include-angle-brackets'."
  :global t
  (if bracket-content-flash-mode
      (add-hook 'post-command-hook #'bracket-content-flash--post-command)
    (remove-hook 'post-command-hook #'bracket-content-flash--post-command)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when bracket-content-flash--overlay
          (bracket-content-flash--cleanup))
        (setq bracket-content-flash--last-range nil)))))

(provide 'bracket-content-flash)
