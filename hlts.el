;;; hlts.el --- Highlight symbol at point  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmail.com>
;; Maintainer: Stephane Zermatten <szermatt@gmail.com>
;; Version: 0.1
;; Keywords: symbol highlight font-lock
;; URL: http://github.com/szermatt/hlts
;; Package-Requires: ((emacs "26.1"))


;;; Commentary:
;;
;; This package highlights the symbol at point in the current buffer
;; if it appears more than once on the screen. Symbols are highlighted
;; with the face `hlts-face' after `hlts-idle-timeout' seconds
;; of inactivity.
;;
;; On buffers on which font-lock is active, it relies on it to avoid
;; highlighting keywords, string or documentation.
;;
;; To enable symbol highlighting on a buffer, turn on the minor mode
;; `hlts-mode'.
;;
;; Example:
;;  (add-hook 'prog-mode-hook 'hlts-mode)
;;
;; The approach taken by this package to highlight symbols avoids
;; slowdowns when displaying very large buffers, as only the visible
;; area of the buffer matters. It leaves font-lock's operations alone.

;;; Code:

(defgroup hlts nil
  "Highlight symbol under point."
  :prefix "hlts-" :group 'editing)

(defface hlts-face
  '((t :inherit highlight))
  "Face used to highlight the symbol under point."
  :group 'hlts)

(defvar hlts--current nil
  "The current highlight. If non-nil, this is a hlts--d struct.")

(defvar hlts--timer nil
  "Idle timer for auto-highlight.")

(defcustom hlts-idle-timeout 0.25
  "Delay before highlighting the symbol at point.

This sets the number of seconds of idle time needed before
updating the symbol highlights."
  :type 'number
  :group 'hlts
  :set (lambda (var value)
         (set var value)
         (when hlts--timer
           (hlts--update-timer))
         value))

(defcustom hlts-disable-for-faces '(font-lock-keyword-face
                                       font-lock-type-face
                                       font-lock-preprocessor-face
                                       font-lock-comment-face
                                       font-lock-string-face
                                       font-lock-doc-face)
  "Disable highlighting in text with one of the given faces.

This is used to avoid uninteresting elements or uninteresting
regions, based on font lock."
  :type '(repeat face)
  :group 'hlts)

(defcustom hlts-overlay-priority 0
  "Priority of the overlays used to highlight symbol at point."
  :type 'number
  :group 'hlts)

(cl-defstruct (hlts--d (:constructor hlts--d-create)
                          (:copier nil))
  buffer         ; buffer containing the highlight
  symbol         ; symbol text
  symbol-start   ; start of highlighted symbol in buffer
  symbol-end     ; end of highlighed symbol in buffer
  overlays       ; list of overlays created for the symbol
  )

;;;###autoload
(define-minor-mode hlts-mode
  "Minor mode that highlights the symbol under point."
  nil " hlts" nil
  (if hlts-mode
      ;; on
      (progn
        (hlts--set-timer)
        (add-hook 'post-command-hook 'hlts--post-command nil t)
        (add-hook 'activate-mark-hook 'hlts--post-command nil t)
        (add-hook 'deactivate-mark-hook 'hlts--post-command nil t)
        (add-hook 'window-size-change-functions 'hlts--window-size-change))
    ;; off
    (hlts--off)
    (remove-hook 'post-command-hook 'hlts--post-command t)
    (remove-hook 'activate-mark-hook 'hlts--post-command t)
    (remove-hook 'deactivate-mark-hook 'hlts--post-command t)
    (when (not (hlts--turned-on-anywhere-p))
      (hlts--clear-timer)
      (remove-hook 'window-size-change-functions 'hlts--window-size-change))))

(defun hlts--clear-timer ()
  "Clears the idle timer."
  (when hlts--timer
    (cancel-timer hlts--timer)
    (setq hlts--timer nil)))

(defun hlts--set-timer ()
  "Set or reset the idle timer configured by `hlts-idle-timeout'."
  (hlts--clear-timer)
  (setq hlts--timer
        (run-with-idle-timer
         hlts-idle-timeout 'repeat 'hlts--maybe-on)))

(defun hlts--update-timer ()
  "Update the timer, if it already exists."
  (if hlts--timer (hlts--set-timer)))

(defun hlts--maybe-on ()
  "Highlight symbol at point, if appropriate."
  (when (and hlts-mode
             (null (hlts--get-current))
             (not mark-active)
             (not (hlts--disabled-at-point (point))))
    (hlts--off)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when (and bounds (< (point) (cdr bounds)))
        (hlts--on bounds)))))

(defun hlts--disabled-at-point (pos)
  "Check whether highlighting should be disabled at POS."
  (memq (get-char-property pos 'face)
        hlts-disable-for-faces))

(defun hlts--on (bounds)
  "Highlight the text within BOUNDS.

Highlighting applies to all windows showing the current buffer."
  (let ((symbol (buffer-substring-no-properties
                 (car bounds) (cdr bounds)))
        (matches))
    (setq matches
          (hlts--search
           (hlts--regions)
           (concat "\\_<"  (regexp-quote symbol) "\\_>")))
    (when (> (length matches) 1)
      (let ((c (hlts--d-create
                :buffer (current-buffer)
                :symbol symbol
                :symbol-start (car bounds)
                :symbol-end (cdr bounds)))
            (symbol-len (length symbol)))
        (setq hlts--current c)
        (dolist (match matches)
          (let ((overlay (make-overlay (- match symbol-len) match)))
            (push overlay (hlts--d-overlays c))
            (overlay-put overlay 'face 'hlts-face)
            (overlay-put overlay 'priority hlts-overlay-priority)))))))

(defun hlts--post-command ()
  "Check the current highlight, disable it if not appropriate anymore."
  (let ((c (hlts--get-current)))
    (when (and c
               (or (not hlts-mode)
                   mark-active
                   (not (eq (current-buffer) (hlts--d-buffer c)))
                   (< (point) (hlts--d-symbol-start c))
                   (>= (point) (hlts--d-symbol-end c))
                   (not (string= (hlts--d-symbol c)
                                 (buffer-substring-no-properties
                                  (hlts--d-symbol-start c)
                                  (hlts--d-symbol-end c))))))
      (hlts--off))))

(defun hlts--window-size-change (frame)
  "Re-compute any highlight in FRAME, as the window size might have changed."
  (let ((c (hlts--get-current)))
    (when (and c (get-buffer-window-list (hlts--d-buffer c) nil frame))
      (run-with-idle-timer 0 nil 'hlts--refresh))))

(defun hlts--refresh ()
  "Refresh the current highlight.

This should be called wheneven window sizes might have changed."
  (hlts--off)
  (hlts--maybe-on))

(defun hlts--get-current ()
  "Return the current highlight, if any.

Ignores highlights of killed buffers."
  (when (and hlts--current
             (not (buffer-live-p (hlts--d-buffer hlts--current))))
    (setq hlts--current nil))
  hlts--current)

(defun hlts--off ()
  "Turn off symbol highlighting."
  (let ((c (hlts--get-current)))
    (when c
      (setq hlts--current nil)
      (with-current-buffer (hlts--d-buffer c)
        (dolist (overlay (hlts--d-overlays c))
          (delete-overlay overlay))))))

(defun hlts--regions ()
  "Return a list of regions to look for symbols to highlight.

This returns the visible regions of the current buffer."
  (mapcar
   (lambda (w) (cons (max (point-min) (window-start w))
                     (min (point-max) (window-end w))))
   (get-buffer-window-list nil nil t)))

(defun hlts--search (regions regexp)
  "Search in REGIONS for REGEXP.

Returns the end position of all matches found, after weeding out
the matches rejected by `hlts--disabled-at-point'."
  (let ((case-fold-search nil)
        (matches) (found))
    (dolist (region regions)
      (let ((start (car region))
            (end (cdr region)))
        (save-excursion
          (goto-char start)
          (while (setq found (search-forward-regexp regexp end 'noerror))
            (unless (or (memq found matches)
                        (hlts--disabled-at-point (1- found)))
              (push found matches))))))
    matches))

(defun hlts--turned-on-anywhere-p ()
  "True if `hlts-mode' is on on any buffers."
  (let ((blist (buffer-list)) (found))
    (while (and blist
                (not (setq found
                           (buffer-local-value 'hlts-mode (car blist)))))
      (setq blist (cdr blist)))
    found))

(provide 'hlts)
;;; hlts.el ends here
