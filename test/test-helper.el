;;; test-helper.el --- Helpers for hlts-test.el

(require 'hlts)
(require 'ert)
(require 'ert-x)

(defun test-hlts-simulate-command (command)
  "Simulate execution of COMMAND and a delay after that.

This gives the opportunity for the highlights to change as
a result of COMMAND."
  (ert-simulate-command command)
  (ert-run-idle-timers))

(defun test-hlts-setup (content)
  "Puts CONTENT into body and enables `hlts-mode'.

This gives the opportunity for the highlights to change as
a result of COMMAND."
  (insert content)
  (font-lock-mode 1)
  (font-lock-fontify-syntactically-region (point-min) (point-max))  
  (hlts-mode 1)
  (ert-run-idle-timers))

(defmacro test-hlts-env-with-10x9-frame (&rest body)
  "Run BODY in a test environment with a fake frame.

BODY is run from within a fake frame, of size 10x9 as available
in noninteractive mode. It cannot be run in interactive mode."
  ;; window-end is never updated in noninteractive mode. Fake it based
  ;; on window-start and window-height.
  `(cl-letf (((symbol-function 'window-end)
              (lambda (window &optional _update)
                (save-excursion
                  (goto-char (window-start window))
                  (goto-char (line-beginning-position (window-height window)))
                  (point))))
             (window-min-height 1))
     (test-hlts-check-10x9-frame)
     (test-hlts-env ,@body)))

(defun test-hlts-goto-nth (text &optional nth)
  "Go to the beginning of TEXT in the buffer.

If NTH is not-nil, go to the NTH occurrence of
text in the buffer."
  (interactive "Mp")
  (goto-char (point-min))
  (search-forward text nil nil nth)
  (goto-char (match-beginning 0)))

(defmacro test-hlts-env (&rest body)
  "Run BODY within a test environment.

This macro overwrites relevant flags and options, creates a temp
buffer in emacs-lisp mode and enables the hlts minor mode."
  `(progn ;;save-window-excursion
     (let ((hlts-idle-timeout 0.0)
           (hlts-disable-for-faces '(font-lock-comment-face
                                     font-lock-string-face))
           (hlts-overlay-priority 0)
           (hlts--current nil)
           (hlts--timer nil)
           (emacs-lisp-mode-hook nil))
       (ert-with-test-buffer ()
         (with-selected-window (display-buffer (current-buffer))
           (delete-other-windows)
           ;; emacs-lisp-mode provides configuration for font-lock.
           (emacs-lisp-mode)
           (hlts-mode 1)
           (unwind-protect
               (progn ,@body))
           (hlts-mode -1))))))
  
(defun test-hlts-text-with-face (start end face)
  "Return text region from START and END with FACE highlighted.

The region of the text on which FACE is active are put withing brackets
in the text that's returned."
  (let ((result nil)
        (active nil)
        (last-pos start))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (cond
         ((and (not active)
               (memq face (face-at-point nil t)))
          (push (buffer-substring-no-properties last-pos (point)) result)
          (push "[" result)
          (setq last-pos (point))
          (setq active t))
         ((and active
               (not (memq face (face-at-point nil t))))
          (push (buffer-substring-no-properties last-pos (point)) result)
          (push "]" result)
          (setq last-pos (point))
          (setq active nil)))
        (goto-char (next-char-property-change (point) end)))
      (push (buffer-substring-no-properties last-pos (point)) result)
      (when active (push "]" result)))
    (apply 'concat (nreverse result))))

(defun test-hlts-check-10x9-frame ()
  "Check the current frame.

This function makes sure that the current frame is the one setup
in batch mode, a fake fixed-size 10x9 frame that tests can rely
on. Tests that call this method cannot run in interactive mode."
  (unless noninteractive
    (error "Test can only be run in batch mode"))
  (unless (and (eq 10 (frame-width))
               (eq 9 (frame-height)))
    (error "Unexpected frame size in batch mode: %sx%s"
           (frame-width) (frame-height))))

(defun test-hlts-line-position (line)
  (save-excursion
    (goto-char (point-min))
    (goto-char (line-beginning-position line))
    (point)))

(defun test-hlts-hellos (start end &optional highlighted)
  "Returns lines containing (hello n) with n between START and END.

If HIGHLIGHTED, lines contain ([hello] n) instead, as were
returned by `test-hlts-text-with-face'."
  (mapconcat (lambda (n) (format
                          (if highlighted "([hello] %d)" "(hello %d)") n))
             (number-sequence start end) "\n"))

;;; test-helper.el ends here
