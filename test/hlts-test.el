;;; hlts-test.el --- Tests for hlts

(ert-deftest test-hlts-highlight ()
  (test-hlts-env
   (insert "(let ((hello)) (setq hello 1))")
   (goto-char (point-min))
   (search-forward "hello")
   (goto-char (match-beginning 0))
   (ert-run-idle-timers)
   (should
    (equal
     (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
     "(let (([hello])) (setq [hello] 1))"))))

(ert-deftest test-hlts-single-symbol ()
  (test-hlts-env
   (insert "(let ((hello)) (setq not-hello 1))")
   (goto-char (point-min))
   (search-forward "hello")
   (goto-char (match-beginning 0))
   (ert-run-idle-timers)
   (should
    (equal
     (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
     "(let ((hello)) (setq not-hello 1))"))))

(ert-deftest test-hlts-ignore-comments-and-strings ()
  (test-hlts-env
   (insert "(let ((hello))
;; say hello 
(setq hello \"hello\")
(message hello))")
   (font-lock-fontify-syntactically-region (point-min) (point-max))
   (goto-char (point-min))
   (search-forward "hello")
   (goto-char (match-beginning 0))
   (ert-run-idle-timers)
   (should
    (equal
     (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
     "(let (([hello]))
;; say hello 
(setq [hello] \"hello\")
(message [hello]))"))))


;;; hlts-test.el ends here
