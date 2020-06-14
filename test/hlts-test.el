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

;;; hlts-test.el ends here
