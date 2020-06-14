;;; hlts-test.el --- Tests for hlts

(ert-deftest test-hlts-highlight ()
  "Highlight symbol under point."
  (test-hlts-env
   (test-hlts-setup "(let ((hello)) (setq hello 1))")
   (test-hlts-simulate-command '(test-hlts-goto-nth "hello"))
   (should
    (equal
     (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
     "(let (([hello])) (setq [hello] 1))"))))

(ert-deftest test-hlts-highlight-2nd ()
  "Highlight symbol under point, even before the point."
  (test-hlts-env
   (test-hlts-setup "(let ((hello)) (setq hello 1))")
   (test-hlts-simulate-command '(test-hlts-goto-nth "hello" 2))
   (should
    (equal
     (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
     "(let (([hello])) (setq [hello] 1))"))))

(ert-deftest test-hlts-unhighlight-after-move ()
  "Remove highlight when point leaves symbol."
  (test-hlts-env
   (test-hlts-setup "(let ((hello)) (setq hello 1))")
   (test-hlts-simulate-command '(test-hlts-goto-nth "hello"))
   (test-hlts-simulate-command '(left-char))
   (should
    (equal
     (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
     "(let ((hello)) (setq hello 1))"))))

(ert-deftest test-hlts-rehighlight-after-move ()
  "Put back highlight when point goes back to symbol."
  (test-hlts-env
   (test-hlts-setup "(let ((hello)) (setq hello 1))")
   (test-hlts-simulate-command '(test-hlts-goto-nth "hello"))
   (test-hlts-simulate-command '(left-char))
   (test-hlts-simulate-command '(right-char))
   (should
    (equal
     (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
     "(let (([hello])) (setq [hello] 1))"))))

(ert-deftest test-hlts-single-symbol ()
  "Do not highlight a symbol that appears only once."
  (test-hlts-env
   (test-hlts-setup "(let ((hello)) (setq not-hello 1))")
   (test-hlts-simulate-command '(test-hlts-goto-nth "hello"))
   (should
    (equal
     (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
     "(let ((hello)) (setq not-hello 1))"))))

(ert-deftest test-hlts-ignore-comments-and-strings ()
  "Ignore matches within comments and strings."
  (test-hlts-env
   (test-hlts-setup "(let ((hello))
;; say hello 
(setq hello \"hello\")
(message hello))")
   (test-hlts-simulate-command '(test-hlts-goto-nth "hello"))
   (should
    (equal
     (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
     "(let (([hello]))
;; say hello 
(setq [hello] \"hello\")
(message [hello]))"))))


;;; hlts-test.el ends here
