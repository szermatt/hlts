;;; hlts-frame-test.el --- Tests for hlts that manipulate frame

(ert-deftest test-hlts-frame-single-window ()
  "The highlights should only be set on the visible portion of the buffer."
  :tags '(frame)
  (test-hlts-env-with-10x9-frame
   (test-hlts-setup (test-hlts-hellos 1 20))
   (test-hlts-simulate-command '(test-hlts-goto-nth "hello"))
   (should
    (equal
     (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
     (concat
      (test-hlts-hellos 1 7 t)
      "\n"
      (test-hlts-hellos 8 20))))))

(ert-deftest test-hlts-frame-multiple-windows ()
  "A buffer might have more than one visible region."
  :tags '(frame)
  (test-hlts-env-with-10x9-frame
   (test-hlts-setup (test-hlts-hellos 1 20))
   (let ((buf (current-buffer))
         (win1 (selected-window))
         (win2 (split-window-below)))
     (set-window-new-normal win1 5)
     (set-window-new-normal win2 4)
     (window-resize-apply)
     (set-window-start win2 (test-hlts-line-position 15))
     (test-hlts-simulate-command '(test-hlts-goto-nth "hello"))
     (should
      (equal
       (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
       (concat
        (test-hlts-hellos 1 4 t)
        "\n"
        (test-hlts-hellos 5 14)
        "\n"
        (test-hlts-hellos 15 17 t)
        "\n"
        (test-hlts-hellos 18 20)))))))

(ert-deftest test-hlts-frame-overlapping-windows ()
  "Visible regions of a buffer might overlap."
  :tags '(frame)
  (test-hlts-env-with-10x9-frame
   (test-hlts-setup (mapconcat (lambda (n) (format "(hello %d)" n))
                               (number-sequence 1 20) "\n"))
   (let ((buf (current-buffer))
         (win1 (selected-window))
         (win2 (split-window-below)))
     (set-window-new-normal win1 5)
     (set-window-new-normal win2 4)
     (window-resize-apply)
     (set-window-start win2 (test-hlts-line-position 3))
     (test-hlts-simulate-command '(test-hlts-goto-nth "hello"))
     (should
      (equal
       (test-hlts-text-with-face (point-min) (point-max) 'hlts-face)
       (concat
        (test-hlts-hellos 1 5 t)
        "\n"
        (test-hlts-hellos 6 20)))))))

;;; hlts-frame-test.el ends here
