;;; gnus-timeline-test.el --- Timeline regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'gnus-timeline)
(require 'mm-decode)

(ert-deftest gnus-timeline-native-group-updates-only-queue-worker-results ()
  (let ((queued 0))
    (cl-letf (((symbol-function 'gnus-timeline--refresh-after-scan)
               (lambda (&rest _) (cl-incf queued))))
      (gnus-timeline--after-group-update "foreground")
      (should (= queued 0))
      (thread-join
       (make-thread (lambda () (gnus-timeline--after-group-update "source"))
                    "test-backend-worker"))
      (should (= queued 1)))))

(ert-deftest gnus-timeline-source-completions-share-one-pending-render ()
  (let ((gnus-timeline--refresh-timer nil)
        (gnus-timeline--refresh-pending nil)
        scheduled rendered)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (cl-incf scheduled) 'pending-timer))
              ((symbol-function 'gnus-timeline--rebuild-after-scan)
               (lambda () (cl-incf rendered)))
              ((symbol-function 'input-pending-p) (lambda () nil)))
      (setq scheduled 0 rendered 0)
      (dotimes (_ 10) (gnus-timeline--refresh-after-scan "example"))
      (should (= scheduled 1))
      (should (= rendered 0))
      (gnus-timeline--flush-refresh)
      (should (= rendered 1))
      (should-not gnus-timeline--refresh-pending))))

(ert-deftest gnus-timeline-input-takes-priority-over-render ()
  (let ((gnus-timeline--refresh-timer nil)
        (gnus-timeline--refresh-pending t)
        postponed)
    (cl-letf (((symbol-function 'input-pending-p) (lambda () t))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (setq postponed t) 'pending-timer))
              ((symbol-function 'gnus-timeline--rebuild-after-scan)
               (lambda () (ert-fail "Must not block pending input"))))
      (gnus-timeline--flush-refresh)
      (should postponed)
      (should gnus-timeline--refresh-pending))))

(ert-deftest gnus-timeline-render-does-not-run-in-worker ()
  (let ((gnus-timeline--refresh-timer nil)
        (gnus-timeline--refresh-pending t)
        postponed)
    (cl-letf (((symbol-function 'current-thread) (lambda () 'worker))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (setq postponed t) 'pending-timer))
              ((symbol-function 'gnus-timeline--rebuild-after-scan)
               (lambda () (ert-fail "Must not render in worker"))))
      (gnus-timeline--flush-refresh)
      (should postponed)
      (should gnus-timeline--refresh-pending))))

(ert-deftest gnus-timeline-article-without-summary-is-safe ()
  (with-temp-buffer
    (setq-local gnus-summary-buffer nil)
    (gnus-timeline--prepare-article)
    (should-not gnus-timeline-reader-return-mode)))

(ert-deftest gnus-timeline-article-with-dead-summary-is-safe ()
  (let ((summary (generate-new-buffer " *timeline-dead-summary*")))
    (kill-buffer summary)
    (with-temp-buffer
      (setq-local gnus-summary-buffer summary)
      (gnus-timeline--prepare-article)
      (should-not gnus-timeline-reader-return-mode))))

(ert-deftest gnus-timeline-refresh-keeps-per-group-buffer-names ()
  (let ((gnus-single-article-buffer nil)
        (gnus-timeline--preserving-article t)
        exit-single)
    (gnus-timeline--exit-for-refresh
     (lambda (&rest _) (setq exit-single gnus-single-article-buffer)) nil t)
    (should exit-single)
    ;; Group entry must still select *Article group*, not shared *Article*.
    (should-not gnus-single-article-buffer)))

(ert-deftest gnus-timeline-refresh-with-no-article-buffer ()
  (let ((gnus-article-buffer nil) ran)
    (gnus-timeline--preserve-article (lambda () (setq ran t)))
    (should ran)))

(ert-deftest gnus-timeline-article-q-restores-timeline-layout ()
  (with-temp-buffer
    (let ((summary (current-buffer)) returned)
      (cl-letf (((symbol-function 'gnus-timeline-summary-return)
                 (lambda () (setq returned (current-buffer)))))
        (setq-local gnus-thread-reader--summary summary)
        (gnus-timeline--quit-thread)
        (should (eq returned summary))))))

(ert-deftest gnus-timeline-refresh-keeps-html-and-pending-image-markers ()
  (let ((article (generate-new-buffer " *timeline-test-article*"))
        (part (generate-new-buffer " *timeline-test-mime*")))
    (unwind-protect
        (with-current-buffer article
          (insert "*\nArticle text after image\n")
          (let* ((gnus-article-buffer article)
                 (start (copy-marker 1))
                 (end (copy-marker 2))
                 (handle (mm-make-handle
                          part '("text/html") nil
                          (lambda () (erase-buffer)))))
            (setq-local gnus-article-mime-handles (list handle)
                        gnus-article-mime-handle-alist (list (cons 1 handle)))
            (gnus-timeline--preserve-article
             (lambda ()
               ;; This is the destructive MIME cleanup on Summary exit.
               (mm-destroy-parts gnus-article-mime-handles)
               (setq gnus-article-mime-handles nil
                     gnus-article-mime-handle-alist nil)))
            (should (buffer-live-p part))
            (should (equal (buffer-string) "*\nArticle text after image\n"))
            (should (eq (car gnus-article-mime-handles) handle))
            ;; A later image callback still replaces only its placeholder.
            (delete-region start end)
            (goto-char start)
            (insert "IMAGE")
            (should (equal (buffer-string) "IMAGE\nArticle text after image\n"))))
      (when (buffer-live-p article) (kill-buffer article))
      (when (buffer-live-p part) (kill-buffer part)))))

(ert-deftest gnus-timeline-q-keeps-timeline-and-restores-summary-layout ()
  (with-temp-buffer
    (setq-local gnus-newsgroup-name gnus-timeline-group)
    (let (saved configured)
      (cl-letf (((symbol-function 'gnus-summary-update-info)
                 (lambda (&rest _) (setq saved t)))
                ((symbol-function 'gnus-configure-windows)
                 (lambda (&rest args) (setq configured args)))
                ((symbol-function 'gnus-summary-exit)
                 (lambda (&rest _) (ert-fail "Timeline must stay open"))))
        (gnus-timeline-summary-return)
        (should saved)
        (should (equal configured '(summary force)))))))

(ert-deftest gnus-timeline-q-closes-other-group-before-returning ()
  (with-temp-buffer
    (setq-local gnus-newsgroup-name "example")
    (let (events)
      (cl-letf (((symbol-function 'gnus-summary-exit)
                 (lambda (&rest args) (push (cons 'exit args) events)))
                ((symbol-function 'gnus-timeline--show)
                 (lambda () (push 'timeline events))))
        (gnus-timeline-summary-return)
        (should (equal (nreverse events) '((exit nil t) timeline)))))))

(ert-deftest gnus-timeline-foreground-uses-fast-backend-data ()
  (let ((gnus-timeline--cached-view t)
        received)
    (gnus-timeline--activate-cached-group
     (lambda (&rest args) (setq received args)) "example" nil nil '(nnatom "feed") nil)
    (should (equal received '("example" nil nil (nnatom "feed") t)))))

(ert-deftest gnus-timeline-background-keeps-normal-backend-activation ()
  (let ((gnus-timeline--cached-view nil)
        received)
    (gnus-timeline--activate-cached-group
     (lambda (&rest args) (setq received args)) "example" 'scan nil '(nnatom "feed") nil)
    (should (equal received '("example" scan nil (nnatom "feed") nil)))))

(ert-deftest gnus-timeline-refresh-restores-source-after-renumbering ()
  (dolist (remove-current '(nil t))
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (insert "old first\nold current\nold next\n")
        (goto-char (point-min))
        (forward-line 1)
        (set-window-start (selected-window) (point) t)
        (let* ((old-rows (list (cons '("source" . 1) 1)
                               (cons '("source" . 2) (point))
                               (cons '("source" . 3) (save-excursion
                                                      (forward-line 1) (point)))))
               (rows old-rows))
          (cl-letf (((symbol-function 'gnus-timeline--view-rows)
                     (lambda () rows))
                    ((symbol-function 'gnus-summary-position-point) #'ignore))
            (let ((state (gnus-timeline--save-view)))
              ;; A new row and changed lengths invalidate saved positions.
              ;; The virtual number formerly assigned to current is reused.
              (erase-buffer)
              (insert "new arrival\nfirst with changed length\n")
              (let ((target (point)))
                (unless remove-current (insert "current\n"))
                (let ((next (point)))
                  (insert "next\n")
                  (setq rows (append (list (cons '("source" . 9) 1)
                                           (cons '("source" . 1) 13))
                                     (unless remove-current
                                       (list (cons '("source" . 2) target)))
                                     (list (cons '("source" . 3) next)))))
                (goto-char (point-min))
                (gnus-timeline--restore-view state)
                (should (= (point) target))
                (should (= (window-point (selected-window)) target))
                (should (= (window-start (selected-window)) target))))))))))
