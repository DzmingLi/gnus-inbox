;;; gnus-timeline.el --- A Gnus inbox timeline -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1") (gnus-inbox "0.1.0"))

;;; Commentary:
;; Show level 1 and 2 inbox articles in one native virtual group.
;; Dispositions and inbox visibility belong to gnus-inbox.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-sum)
(require 'nnvirtual)
(require 'gnus-inbox)

(defvar gnus-thread-reader--summary)

(defgroup gnus-timeline nil
  "A combined Gnus inbox."
  :group 'gnus
  :prefix "gnus-timeline-")

(defcustom gnus-timeline-group "nnvirtual:timeline"
  "Name of the combined timeline group."
  :type 'string)

(defvar-local gnus-timeline--dividers nil)
(defvar gnus-timeline--cached-view nil)
(defvar gnus-timeline--preserving-article nil)
(defvar gnus-timeline--refresh-timer nil)
(defvar gnus-timeline--refresh-pending nil)
(defvar gnus-timeline--refresh-running nil)

(defun gnus-timeline--schedule-refresh ()
  "Arrange one list rebuild after a brief pause in user input."
  (unless gnus-timeline--refresh-timer
    (setq gnus-timeline--refresh-timer
          (run-with-idle-timer 0.15 nil #'gnus-timeline--flush-refresh))))

(defun gnus-timeline--refresh-after-scan (&rest _)
  "Queue a timeline update without blocking a completed group callback.
Nearby source completions share one rebuild of the latest group state."
  (setq gnus-timeline--refresh-pending t)
  (gnus-timeline--schedule-refresh))

(defun gnus-timeline--flush-refresh ()
  "Render pending updates on the main thread when input is quiet."
  (setq gnus-timeline--refresh-timer nil)
  (when gnus-timeline--refresh-pending
    (if (or gnus-timeline--refresh-running
            (not (eq (current-thread) main-thread))
            (input-pending-p))
        (gnus-timeline--schedule-refresh)
      (setq gnus-timeline--refresh-pending nil)
      (unwind-protect
          (let ((gnus-timeline--refresh-running t)
                ;; A full native Summary allocates enough temporary data
                ;; to trigger several collections at the default 800 KB.
                (gc-cons-threshold (max gc-cons-threshold (* 32 1024 1024))))
            (gnus-timeline--rebuild-after-scan))
        (when gnus-timeline--refresh-pending
          (gnus-timeline--schedule-refresh))))))

(defun gnus-timeline--exit-for-refresh (exit &rest args)
  "Keep per-group article buffers when EXIT is part of a list refresh.
Bind this only during exit: binding it for the subsequent group entry
would redirect the Summary to an empty shared `*Article*' buffer."
  (let ((gnus-single-article-buffer
         (or gnus-single-article-buffer
             (and gnus-timeline--preserving-article
                  (eq (current-thread) main-thread)))))
    (apply exit args)))

(advice-add 'gnus-summary-exit :around #'gnus-timeline--exit-for-refresh)

(defun gnus-timeline--activate-cached-group (activate group &optional scan dont-check method dont-sub-check)
  "Use existing backend data when building the foreground timeline."
  (funcall activate group scan dont-check method
           (or dont-sub-check
               (and gnus-timeline--cached-view
                    (eq (current-thread) main-thread)))))

(advice-add 'gnus-activate-group :around #'gnus-timeline--activate-cached-group)

(defun gnus-timeline--sources ()
  "Return native level 1 and 2 groups for the timeline."
  (cl-loop for (name . _) in gnus-newsrc-alist
             for info = (gnus-get-info name)
             when (and info
                       (memq (gnus-info-level info) '(1 2))
                       (not (string-prefix-p "nnvirtual:" name)))
             collect name))

(defun gnus-timeline-refresh ()
  "Update the timeline's native virtual group from current Gnus levels."
  (interactive)
  (unless gnus-group-buffer
    (user-error "Start Gnus first"))
  (with-current-buffer gnus-group-buffer
    (let* ((group gnus-timeline-group)
           (sources (gnus-timeline--sources))
           (regexp (if sources
                       (concat "\\`" (regexp-opt sources t) "\\'")
                     "\\`\\'"))
           (method `(nnvirtual ,regexp)))
      (unless (gnus-group-entry group)
        (gnus-group-make-group (gnus-group-real-name group) method))
      (when-let* ((info (gnus-get-info group)))
        (unless (equal (gnus-info-method info) method)
          (gnus-group-set-info method group 'method))
        (unless (= (gnus-info-level info) 2)
          (gnus-group-change-level (gnus-group-entry group) 2))
        (dolist (parameter
                 '((gnus-thread-sort-functions
                    '(gnus-thread-sort-by-most-recent-date
                      gnus-timeline-thread-sort-by-level))
                   (gnus-subthread-sort-functions
                    '(gnus-thread-sort-by-date))))
          (unless (member parameter (gnus-info-params info))
            (gnus-group-add-parameter group parameter)))
        (gnus-group-set-parameter group 'comment "Timeline · inbox")
        (gnus-group-set-parameter group 'display
                                  [gnus-inbox-visible-p])))))

(defun gnus-timeline--article-level (article)
  "Return the source group level for virtual ARTICLE."
  (let* ((source (nnvirtual-find-group-art
                  (gnus-group-real-name gnus-timeline-group) article))
         (info (and source (gnus-get-info (car source)))))
    (if info (gnus-info-level info) 99)))

(defun gnus-timeline-thread-sort-by-level (a b)
  "Sort thread A before B when its source has a higher priority level."
  (< (gnus-timeline--article-level
      (mail-header-number (gnus-thread-header a)))
     (gnus-timeline--article-level
      (mail-header-number (gnus-thread-header b)))))

(defun gnus-timeline--clear-divider ()
  "Remove previous timeline section labels."
  (mapc #'delete-overlay gnus-timeline--dividers)
  (setq gnus-timeline--dividers nil))

(defun gnus-timeline--label (position label)
  "Display LABEL before the Summary line at POSITION."
  (let ((overlay (make-overlay position position nil t t)))
    (overlay-put overlay 'before-string
                 (propertize (concat "──────── " label " ────────\n")
                             'face 'shadow))
    (push overlay gnus-timeline--dividers)))

(defun gnus-timeline--decorate-summary ()
  "Separate the two priority levels in the timeline Summary."
  (when (equal gnus-newsgroup-name gnus-timeline-group)
    (gnus-timeline--clear-divider)
    (save-excursion
      (goto-char (point-min))
      (let ((first-one nil) (first-two nil))
        (while (not (eobp))
          (let ((article (gnus-summary-article-number)))
            (when article
              (pcase (gnus-timeline--article-level article)
                (1 (unless first-one (setq first-one (line-beginning-position))))
                (2 (unless first-two (setq first-two (line-beginning-position)))))))
          (forward-line 1))
        (when first-one (gnus-timeline--label first-one "Level 1"))
        (when first-two
          (gnus-timeline--label first-two "Level 2"))))))

(defun gnus-timeline--read-group-around (read-group &rest args)
  "Call READ-GROUP without a large-group prompt for the timeline.
ARGS are Gnus's original arguments."
  (let ((gnus-large-newsgroup
         (if (equal (or (nth 2 args)
                        (and (derived-mode-p 'gnus-group-mode)
                             (gnus-group-group-name)))
                    gnus-timeline-group)
             nil
           gnus-large-newsgroup)))
    (apply read-group args)))

(defun gnus-timeline--quit-thread ()
  "Close this reading view and restore the timeline Summary."
  (interactive)
  (let ((summary (or (and (boundp 'gnus-thread-reader--summary)
                          (buffer-live-p gnus-thread-reader--summary)
                          gnus-thread-reader--summary)
                     (get-buffer (format "*Summary %s*" gnus-timeline-group)))))
    (bury-buffer)
    (if (buffer-live-p summary)
        (with-current-buffer summary
          (gnus-timeline-summary-return))
      (gnus-timeline--show))))

(define-minor-mode gnus-timeline-reader-return-mode
  "Return native articles opened from the timeline there on `q'."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'gnus-timeline--quit-thread)
            map))

(defun gnus-timeline--prepare-article ()
  "Install the timeline return command only in its native articles."
  (let ((summary (and gnus-summary-buffer
                      (get-buffer gnus-summary-buffer))))
    (gnus-timeline-reader-return-mode
     (if (and (buffer-live-p summary)
              (with-current-buffer summary
                (equal gnus-newsgroup-name gnus-timeline-group)))
         1 -1))))

(defun gnus-timeline--open-thread-around (open &rest args)
  "Make a thread opened from the timeline return there on `q'."
  (let ((timeline-p (and (derived-mode-p 'gnus-summary-mode)
                          (equal gnus-newsgroup-name gnus-timeline-group))))
    (let ((thread (apply open args)))
      (when timeline-p
        (with-current-buffer thread
          (let ((map (copy-keymap (current-local-map))))
            (define-key map (kbd "q") #'gnus-timeline--quit-thread)
            (use-local-map map))))
      thread)))

(with-eval-after-load 'gnus-thread-reader
  (unless (advice-member-p #'gnus-timeline--open-thread-around
                           'gnus-thread-reader-open)
    (advice-add 'gnus-thread-reader-open :around
                #'gnus-timeline--open-thread-around)))

(defun gnus-timeline--show ()
  "Show the combined Gnus inbox."
  (gnus-timeline-refresh)
  (gnus-inbox-enable)
  (with-current-buffer gnus-group-buffer
    (let ((gnus-timeline--cached-view t)
          (gnus-auto-select-first nil))
      (gnus-group-read-group nil t gnus-timeline-group)))
  (when-let* ((summary (get-buffer (gnus-summary-buffer-name
                                  gnus-timeline-group t))))
    (with-current-buffer summary
      (gnus-configure-windows 'summary 'force))))

(defun gnus-timeline-summary-return ()
  "Return from a Summary to the timeline's normal window layout."
  (interactive)
  (if (equal gnus-newsgroup-name gnus-timeline-group)
      (progn
        (gnus-summary-update-info)
        (gnus-configure-windows 'summary 'force))
    ;; Let Gnus save marks and close the group, without restoring its
    ;; Group layout just before we return to the timeline.
    (gnus-summary-exit nil t)
    (gnus-timeline--show)))

(define-minor-mode gnus-timeline-summary-return-mode
  "Return from Summary to the timeline on `q'."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'gnus-timeline-summary-return)
            map))

(add-hook 'gnus-summary-mode-hook #'gnus-timeline-summary-return-mode)

(defun gnus-timeline--preserve-article (refresh)
  "Run REFRESH without destroying the current article's MIME rendering.
Reselecting a Summary exits the group, which normally undisplays MIME
parts.  A background list update must leave the article and pending SHR
image markers intact."
  (let ((article (and gnus-article-buffer (get-buffer gnus-article-buffer)))
        (gnus-timeline--preserving-article t)
        (gnus-auto-select-first nil))
    (if (not article)
        (funcall refresh)
      (with-current-buffer article
        (let ((gnus-article-mime-handles nil)
              (gnus-article-mime-handle-alist nil))
          (funcall refresh))))))

(defun gnus-timeline--view-rows ()
  "Return visible rows keyed by stable source group and article number."
  (mapcar (lambda (row)
            (cons (gnus-inbox--source (car row)) (cdr row)))
          (gnus-inbox--visible-rows)))

(defun gnus-timeline--view-anchor (rows position)
  "Prefer the row at POSITION, then its successors and predecessors."
  (let ((line (save-excursion (goto-char position)
                             (line-beginning-position))))
    (append (cl-remove-if (lambda (row) (< (cdr row) line)) rows)
            (reverse (cl-remove-if-not (lambda (row) (< (cdr row) line)) rows)))))

(defun gnus-timeline--save-view ()
  "Capture source identities for point and every Summary window."
  (let ((rows (gnus-timeline--view-rows)))
    (list
     (gnus-timeline--view-anchor rows (point))
     (mapcar
      (lambda (window)
        (let* ((start (window-start window))
               (top (gnus-timeline--view-anchor rows start)))
          (list window
                (gnus-timeline--view-anchor rows (window-point window))
                top (and top (count-lines start (cdar top)))
                (window-hscroll window) (window-vscroll window t))))
      (get-buffer-window-list (current-buffer) nil t)))))

(defun gnus-timeline--restore-view (state)
  "Restore STATE after virtual article numbers have been regenerated."
  (let ((positions (make-hash-table :test #'equal)))
    (dolist (row (gnus-timeline--view-rows))
      (puthash (car row) (cdr row) positions))
    (cl-labels ((survivor (rows)
                 (cl-find-if (lambda (row) (gethash (car row) positions)) rows)))
      (when-let* ((row (survivor (car state))))
        (goto-char (gethash (car row) positions))
        (gnus-summary-position-point))
      (dolist (view (cadr state))
        (pcase-let ((`(,window ,cursor ,top ,offset ,hscroll ,vscroll) view))
          (when (window-live-p window)
            (set-window-buffer window (current-buffer))
            (when-let* ((row (survivor cursor)))
              (save-excursion
                (goto-char (gethash (car row) positions))
                (gnus-summary-position-point)
                (set-window-point window (point))))
            (when-let* ((row (survivor top)))
              (save-excursion
                (goto-char (gethash (car row) positions))
                (when (eq row (car top)) (forward-line (- offset)))
                (set-window-start window (point) t)))
            (set-window-hscroll window hscroll)
            (set-window-vscroll window vscroll t)))))))

(defun gnus-timeline--rebuild-after-scan ()
  "Rebuild the visible timeline from the latest completed group scans."
  (gnus-timeline-refresh)
  (let* ((summary (get-buffer (format "*Summary %s*"
                                      gnus-timeline-group)))
         (summary-window (and summary (get-buffer-window summary)))
         (view (and summary-window
                    (with-current-buffer summary (gnus-timeline--save-view)))))
    (when summary-window
      ;; Reselecting rebuilds nnvirtual's article mapping, but its normal
      ;; window changes must not interrupt the user's current reading view.
      (let ((inhibit-redisplay t)
            (gnus-timeline--cached-view t)
            (gnus-kill-summary-on-exit nil))
        (save-window-excursion
          (with-current-buffer summary
            (gnus-timeline--preserve-article
             (lambda ()
               (with-selected-window summary-window
                 (with-current-buffer summary
                   (when (derived-mode-p 'gnus-summary-mode)
                     (gnus-summary-reselect-current-group)))))))))
      ;; Gnus may replace the Summary buffer while reselecting.  Native
      ;; articles also retain a separate current-Summary reference for
      ;; forwarded keys; repair it together with the thread reader's link.
      (let ((replacement (get-buffer (format "*Summary %s*"
                                              gnus-timeline-group))))
        (when replacement
          (with-current-buffer replacement
            (gnus-timeline--restore-view view))
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (and (derived-mode-p 'gnus-article-mode)
                         (equal gnus-newsgroup-name gnus-timeline-group))
                (setq gnus-article-current-summary replacement
                      gnus-summary-buffer replacement)
                (gnus-timeline--prepare-article))
              (when (and (local-variable-p 'gnus-thread-reader--summary)
                         (eq gnus-thread-reader--summary summary))
                (setq gnus-thread-reader--summary replacement)))))))))

(defun gnus-timeline--after-gnus (&rest _)
  "Show the timeline after Gnus has finished opening its Group buffer."
  (when (and (boundp 'gnus-group-buffer) (get-buffer gnus-group-buffer))
    (gnus-timeline--show)))

(add-hook 'gnus-summary-prepare-hook #'gnus-timeline--decorate-summary)
(add-hook 'gnus-article-prepare-hook #'gnus-timeline--prepare-article)
(unless (advice-member-p #'gnus-timeline--read-group-around
                         'gnus-group-read-group)
  (advice-add 'gnus-group-read-group :around
              #'gnus-timeline--read-group-around))
(unless (advice-member-p #'gnus-timeline--after-gnus 'gnus)
  (advice-add 'gnus :after #'gnus-timeline--after-gnus))

(defun gnus-timeline--after-group-update (&rest _)
  "Queue a view refresh when a backend worker has updated a group.
Foreground group updates also occur while reselecting the timeline;
ignoring those prevents a render from scheduling another render."
  (unless (eq (current-thread) main-thread)
    (gnus-timeline--refresh-after-scan)))

(unless (advice-member-p #'gnus-timeline--after-group-update
                         'gnus-group-update-group)
  (advice-add 'gnus-group-update-group :after
              #'gnus-timeline--after-group-update))
(add-hook 'gnus-after-getting-new-news-hook
          #'gnus-timeline--refresh-after-scan)

;; Remove the old adapter when this file is hot-loaded in an older editor.
(when (boundp 'commercial-gnus-group-updated-hook)
  (remove-hook 'commercial-gnus-group-updated-hook
               #'gnus-timeline--refresh-after-scan))

(provide 'gnus-timeline)
;;; gnus-timeline.el ends here
