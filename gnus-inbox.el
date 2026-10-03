;;; gnus-inbox.el --- Process every Gnus group as an inbox -*- lexical-binding: t; -*-

;; Author: Lee
;; Version: 0.1.1
;; URL: https://github.com/DzmingLi/gnus-inbox
;; Package-Requires: ((emacs "30.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.  See COPYING for the full license, including
;; the disclaimer of warranty.

;;; Commentary:
;; Every group can serve as an inbox.  Read later, archive and delete are
;; mutually exclusive local dispositions, all hidden from the inbox view.
;; Archive also keeps a local copy.  None changes the read or tick state, and
;; delete never asks a backend to remove an article from a remote service.

;;; Code:

(require 'cl-lib)
(require 'gnus)
(require 'gnus-sum)
(require 'gnus-cache)
(require 'gnus-range)
(require 'nnvirtual)
(require 'nnselect)

(defgroup gnus-inbox nil
  "Process articles from any Gnus group as an inbox."
  :group 'gnus)

(defvar gnus-newsgroup-archived nil)
(defvar gnus-newsgroup-trashed nil)
(defvar gnus-newsgroup-read-later nil)
(defvar gnus-number)
(defvar gnus-newsgroup-display)
(defvar gnus-inbox--enabled nil)
(defvar gnus-thread-reader-mode-map)
(declare-function gnus-thread-reader--source "gnus-thread-reader" ())
(declare-function gnus-thread-reader--target "gnus-thread-reader" ())

(defun gnus-inbox--register-marks ()
  "Register independent, locally persisted Gnus article marks."
  (dolist (entry '((read-later . read-later)
                   (archived . archive) (trashed . trash)))
    (add-to-list 'gnus-article-mark-lists entry t)
    (add-to-list 'gnus-article-unpropagated-mark-lists (cdr entry) t))
  (dolist (variable '(gnus-newsgroup-read-later
                      gnus-newsgroup-archived gnus-newsgroup-trashed))
    (make-variable-buffer-local variable)
    ;; GNU Gnus also initializes these through its Summary variable list.
    ;; Commercial Gnus uses automatically buffer-local variables instead.
    (when (boundp 'gnus-summary-local-variables)
      (add-to-list 'gnus-summary-local-variables variable t))))

(gnus-inbox--register-marks)

(defconst gnus-inbox--display-rule
  '("\\`.*\\'" (display . [gnus-inbox-visible-p]))
  "Fallback display rule for new Gnus groups.")

(defun gnus-inbox--install-display ()
  "Give existing Gnus groups the inbox display predicate."
  (dolist (entry gnus-newsrc-alist)
    (let ((group (car entry)))
      (unless (equal (gnus-group-get-parameter group 'display t)
                     [gnus-inbox-visible-p])
        (gnus-group-set-parameter group 'display
                                  [gnus-inbox-visible-p])))))

(defun gnus-inbox-enable ()
  "Enable inbox display for existing and future Gnus groups."
  (interactive)
  (setq gnus-inbox--enabled t)
  (gnus-inbox--bind-summary-keys)
  (when (featurep 'gnus-thread-reader)
    (gnus-inbox--bind-thread-keys))
  (add-to-list 'gnus-parameters gnus-inbox--display-rule t)
  (add-hook 'gnus-started-hook #'gnus-inbox--install-display)
  (when gnus-newsrc-alist
    (gnus-inbox--install-display)))

(defun gnus-inbox--bind-summary-keys ()
  "Use inbox actions in every Gnus Summary buffer."
  (define-key gnus-summary-mode-map (kbd "a") #'gnus-inbox-archive)
  (define-key gnus-summary-mode-map (kbd "d") #'gnus-inbox-delete)
  (define-key gnus-summary-mode-map (kbd "k") #'gnus-inbox-delete)
  (define-key gnus-summary-mode-map (kbd "l") #'gnus-inbox-read-later)
  (define-key gnus-summary-mode-map (kbd "!")
    #'gnus-summary-tick-article-forward)
  (define-key gnus-summary-mode-map (kbd "u") #'gnus-inbox-restore)
  (define-key gnus-summary-mode-map (kbd "C-c C-r") #'gnus-inbox-restore))

(defun gnus-inbox--bind-thread-keys ()
  "Use the same inbox dispositions in the optional thread reader."
  (define-key gnus-thread-reader-mode-map (kbd "a") #'gnus-inbox-thread-archive)
  (define-key gnus-thread-reader-mode-map (kbd "l") #'gnus-inbox-thread-read-later)
  (define-key gnus-thread-reader-mode-map (kbd "k") #'gnus-inbox-thread-delete))

(with-eval-after-load 'gnus-thread-reader
  (when gnus-inbox--enabled
    (gnus-inbox--bind-thread-keys)))

(defun gnus-inbox--source (article)
  "Return the real (GROUP . NUMBER) for ARTICLE in this Summary."
  (cond
   ((gnus-nnselect-group-p gnus-newsgroup-name)
    (cons (nnselect-article-group article) (nnselect-article-number article)))
   ((gnus-virtual-group-p gnus-newsgroup-name)
    (nnvirtual-find-group-art (gnus-group-real-name gnus-newsgroup-name)
                              article))
   (t (cons gnus-newsgroup-name article))))

(defun gnus-inbox--marked-p (source mark)
  "Whether SOURCE has local MARK in Gnus group information."
  (let* ((info (gnus-get-info (car source)))
         (entry (and info (assq mark (gnus-info-marks info)))))
    (and entry (memq (cdr source) (range-uncompress (cdr entry))))))

(defun gnus-inbox--set-mark (source mark enabled)
  "Set local MARK for SOURCE according to ENABLED."
  (let* ((info (or (gnus-get-info (car source))
                   (error "Unknown Gnus group: %s" (car source))))
         (entry (assq mark (gnus-info-marks info)))
         (numbers (range-uncompress (cdr entry)))
         (number (cdr source)))
    (setq numbers (if enabled (cl-adjoin number numbers) (delq number numbers)))
    (if entry
        (if numbers
            (setcdr entry (range-compress-list (sort numbers #'<)))
          (setf (gnus-info-marks info) (delq entry (gnus-info-marks info))))
      (when numbers
        (push (cons mark (range-compress-list numbers))
              (gnus-info-marks info))))
    ;; Keep the open real group in sync with the info written on exit.
    (when (equal gnus-newsgroup-name (car source))
      (let ((variable (pcase mark
                        ('read-later 'gnus-newsgroup-read-later)
                        ('archive 'gnus-newsgroup-archived)
                        ('trash 'gnus-newsgroup-trashed))))
        (when variable
          (make-local-variable variable)
          (set variable numbers))))
    ;; nnvirtual writes its complete marks snapshot back to component groups
    ;; on Summary exit/reselection.  Keep that snapshot and the live Summary
    ;; lists current, or it will erase the mark just stored on SOURCE.
    (when (and (not (equal gnus-newsgroup-name (car source)))
               (gnus-virtual-group-p gnus-newsgroup-name))
      (let ((virtual-number (nnvirtual-reverse-map-article
                             (car source) number)))
        (when virtual-number
          (gnus-inbox--set-mark (cons gnus-newsgroup-name virtual-number)
                               mark enabled))))))

(defun gnus-inbox--article (&optional article)
  "Return ARTICLE or the one under point in the current Summary."
  (or article (gnus-summary-article-number)
      (user-error "No Gnus article at point")))

(defun gnus-inbox--disposition (source mark)
  "Give SOURCE exactly one disposition MARK."
  (dolist (other '(read-later archive trash))
    (gnus-inbox--set-mark source other (eq other mark))))

(defun gnus-inbox--refresh-inbox ()
  "Apply the current group's inbox display predicate, when present."
  (when gnus-newsgroup-display
    (gnus-summary-limit-to-display-predicate)))

(defun gnus-inbox-read-later (&optional article)
  "Set ARTICLE aside for later without changing its read or tick mark."
  (interactive)
  (let* ((number (gnus-inbox--article article))
         (source (gnus-inbox--source number)))
    (gnus-inbox--disposition source 'read-later)
    (gnus-inbox--refresh-inbox)
    (message "Set article %s aside for later" number)))

(defun gnus-inbox-archive (&optional article)
  "Cache and archive ARTICLE without setting its read or tick mark."
  (interactive)
  (let* ((number (gnus-inbox--article article))
         (source (gnus-inbox--source number))
         (file (gnus-cache-file-name (car source) (cdr source))))
    ;; A successful archive must have a local copy, even after Gnus marks it read.
    (unless (file-exists-p file)
      (gnus-cache-possibly-enter-article gnus-newsgroup-name
                                          number nil nil nil t))
    (unless (file-exists-p file)
      (user-error "Could not cache article %s" number))
    (gnus-inbox--disposition source 'archive)
    (gnus-inbox--refresh-inbox)
    (message "Archived article %s" number)))

(defun gnus-inbox-delete (&optional article)
  "Locally dismiss ARTICLE; do not delete it from its backend."
  (interactive)
  (let* ((number (gnus-inbox--article article))
         (source (gnus-inbox--source number)))
    (gnus-inbox--disposition source 'trash)
    (gnus-inbox--refresh-inbox)
    (message "Dismissed article %s locally" number)))

(defun gnus-inbox-restore (&optional article)
  "Clear all local disposition marks for ARTICLE."
  (interactive)
  (let* ((number (gnus-inbox--article article))
         (source (gnus-inbox--source number)))
    (dolist (mark '(read-later archive trash))
      (gnus-inbox--set-mark source mark nil))
    (message "Restored article %s" number)))

(defun gnus-inbox-visible-p ()
  "Return non-nil if the current article is still in this group's inbox."
  (let ((source (gnus-inbox--source gnus-number)))
    (not (cl-some (lambda (mark)
                    (gnus-inbox--marked-p source mark))
                  '(read-later archive trash)))))

(defun gnus-user-format-function-A (header)
  "Display `A' for a locally archived HEADER in a Gnus Summary."
  (if (gnus-inbox--marked-p
       (gnus-inbox--source (mail-header-number header)) 'archive)
      "A" " "))

(defun gnus-user-format-function-L (header)
  "Display `L' for a read-later HEADER in a Gnus Summary."
  (if (gnus-inbox--marked-p
       (gnus-inbox--source (mail-header-number header)) 'read-later)
      "L" " "))

(defun gnus-user-format-function-T (header)
  "Display `D' for a locally deleted HEADER in a Gnus Summary."
  (if (gnus-inbox--marked-p
       (gnus-inbox--source (mail-header-number header)) 'trash)
      "D" " "))

(defun gnus-inbox--list (mark)
  "Show articles carrying local MARK in this Summary."
  (let (articles)
    (dolist (number gnus-newsgroup-articles)
      (when (gnus-inbox--marked-p
             (gnus-inbox--source number) mark)
        (push number articles)))
    (let ((gnus-newsgroup-display nil))
      (gnus-summary-limit (nreverse articles)))))

(defun gnus-inbox-list-archived ()
  "Limit the current Summary to locally archived articles."
  (interactive)
  (gnus-inbox--list 'archive))

(defun gnus-inbox-list-read-later ()
  "Limit the current Summary to articles set aside for later."
  (interactive)
  (gnus-inbox--list 'read-later))

(defun gnus-inbox-list-deleted ()
  "Limit the current Summary to locally dismissed articles."
  (interactive)
  (gnus-inbox--list 'trash))

(defun gnus-inbox--thread-action (command)
  "Run Summary COMMAND on the current Gnus thread reader entry."
  (unless (featurep 'gnus-thread-reader)
    (user-error "gnus-thread-reader is unavailable"))
  (let ((number (gnus-thread-reader--target))
        (summary (gnus-thread-reader--source)))
    (with-current-buffer summary
      (funcall command number))
    (pop-to-buffer summary)))

(defun gnus-inbox-thread-read-later ()
  "Set aside the current Gnus thread reader entry and return to Summary."
  (interactive)
  (gnus-inbox--thread-action #'gnus-inbox-read-later))

(defun gnus-inbox-thread-archive ()
  "Archive the current Gnus thread reader entry and return to Summary."
  (interactive)
  (gnus-inbox--thread-action #'gnus-inbox-archive))

(defun gnus-inbox-thread-delete ()
  "Locally dismiss the current Gnus thread reader entry and return to Summary."
  (interactive)
  (gnus-inbox--thread-action #'gnus-inbox-delete))

(provide 'gnus-inbox)
;;; gnus-inbox.el ends here
