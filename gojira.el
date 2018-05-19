;;; gojira.el --- import jira issue into org-mode                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tosh Lyons

;; Author: Tosh Lyons <tosh.lyons@gmail.com>
;; Version: 0.1
;; Package-Requires ((org-jira))
;; Keywords: lisp, jira, org-mode
;; URL: https://github.com/toshism/gojira

;;; Commentary:

;; Import a jira issue into org-mode as a todo.
;;
;; Gojira is built on top of org-jira and acts as an alternative interface for
;; using org-jira. It allows you to use an existing org file and import issues
;; as single standalone entities.
;;
;; It's currently only one way. Jira->Org. I may someday add the other
;; direction, but i doubt it.
;;
;; Depends on org-jira here https://github.com/ahungry/org-jira

;;; Code:

(require 'org-jira)

(defun gojira-insert-issue-as-org (issue-id)
  "Insert jira issue ISSUE-ID into current org document."
  (interactive "sJira Issue: ")
  (save-excursion
    (org-insert-heading-respect-content)
    (backward-char)
    (let ((heading-level (car (org-heading-components))))
      (org-narrow-to-subtree)
      (insert (org-element-interpret-data (gojira-get-issue-by-id issue-id heading-level)))
      (gojira-insert-comment-block issue-id (+ heading-level 1))
      (indent-region (point-min) (point-max))
      (widen))))

(defun gojira-refresh-issue-for-id (issue-id)
  "Refresh description and comments from jira for ISSUE-ID."
  (interactive "sJira Issue: ")
  (save-excursion
    (gojira-refresh-comments-for-issue-id issue-id)
    (gojira-refresh-description-for-issue-id issue-id)
    (gojira-refresh-issue-properties issue-id)))

(defun gojira-refresh-issue ()
  "Refresh description and comments from jira for current issue."
  (interactive)
  (let ((issue-id (gojira-find-issue-id)))
    (gojira-refresh-comments-for-issue-id issue-id)
    (gojira-refresh-description-for-issue-id issue-id)
    (gojira-refresh-issue-properties issue-id)))

(defun gojira-insert-comment-block (issue-id heading-level)
  (gojira-insert-comments-header issue-id heading-level)
  (gojira-insert-comments-for-issue-id issue-id (+ heading-level 1)))

(defun gojira-insert-comments-header (issue-id heading-level)
  (gojira-insert-org-element `(headline (:title "Comments" :level ,heading-level)
                                        (property-drawer nil ((node-property (:key "ID" :value ,(upcase (concat issue-id "-COMMENTS")))))))))

(defun gojira-insert-comments-for-issue-id (issue-id heading-level)
  "Insert all comments for given ISSUE-ID. HEADING-LEVEL is the level of the issue heading."
  (mapc #'(lambda (comment) (gojira-insert-org-element (gojira-comment-to-org-element comment heading-level))) (gojira-get-comments-by-issue-id issue-id)))

(defun gojira-refresh-comments-for-issue-id (issue-id)
  "Refresh comments on ISSUE-ID from jira to org."
  (save-excursion
    (gojira-narrow-to-issue-id issue-id)
    (let ((heading-level (car (org-heading-components)))
          (comment-id-p (concat "+ID=\"" issue-id "-COMMENTS\""))
          (comment-id (concat issue-id "-COMMENTS")))
      (gojira-narrow-to-issue-id comment-id)
      (org-cut-subtree)
      (gojira-insert-comment-block issue-id (+ heading-level 1))
      (indent-region (point-min) (point-max))
      (widen))))

(defun gojira-update-element-by-id (element-id issue-id insert-func)
  (save-excursion
    (gojira-narrow-to-issue-id issue-id)
    (let ((heading-level (car (org-heading-components))))
      (gojira-narrow-to-issue-id element-id)
      (org-cut-subtree)
      (funcall insert-func issue-id (+ heading-level 1))
      (indent-region (point-min) (point-max))
      (widen))))

(defun gojira-refresh-issue-properties (issue-id)
  "Update the properties for ISSUE-ID."
  (let ((issue (car (org-jira-get-issue-by-id issue-id))))
    (goto-char (org-find-entry-with-id issue-id))
    (gojira-put-current-issue-property "CREATED" (org-jira-get-issue-val 'created issue))
    (gojira-put-current-issue-property "UPDATED" (org-jira-get-issue-val 'updated issue))
    (gojira-put-current-issue-property "ASSIGNEE" (org-jira-get-issue-val 'assignee issue))
    (gojira-put-current-issue-property "REPORTER" (org-jira-get-issue-val 'reporter issue))
    (gojira-put-current-issue-property "JIRA_PRIORITY" (org-jira-get-issue-val 'priority issue))
    (gojira-put-current-issue-property "STATUS" (org-jira-get-issue-val 'status issue))
    (gojira-put-current-issue-property "TYPE" (org-jira-get-issue-val 'type issue))))

(defun gojira-refresh-description-for-issue-id (issue-id)
    (gojira-update-element-by-id (concat issue-id "-DESCRIPTION") issue-id 'gojira-insert-description))

(defun gojira-insert-description (issue-id heading-level)
  (let ((issue (car (org-jira-get-issue-by-id issue-id))))
    (gojira-insert-org-element (gojira-get-description issue heading-level))))

(defun gojira-refresh-comments-for-issue ()
  "Refresh comments on current issue."
  (gojira-refresh-comments-for-issue-id (gojira-find-issue-id)))

(defun gojira-refresh-description-for-issue ()
  "Refresh description on current issue."
  (gojira-refresh-description-for-issue-id (gojira-find-issue-id)))

(defun gojira-get-current-issue-property (property)
  "Return the value or PROPERTY for the current issue."
  (org-entry-get (gojira-point-of-parent-issue) property))

(defun gojira-put-current-issue-property (property value)
  "Set PROPERTY to VALUE for current issue."
  (org-entry-put (gojira-point-of-parent-issue) property value))

(defun gojira-find-issue-id ()
  "Get issue id from any place in issue."
  (gojira-get-current-issue-property "ID"))

(defun gojira-point-of-parent-issue ()
  "Return a point in parent issue heading."
  (save-excursion
    (let ((continue t)
          issue-id)
      (while continue
        ;; all imported issues should have a jira_link property
        (if (and (org-entry-get nil "JIRA_LINK")
                 (setq heading-point (point)))
            (setq continue nil))
        (unless (and continue (org-up-heading-safe))
          (setq continue nil)))
      heading-point)))

(defun gojira-insert-org-element (element)
  "Insert an org ELEMENT at current point."
  (insert (org-element-interpret-data element)))

(defun gojira-narrow-to-issue-id (issue-id)
  "Narrow to a single jira issue by ISSUE-ID."
  (let ((start (org-find-entry-with-id issue-id)))
    (when (and start (>= start (point-min))
               (<= start (point-max)))
      (goto-char start)
      (org-narrow-to-subtree))))

(defun gojira-get-comments-by-issue-id (issue-id)
  "Get all comments for specified ISSUE-ID."
  (jiralib-get-comments issue-id))

(defun gojira-process-body (body)
  "Format the BODY text of an issue/comment."
  ;; TODO replace jira code blocks with org code blocks
  (replace-regexp-in-string "^\*" "-" (replace-regexp-in-string "" "" (replace-regexp-in-string "^" "  " body))))

(defun gojira-comment-to-org-element (comment heading-level)
  "Take a COMMENT and turn it into an org-element at the provided HEADING-LEVEL."
  (let* ((comment-id (org-jira-get-comment-id comment))
         (comment-author (or (car (rassoc
                                   (org-jira-get-comment-author comment)
                                   org-jira-users))
                             (org-jira-get-comment-author comment)))
         (comment-headline (format "%s" comment-author)))
    `(headline (:title ,comment-headline :level ,heading-level)
               (property-drawer nil ((node-property (:key "CREATED" :value ,(org-jira-get-comment-val 'created comment)))
                                     ,(unless (string= (org-jira-get-comment-val 'updated comment) (org-jira-get-comment-val 'created comment))
                                        `(node-property (:key "UPDATED" :value ,(org-jira-get-comment-val 'updated comment))))
                                     (node-property (:key "ID" :value ,(org-jira-get-comment-val 'id comment)))))
               (,(gojira-process-body (org-jira-find-value comment 'body))))))

(defun gojira-get-issue-by-id (issue-id heading-level)
  "Get the issue with ISSUE-ID from jira. Return an org element."
  (gojira-get-issue (car (org-jira-get-issue-by-id issue-id)) heading-level))

(defun gojira-get-description (issue heading-level)
  "Create Description org element"
  `((headline (:title "Description" :level ,heading-level)
              (property-drawer nil ((node-property (:key "ID" :value ,(concat (org-jira-get-issue-key issue) "-DESCRIPTION")))))
              ,(gojira-process-body (org-jira-get-issue-val 'description issue)))))

(defun gojira-get-issue (issue heading-level)
  "Format ISSUE into org element."
  (interactive)
  (let* ((proj-key (org-jira-get-issue-project issue))
         (issue-id (org-jira-get-issue-key issue))
         (issue-summary (org-jira-get-issue-summary issue))
         (issue-headline issue-summary)
         (issue-tag (replace-regexp-in-string "-" "_" issue-id)))
    (progn
      (let ((status (org-jira-get-issue-val 'status issue)))
        (setq heading (concat (cond (org-jira-use-status-as-todo
                                     (upcase (replace-regexp-in-string " " "-" status)))
                                    ((member status org-jira-done-states) "DONE")
                                    ("TODO")) " "
                                    issue-headline)))
      `(headline (:title ,heading :level 0 :tags (,issue-tag))
                 (property-drawer nil ((node-property (:key "CREATED" :value ,(org-jira-get-issue-val 'created issue)))
                                       (node-property (:key "UPDATED" :value ,(org-jira-get-issue-val 'updated issue)))
                                       (node-property (:key "ASSIGNEE" :value ,(org-jira-get-issue-val 'assignee issue)))
                                       (node-property (:key "REPORTER" :value ,(org-jira-get-issue-val 'reporter issue)))
                                       (node-property (:key "JIRA_PRIORITY" :value ,(org-jira-get-issue-val 'priority issue)))
                                       (node-property (:key "STATUS" :value ,(org-jira-get-issue-val 'status issue)))
                                       (node-property (:key "TYPE" :value ,(org-jira-get-issue-val 'type issue)))
                                       (node-property (:key "ID" :value ,issue-id))
                                       (node-property (:key "JIRA_LINK" :value ,(concat jiralib-url "/browse/" issue-id)))))
                 ,(gojira-get-description issue (+ heading-level 1))))))

(provide 'gojira)

;;; gojira.el ends here
