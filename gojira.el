;;; gojira.el --- import jira issue into org-mode                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tosh Lyons

;; Author: Tosh Lyons <tosh.lyons@gmail.com>
;; Version: 0.0.1
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
      (insert (org-element-interpret-data (gojira-get-issue-by-id issue-id)))
      (org-narrow-to-subtree)
      (gojira-insert-comments-for-issue-id issue-id (+ heading-level 1))
      (indent-region (point-min) (point-max))
      (widen))))

(defun gojira-insert-comments-for-issue-id (issue-id heading-level)
  "Insert all comments for given ISSUE-ID. HEADING-LEVEL is the level of the issue heading."
  (mapc #'(lambda (comment) (gojira-insert-org-element (gojira-comment-to-org-element comment heading-level))) (gojira-get-comments-by-issue-id issue-id)))

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
         (comment-headline (format "Comment: %s" comment-author)))
    `(headline (:title ,comment-headline :level ,heading-level)
               (property-drawer nil ((node-property (:key "CREATED" :value ,(org-jira-get-comment-val 'created comment)))
                                     ,(unless (string= (org-jira-get-comment-val 'updated comment) (org-jira-get-comment-val 'created comment))
                                        `(node-property (:key "UPDATED" :value ,(org-jira-get-comment-val 'updated comment))))
                                     (node-property (:key "ID" :value ,(org-jira-get-comment-val 'id comment)))))
               (,(gojira-process-body (org-jira-find-value comment 'body))))))

(defun gojira-get-issue-by-id (issue-id)
  "Get the issue with ISSUE-ID from jira. Return an org element."
  (gojira-get-issue (car (org-jira-get-issue-by-id issue-id))))

(defun gojira-get-issue (issue)
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
                                       (node-property (:key "PRIORITY" :value ,(org-jira-get-issue-val 'priority issue)))
                                       (node-property (:key "STATUS" :value ,(org-jira-get-issue-val 'status issue)))
                                       (node-property (:key "TYPE" :value ,(org-jira-get-issue-val 'type issue)))
                                       (node-property (:key "ID" :value ,issue-id))
                                       (node-property (:key "JIRA_LINK" :value ,(concat jiralib-url "/browse/" issue-id)))))
                 (,(gojira-process-body (org-jira-get-issue-val 'description issue)))))))

(provide 'gojira)

;;; gojira.el ends here
