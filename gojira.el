;;; gojira.el --- import jira issue into org-mode                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tosh Lyons

;; Author: Tosh Lyons <tosh.lyons@gmail.com>
;; Keywords: lisp jira org-mode
;; Version: 0.0.1

;;; Commentary:

;; Import a jira issue into org-mode as a todo.
;;
;; Gojira is built on top of org-jira and acts as an alternative interface for
;; using org-jira. It allows you to use an existing org file and import issues
;; as single standalone entities.
;;
;; It's currently only one way. Jira->Org. I may someday add the other
;; direction.
;;
;; Depends on org-jira here https://github.com/ahungry/org-jira

;;; Code:

(require 'org-jira)

(defun gojira-insert-issue-as-org ()
  (interactive)
  (save-excursion
    (org-insert-heading-respect-content)
    (backward-char)
    (insert (org-element-interpret-data (gojira-get-issue-by-id (read-from-minibuffer "Jira Issue: "))))
    (org-narrow-to-subtree)
    (indent-region (point-min) (point-max))
    (widen)))

(defun gojira-get-issue-by-id (issue-id)
  (gojira-get-issue (car (org-jira-get-issue-by-id issue-id))))

(defun gojira-get-issue (issue)
  (interactive)
  (let* ((proj-key (org-jira-get-issue-project issue))
         (issue-id (org-jira-get-issue-key issue))
         (issue-summary (org-jira-get-issue-summary issue))
         (issue-headline issue-summary)
         (issue-hash ()))
    (progn
      (let ((status (org-jira-get-issue-val 'status issue)))
        (setq heading (concat (cond (org-jira-use-status-as-todo
                                     (upcase (replace-regexp-in-string " " "-" status)))
                                    ((member status org-jira-done-states) "DONE")
                                    ("TODO")) " "
                                    issue-headline)))
      `(headline (:title ,heading :level 0)
                 (property-drawer nil ((node-property (:key "CREATED" :value ,(org-jira-get-issue-val 'created issue)))
                                       (node-property (:key "UPDATED" :value ,(org-jira-get-issue-val 'updated issue)))
                                       (node-property (:key "ASSIGNEE" :value ,(org-jira-get-issue-val 'assignee issue)))
                                       (node-property (:key "REPORTER" :value ,(org-jira-get-issue-val 'reporter issue)))
                                       (node-property (:key "PRIORITY" :value ,(org-jira-get-issue-val 'type issue)))
                                       (node-property (:key "ID" :value ,issue-id))
                                       (node-property (:key "JIRA_LINK" :value ,(concat jiralib-url "/browse/" issue-id)))))
                 (,(replace-regexp-in-string "" "" (org-jira-get-issue-val 'description issue)))))))

(provide 'gojira)
