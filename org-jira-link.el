;;; org-jira-link.el --- Parsing to and from JIRA markup -*- lexical-binding: t -*-
;;; Commentary:
;; directional parser for JIRA-markdown. For translating org-mode string to
;; JIRA-format, the ox-jira -module is used directly. Translation in the other
;; direction is done with regular expressions and is still in early beta state.

;; TODO:

;;; Code:

(require 'org)
(require 'jiralib)

(defvar org-jira-link-lookup
  (list (list :org "github:" :normal "https://github.com/")
        (list :org "atlassian:" :normal (concat jiralib-url "/browse/")))
  "Lookup for translation ORG-link to JIRA links.")


(defvar org-jira-link-to-org nil
  "Current convert process.")


(defun org-jira-link-remove-smartlink (path)
  "Return PATH without Jira smart-link."
  (seq-first (string-split path "|")))


(defun org-jira-link-url-get (url &optional remove-smart-link)
  "Covnert URL to special ORG-mode links or back.
If non-nil then REMOVE-SMART-LINK from URL."
  (let ((from-url (if org-jira-link-to-org :normal :org))
        (to-url (if org-jira-link-to-org :org :normal)))
    (dolist (lookup org-jira-link-lookup)
      (when (string-prefix-p (plist-get lookup from-url) url)
        (setq url (concat
                   (plist-get lookup to-url)
                   (string-remove-prefix
                    (plist-get lookup from-url) url)))))
    (if remove-smart-link
        (org-jira-link-remove-smartlink url)
      url)))


(org-link-set-parameters "atlassian"
                         :follow #'org-jira-links-atlassian-browse
                         :export #'org-jira-links-atlassian-export)


(defun org-jira-links-atlassian-url-get (path &optional remove-smart-link)
  "Return URL from PATH and if non-nil REMOVE-SMART-LINK."
  (org-jira-link-url-get (concat "atlassian:" path)
                         remove-smart-link))


(defun org-jira-links-atlassian-browse (path)
  "Open a Jira Link from PATH."
  ;; (message "URL %s PATH %S"(org-jira-convert--url-get path) path)
  (browse-url (org-jira-links-atlassian-url-get path)))


(defun org-jira-links-atlassian-export (path _desc back-end _channel)
  "Export Atlassian link to JIRA with PATH DESC BACK-END.
Description of variables:
- the path, as a string,
- the description as a string, or nil,
- the export back-end,
- the export communication channel, as a plist."
  (when (equal back-end 'jira)
    ;; (message "Info %s %s" path desc)
    (let* ((split-path (string-split path "|"))
           (smart-link (if (length= split-path 1) "" (concat "|" (seq-elt split-path 1))))
           (url (org-jira-links-atlassian-url-get path)))
      ;; (message "Export %S" (concat url smart-link))
      (concat url smart-link))))


(org-link-set-parameters "github"
                         :follow #'org-jira-links-github-browse
                         :export #'org-jira-links-github-export)


(defun org-jira-links-github-url-get (path)
  "Return Github URL from PATH."
  (org-jira-link-url-get (concat "github:" path) t))


(defun org-jira-links-github-browse (path)
  "Open a Github Link from PATH."
  ;; (message "URL %s PATH %S"(org-jira-convert--url-get path) path)
  (browse-url (org-jira-links-github-url-get path)))


(defun org-jira-links-github-export (path _desc _back-end _channel)
  "Export Github link with PATH DESC BACK-END.
Description of variables:
- the path, as a string,
- the description as a string, or nil,
- the export back-end,
- the export communication channel, as a plist."
  (org-jira-links-github-url-get path))

(provide 'org-jira-link)
;;; org-jira-link.el ends here.
