;;; org-jira-comment-t.el --- ERT tests
;; Copyright (C) 2017 Matthew Carter <m@ahungry.com>
;;
;; Authors:
;; Matthew Carter <m@ahungry.com>
;;
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/org-jira
;; Version: 2.6.2
;; Keywords: ahungry jira org bug tracker
;; Package-Requires: ((emacs "24.5") (cl-lib "0.5") (request "0.2.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/> or write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This tests the extension to org-mode for syncing issues with JIRA
;; issue servers.

;;; News:

;;;; Changes since 0.0.0:
;; - Add some basic tests

;;; Code:
(require 'org-jira)
(require 'org-jira-parse)

(ert-deftest org-jira-date-to-org-clock-test ()
  (should
   (string= "2017-01-01 Sun 00:00"
            (org-jira-date-to-org-clock "2017-01-01T00:00:00.000+0000")))
  (should
   (string= "2017-02-05 Sun 00:00"
            (org-jira-date-to-org-clock "2017-02-05T00:00:00.000+0000"))))


(ert-deftest org-jira-parse-account-link ()
  (should
   (string= "some text[[https://rsrg.atlassian.net/secure/ViewProfile.jspa?name=63348c3e2eaaa5dcfa150af2][Erich Raschle]] some other"
            (org-jira-parse-jira-to-org "some text[~accountid:63348c3e2eaaa5dcfa150af2] some other"))))

(ert-deftest org-jira-account-id-regex ()
  (should
   (with-temp-buffer
     (insert "[~accountid:63348c3e2eaaa5dcfa150af2] mal schauen wie es angezeigt wird...")
     (goto-char (point-min))
     (re-search-forward  "\\[.?accountid.?\\([0-9a-zA-Z:-]\\{20,\\}\\)*\\]")
     (message "Match 0: %S" (match-string 0))
     (message "Match 1: %S" (match-string 1))
     (when (string= "[~accountid:63348c3e2eaaa5dcfa150af2]"
                    (match-string 0))

       (goto-char (point-min))
       (re-search-forward  "\\[.?accountid.?\\([0-9a-zA-Z:-]+\\)\\]")
       (string= "63348c3e2eaaa5dcfa150af2"
                (match-string 1))))))


(ert-deftest org-jira-user-name-by-account-id ()
  (should
   (string= "Erich Raschle"
            (org-jira-get-user-name-by "63348c3e2eaaa5dcfa150af2"))))


(ert-deftest org-jira-org-link-without-display-name-extern ()
  (should
   (string= "some text [[github:nyyManni/ejira-parse.el|smart-link][ejira-parse.el]] some other"
            (org-jira-parse-jira-to-org
             "some text[https://github.com/nyyManni/ejira-parse.el|https://github.com/nyyManni/ejira-parse.el|smart-link] some other"))))


(ert-deftest org-jira-jira-link-without-display-name-extern ()
  (should
   (string= "some text[ejira-parse.el|https://github.com/nyyManni/ejira-parse.el|smart-link] some other"
            (org-jira-parse-org-to-jira
             "some text[[github:nyyManni/ejira-parse.el|smart-link][ejira-parse.el]] some other"))))


(ert-deftest org-jira-org-link-with-display-name-extern ()
  (should
   (string= "[[github:nyyManni/ejira/blob/master/ejira-parse.el][ejira-parse]]"
            (org-jira-parse-jira-to-org
             "[ejira-parse|https://github.com/nyyManni/ejira/blob/master/ejira-parse.el]"))))

(ert-deftest org-jira-jira-link-with-display-name-extern ()
  (should
   (string= "[ejira-parse|https://github.com/nyyManni/ejira/blob/master/ejira-parse.el]"
            (org-jira-parse-org-to-jira
             "[[github:nyyManni/ejira/blob/master/ejira-parse.el][ejira-parse]]"))))


(ert-deftest org-jira-org-link-with-smart-link-intern ()
  (should
   (string= "[[https://rsrg.atlassian.net/browse/ERASC-10][ERASC-10|smart-link]]"
            (org-jira-parse-jira-to-org
             "[https://rsrg.atlassian.net/browse/ERASC-10|https://rsrg.atlassian.net/browse/ERASC-10|smart-link]"))))

(ert-deftest org-jira-jira-link-with-smart-link-intern ()
  (should
   (string= "[ERASC-10|https://rsrg.atlassian.net/browse/ERASC-10|smart-link]"
            (org-jira-parse-org-to-jira
             "[[https://rsrg.atlassian.net/browse/ERASC-10][ERASC-10|smart-link]"))))


(ert-deftest org-jira-jira-link-with-smart-link-intern ()
  (should
   (string=  "| *Kategorie* | *Min. Höhe* |
|Kabelrohre|1.20m|
|Kabelschacht|0.70m|
|Kabelschacht (Gleise)|0.60m|"
             (org-jira-parse-org-to-jira
              "||*Kategorie*||*Min. Höhe*||
|Kabelrohre|1.20m|
|Kabelschacht|0.70m|
|Kabelschacht (Gleise)|0.60m|"))))




(ert-deftest org-jira-org-table-header-and-link ()
  (should
   (string= "| *Name* | [[https://some.atlassian.net/url|smart-link][Display]] |
| some value | other value |"
            (org-jira-parse-jira-to-org "||*Name*||[Display|https://some.atlassian.net/url|smart-link]||
| some value | other value |"))))





















(provide 'org-jira-comment-t)
;;; org-jira-comment-t.el ends here
