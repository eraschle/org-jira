;;; org-jira-parser.el --- Parsing to and from JIRA markup
;; directional parser for JIRA-markdown. For translating org-mode string to
;; JIRA-format, the ox-jira -module is used directly. Translation in the other
;; direction is done with regular expressions and is still in early beta state.

;; TODO:

;;; Code:

(require 'org)
(require 'jiralib)
(require 'ox-jira)
(require 'cl-lib)
(require 'language-detection)
(require 's)

(defvar org-jira-parser-org-link-lookup
  (list (list :org "github:" :jira "https://github.com/")
        (list :org "atlassian:" :jira (concat jiralib-url "/browse/")))
  "Lookup for translation ORG-link to JIRA links.")


(defvar org-jira-parser-to-org nil
  "Current convert process.")

(defun org-jira-parser-org-link (url)
  "Covnert URL to special ORG-mode links or back."
  (let ((src-prop (if org-jira-parser-to-org :jira :org))
        (rpl-prop (if org-jira-parser-to-org :org :jira)))
    (dolist (lookup org-jira-parser-org-link-lookup)
      (when (string-prefix-p (plist-get lookup src-prop) url)
        (setq url (concat
                   (plist-get lookup rpl-prop)
                   (string-remove-prefix
                    (plist-get lookup src-prop) url)))))
    url))

(defun org-jira-parser-org-placeholder (placeholder)
  "Covnert PLACEHOLDER to OGR-placeholder."
  (if (s-contains-p "/" placeholder)
      (seq-first (seq-reverse (string-split placeholder "/")))
    placeholder))


(defvar org-jira-parser-export-process-underscores t
  "If nil, the parser will not make underscores into anchors.")


(defvar org-jira-parser-patterns
  '(
    ;; Code block
    ("^{code\\(?::.*language=\\(?1:[a-z]+\\)\\)?[^}]*}\\(?2:.*\\(?:
.*\\)*?\\)?
?{code}"
     . (lambda ()
         (let ((lang (match-string 1))
               (body (match-string 2))
               (md (match-data)))
           ;; (when (equal lang "none")
           (setq lang (symbol-name (language-detection-string body)))
           (when (equal lang "awk")
             ;; Language-detection seems to fallback to awk. In that case
             ;; it most likely is not code at all. (Some coworkers like to
             ;; use source blocks for other things than code...)
             (setq lang ""))
           (when (equal lang "emacslisp")
             (setq lang "elisp"))
           ;; )
           (prog1
               (concat
                "#+BEGIN_SRC " lang "\n"
                (replace-regexp-in-string "^" "  " body)
                "\n#+END_SRC")

             ;; Auto-detecting language alters match data, restore it.
             (set-match-data md)))))

    ;; Quote block
    ("^{quote}\\(.*\\(?:
.*\\)*?\\)?
?{quote}"
     . (lambda ()
         (let ((body (match-string 1))
               (md (match-data)))
           (prog1
               (concat
                "#+BEGIN_QUOTE\n"
                (replace-regexp-in-string "^" "  " body)
                "\n#+END_QUOTE")

             ;; Auto-detecting language alters match data, restore it.
             (set-match-data md)))))

    ;; Link to a user
    ("\\[~accountid:\\([a-zA-Z0-9:-]*\\)\\]"
     . (lambda ()
         (let* ((match (match-string 0))
                (account-id (string-remove-prefix ":" (match-string 1)))
                (name (org-jira-get-user-name-by account-id)))
           ;; (message "Match: %S " match)
           (when (and name (not (length= name 0)) (string-prefix-p "[~accountid:" match))
             ;; (message "Name: %S %S %s" account-id name match)
             (format "[[%s/secure/ViewProfile.jspa?name=%s][%s]]"
                     jiralib-url account-id name)))))

    ;; Link
    ("\\[\\(?:\\(.*?\\)\\)?|\\([^\\]*?\\)\\(?:|\\(.*?\\)\\)?\\]"
     . (lambda ()
         (let* ((url-raw (org-jira-parser-org-link (match-string 2)))
                (url (if (match-string 3)
                         (format "[%s|%s]" url-raw (string-remove-prefix
                                                    (concat (match-string 2) "|")
                                                    (match-string 3)))
                       (format "[%s]" url-raw)))
                (placeholder (if (match-string 1)
                                 (format "[%s]" (org-jira-parser-org-placeholder
                                                 (match-string 1)))
                               "")))
           (format "[%s%s]" url placeholder))))

    ;; Table
    ("\\(^||.*||\\)\\(\\(?:
|.*|\\)*$\\)"
     . (lambda ()
         (let ((header (match-string 1))
               (body (match-string 2))
               (md (match-data)))
           (with-temp-buffer
             (insert
              (concat
               (replace-regexp-in-string "||" "|" header)
               "\n|"
               (replace-regexp-in-string
                "" "-"
                (make-string (- (s-count-matches "||" header) 1) ?+)
                nil nil nil 1)
               "-|"
               body))
             (org-table-align)

             (prog1
                 ;; Get rid of final newline that may be injected by org-table-align
                 (replace-regexp-in-string "\n$" "" (buffer-string))

               ;; org-table-align modifies match data, restore it.
               (set-match-data md))))))

    ;; Bullet- or numbered list
    ;; For some reason JIRA sometimes inserts a space in front of the marker.
    ("^ ?\\([#*]+\\) "
     . (lambda ()
         (let* ((prefixes (match-string 1))
                (level (- (length prefixes) 1))
                (indent (make-string (max 0 (* 4 level)) ? )))
           ;; Save numbered lists with a placeholder, they will be calculated
           ;; later.
           (concat indent (if (s-ends-with? "#" prefixes) "########" "-") " "))))

    ;; Heading
    ("^h\\([1-6]\\)\\. "
     . (lambda ()
         (concat
          ;; NOTE: Requires dynamic binding to be active.
          (make-string (+ (if (boundp 'jira-to-org--convert-level)
                              jira-to-org--convert-level
                            0)
                          (string-to-number (match-string 1))) ?*) " ")))

    ;; Escaped curly braces
    ("\\\\{\\([^}]*\\)}"
     . (lambda () (concat "{" (match-string 1) "}")))

    ;; Verbatim text
    ("{{\\(.*?\\)}}"
     . (lambda () (concat "=" (match-string 1) "=")))

    ;; Italic text
    ("\\([^a-z]\\|^\\)_\\(.*?\\)_\\([^a-z]\\|$\\)"
     . (lambda () (concat (match-string 1) "/" (match-string 2) "/" (match-string 3)))))
  "Regular expression - replacement pairs used in parsing JIRA markup.")


(defun org-jira-parse-org-to-jira (s)
  "Transform org-style string S into JIRA format."
  (setq org-jira-parser-to-org nil)
  (if org-jira-parser-export-process-underscores
      (org-export-string-as s 'jira t)
    (org-export-string-as
     (concat "#+OPTIONS: ^:nil\n" s)
     'jira t)))

(defun random-alpha ()
  "Generate a random lowercase character."
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i))))

(defun random-identifier (&optional length)
  "Create a random string of length LENGTH containing only lowercase letters."
  (mapconcat (lambda (_) (random-alpha)) (make-list (or length 16) nil) ""))

(defun org-jira-parse-jira-to-org (s &optional level)
  "Transform JIRA-style string S into org-style.
If LEVEL is given, shift all
headings to the right by that amount."
  (setq org-jira-parser-to-org t)
  (condition-case nil
      (let ((backslash-replacement (random-identifier 32))
            (percent-replacement (random-identifier 32)))
        (with-temp-buffer
          (let ((replacements ())
                (jira-to-org--convert-level (or level 0)))

            ;; Literal backslashes need to be handled separately, they mess up
            ;; other regexp patching. They get replaced with the identifier

            ;; first, and restored last.
            (insert (decode-coding-string (replace-regexp-in-string
                                           "\\\\" backslash-replacement
                                           (replace-regexp-in-string
                                            "%" percent-replacement s))
                                          'utf-8))
            (cl-loop
             for (pattern . replacement) in org-jira-parser-patterns do
             (goto-char (point-min))
             (while (re-search-forward pattern nil t)
               (let ((identifier (random-identifier 32))
                     (match (match-string 0))
                     (rep (funcall replacement)))
                 (when rep
                   (replace-string-in-region match
                                             identifier
                                             (pos-bol) (pos-eol))
                   (push `(,identifier . ,rep) replacements)))))
            (mapc
             (lambda (r)
               (goto-char (point-min))
               (search-forward (car r))
               (replace-match (cdr r)))
             replacements)
            (goto-char (point-min))
            (let ((counters (make-list 6 1)))  ; JIRA Supports 6 levels of headings
              (dolist (n (split-string (buffer-string) "\n"))
                (cond ((search-forward-regexp "^\\([[:blank:]]*\\)########"
                                              (line-end-position) t)
                       (let ((level (/ (length (match-string 1)) 4)))
                         (replace-match (format "%s%i." (match-string 1) (nth level counters)))
                         (setcar (nthcdr level counters) (1+ (nth level counters)))))
                      ((search-forward-regexp "^\\([[:blank:]]*\\)- " (line-end-position) t)
                       nil)
                      (t (setq counters (make-list 6 1))))
                (forward-line 1)))
            (delete-trailing-whitespace))

          (replace-regexp-in-string backslash-replacement "\\\\" (buffer-string))
          (replace-regexp-in-string percent-replacement "%" (buffer-string))
          (buffer-substring-no-properties (point-min) (point-max))
          ))

    (error s)))


(org-link-set-parameters "atlassian"
                         :follow #'org-jira-convert-browse
                         :export #'org-jira-convert-export)
;; (org-link-set-parameters "elisp" :follow #'org-link--open-elisp)

(defun org-jira-convert--url-get (path)
  "Return URL from PATH."
  (concat jiralib-url "/browse/" (seq-first (string-split path "|"))))

(defun org-jira-convert-browse (path)
  "Open a Jira Link from PATH."
  ;; (message "URL %s PATH %S"(org-jira-convert--url-get path) path)
  (browse-url (org-jira-convert--url-get path)))

(defun org-jira-convert-export (path _desc back-end _channel)
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
           (url (org-jira-convert--url-get path)))
      ;; (message "Export %S" (concat url smart-link))
      (concat url smart-link))))


;;;###autoload
(defun org-jira-parse-comment (comment &optional to-org)
  "Return COMMENT TO-ORG is non-nil, otherwise for JIRA."
  (if to-org
      (org-jira-parse-jira-to-org comment)
    (org-jira-parse-org-to-jira comment)))


(provide 'org-jira-parser)
;;; org-jira-parser.el ends here.
