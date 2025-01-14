;;; org-jira-parse.el --- Parsing to and from JIRA markup -*- lexical-binding: t -*-
;;; Commentary:
;; directional parser for JIRA-markdown. For translating org-mode string to
;; JIRA-format, the ox-jira -module is used directly. Translation in the other
;; direction is done with regular expressions and is still in early beta state.

;; TODO:

;;; Code:

(require 'org)
(require 'org-jira-link)
(require 'jiralib)
(require 'ox-jira)
(require 'cl-lib)
(require 'language-detection)
(require 's)


(defun org-jira-parse-org-placeholder (placeholder)
  "Convert PLACEHOLDER to OGR-placeholder."
  (if (s-contains-p "/" placeholder)
      (seq-first (seq-reverse (string-split placeholder "[/]+")))
    placeholder))


(defvar org-jira-parse-export-process-underscores t
  "If nil, the parser will not make underscores into anchors.")

(defvar org-jira-parse-to-org nil
  "Convert direction either from ORG to JIRA or vis versa.")

(defvar jira-to-org--parse-level 0
  "Indent offset to convert org heading and lists.")


(defvar org-jira-parse-patterns
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
           (setq lang-dect (symbol-name (language-detection-string body)))
           (if (equal lang-dect "awk")
               ;; Language-detection seems to fallback to awk. In that case
               ;; it most likely is not code at all. (Some coworkers like to
               ;; use source blocks for other things than code...)
               (setq lang lang-dect)
             (message "Detected language: %s" lang-dect)
             (setq lang lang))
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
                (name (org-jira-user-name-by account-id)))
           ;; (message "Match: %S " match)
           (when (and name (not (length= name 0)) (string-prefix-p "[~accountid:" match))
             ;; (message "Name: %S %S %s" account-id name match)
             (format "[[%s/secure/ViewProfile.jspa?name=%s][%s]]"
                     jiralib-url account-id name)))))

    ;; Link
    ("\\[\\(.*\\)|\\(.*\\)\\(\\]\\||\\(.*\\)\\]"
     . (lambda ()
         (message "Gruppe 0: %s\\n1: %s\\n2: %s\\n3: %s" (match-string 0) (match-string 1) (match-string 2) (match-string 3))
         (let* ((url-raw (match-string 2))
                (name (match-string 1))
                (url (if (match-string 3)
                         (format "[%s|%s]" url-raw (string-remove-prefix
                                                    (concat (match-string 2) "|")
                                                    (match-string 3)))
                       (format "[%s]" url-raw)))
                (placeholder (if (match-string 1)
                                 (format "[%s]" (org-jira-parse-org-placeholder
                                                 (match-string 1)))
                               "")))
           (message "URL: %s" url placeholder)
           (format "[%s%s]" url placeholder))))

    ;; Table
    ("\\(^||.*||\\)\\(\\(?:|.*|\\)*$\\)"
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
    ("^ +h\\([1-6]\\)\\. "
     . (lambda ()
         (concat
          (make-string (+ jira-to-org--parse-level
                          (string-to-number (match-string 1)))
                       ?*)
          " ")))

    ;; Escaped curly braces
    ("\\\\{\\([^}]*\\)}"
     . (lambda () (concat "{" (match-string 1) "}")))

    ;; Verbatim text
    ("{{\\(.*?\\)}}"
     . (lambda () (concat "=" (match-string 1) "=")))

    ;; Italic text
    ("\\([^a-z]\\|^\\)_\\(.*?\\)_\\([^a-z]\\|$\\)"
     . (lambda () (concat (match-string 1) "/" (match-string 2) "/" (match-string 3))))

    ;; Underline text
    ("\\([ ]+\\|^\\)[+]\\(.*?\\)[+]\\([ ]+\\|$\\)"
     . (lambda () (concat (match-string 1) "_" (match-string 2) "_" (match-string 3))))
    )
  "Regular expression - replacement pairs used in parsing JIRA markup.")


(defun org-jira-parse-org-to-jira (s)
  "Transform org-style string S into JIRA format."
  (setq org-jira-parse-to-org nil)
  (unless org-jira-parse-export-process-underscores
    (setq s (concat "#+OPTIONS: ^:nil\n" s)))
  (org-export-string-as s 'jira t))


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
  (setq org-jira-parse-to-org t)
  (condition-case nil
      (let ((backslash-replacement (random-identifier 32))
            (percent-replacement (random-identifier 32)))
        (with-temp-buffer
          (setq jira-to-org--parse-level (or level 0))
          (let ((replacements ()))

            ;; Literal backslashes need to be handled separately, they mess up
            ;; other regexp patching. They get replaced with the identifier
            (insert (decode-coding-string (replace-regexp-in-string
                                           "\\\\" backslash-replacement
                                           (replace-regexp-in-string
                                            "%" percent-replacement s))
                                          'utf-8))
            (cl-loop
             for (pattern . replacement) in org-jira-parse-patterns do
             (goto-char (point-min))
             (while (re-search-forward pattern nil t)
               (let ((identifier (random-identifier 32))
                     (match (match-string 0))
                     (rep (funcall replacement)))
                 (when rep
                   ;; (message "Match: %s replace %s" match rep)
                   (replace-string-in-region
                    match identifier (pos-bol) (pos-eol))
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
          (buffer-substring-no-properties (point-min) (point-max))))
    (error s)))


;;;###autoload
(defun org-jira-parse-comment (comment &optional to-org)
  "Return COMMENT TO-ORG is non-nil, otherwise for JIRA."
  (message "Parse Comment: %s" (org-jira-parse-jira-to-org comment))
  (if to-org
      (org-jira-parse-jira-to-org comment)
    (org-jira-parse-org-to-jira comment)))


(provide 'org-jira-parse)
;;; org-jira-parse.el ends here.
