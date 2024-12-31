;;; azure-devops.el --- Azure DevOps integration for Emacs -*- lexical-binding: t; -*-

;; Version: 0.5
;; Keywords: azure, devops

;;; Commentary:

;; This package provides basic integration with Azure DevOps boards.
;; It allows you to:
;;  - View recent tickets from the last 90 days,
;;  - Create new tickets,
;;  - Update existing tickets (via a simple search + org-mode edit).
;;
;; The Hydra offers three main commands:
;;  (t) View Recent Tickets
;;  (u) Update a Ticket
;;  (c) Create Ticket

;;; Code:

(require 'json)
(require 'hydra)
(require 'url)
(require 'url-http)
(require 'url-auth)
(require 'org)
(require 'html2org)
(require 'seq)
(require 'ox)


;; ----------------------------------------------------------------------------
;; 1. Configuration Variables
;; ----------------------------------------------------------------------------

(defgroup azure-devops nil
  "Azure DevOps integration."
  :group 'tools)

(defcustom azure-devops-username "<your-username>"
  "Azure DevOps username."
  :type 'string
  :group 'azure-devops)

(defcustom azure-devops-organization-url "https://dev.azure.com/<your-org-name>/"
  "Azure DevOps organization URL."
  :type 'string
  :group 'azure-devops)

(defcustom azure-devops-project-name "<your-project-name>"
  "Azure DevOps project name."
  :type 'string
  :group 'azure-devops)

(defcustom azure-devops-pat "<your-personal-access-token>"
  "Azure DevOps Personal Access Token (PAT). Keep this value secure."
  :type 'string
  :group 'azure-devops)


;; ----------------------------------------------------------------------------
;; 2. Utility / Helper Functions
;; ----------------------------------------------------------------------------

(defun azure-devops--escape-wiql-string (str)
  "Escape single quotes in STR for a WIQL query."
  (replace-regexp-in-string "'" "''" str))

(defun azure-devops--get-auth-header ()
  "Return an Authorization header value for Azure DevOps using Basic + PAT."
  (let* ((token (concat ":" azure-devops-pat))
         (encoded-token (base64-encode-string token t)))
    (concat "Basic " encoded-token)))

(defun azure-devops--strip-container-tags (html-content)
  "Remove wrapping <div> or <p> tags from HTML-CONTENT."
  (string-trim
   (replace-regexp-in-string
    "\\<\\(div\\|p\\)[^>]*>\\(.*\\)</\\(div\\|p\\)>\\'"
    "\\2"
    html-content)))

(defun azure-devops--convert-org-to-html (org-content)
  "Convert ORG-CONTENT to minimal HTML, removing container tags."
  (let ((org-export-show-temporary-export-buffer nil)
        (org-export-with-toc nil)
        (org-export-with-section-numbers nil)
        (org-html-container-element "div")
        (org-export-preserve-breaks t))
    (azure-devops--strip-container-tags
     (org-export-string-as
      org-content
      'html
      t
      '(:html-doctype "html5"
        :html-html5-fancy nil
        :with-toc nil
        :with-tags nil
        :with-todo-keywords nil)))))

(defun azure-devops--convert-html-to-org (html)
  "Convert HTML string HTML to Org-mode format, removing leftover tags."
  (with-temp-buffer
    (insert html)
    (html2org)
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match ""))
    (buffer-string)))

(defun azure-devops--display-response-buffer ()
  "Display the current buffer contents in a temporary buffer for debugging."
  (let ((response-buffer (get-buffer-create "*Azure DevOps Response*")))
    (with-current-buffer response-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert-buffer-substring (current-buffer))
      (read-only-mode 1))
    (display-buffer response-buffer)))


;; ----------------------------------------------------------------------------
;; 3. Create Tickets
;; ----------------------------------------------------------------------------

(defun azure-devops--get-description-from-org ()
  "Open a temporary org buffer for the user to enter a description.
Press C-c C-c to finalize. Returns the text as basic HTML."
  (let ((buffer (get-buffer-create "*ADO Description*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (local-set-key (kbd "C-c C-c") #'exit-recursive-edit))
    (pop-to-buffer buffer)
    (message "Enter your description in Org mode. Press C-c C-c when done.")
    (recursive-edit)
    (let ((org-contents (with-current-buffer buffer
                          (buffer-string))))
      (kill-buffer buffer)
      (org-export-string-as
       org-contents 'html t
       '(:with-toc nil
         :with-section-numbers nil
         :with-title nil
         :preserve-breaks t)))))

(defun azure-devops-create-ticket ()
  "Create a new Azure DevOps work item (Enhancement or Bug).
Prompts for Work Item Type, Title, opens a momentary Org buffer for description,
prompts for story points, and assigns to `azure-devops-username`."
  (interactive)
  (let* ((work-item-type (completing-read
                          "Work Item Type: "
                          '("Enhancement" "Bug")
                          nil t nil nil "Enhancement"))
         (title (read-string "Title: "))
         (description (azure-devops--get-description-from-org))
         (story-points (read-number "Story Points: "))
         (patch-doc
          `[(:op "add" :path "/fields/System.Title" :value ,title)
            (:op "add" :path "/fields/System.Description" :value ,description)
            (:op "add" :path "/fields/System.AssignedTo" :value ,azure-devops-username)
            (:op "add" :path "/fields/Microsoft.VSTS.Scheduling.StoryPoints" :value ,story-points)]))
    (let* ((json-body (json-encode patch-doc))
           (url (format "%s%s/_apis/wit/workitems/$%s?api-version=7.1"
                        azure-devops-organization-url
                        azure-devops-project-name
                        (url-hexify-string work-item-type)))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json-patch+json")
              ("Authorization" . ,(azure-devops--get-auth-header))))
           (url-request-data json-body))
      (message "Creating a %s in ADO with Title: %s" work-item-type title)
      (url-retrieve
       url
       (lambda (_status)
         (goto-char (point-min))
         (when (search-forward "\n\n" nil t)
           (condition-case err
               (let* ((json-object-type 'alist)
                      (json-array-type 'list)
                      (response (json-read))
                      (work-item-id (alist-get 'id response)))
                 (if work-item-id
                     (message "Created %s ticket (ID %s) successfully!"
                              work-item-type work-item-id)
                   (message "Unexpected response: %S" response)))
             (json-error
              (message "JSON parse error: %S" err)))))
       nil t))))


;; ----------------------------------------------------------------------------
;; 4. View Recent Tickets
;; ----------------------------------------------------------------------------

(defun azure-devops-view-recent-tickets ()
  "View tickets assigned to `azure-devops-username` created in last 45 days,
not in Closed state, sorted by CreatedDate descending.
Splits into Current Sprint vs. Backlog in an Org buffer."
  (interactive)
  (let* ((wiql (format "SELECT [System.Id]
                        FROM WorkItems
                        WHERE [System.AssignedTo] = '%s'
                          AND [System.CreatedDate] >= @Today-90
                          AND [System.State] <> 'Closed'
                        ORDER BY [System.CreatedDate] DESC"
                       azure-devops-username))
         (wiql-url (format "%s%s/_apis/wit/wiql?api-version=7.1"
                           (file-name-as-directory azure-devops-organization-url)
                           azure-devops-project-name))
         (json-body (json-encode `(("query" . ,wiql))))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(azure-devops--get-auth-header))))
         (url-request-data json-body))
    (message "Executing WIQL query for last 45 days of non-Closed tickets, sorted by CreatedDate ...")
    (url-retrieve
     wiql-url
     (lambda (_status)
       (goto-char (point-min))
       (let ((status-code (url-http-parse-response)))
         (cond
          ((not (eq status-code 200))
           (message "Request failed with status code %s" status-code)
           (azure-devops--display-response-buffer))
          ((not (search-forward "\n\n" nil t))
           (message "No response received."))
          (t
           (let* ((json-object-type 'alist)
                  (json-array-type 'list)
                  (response (json-read))
                  (work-items (assoc-default 'workItems response)))
             (if (null work-items)
                 (message "No recent tickets found in the last 45 days.")
               (let ((ids (mapcar (lambda (it) (alist-get 'id it)) work-items)))
                 (azure-devops--batch-fetch-work-items ids)))))))
     nil t))))

(defun azure-devops--batch-fetch-work-items (ids)
  "Use the WorkItemsBatch API to fetch details for IDS, then display in a read-only Org buffer."
  (let* ((fields '("System.Id"
                   "System.Title"
                   "System.State"
                   "System.Description"
                   "Microsoft.VSTS.Scheduling.StoryPoints"
                   "System.Rev"
                   "System.IterationPath"
                   "Microsoft.VSTS.Common.AcceptanceCriteria"))
         (request-body (json-encode
                        `(("ids" . ,ids)
                          ("fields" . ,fields)
                          ("$expand" . "None"))))
         (batch-url (format "%s%s/_apis/wit/workitemsbatch?api-version=7.1"
                            (file-name-as-directory azure-devops-organization-url)
                            azure-devops-project-name))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(azure-devops--get-auth-header))))
         (url-request-data request-body))
    (message "Fetching details for %d tickets via WorkItemsBatch..." (length ids))
    (url-retrieve
     batch-url
     (lambda (_status)
       (goto-char (point-min))
       (let ((status-code (url-http-parse-response)))
         (cond
          ((not (eq status-code 200))
           (message "Batch request failed with status code %s" status-code)
           (azure-devops--display-response-buffer))
          ((not (search-forward "\n\n" nil t))
           (message "No response received."))
          (t
           (let* ((json-object-type 'alist)
                  (json-array-type 'list)
                  (response (json-read))
                  (items (assoc-default 'value response)))
             (if (null items)
                 (message "No details returned for these items.")
               (azure-devops--display-multiple-work-items items)))))))
     nil t)))

(defun azure-devops--display-work-items-subtree (items)
  "Insert each ITEM as a sub-heading under the current Org section."
  (dolist (item items)
    (let* ((fields (assoc 'fields item))
           (id (alist-get 'System.Id fields))
           (title (alist-get 'System.Title fields))
           (state (alist-get 'System.State fields))
           (description (alist-get 'System.Description fields))
           (story-points (alist-get 'Microsoft.VSTS.Scheduling.StoryPoints fields))
           (iteration (alist-get 'System.IterationPath fields))
           (ac (alist-get 'Microsoft.VSTS.Common.AcceptanceCriteria fields)))
      (insert (format "** [%s] %s (State: %s, SP: %s)\n"
                      id (or title "") (or state "") (or story-points "")))
      (insert (format ":PROPERTIES:\n:Iteration: %s\n:END:\n\n" (or iteration "")))
      (when description
        (insert "*** Description\n")
        (insert (azure-devops--convert-html-to-org description))
        (insert "\n"))
      (when ac
        (insert "*** Acceptance Criteria\n")
        (insert (azure-devops--convert-html-to-org ac))
        (insert "\n"))
      (insert "\n"))))

(defun azure-devops--display-multiple-work-items (items)
  "Display multiple work ITEMS in a read-only Org buffer.
Partition them into 'Current Sprint' vs. 'Backlog' based on iteration name."
  (let ((buffer (get-buffer-create "*Azure DevOps Recent Tickets*"))
        (current-sprint-items '())
        (backlog-items '()))
    ;; Partition items into two lists by iteration path.
    (dolist (item items)
      (let* ((fields (assoc 'fields item))
             (iteration (alist-get 'System.IterationPath fields)))
        (cond
         ((string= iteration "QRP - Business\\Current (Active)")
          (push item current-sprint-items))
         ((string= iteration "QRP - Business")
          (push item backlog-items))
         ;; else skip or place in backlog, see note below:
         ;; (t (push item backlog-items))
         )))

    ;; Reverse lists so they appear in the original order.
    (setq current-sprint-items (nreverse current-sprint-items))
    (setq backlog-items (nreverse backlog-items))

    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)

      ;; Section for Current Sprint
      (insert "* Current Sprint\n\n")
      (azure-devops--display-work-items-subtree current-sprint-items)

      ;; Section for Backlog
      (insert "* Backlog\n\n")
      (azure-devops--display-work-items-subtree backlog-items)

      (goto-char (point-min))
      (read-only-mode 1))

    (display-buffer buffer)))


;; ----------------------------------------------------------------------------
;; 5. Search + Update (Retained for internal usage)
;; ----------------------------------------------------------------------------

(defvar azure-devops-current-ticket-id nil
  "Holds the ID of the currently opened Azure DevOps ticket.")

(defvar-local azure-devops-parent-index nil
  "Holds the index of the parent relation in the relations array.")

(defun azure-devops-search-tickets (&optional update)
  "Search for tickets by title in Azure DevOps.
If UPDATE is non-nil, open them in an editable org buffer for patching."
  (interactive)
  ;; Because we want 'update' to still work, keep this function around:
  ;; If needed, you can call it manually or rely on (u) in the Hydra.
  (let* ((raw-search-term (read-string "Search tickets by title: "))
         (search-term (azure-devops--escape-wiql-string raw-search-term))
         (wiql (format "SELECT [System.Id], [System.Title], [System.State], [System.Description], \
[Microsoft.VSTS.Scheduling.StoryPoints], [System.Rev], [System.IterationPath], [Microsoft.VSTS.Common.AcceptanceCriteria] \
FROM WorkItems \
WHERE [System.Title] CONTAINS '%s'"
                       search-term))
         (json-body (json-encode `(("query" . ,wiql))))
         (url (format "%s%s/_apis/wit/wiql?api-version=7.1"
                      (file-name-as-directory azure-devops-organization-url)
                      azure-devops-project-name))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(azure-devops--get-auth-header))))
         (url-request-data json-body))
    (url-retrieve
     url
     (lambda (_status)
       (goto-char (point-min))
       (let ((status-code (url-http-parse-response)))
         (cond
          ((not (eq status-code 200))
           (message "Request failed with status code %s" status-code)
           (azure-devops--display-response-buffer))
          ((not (search-forward "\n\n" nil t))
           (message "No response received."))
          (t
           (let* ((json-object-type 'alist)
                  (json-array-type 'list)
                  (response (json-read))
                  (work-items (assoc-default 'workItems response)))
             (if work-items
                 (azure-devops--fetch-work-items-details work-items update)
               (message "No work items found matching the search term.")))))))
     nil t)))

;; ----------------------------------------------------------------------------
;; 6. Hydra Menu (Simplified)
;; ----------------------------------------------------------------------------

(defhydra hydra-azure-devops (:color blue :hint nil)
  "
Azure DevOps Menu
---------------------------
_t_: View Recent Tickets
_u_: Update Ticket
_c_: Create Ticket
_q_: Quit
"
  ("t" azure-devops-view-recent-tickets "View Recent Tickets")
  ("u" (lambda () (interactive) (azure-devops-search-tickets 'update)) "Update Ticket")
  ("c" azure-devops-create-ticket "Create Ticket")
  ("q" nil "Quit"))

(defun azure-devops-menu ()
  "Show the Azure DevOps Hydra menu."
  (interactive)
  (hydra-azure-devops/body))

(provide 'azure-devops)

;;; azure-devops.el ends here
