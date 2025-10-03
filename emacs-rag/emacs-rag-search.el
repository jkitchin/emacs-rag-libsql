;;; emacs-rag-search.el --- Search interface for Emacs RAG -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module provides search commands and result navigation
;; for the RAG system.

;;; Code:

(require 'emacs-rag-server)

;;; Customization

(defcustom emacs-rag-search-limit 5
  "Default number of search results to return."
  :type 'integer
  :group 'emacs-rag)

(defcustom emacs-rag-search-enable-rerank t
  "Enable reranking in search by default."
  :type 'boolean
  :group 'emacs-rag)

(defcustom emacs-rag-result-display-width 80
  "Width for wrapping result content in display."
  :type 'integer
  :group 'emacs-rag)

;;; Helper Functions

(defun emacs-rag--read-query (prompt)
  "Read a search query from the user with PROMPT."
  (read-string prompt))

(defun emacs-rag--wrap-text (text width)
  "Wrap TEXT to WIDTH characters."
  (with-temp-buffer
    (insert text)
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun emacs-rag--format-result (result index)
  "Format RESULT for display at INDEX.
Returns a multiline string with header and wrapped content."
  (let* ((path (alist-get 'source_path result))
         (basename (file-name-nondirectory path))
         (chunk (alist-get 'chunk_index result))
         (score (alist-get 'score result))
         (content (alist-get 'content result))
         (header (format "%2d. %-20s chunk %-3d score %.3f"
                        index basename chunk score))
         (wrapped-content (emacs-rag--wrap-text
                          content
                          (- emacs-rag-result-display-width 4))))
    (concat header "\n    "
            (replace-regexp-in-string "\n" "\n    " wrapped-content))))

(defun emacs-rag--open-result (result)
  "Open the file and navigate to the location of RESULT."
  (let* ((path (alist-get 'source_path result))
         (line-number (alist-get 'line_number result)))
    (find-file path)
    (goto-char (point-min))
    (forward-line (1- line-number))
    (recenter)))

;;; Search Commands

(defun emacs-rag-search-vector (query &optional limit rerank)
  "Perform semantic vector search for QUERY.
LIMIT is the maximum number of results (defaults to `emacs-rag-search-limit').
RERANK enables reranking (defaults to `emacs-rag-search-enable-rerank').

With prefix argument, prompt for limit."
  (interactive (list (emacs-rag--read-query "Vector search: ")
                     (when current-prefix-arg
                       (read-number "Number of results: "
                                   emacs-rag-search-limit))
                     emacs-rag-search-enable-rerank))
  (let* ((limit (or limit emacs-rag-search-limit))
         (rerank (if rerank "true" "false"))
         (response (emacs-rag--request "GET" "/search/vector" nil
                                      `((query . ,query)
                                        (limit . ,limit)
                                        (rerank . ,rerank))))
         (results (alist-get 'results response)))
    (emacs-rag--display-results results "Vector" query)))

;;; Result Display

(defun emacs-rag--display-results (results mode query)
  "Display RESULTS from MODE search for QUERY.
Uses completion interface to select and navigate to results."
  (if (null results)
      (message "No %s results for: %s" mode query)
    (let* ((candidates
            (cl-loop for result in results
                     for index from 1
                     collect (cons (emacs-rag--format-result result index)
                                  result)))
           (completion-fn (if (fboundp 'ivy-read)
                             'emacs-rag--display-with-ivy
                           'emacs-rag--display-with-completing-read)))
      (funcall completion-fn candidates mode query))))

(defun emacs-rag--display-with-completing-read (candidates mode query)
  "Display CANDIDATES using `completing-read'.
MODE and QUERY are used in the prompt."
  (let* ((choice (completing-read
                 (format "%s search for '%s': " mode query)
                 (mapcar #'car candidates)
                 nil t))
         (result (cdr (assoc choice candidates))))
    (when result
      (emacs-rag--open-result result))))

(defun emacs-rag--display-with-ivy (candidates mode query)
  "Display CANDIDATES using Ivy if available.
MODE and QUERY are used in the prompt."
  (ivy-read (format "%s search for '%s': " mode query)
            (mapcar #'car candidates)
            :action (lambda (choice)
                     (let ((result (cdr (assoc choice candidates))))
                       (when result
                         (emacs-rag--open-result result))))))

;;; Statistics

(defun emacs-rag-stats ()
  "Display database statistics."
  (interactive)
  (let* ((stats (emacs-rag--request "GET" "/stats"))
         (chunks (alist-get 'total_chunks stats))
         (files (alist-get 'total_unique_files stats)))
    (message "Index contains %s chunks across %s files" chunks files)))

;;; Database Management

(defun emacs-rag-delete-database ()
  "Delete the entire RAG database.
This operation cannot be undone!"
  (interactive)
  (when (yes-or-no-p "Delete entire database? This cannot be undone! ")
    (when (emacs-rag-server-running-p)
      (emacs-rag-stop-server)
      (sit-for 1))
    (let ((db-dir (expand-file-name emacs-rag-db-path)))
      (when (file-directory-p db-dir)
        (delete-directory db-dir t)
        (message "Database deleted: %s" db-dir)))))

;;; Search at Point

(defun emacs-rag-search-at-point ()
  "Search for the word or region at point."
  (interactive)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'word t))))
    (if query
        (emacs-rag-search-vector query)
      (user-error "No word or region at point"))))

;;; Search History

(defvar emacs-rag-search-history nil
  "History of search queries.")

(defun emacs-rag-search-vector-with-history (query &optional limit)
  "Perform vector search with QUERY, using history.
LIMIT is the maximum number of results."
  (interactive (list (read-string "Vector search: "
                                  nil 'emacs-rag-search-history)
                     (when current-prefix-arg
                       (read-number "Number of results: "
                                   emacs-rag-search-limit))))
  (emacs-rag-search-vector query limit))

(provide 'emacs-rag-search)
;;; emacs-rag-search.el ends here
