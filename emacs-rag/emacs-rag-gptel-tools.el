;;; emacs-rag-gptel-tools.el --- gptel tools for RAG search -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))

;;; Commentary:

;; This module provides gptel tools that integrate the RAG search
;; functionality with LLM function calling. These tools allow LLMs
;; to search through indexed documents and retrieve relevant content.

;;; Code:

(require 'emacs-rag-server)
(require 'gptel)

;;; Helper Functions

(defun emacs-rag-gptel--get-top-result-text (query &optional limit)
  "Perform vector search for QUERY and return matching chunks with org-links.
LIMIT specifies the maximum number of results to return (default 5).
Returns nil if no results found or if the server is not running."
  (condition-case err
      (if (not (emacs-rag-server-running-p))
          (error "RAG server is not running. Start it first with emacs-rag-start-server")
        (let* ((num-results (or limit 5))
               (response (emacs-rag--request "GET" "/search/vector" nil
                                            `((query . ,query)
                                              (limit . ,num-results)
                                              (rerank . "true"))))
               (results (alist-get 'results response)))
          (if (null results)
              (format "No results found for query: %s" query)
            (concat
             (format "* Search results for: %s\n\n" query)
             (mapconcat
              (lambda (result)
                (let* ((path (alist-get 'source_path result))
                       (score (alist-get 'score result))
                       (line-number (alist-get 'line_number result))
                       (chunk-content (alist-get 'content result)))
                  (format "** Match (score: %.3f)\n\n[[file:%s::%d][%s::%d]]\n\n#+begin_quote\n%s\n#+end_quote"
                          score
                          path
                          line-number
                          path
                          line-number
                          chunk-content)))
              results
              "\n\n")))))
    (error
     (format "Error during RAG search: %s" (error-message-string err)))))

;;; gptel Tool Definitions

;;;###autoload
(defvar emacs-rag-gptel-tool-search
  (gptel-make-tool
   :function #'emacs-rag-gptel--get-top-result-text
   :name "rag_search"
   :description "Search through indexed documents using semantic vector search and return matching chunks with org-links. Use this when you need to retrieve relevant information from the user's knowledge base or indexed documents."
   :args (list '(:name "query"
                 :type "string"
                 :description "The search query to find relevant documents")
               '(:name "limit"
                 :type "integer"
                 :description "Maximum number of results to return (default: 5)"
                 :optional t))
   :category "rag")
  "gptel tool for RAG semantic search that returns matching chunks with org-links.")

;;; Interactive Commands

;;;###autoload
(defun emacs-rag-gptel-enable-tool ()
  "Enable the RAG search tool for gptel.
Adds the rag_search tool to the list of available gptel tools."
  (interactive)
  (unless (fboundp 'gptel-make-tool)
    (user-error "gptel-make-tool is not available. Please update gptel to a version that supports tools"))
  (add-to-list 'gptel-tools emacs-rag-gptel-tool-search)
  (message "RAG search tool enabled for gptel"))

;;;###autoload
(defun emacs-rag-gptel-disable-tool ()
  "Disable the RAG search tool for gptel.
Removes the rag_search tool from the list of available gptel tools."
  (interactive)
  (setq gptel-tools (delq emacs-rag-gptel-tool-search gptel-tools))
  (message "RAG search tool disabled for gptel"))

(provide 'emacs-rag-gptel-tools)
;;; emacs-rag-gptel-tools.el ends here
