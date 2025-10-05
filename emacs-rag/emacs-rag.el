;;; emacs-rag.el --- Retrieval-Augmented Generation for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/emacs-rag-libsql
;; Keywords: tools, convenience, matching
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))

;;; Commentary:

;; Emacs RAG provides semantic search and retrieval-augmented generation
;; capabilities for Emacs, using a FastAPI server with LibSQL backend.
;;
;; Features:
;; - Semantic vector search over local documents
;; - Two-stage retrieval with reranking
;; - Automatic file indexing on save
;; - Line-level navigation to search results
;; - Transient menu interface
;;
;; Quick Start:
;;
;; 1. Start the server:
;;    M-x emacs-rag-menu, then 'a' (start server)
;;
;; 2. Index files:
;;    M-x emacs-rag-menu, then 'b' (index buffer) or 'd' (index directory)
;;
;; 3. Search:
;;    M-x emacs-rag-search-vector
;;    or M-x emacs-rag-menu, then 'v' (vector search)
;;
;; Configuration:
;;
;; See the customization group 'emacs-rag' for all options.
;; Key variables:
;; - `emacs-rag-db-path': Database location
;; - `emacs-rag-indexed-extensions': File extensions to index
;; - `emacs-rag-auto-index-on-save': Auto-reindex on save

;;; Code:

(require 'emacs-rag-server)
(require 'emacs-rag-index)
(require 'emacs-rag-search)
(require 'transient)

;;; Transient Menu

;;;###autoload (autoload 'emacs-rag-menu "emacs-rag" nil t)
(transient-define-prefix emacs-rag-menu ()
  "Emacs RAG main menu."
  [["Search"
    ("v" "Vector search" emacs-rag-search-vector)
    ("t" "Text search" emacs-rag-search-text)
    ("y" "Hybrid search" emacs-rag-search-hybrid)
    ("h" "Jump to org heading" emacs-rag-jump-to-org-heading)
    ("F" "Open indexed file" emacs-rag-open-indexed-file)]
   ["Server"
    ("a" "Start server" emacs-rag-start-server)
    ("p" "Stop server" emacs-rag-stop-server)
    ("r" "Restart server" emacs-rag-restart-server)
    ("S" "Show statistics" emacs-rag-stats)
    ("l" "Show server log" emacs-rag-show-server-buffer)]
   ["Index"
    ("b" "Index buffer" emacs-rag-index-buffer)
    ("f" "Index file" emacs-rag-index-file)
    ("d" "Index directory" emacs-rag-index-directory)
    ("o" "Reindex open buffers" emacs-rag-reindex-all-open-buffers)]]
  [["Delete"
    ("x" "Delete buffer from index" emacs-rag-delete-buffer)
    ("X" "Delete file from index" emacs-rag-delete-file)
    ("R" "Remove database" emacs-rag-delete-database)]
   ["Maintenance"
    ("M" "Rebuild FTS index" emacs-rag-rebuild-fts-index)
    ("B" "Rebuild database" emacs-rag-rebuild-database)]
   ["Debug"
    ("D" "Debug info" emacs-rag-debug)]])

;;; Debug Information

(defun emacs-rag-debug ()
  "Display diagnostic information in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*emacs-rag-debug*")
    (let ((inhibit-read-only t))
      (erase-buffer)
    (insert "# Emacs RAG Debug Information\n\n")

    ;; Emacs Configuration
    (insert "## Emacs Configuration\n\n")
    (insert (format "- Server Host: %s\n" emacs-rag-server-host))
    (insert (format "- Server Port: %d\n" emacs-rag-server-port))
    (insert (format "- Server URL: %s\n" (emacs-rag--server-url)))
    (insert (format "- DB Path: %s\n" emacs-rag-db-path))
    (insert (format "- Server Status: %s\n" (emacs-rag-server-status)))
    (insert (format "- Server Command: %s\n" (mapconcat 'identity emacs-rag-server-command " ")))
    (insert (format "- Working Directory: %s\n" (emacs-rag--get-server-directory)))
    (insert "\n")

    ;; Search Configuration
    (insert "## Search Configuration\n\n")
    (insert (format "- Search Limit: %d\n" emacs-rag-search-limit))
    (insert (format "- Reranking Enabled: %s\n" emacs-rag-search-enable-rerank))
    (insert "\n")

    ;; Indexing Configuration
    (insert "## Indexing Configuration\n\n")
    (insert (format "- Indexed Extensions: %s\n"
                   (mapconcat 'identity emacs-rag-indexed-extensions ", ")))
    (insert (format "- Auto-index on Save: %s\n" emacs-rag-auto-index-on-save))
    (insert "\n")

    ;; Environment Variables
    (insert "## Environment Variables\n\n")
    (dolist (var '("EMACS_RAG_DB_PATH" "EMACS_RAG_HOST" "EMACS_RAG_PORT"
                   "EMACS_RAG_CHUNK_SIZE" "EMACS_RAG_CHUNK_OVERLAP"
                   "EMACS_RAG_EMBEDDING_MODEL" "EMACS_RAG_RERANK_ENABLED"))
      (insert (format "- %s: %s\n" var (or (getenv var) "not set"))))
    (insert "\n")

    ;; Database Info
    (insert "## Database Information\n\n")
    (let ((db-path (expand-file-name emacs-rag-db-path)))
      (insert (format "- DB Path: %s\n" db-path))
      (insert (format "- DB Exists: %s\n"
                     (if (file-exists-p db-path) "yes" "no")))
      (when (file-exists-p db-path)
        (insert (format "- DB Size: %s\n"
                       (file-size-human-readable
                        (file-attribute-size (file-attributes db-path)))))))
    (insert "\n")

    ;; Server Health Check
    (insert "## Server Health\n\n")
    (condition-case err
        (let ((response (emacs-rag--request "GET" "/health")))
          (insert (format "- Status: %s\n" (alist-get 'status response))))
      (error
       (insert (format "- Error: %s\n" (error-message-string err)))))
    (insert "\n")

    ;; Statistics
    (insert "## Index Statistics\n\n")
    (condition-case err
        (let ((stats (emacs-rag--request "GET" "/stats")))
          (insert (format "- Total Chunks: %s\n" (alist-get 'total_chunks stats)))
          (insert (format "- Total Files: %s\n" (alist-get 'total_unique_files stats))))
      (error
       (insert (format "- Error: %s\n" (error-message-string err))))))

    (goto-char (point-min))
    (org-mode)
    (view-mode 1)
    (pop-to-buffer (current-buffer))))

;;; Version Information

(defun emacs-rag-version ()
  "Display version information."
  (interactive)
  (message "Emacs RAG version 0.1.0"))

;;; Reload Helper

(defun emacs-rag-reload ()
  "Reload all emacs-rag modules.
Useful during development to pick up changes."
  (interactive)
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; Unload features
    (unload-feature 'emacs-rag t)
    (unload-feature 'emacs-rag-search t)
    (unload-feature 'emacs-rag-index t)
    (unload-feature 'emacs-rag-server t)

    ;; Reload in correct order
    (load-file (expand-file-name "emacs-rag-server.el" dir))
    (load-file (expand-file-name "emacs-rag-index.el" dir))
    (load-file (expand-file-name "emacs-rag-search.el" dir))
    (load-file (expand-file-name "emacs-rag.el" dir))

    (message "Emacs RAG modules reloaded")))

;;; Convenience Functions

;;;###autoload
(defun emacs-rag-quick-start ()
  "Quick start guide for Emacs RAG."
  (interactive)
  (with-current-buffer (get-buffer-create "*emacs-rag-quick-start*")
    (erase-buffer)
    (insert "# Emacs RAG Quick Start Guide\n\n")
    (insert "## 1. Start the Server\n\n")
    (insert "   M-x emacs-rag-start-server\n")
    (insert "   or: M-x emacs-rag-menu, then 'a'\n\n")
    (insert "## 2. Index Some Files\n\n")
    (insert "   Index current buffer:\n")
    (insert "     M-x emacs-rag-index-buffer\n\n")
    (insert "   Index a directory:\n")
    (insert "     M-x emacs-rag-index-directory\n\n")
    (insert "## 3. Search\n\n")
    (insert "   M-x emacs-rag-search-vector\n")
    (insert "   Enter your search query and browse results\n\n")
    (insert "## 4. Configuration\n\n")
    (insert "   M-x customize-group RET emacs-rag RET\n\n")
    (insert "   Key variables:\n")
    (insert "   - emacs-rag-db-path: Database location\n")
    (insert "   - emacs-rag-indexed-extensions: File types to index\n")
    (insert "   - emacs-rag-auto-index-on-save: Auto-reindex on save\n\n")
    (insert "## 5. Main Menu\n\n")
    (insert "   M-x emacs-rag-menu\n")
    (insert "   Access all commands from the transient menu\n\n")
    (insert "Press 'q' to close this buffer\n")
    (goto-char (point-min))
    (org-mode)
    (view-mode 1)
    (pop-to-buffer (current-buffer))))

(provide 'emacs-rag)
;;; emacs-rag.el ends here
