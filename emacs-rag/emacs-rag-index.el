;;; emacs-rag-index.el --- Indexing commands for Emacs RAG -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module provides commands for indexing files and directories
;; into the RAG database.

;;; Code:

(require 'emacs-rag-server)

;;; Customization

(defcustom emacs-rag-indexed-extensions '("org")
  "List of file extensions to index (without the dot)."
  :type '(repeat string)
  :group 'emacs-rag)

(defcustom emacs-rag-auto-index-on-save t
  "Automatically reindex buffer on save if it's an eligible file."
  :type 'boolean
  :group 'emacs-rag)

(defcustom emacs-rag-async-index-delay 0.1
  "Delay in seconds between indexing files in async operations."
  :type 'number
  :group 'emacs-rag)

;;; Helper Functions

(defun emacs-rag--eligible-file-p (file)
  "Return non-nil if FILE should be indexed based on extension."
  (when file
    (let ((ext (file-name-extension file)))
      (and ext (member ext emacs-rag-indexed-extensions)))))

;;; Indexing Commands

(defun emacs-rag-index-file (file &optional metadata)
  "Index FILE with optional METADATA.
Returns the server response."
  (interactive "fIndex file: ")
  (let* ((absolute-path (expand-file-name file))
         (payload `((path . ,absolute-path)
                   ,@(when metadata `((metadata . ,metadata)))))
         (response (emacs-rag--request "POST" "/index" payload)))
    (when (called-interactively-p 'interactive)
      (if response
          (message "Indexed %s: %d chunks"
                   (alist-get 'path response)
                   (alist-get 'chunks_indexed response))
        (message "Failed to index file: %s" file)))
    response))

(defun emacs-rag-index-buffer ()
  "Index the current buffer using its content.
This is useful for indexing unsaved changes."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (let* ((path (buffer-file-name))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (payload `((path . ,path)
                   (content . ,content)))
         (response (emacs-rag--request "POST" "/index" payload)))
    (when response
      (message "Indexed buffer: %d chunks" (alist-get 'chunks_indexed response)))
    response))

(defun emacs-rag-index-directory (directory)
  "Recursively index all eligible files in DIRECTORY.
Only files with extensions in `emacs-rag-indexed-extensions' are indexed."
  (interactive "DIndex directory: ")
  (let* ((pattern (concat "\\.\\("
                         (mapconcat 'identity emacs-rag-indexed-extensions "\\|")
                         "\\)\\'"))
         (files (directory-files-recursively
                (expand-file-name directory)
                pattern))
         (total (length files)))
    (if (zerop total)
        (message "No files found in %s matching extensions: %s"
                 directory
                 (mapconcat 'identity emacs-rag-indexed-extensions ", "))
      (message "Starting async indexing of %d files..." total)
      (emacs-rag--index-files-async files 0 total))))

(defun emacs-rag--index-files-async (files indexed total)
  "Async indexing helper.
FILES is the list of remaining files to index.
INDEXED is the count of successfully indexed files.
TOTAL is the total number of files."
  (if (null files)
      (message "Indexing complete: %d/%d files indexed" indexed total)
    (let ((file (car files)))
      (condition-case err
          (progn
            (emacs-rag-index-file file)
            (message "Indexed [%d/%d]: %s" (1+ indexed) total file)
            (run-with-timer emacs-rag-async-index-delay nil
                           #'emacs-rag--index-files-async
                           (cdr files) (1+ indexed) total))
        (error
         (message "Failed [%d/%d]: %s - %s"
                  (1+ indexed) total file (error-message-string err))
         (run-with-timer emacs-rag-async-index-delay nil
                        #'emacs-rag--index-files-async
                        (cdr files) indexed total))))))

(defun emacs-rag-delete-file (file)
  "Remove FILE from the index."
  (interactive "fDelete file from index: ")
  (let* ((absolute-path (expand-file-name file))
         (response (emacs-rag--request "DELETE" "/files" nil
                                      `((path . ,absolute-path)))))
    (when (called-interactively-p 'interactive)
      (if response
          (message "Deleted from index: %s" (alist-get 'path response))
        (message "Failed to delete file: %s" file)))
    response))

(defun emacs-rag-delete-buffer ()
  "Remove the current buffer's file from the index."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (emacs-rag-delete-file (buffer-file-name)))

;;; Auto-indexing on Save

(defun emacs-rag--maybe-index-on-save ()
  "Hook function to reindex buffer after saving.
Only indexes if auto-indexing is enabled and file is eligible."
  (when (and emacs-rag-auto-index-on-save
             (buffer-file-name)
             (emacs-rag--eligible-file-p (buffer-file-name)))
    (condition-case err
        (emacs-rag-index-buffer)
      (error
       (message "Auto-indexing failed: %s" (error-message-string err))))))

(add-hook 'after-save-hook #'emacs-rag--maybe-index-on-save)

;;; Batch Operations

(defun emacs-rag-reindex-all-open-buffers ()
  "Reindex all open buffers that are eligible."
  (interactive)
  (let ((indexed 0)
        (failed 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (buffer-file-name)
                   (emacs-rag--eligible-file-p (buffer-file-name)))
          (condition-case err
              (progn
                (emacs-rag-index-buffer)
                (setq indexed (1+ indexed)))
            (error
             (setq failed (1+ failed))
             (message "Failed to index %s: %s"
                     (buffer-file-name)
                     (error-message-string err)))))))
    (message "Reindexed %d buffers (%d failed)" indexed failed)))

(provide 'emacs-rag-index)
;;; emacs-rag-index.el ends here
