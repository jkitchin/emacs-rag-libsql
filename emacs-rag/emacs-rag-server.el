;;; emacs-rag-server.el --- Server lifecycle management for Emacs RAG -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This module manages the lifecycle of the Python FastAPI server process.
;; It provides functions to start, stop, and check the status of the server.

;;; Code:

(require 'json)
(require 'url)

;;; Customization

(defgroup emacs-rag nil
  "Emacs RAG integration."
  :group 'tools
  :prefix "emacs-rag-")

(defcustom emacs-rag-server-host "127.0.0.1"
  "Hostname for the RAG server."
  :type 'string
  :group 'emacs-rag)

(defcustom emacs-rag-server-port 8765
  "Port number for the RAG server."
  :type 'integer
  :group 'emacs-rag)

(defcustom emacs-rag-server-command '("uv" "run" "emacs-rag-server" "serve")
  "Command to start the RAG server.
This should be a list of strings (program and arguments)."
  :type '(repeat string)
  :group 'emacs-rag)

(defcustom emacs-rag-server-working-directory nil
  "Working directory for the server process.
If nil, auto-detect based on the emacs-rag-server location."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Directory"))
  :group 'emacs-rag)

(defcustom emacs-rag-db-path "~/.emacs-rag/libsql"
  "Path to the RAG database directory."
  :type 'string
  :group 'emacs-rag)

(defcustom emacs-rag-http-timeout 10
  "HTTP request timeout in seconds."
  :type 'integer
  :group 'emacs-rag)

;;; Variables

(defvar emacs-rag-server-process nil
  "Process object for the running RAG server.")

;;; Helper Functions

(defun emacs-rag--get-server-directory ()
  "Get the working directory for the server process."
  (or emacs-rag-server-working-directory
      (let ((server-dir (locate-file "emacs-rag-server" exec-path)))
        (if server-dir
            (file-name-directory server-dir)
          default-directory))))

(defun emacs-rag--server-url ()
  "Return the base URL for the server."
  (format "http://%s:%d" emacs-rag-server-host emacs-rag-server-port))

(defun emacs-rag--server-sentinel (process event)
  "Sentinel function for the server PROCESS.
EVENT describes the process state change."
  (when (memq (process-status process) '(exit signal))
    (message "Emacs RAG server stopped: %s" (string-trim event))))

;;; Server Management

(defun emacs-rag-start-server ()
  "Start the RAG server process if not already running."
  (interactive)
  (if (emacs-rag-server-running-p)
      (message "Emacs RAG server is already running")
    (let* ((default-directory (emacs-rag--get-server-directory))
           (process-environment
            (cons (format "EMACS_RAG_DB_PATH=%s"
                         (expand-file-name emacs-rag-db-path))
                  (cons (format "EMACS_RAG_HOST=%s" emacs-rag-server-host)
                        (cons (format "EMACS_RAG_PORT=%d" emacs-rag-server-port)
                              process-environment)))))
      (setq emacs-rag-server-process
            (make-process
             :name "emacs-rag-server"
             :buffer "*emacs-rag-server*"
             :command emacs-rag-server-command
             :noquery t
             :sentinel #'emacs-rag--server-sentinel))
      (message "Starting Emacs RAG server on %s:%d..."
               emacs-rag-server-host emacs-rag-server-port)
      ;; Give the server a moment to start
      (run-with-timer 2 nil
                      (lambda ()
                        (if (emacs-rag-server-running-p)
                            (message "Emacs RAG server started successfully")
                          (message "Warning: Server may not have started properly")))))))

(defun emacs-rag-stop-server ()
  "Stop the running RAG server."
  (interactive)
  (if (not (emacs-rag-server-running-p))
      (message "Emacs RAG server is not running")
    (delete-process emacs-rag-server-process)
    (setq emacs-rag-server-process nil)
    (message "Emacs RAG server stopped")))

(defun emacs-rag-restart-server ()
  "Restart the RAG server."
  (interactive)
  (when (emacs-rag-server-running-p)
    (emacs-rag-stop-server)
    (sleep-for 1))
  (emacs-rag-start-server))

(defun emacs-rag-server-running-p ()
  "Check if the server process is alive."
  (and emacs-rag-server-process
       (process-live-p emacs-rag-server-process)))

(defun emacs-rag-ensure-server ()
  "Ensure the server is running, starting it if necessary."
  (unless (emacs-rag-server-running-p)
    (emacs-rag-start-server)
    ;; Wait a bit for server to start
    (sleep-for 2)))

(defun emacs-rag-server-status ()
  "Return a human-readable string describing the server status."
  (if (emacs-rag-server-running-p)
      (format "Running (PID: %d)" (process-id emacs-rag-server-process))
    "Stopped"))

(defun emacs-rag-show-server-buffer ()
  "Display the server process buffer."
  (interactive)
  (if (get-buffer "*emacs-rag-server*")
      (pop-to-buffer "*emacs-rag-server*")
    (message "Server buffer not found. Has the server been started?")))

;;; HTTP Request Helper

(defun emacs-rag--request (method endpoint &optional payload query-params)
  "Make HTTP request to the RAG server.

METHOD is the HTTP method (GET, POST, DELETE).
ENDPOINT is the API endpoint path (e.g., /index).
PAYLOAD is an alist to encode as JSON body (for POST).
QUERY-PARAMS is an alist of query parameters.

Returns the parsed JSON response as an alist."
  (emacs-rag-ensure-server)
  (let* ((url-request-method method)
         (url-request-extra-headers
          (when payload
            '(("Content-Type" . "application/json"))))
         (url-request-data
          (when payload
            (encode-coding-string (json-encode payload) 'utf-8)))
         (query-string
          (if query-params
              (concat "?"
                      (mapconcat
                       (lambda (param)
                         (format "%s=%s"
                                 (url-hexify-string (format "%s" (car param)))
                                 (url-hexify-string (format "%s" (cdr param)))))
                       query-params
                       "&"))
            ""))
         (url (concat (emacs-rag--server-url) endpoint query-string)))
    (with-current-buffer (url-retrieve-synchronously url nil nil emacs-rag-http-timeout)
      (goto-char (point-min))
      ;; Check for HTTP errors
      (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
        (let ((status-code (string-to-number (match-string 1))))
          (unless (and (>= status-code 200) (< status-code 300))
            (goto-char (point-min))
            (re-search-forward "^$" nil t)
            (let ((error-body (buffer-substring-no-properties (point) (point-max))))
              (error "Request failed (%d): %s" status-code error-body)))))
      ;; Parse JSON response
      (goto-char (point-min))
      (re-search-forward "^$")
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol)
            (json-start (point)))
        (condition-case err
            (progn
              (skip-chars-forward " \t\n\r")
              (if (eobp)
                  (error "Empty response from server")
                (json-read)))
          (error
           (goto-char (point-min))
           (message "Error parsing JSON response: %s" err)
           (message "Full response:\n%s" (buffer-substring-no-properties (point-min) (point-max)))
           (error "Failed to parse server response: %s" err)))))))

(provide 'emacs-rag-server)
;;; emacs-rag-server.el ends here
