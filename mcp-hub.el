;;; mcp-hub.el --- manager mcp server                -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'mcp)

(defvar mcp-hub--project-server-table (make-hash-table)
  "Configuration for MCP servers.
Each server configuration is a list of the form
 (NAME . (:command COMMAND :args ARGS)) or (NAME . (:url URL)), where:
- NAME is a string identifying the server.
- COMMAND is the command to start the server.
- ARGS is a list of arguments passed to the command.
- URL is a string arguments to connect sse mcp server.")

(defun mcp-hub-register-servers-for (servers &optional project)
  "Insert SERVERS into `mcp-hub--project-server-table' keyed by PROJECT.

You can set a default server configuration 
(mcp server definitions for buffers not tied to a project)
by omitting PROJECT."
  (puthash project servers mcp-hub--project-server-table))


(defun mcp-hub--start-server (server &optional inited-callback project)
  "Start an MCP server with the given configuration.
SERVER should be a cons cell of the form (NAME . CONFIG) where:
- NAME is a string identifying the server
- CONFIG is a plist containing either:
  - :command and :args for local servers
  - :url for remote servers

Optional argument INITED-CALLBACK is a function called when the server
has successfully initialized and tools are available. The callback
receives no arguments."
  (apply #'mcp-connect-server
         (append (list (car server))
                 (cdr server)
                 (list :initial-callback
                       (lambda (_)
                         (mcp-hub-update))
                       :tools-callback
                       (lambda (_ _)
                         (mcp-hub-update)
                         (when inited-callback
                           (funcall inited-callback)))
                       :prompts-callback
                       (lambda (_ _)
                         (mcp-hub-update))
                       :resources-callback
                       (lambda (_ _)
                         (mcp-hub-update))
                       :resources-templates-callback
                       (lambda (_ _)
                         (mcp-hub-update))
                       :error-callback
                       (lambda (_ _)
                         (mcp-hub-update))
		       :project project))))

;;;###autoload
(cl-defun mcp-hub-get-all-tool (&key asyncp categoryp project)
  "Retrieve all available tools from connected MCP servers.
This function collects all tools from currently connected MCP servers,
filtering out any invalid entries. Each tool is created as a text tool
that can be used for interaction.

When ASYNCP is non-nil, the tools will be created asynchronously.

When CATEGORYP is non-nil, the tools will be add to a category.

Returns a list of text tools created from all valid tools across all
connected servers. The list excludes any tools that couldn't be created
due to missing or invalid names.

Example:
  (mcp-hub-get-all-tool)  ; Get all tools synchronously
  (mcp-hub-get-all-tool t)  ; Get all tools asynchronously"
  (let ((res))
    (maphash (lambda (name server)
               (when (and server
                          (equal (mcp--status server)
                                 'connected))
                 (when-let* ((tools (mcp--tools server))
                             (tool-names (mapcar (lambda (tool) (plist-get tool :name)) tools)))
                   (dolist (tool-name tool-names)
                     (push (let ((tool (mcp-make-text-tool name tool-name asyncp)))
                             (if categoryp
                                 (plist-put
                                  tool
                                  :category
                                  (format "mcp-%s"
                                          name))
                               tool))
                           res)))))
	     (mcp-server-connections project))
    (nreverse res)))

;;;###autoload
(defun mcp-hub-start-all-server (&optional project callback servers)
  "Start all configured MCP servers.
This function will attempt to start each server listed in `mcp-hub--project-server-table'
if it's not already running.

Optional argument CALLBACK is a function to be called when all servers have
either started successfully or failed to start.The callback receives no
arguments.

Optional argument SERVERS is a list of server names (strings) to filter which
servers should be started. When nil, all configured servers are considered."
  (interactive (list (project-current) nil nil))

  (let* ((servers-to-start (cl-remove-if (lambda (server)
                                           (or (and servers
                                                    (not (cl-find (car server) servers :test #'string=)))
                                               (mcp--server-running-p (car server) project)))
                                         (gethash project mcp-hub--project-server-table)))
         (total (length servers-to-start))
         (started 0))
    (if (zerop total)
        (progn
          (message "All MCP servers already running")
          (when callback (funcall callback)))
      (message "Starting %d MCP server(s)..." total)
      (dolist (server servers-to-start)
        (condition-case err
            (mcp-hub--start-server
             server
             (lambda ()
               (cl-incf started)
               (message "Started server %s (%d/%d)" (car server) started total)
               (when (and callback (>= started total))
                 (funcall callback)))
	     project)
          (error
           (message "Failed to start server %s: %s" (car server) err)
           (cl-incf started)
           (when (and callback (>= started total))
             (funcall callback))))))))

;;;###autoload
(defun mcp-hub-close-all-server (&optional project)
  "Stop all running MCP servers.
This function will attempt to stop each server listed in `mcp-hub--project-server-table'
that is currently running."
  (interactive (list (project-current)))
  
  (dolist (server (gethash project mcp-hub--project-server-table))
    (when (gethash (car server)
		   (mcp-server-connections project))
      (mcp-stop-server (car server) project)))
  (mcp-hub-update nil nil project))

;;;###autoload
(defun mcp-hub-restart-all-server ()
  "Restart all configured MCP servers.
This function first stops all running servers, then starts them again.
It's useful for applying configuration changes or recovering from errors."
  (interactive)
  (mcp-hub-close-all-server)
  (mcp-hub-start-all-server))

(defun mcp-hub-get-servers (&optional project)
  "Retrieve status information for all configured servers.
Returns a list of server statuses, where each status is a plist containing:
- :name - The server's name
- :status - Either `connected' or `stop'
- :tools - Available tools (if connected)
- :resources - Available resources (if connected)
- :template-resources - Available template resources (if connected)
- :prompts - Available prompts (if connected)"
  (mapcar (lambda (server)
            (let ((name (car server)))
              (if-let* ((connection (gethash name (mcp-server-connections project))))
                  (list :name name
                        :type (mcp--connection-type connection)
                        :status (mcp--status connection)
                        :tools (mcp--tools connection)
                        :resources (mcp--resources connection)
                        :template-resources (mcp--template-resources connection)
                        :prompts (mcp--prompts connection))
                (list :name name :status 'stop))))
          (gethash (or project (project-current)) mcp-hub--project-server-table)))

(defun mcp-hub-update (&optional ignore-auto noconfirm project)
  "Update the MCP Hub display with current server status.
If called interactively, ARG is the prefix argument.
When SILENT is non-nil, suppress any status messages.
This function refreshes the *Mcp-Hub* buffer with the latest server information,
including connection status, available tools, resources, template resources and
prompts."
  (interactive)
  (ignore ignore-auto noconfirm) ; unused variables
  (let ((project (or project (project-current))))
    (when-let* ((server-list (mcp-hub-get-servers project))
		(server-show (mapcar (lambda (server)
                                       (let* ((name (plist-get server :name))
                                              (status (plist-get server :status)))
					 (append (list name
                                                       (symbol-name (plist-get server :type))
                                                       (pcase status
							 ('connected
                                                          (propertize (symbol-name status)
                                                                      'face 'success))
							 ('error
                                                          (propertize (symbol-name status)
                                                                      'face 'error))
							 (_
                                                          (symbol-name status))))
						 (if (equal status 'connected)
                                                     (mapcar (lambda (x)
                                                               (format "%d"
                                                                       (length x)))
                                                             (list (plist-get server :tools)
                                                                   (plist-get server :resources)
                                                                   (plist-get server :template-resources)
                                                                   (plist-get server :prompts)))
                                                   (list "nil" "nil" "nil" "nil")))))
                                     server-list)))
      (with-current-buffer (get-buffer-create (mcp-hub--buffer-name project))
	(setq tabulated-list-entries
              (cl-mapcar (lambda (statu index)
                           (list (format "%d" index)
				 (vconcat statu)))
			 server-show
			 (number-sequence 1 (length server-list))))
	(tabulated-list-print t)))))

(defun mcp-hub--buffer-name (&optional project)
  (if project
      (format "*Mcp-Hub %s*" (project-name project))
    "*Mcp-Hub*"))

;;;###autoload
(defun mcp-hub (&optional start project)
  "View mcp hub server.
Start all servers if START is non-nil or if called interactively with a prefix
argument."
  (interactive (list current-prefix-arg (project-current)))
  
  ;; start all server
  (when (and start
	     (gethash project mcp-hub--project-server-table)
             (= (hash-table-count (mcp-server-connections project))
                0))
    (mcp-hub-start-all-server))
  ;; show buffer
  (pop-to-buffer (mcp-hub--buffer-name project))
  (mcp-hub-mode))

;;;###autoload
(defun mcp-hub-start-server (&optional project)
  "Start the currently selected MCP server.
This function starts the server that is currently highlighted in the *Mcp-Hub*
buffer. It sets up callbacks for connection status, tools, prompts, and
resources updates, and refreshes the hub view after starting the server."
  (interactive (list (project-current)))

  (when-let* ((server (tabulated-list-get-entry))
              (name (elt server 0))
              (server-arg (cl-find name (gethash (project-current) mcp-hub--project-server-table) :key #'car :test #'equal)))
    (mcp-hub--start-server server-arg nil project)
    (mcp-hub-update)))

;;;###autoload
(defun mcp-hub-close-server (&optional project)
  "Stop the currently selected MCP server.
This function stops the server that is currently highlighted in the *Mcp-Hub*
buffer and updates the hub view to reflect the change in status."
  (interactive (list (project-current)))
  
  (when-let* ((server (tabulated-list-get-entry))
              (name (elt server 0)))
    (mcp-stop-server name project)
    (mcp-hub-update nil nil project)))

;;;###autoload
(defun mcp-hub-restart-server ()
  "Restart the currently selected MCP server.
This function stops and then starts the server that is currently highlighted
in the *Mcp-Hub* buffer. It's useful for applying configuration changes or
recovering from errors."
  (interactive)
  (mcp-hub-close-server)
  (mcp-hub-start-server))

;;;###autoload
(defun mcp-hub-view-log ()
  "View the event log for the currently selected MCP server.
This function opens a buffer showing the event log for the server that is
currently highlighted in the *Mcp-Hub* buffer."
  (interactive)
  (when-let* ((server (tabulated-list-get-entry))
              (name (elt server 0)))
    (switch-to-buffer (format "*%s events*"
                              name))))

(define-derived-mode mcp-hub-mode tabulated-list-mode "Mcp Hub"
  "A major mode for viewing a list of mcp server."
  (setq-local revert-buffer-function #'mcp-hub-update)
  (setq tabulated-list-format
        [("Name" 18 t)
         ("Type" 10 t)
         ("Status" 15 t)
         ("Tools" 10 t)
         ("Resources" 10 t)
         ("Template" 10 t)
         ("Prompts" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header)

  (keymap-set mcp-hub-mode-map "l" #'mcp-hub-view-log)
  (keymap-set mcp-hub-mode-map "s" #'mcp-hub-start-server)
  (keymap-set mcp-hub-mode-map "k" #'mcp-hub-close-server)
  (keymap-set mcp-hub-mode-map "r" #'mcp-hub-restart-server)
  (keymap-set mcp-hub-mode-map "S" #'mcp-hub-start-all-server)
  (keymap-set mcp-hub-mode-map "R" #'mcp-hub-restart-all-server)
  (keymap-set mcp-hub-mode-map "K" #'mcp-hub-close-all-server)

  (mcp-hub-update))

(provide 'mcp-hub)
;;; mcp-hub.el ends here
