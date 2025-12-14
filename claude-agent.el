;;; claude-agent.el --- Claude interaction buffer -*- lexical-binding: t; -*-

;; This file is part of Claude.
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; This module provides a single-buffer interface for interacting with Claude.
;; The buffer is organized into distinct sections:
;;
;; 1. Header Section: Session name (read-only)
;; 2. Log Section: Conversation history (read-only, append-only)
;; 3. Status Section: Model/cost/session info + thinking indicator (read-only)
;; 4. Input Section: Header line (read-only) + editable typing area
;;
;; The Python agent outputs structured markers which are parsed and formatted.

;;; Code:

(require 'ansi-color)
(require 'org)

;;;; Customization

(defgroup claude-agent nil
  "Claude interaction buffer."
  :group 'Claude)

(defcustom claude-agent-python-command "uv"
  "Command to run Python for the agent wrapper."
  :type 'string
  :group 'claude-agent)

;;;; Faces

(defface claude-agent-header-face
  '((t :foreground "#56b6c2" :slant italic))
  "Face for the header section."
  :group 'claude-agent)

(defface claude-agent-user-header-face
  '((t :foreground "#61afef" :weight bold))
  "Face for user message headers."
  :group 'claude-agent)

(defface claude-agent-user-face
  '((t :foreground "#c8ccd4"))  ; Slightly off-white (lighter than default)
  "Face for user message text."
  :group 'claude-agent)

(defface claude-agent-assistant-header-face
  '((t :foreground "#c678dd" :weight bold))
  "Face for assistant message headers."
  :group 'claude-agent)

(defface claude-agent-assistant-face
  '((t :foreground "#e5e5e5"))
  "Face for assistant message text."
  :group 'claude-agent)

(defface claude-agent-tool-face
  '((t :foreground "#e5c07b" :slant italic))
  "Face for tool call indicators."
  :group 'claude-agent)

(defface claude-agent-status-face
  '((t :foreground "#56b6c2" :slant italic))
  "Face for status info section (model, cost, session)."
  :group 'claude-agent)

(defface claude-agent-thinking-face
  '((t :foreground "#98c379" :weight bold))
  "Face for thinking indicator."
  :group 'claude-agent)

(defface claude-agent-error-face
  '((t :foreground "#e06c75" :weight bold))
  "Face for error messages."
  :group 'claude-agent)

(defface claude-agent-session-face
  '((t :foreground "#56b6c2" :slant italic))
  "Face for session info messages."
  :group 'claude-agent)

(defface claude-agent-input-header-face
  '((t :foreground "#5c6370" :weight bold))
  "Face for the input area header."
  :group 'claude-agent)

(defface claude-agent-diff-removed
  '((t :inherit diff-refine-removed))
  "Face for removed lines in diff display."
  :group 'claude-agent)

(defface claude-agent-diff-added
  '((t :inherit diff-refine-added))
  "Face for added lines in diff display."
  :group 'claude-agent)

(defface claude-agent-diff-header
  '((t :foreground "#5c6370"))
  "Face for diff box drawing characters."
  :group 'claude-agent)

(defface claude-agent-file-link
  '((t :inherit link :underline t))
  "Face for clickable file paths."
  :group 'claude-agent)

(defface claude-agent-line-number
  '((t :foreground "#5c6370"))
  "Face for line numbers in file content display."
  :group 'claude-agent)


;;;; Buffer-local variables - Section markers
;;
;; Buffer has 3 zones with different update semantics:
;;
;;   [STATIC]  - Header + completed conversation turns
;;               Append-only, never modified after written
;;               Ends at `static-end-marker`
;;
;;   [DYNAMIC] - Current in-progress turn + status bar
;;               Fully deleted and rebuilt on each update
;;               Content stored in variables, rendered fresh each time
;;
;;   [INPUT]   - User typing area
;;               Preserved across dynamic rebuilds
;;               Starts at `input-start-marker` (set after each rebuild)

(defvar-local claude-agent--process nil
  "The agent process for this session.")

(defvar-local claude-agent--static-end-marker nil
  "Marker for end of static section (start of dynamic section).
Everything before this is completed content that never changes.")

(defvar-local claude-agent--input-start-marker nil
  "Marker for start of input section (where user types).
Set fresh after each dynamic section rebuild.")

;;;; Buffer-local variables - State

(defvar-local claude-agent--parse-state nil
  "Current parsing state: nil, user, assistant, tool, error, session.")

(defvar-local claude-agent--pending-output ""
  "Buffer for incomplete lines from process output.")

(defvar-local claude-agent--session-info nil
  "Plist with session info: :model :session-id :cost.")

(defvar-local claude-agent--mcp-server-status nil
  "List of MCP server status objects from the agent.
Each element is an alist with keys: name, status.")

(defvar-local claude-agent--input-history nil
  "History of inputs sent to Claude.")

(defvar-local claude-agent--input-history-index 0
  "Current position in input history.")

;;;; Buffer-local variables - Thinking status

(defconst claude-agent--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames for the spinner animation.")



(defvar-local claude-agent--spinner-index 0
  "Current index in spinner frames.")

(defvar-local claude-agent--spinner-timer nil
  "Timer for spinner animation.")

(defvar-local claude-agent--thinking-start-time nil
  "Time when thinking started, for elapsed time display.")

(defvar-local claude-agent--thinking-status nil
  "Current thinking status text, or nil if not thinking.")

(defvar-local claude-agent--input-tokens 0
  "Input token count for current turn.")

(defvar-local claude-agent--output-tokens 0
  "Output token count for current turn.")

(defvar-local claude-agent--has-conversation nil
  "Non-nil if conversation has started (first message sent).")

(defvar-local claude-agent--placeholder-overlay nil
  "Overlay for the placeholder text in empty input area.")

(defconst claude-agent--placeholder-text "Enter your message... (C-c C-c to send)"
  "Placeholder text shown when input area is empty.")

(defface claude-agent-placeholder-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for placeholder text in empty input area."
  :group 'claude-agent)

;;;; Buffer-local variables - Input mode

(defvar-local claude-agent--input-mode 'text
  "Current input mode: `text' for normal input, `permission' for permission prompt.")

(defvar-local claude-agent--saved-input ""
  "Saved input text when switching away from text mode.")

;;;; Buffer-local variables - Message queue

(defvar-local claude-agent--message-queue nil
  "List of messages queued while agent is busy. Each is a string.")


(defface claude-agent-queued-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for queued messages (grayed out)."
  :group 'claude-agent)

(defface claude-agent-queued-header-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for queued message headers."
  :group 'claude-agent)

;;;; Mode definition

(defvar claude-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-agent-send)
    (define-key map (kbd "C-<return>") #'claude-agent-send)
    (define-key map (kbd "C-c C-k") #'claude-agent-interrupt)
    (define-key map (kbd "C-c C-q") #'claude-agent-quit)
    (define-key map (kbd "M-p") #'claude-agent-previous-input)
    (define-key map (kbd "M-n") #'claude-agent-next-input)
    map)
  "Keymap for `claude-agent-mode'.")

(define-derived-mode claude-agent-mode fundamental-mode "Claude"
  "Major mode for Claude interaction buffer.
Uses org-mode fontification without org-mode keybindings."
  :group 'claude-agent
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local buffer-read-only nil)
  (visual-line-mode 1)
  ;; Set up org-mode fontification without org-mode keybindings
  ;; This calls org's internal function to populate font-lock-keywords
  (org-set-font-lock-defaults)
  (font-lock-mode 1)
  ;; Ensure our keybindings are set (defvar doesn't reinit on re-eval)
  (use-local-map claude-agent-mode-map)
  ;; Re-define keys to ensure they're set
  (local-set-key (kbd "C-c C-c") #'claude-agent-send)
  (local-set-key (kbd "C-<return>") #'claude-agent-send)
  (local-set-key (kbd "C-c C-k") #'claude-agent-interrupt)
  (local-set-key (kbd "C-c C-q") #'claude-agent-quit)
  (local-set-key (kbd "M-p") #'claude-agent-previous-input)
  (local-set-key (kbd "M-n") #'claude-agent-next-input)
  ;; Set up placeholder update hook
  (add-hook 'post-command-hook #'claude-agent--post-command-hook nil t)
  ;; Set up evil insert state entry hook to move to input area
  ;; Use after-change-major-mode-hook to ensure evil is loaded
  (add-hook 'evil-insert-state-entry-hook
            #'claude-agent--on-insert-state-entry nil t))

;;;; Helper functions

(defun claude-agent--in-input-area-p ()
  "Return t if point is in the input area."
  (and claude-agent--input-start-marker
       (>= (point) claude-agent--input-start-marker)))

(defmacro claude-agent--in-base-buffer (&rest body)
  "Execute BODY in the base buffer (for polymode compatibility)."
  `(let ((base (or (buffer-base-buffer) (current-buffer))))
     (with-current-buffer base
       ,@body)))

;;;; Placeholder management

(defun claude-agent--input-empty-p ()
  "Return t if the input area is empty (only whitespace)."
  (and claude-agent--input-start-marker
       ;; string-blank-p returns match position (0) for empty, so convert to t
       (not (null (string-blank-p (buffer-substring-no-properties
                                   claude-agent--input-start-marker (point-max)))))))

(defun claude-agent--update-placeholder ()
  "Show or hide placeholder based on input area content."
  (when (and claude-agent--input-start-marker
             (marker-position claude-agent--input-start-marker))
    (if (claude-agent--input-empty-p)
        ;; Show placeholder at current input-start position
        (let ((pos (marker-position claude-agent--input-start-marker)))
          ;; Move existing overlay or create new one
          (if (and claude-agent--placeholder-overlay
                   (overlay-buffer claude-agent--placeholder-overlay))
              ;; Move to new position
              (move-overlay claude-agent--placeholder-overlay pos pos)
            ;; Create new overlay
            (setq claude-agent--placeholder-overlay (make-overlay pos pos))
            (overlay-put claude-agent--placeholder-overlay 'before-string
                         (propertize claude-agent--placeholder-text
                                     'face 'claude-agent-placeholder-face
                                     'cursor t))
            (overlay-put claude-agent--placeholder-overlay 'evaporate nil)))
      ;; Hide placeholder
      (when (and claude-agent--placeholder-overlay
                 (overlay-buffer claude-agent--placeholder-overlay))
        (delete-overlay claude-agent--placeholder-overlay)
        (setq claude-agent--placeholder-overlay nil)))))

(defun claude-agent--in-insert-state-p ()
  "Return t if evil-mode is active and in insert state."
  (and (bound-and-true-p evil-local-mode)
       (eq evil-state 'insert)))

(defun claude-agent--on-insert-state-entry ()
  "Hook called when entering evil insert state.
Moves cursor to input area if currently outside it."
  (when (and claude-agent--input-start-marker
             (marker-position claude-agent--input-start-marker)
             (< (point) claude-agent--input-start-marker))
    (goto-char claude-agent--input-start-marker)))

(defun claude-agent--post-command-hook ()
  "Hook run after each command to update placeholder visibility.
Also constrains cursor to input area when in evil insert state."
  (claude-agent--update-placeholder)
  ;; In insert mode, keep cursor in input area
  (when (and claude-agent--input-start-marker
             (marker-position claude-agent--input-start-marker)
             (claude-agent--in-insert-state-p)
             (< (point) claude-agent--input-start-marker))
    (goto-char claude-agent--input-start-marker))
  ;; If input is empty and we're in the input area, keep cursor at prompt
  (when (and (claude-agent--input-empty-p)
             (claude-agent--in-input-area-p)
             claude-agent--input-start-marker)
    (goto-char claude-agent--input-start-marker)))

;;;; Section management
;;
;; Two-zone architecture:
;; - Static section: Append-only (header + conversation log)
;; - Dynamic section: Re-rendered from state (status bar + input area)

(defun claude-agent--init-buffer (session-name)
  "Initialize buffer with section structure for SESSION-NAME."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; === HEADER (part of static section) ===
    (let ((start (point)))
      (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
      (insert (format " Claude Session: %s\n" session-name))
      (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
      (claude-agent--apply-face start (point) 'claude-agent-header-face))

    ;; === STATIC END MARKER ===
    ;; Everything before this is committed content that never changes
    (setq claude-agent--static-end-marker (point-marker))
    (set-marker-insertion-type claude-agent--static-end-marker nil)

    ;; === INPUT START MARKER ===
    ;; Set initially at same position, will be updated by rebuild
    (setq claude-agent--input-start-marker (point-marker))
    (set-marker-insertion-type claude-agent--input-start-marker nil)

    ;; Show placeholder and position cursor
    (claude-agent--update-placeholder)
    (goto-char claude-agent--input-start-marker)

    ;; Make everything before input read-only
    (claude-agent--update-read-only)))

(defun claude-agent--apply-face (start end face)
  "Apply FACE to region from START to END using overlay."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'claude-agent-styled t))
  ;; Also apply to indirect buffers (polymode)
  (let ((base (current-buffer)))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (eq (buffer-base-buffer buf) base))
        (with-current-buffer buf
          (let ((ov (make-overlay start end)))
            (overlay-put ov 'face face)
            (overlay-put ov 'priority 100)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'claude-agent-styled t)))))))

(defun claude-agent--update-read-only ()
  "Update read-only text property to cover everything before prompt marker."
  ;; Use text properties for read-only (overlays don't enforce read-only)
  (when (and claude-agent--input-start-marker
             (> (marker-position claude-agent--input-start-marker) (point-min)))
    ;; Remove read-only from entire buffer first (property list needs property names only)
    (remove-list-of-text-properties (point-min) (point-max) '(read-only rear-nonsticky))
    ;; Apply read-only to everything before prompt, with rear-nonsticky
    ;; so text inserted at the boundary is NOT read-only
    (add-text-properties (point-min) claude-agent--input-start-marker
                         '(read-only t rear-nonsticky (read-only)))))

;;;; Dynamic section management
;;
;; Two-zone architecture:
;; - Static section: Append-only log content (header + conversation)
;; - Dynamic section: Re-rendered from state (status bar + input area)
;;
;; `append-to-static` appends to the static section directly.
;; `render-dynamic-section` clears and re-renders dynamic section from state.

(defun claude-agent--get-input-text ()
  "Get the current text in the input area."
  (if (and claude-agent--input-start-marker
           (marker-position claude-agent--input-start-marker))
      (buffer-substring-no-properties
       claude-agent--input-start-marker (point-max))
    ""))

(defun claude-agent--append-to-static (text)
  "Append TEXT to the static section and re-render dynamic section.
This is a convenience wrapper for `append-to-log' without styling."
  (claude-agent--append-to-log text nil nil))

(defun claude-agent--render-dynamic-section ()
  "Render the dynamic section (status bar + input area).
Clears everything after static-end-marker and re-renders from state.
Handles different input modes: text input vs permission prompt."
  (let* ((inhibit-read-only t)
         ;; Only save cursor offset if in text mode
         (cursor-offset (when (and (eq claude-agent--input-mode 'text)
                                   claude-agent--input-start-marker
                                   (marker-position claude-agent--input-start-marker)
                                   (>= (point) claude-agent--input-start-marker))
                          (- (point) claude-agent--input-start-marker)))
         ;; Only capture input if in text mode (permission mode uses saved-input)
         (input-to-restore (if (eq claude-agent--input-mode 'text)
                               (claude-agent--get-input-text)
                             "")))
    ;; Clear overlays in dynamic section
    (when (and claude-agent--static-end-marker
               (marker-position claude-agent--static-end-marker))
      (dolist (ov (overlays-in claude-agent--static-end-marker (point-max)))
        (when (or (overlay-get ov 'claude-agent-styled)
                  (overlay-get ov 'claude-permission-face))
          (delete-overlay ov)))
      ;; Delete everything from static-end to end of buffer
      (delete-region claude-agent--static-end-marker (point-max)))

    ;; Position at start of dynamic section
    (goto-char (or claude-agent--static-end-marker (point-max)))

    ;; === INSERT STATUS BAR ===
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))

    ;; === RENDER INPUT AREA BASED ON MODE ===
    (pcase claude-agent--input-mode
      ('text
       ;; Normal text input mode
       (setq claude-agent--input-start-marker (point-marker))
       (set-marker-insertion-type claude-agent--input-start-marker nil)
       (unless (string-empty-p input-to-restore)
         (insert input-to-restore))
       ;; Update read-only and placeholder
       (claude-agent--update-read-only)
       (claude-agent--update-placeholder)
       ;; Restore cursor position within input area
       (if (and cursor-offset (>= cursor-offset 0))
           (goto-char (min (+ claude-agent--input-start-marker cursor-offset)
                           (point-max)))
         (goto-char claude-agent--input-start-marker)))

      ('empty
       ;; Empty mode - clear input area and switch to text mode
       ;; This discards any saved input and starts fresh
       (setq claude-agent--saved-input "")
       (setq claude-agent--input-mode 'text)
       (setq claude-agent--input-start-marker (point-marker))
       (set-marker-insertion-type claude-agent--input-start-marker nil)
       ;; Update read-only and placeholder (will show placeholder since empty)
       (claude-agent--update-read-only)
       (claude-agent--update-placeholder)
       (goto-char claude-agent--input-start-marker))

      ('permission
       ;; Permission prompt mode - render the permission dialog
       (setq claude-agent--input-start-marker (point-marker))
       (claude-agent--render-permission-content)
       (claude-agent--update-read-only)))))

;;;; Status bar rendering

(defun claude-agent--format-elapsed-time (start-time)
  "Format elapsed time since START-TIME as Xm Ys."
  (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
         (minutes (floor (/ elapsed 60)))
         (seconds (floor (mod elapsed 60))))
    (if (> minutes 0)
        (format "%dm%ds" minutes seconds)
      (format "%ds" seconds))))

(defun claude-agent--insert-status-bar ()
  "Insert the status bar content at point.
Called by `render-dynamic-section'. Assumes point is positioned correctly."
  ;; === Thinking indicator (if active) ===
  (when claude-agent--thinking-status
    (let ((start (point))
          (spinner (nth claude-agent--spinner-index
                        claude-agent--spinner-frames))
          (elapsed (if claude-agent--thinking-start-time
                       (claude-agent--format-elapsed-time
                        claude-agent--thinking-start-time)
                     "0s"))
          (tokens (format "(+%d/-%d)"
                          claude-agent--input-tokens
                          claude-agent--output-tokens)))
      (insert (format "\n%s %s %s %s (C-c C-k to interrupt)\n"
                      spinner
                      claude-agent--thinking-status
                      elapsed
                      tokens))
      (claude-agent--apply-face start (point) 'claude-agent-thinking-face)))

  ;; === Queued messages (if any) ===
  (when claude-agent--message-queue
    (dolist (msg (reverse claude-agent--message-queue))
      (let ((msg-start (point)))
        (insert "\n┄┄┄ Queued ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄\n")
        (claude-agent--apply-face msg-start (point) 'claude-agent-queued-header-face)
        (setq msg-start (point))
        (let ((lines (split-string msg "\n")))
          (dolist (line lines)
            (insert "  " line "\n")))
        (claude-agent--apply-face msg-start (point) 'claude-agent-queued-face))))

  ;; === Status info line ===
  (let* ((model (or (plist-get claude-agent--session-info :model) "..."))
         (cost (or (plist-get claude-agent--session-info :cost) 0))
         (session-id (or (plist-get claude-agent--session-info :session-id) "..."))
         (status-text (format " Model: %s  |  Cost: $%.4f  |  Session: %s "
                              model cost
                              (if (> (length session-id) 8)
                                  (substring session-id 0 8)
                                session-id)))
         (bar-length (length status-text))
         (bar (make-string bar-length ?━))
         (start (point)))
    (insert "\n")
    (insert bar "\n")
    (insert status-text "\n")
    (insert bar "\n")
    (insert "\n")
    (claude-agent--apply-face start (point) 'claude-agent-header-face)))

(defun claude-agent--spinner-tick ()
  "Advance spinner and rebuild dynamic section."
  (when claude-agent--thinking-status
    (setq claude-agent--spinner-index
          (mod (1+ claude-agent--spinner-index)
               (length claude-agent--spinner-frames)))
    (claude-agent--render-dynamic-section)))

(defun claude-agent--set-thinking (status)
  "Set thinking STATUS, or clear if nil."
  ;; Cancel existing timer
  (when claude-agent--spinner-timer
    (cancel-timer claude-agent--spinner-timer)
    (setq claude-agent--spinner-timer nil))

  (setq claude-agent--thinking-status status)

  (if status
      (progn
        ;; Start timing if not already
        (unless claude-agent--thinking-start-time
          (setq claude-agent--thinking-start-time (current-time)))
        ;; Start spinner timer
        (setq claude-agent--spinner-timer
              (run-with-timer 0.1 0.1 #'claude-agent--spinner-tick)))
    ;; Clear timing when done
    (setq claude-agent--thinking-start-time nil))

  ;; Rebuild dynamic section (handles cursor positioning)
  (claude-agent--render-dynamic-section))

;;;; Content helpers

(defun claude-agent--insert-diff (file-path old-string new-string)
  "Insert a diff display for FILE-PATH with OLD-STRING and NEW-STRING.
Inserts directly at point with proper faces and clickable link."
  (let ((inhibit-read-only t))
    ;; Header with clickable file link
    (let ((header-start (point)))
      (insert "\n┌─ Edit: ")
      (claude-agent--apply-face header-start (point) 'claude-agent-diff-header))
    ;; File path as button
    (insert-text-button file-path
                        'action (lambda (_btn)
                                  (find-file-other-window
                                   (button-get _btn 'file-path)))
                        'file-path file-path
                        'face 'claude-agent-file-link
                        'help-echo "Click to open file"
                        'follow-link t)
    (let ((nl-start (point)))
      (insert "\n")
      (claude-agent--apply-face nl-start (point) 'claude-agent-diff-header))
    ;; Old lines (removed)
    (when (and old-string (not (string-empty-p old-string)))
      (dolist (line (split-string old-string "\n"))
        (let ((line-start (point)))
          (insert "│- " line "\n")
          (claude-agent--apply-face line-start (point) 'claude-agent-diff-removed))))
    ;; New lines (added)
    (when (and new-string (not (string-empty-p new-string)))
      (dolist (line (split-string new-string "\n"))
        (let ((line-start (point)))
          (insert "│+ " line "\n")
          (claude-agent--apply-face line-start (point) 'claude-agent-diff-added))))
    ;; Footer
    (let ((footer-start (point)))
      (insert "└─\n")
      (claude-agent--apply-face footer-start (point) 'claude-agent-diff-header))))

(defun claude-agent--format-tool-call (tool-name args-string)
  "Format a tool call for display with TOOL-NAME and ARGS-STRING.
Returns formatted string like: ⚙ ToolName(args)
with 1-space indent applied via virtual indentation."
  (format "\n ⚙ %s(%s)\n" tool-name args-string))

(defun claude-agent--insert-tool-call (tool-name args-string)
  "Insert a tool call display for TOOL-NAME with ARGS-STRING.
Uses consistent formatting with 1-space virtual indent and tool face."
  (claude-agent--append-to-log
   (claude-agent--format-tool-call tool-name args-string)
   'claude-agent-tool-face
   " "))  ; 1-space virtual indent for wrapped lines

(defun claude-agent--insert-tool-result-start ()
  "Insert the start of a tool result section."
  (claude-agent--append-to-log " #+begin_example\n" nil " "))

(defun claude-agent--insert-tool-result-end ()
  "Insert the end of a tool result section."
  (claude-agent--append-to-log " #+end_example\n" nil))

(defun claude-agent--insert-bash-tool (command)
  "Insert a Bash tool call with COMMAND formatted as org src block."
  (claude-agent--append-to-log
   (format "\n #+begin_src bash\n %s\n #+end_src\n" command)
   nil
   " "))

(defvar-local claude-agent--current-read-file nil
  "File path for current Read tool being displayed.")

(defvar-local claude-agent--current-write-file nil
  "File path for current Write tool being displayed.")

(defvar-local claude-agent--current-write-content nil
  "Content for current Write tool being displayed.")

(defun claude-agent--insert-read-tool (file-path)
  "Insert a Read tool header with FILE-PATH as clickable link."
  (setq claude-agent--current-read-file file-path)
  (let* ((inhibit-read-only t)
         (saved-input (if (eq claude-agent--input-mode 'text)
                          (claude-agent--get-input-text)
                        claude-agent--saved-input))
         (cursor-offset (when (and (eq claude-agent--input-mode 'text)
                                   claude-agent--input-start-marker
                                   (marker-position claude-agent--input-start-marker)
                                   (>= (point) claude-agent--input-start-marker))
                          (- (point) claude-agent--input-start-marker))))
    ;; Delete dynamic section
    (delete-region claude-agent--static-end-marker (point-max))
    (goto-char claude-agent--static-end-marker)
    ;; Insert header with tool icon
    (let ((start (point)))
      (insert "\n ⚙ Read(")
      (claude-agent--apply-face start (point) 'claude-agent-tool-face))
    ;; File path as clickable button
    (insert-text-button file-path
                        'action (lambda (_btn)
                                  (find-file-other-window
                                   (button-get _btn 'file-path)))
                        'file-path file-path
                        'face 'claude-agent-file-link
                        'help-echo "Click to open file"
                        'follow-link t)
    (let ((end-start (point)))
      (insert ")\n")
      (claude-agent--apply-face end-start (point) 'claude-agent-tool-face))
    ;; Update static marker
    (set-marker claude-agent--static-end-marker (point))
    ;; Rebuild dynamic section
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))
    (setq claude-agent--input-start-marker (point-marker))
    (insert saved-input)
    (claude-agent--update-read-only)
    (claude-agent--update-placeholder)
    (goto-char (if (and cursor-offset (>= cursor-offset 0))
                   (min (+ claude-agent--input-start-marker cursor-offset) (point-max))
                 claude-agent--input-start-marker))))

(defun claude-agent--format-read-line (line)
  "Format a LINE from Read tool output with prettier line numbers.
Input format: '     N→content' where N is line number."
  (if (string-match "^\\( *\\)\\([0-9]+\\)→\\(.*\\)$" line)
      (let ((line-num (match-string 2 line))
            (content (match-string 3 line)))
        (cons (format "%4s│ " line-num) content))
    ;; Not a numbered line, return as-is
    (cons nil line)))

(defun claude-agent--insert-read-content (content)
  "Insert Read tool CONTENT with formatted line numbers.
Expects content in the format from Claude's Read tool."
  (let* ((inhibit-read-only t)
         (saved-input (if (eq claude-agent--input-mode 'text)
                          (claude-agent--get-input-text)
                        claude-agent--saved-input))
         (cursor-offset (when (and (eq claude-agent--input-mode 'text)
                                   claude-agent--input-start-marker
                                   (marker-position claude-agent--input-start-marker)
                                   (>= (point) claude-agent--input-start-marker))
                          (- (point) claude-agent--input-start-marker)))
         (lines (split-string content "\n"))
         ;; Remove trailing empty lines
         (trimmed-lines (let ((result lines))
                          (while (and result (string-empty-p (car (last result))))
                            (setq result (butlast result)))
                          result)))
    ;; Delete dynamic section
    (delete-region claude-agent--static-end-marker (point-max))
    (goto-char claude-agent--static-end-marker)
    ;; Insert each line with formatted line numbers
    (dolist (line trimmed-lines)
      (let ((parsed (claude-agent--format-read-line line)))
        (if (car parsed)
            ;; Line with number
            (progn
              (let ((num-start (point)))
                (insert " " (car parsed))
                (claude-agent--apply-face num-start (point) 'claude-agent-line-number))
              (insert (cdr parsed) "\n"))
          ;; Plain line (no number)
          (insert " " (cdr parsed) "\n"))))
    ;; Update static marker
    (set-marker claude-agent--static-end-marker (point))
    ;; Rebuild dynamic section
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))
    (setq claude-agent--input-start-marker (point-marker))
    (insert saved-input)
    (claude-agent--update-read-only)
    (claude-agent--update-placeholder)
    (goto-char (if (and cursor-offset (>= cursor-offset 0))
                   (min (+ claude-agent--input-start-marker cursor-offset) (point-max))
                 claude-agent--input-start-marker))))

(defun claude-agent--insert-write-tool (file-path)
  "Insert a Write tool header with FILE-PATH as clickable link.
Follows same pattern as `claude-agent--insert-read-tool'."
  (setq claude-agent--current-write-file file-path)
  (let* ((inhibit-read-only t)
         (saved-input (if (eq claude-agent--input-mode 'text)
                          (claude-agent--get-input-text)
                        claude-agent--saved-input))
         (cursor-offset (when (and (eq claude-agent--input-mode 'text)
                                   claude-agent--input-start-marker
                                   (marker-position claude-agent--input-start-marker)
                                   (>= (point) claude-agent--input-start-marker))
                          (- (point) claude-agent--input-start-marker))))
    ;; Delete dynamic section
    (delete-region claude-agent--static-end-marker (point-max))
    (goto-char claude-agent--static-end-marker)
    ;; Insert header with tool icon
    (let ((start (point)))
      (insert "\n ⚙ Write(")
      (claude-agent--apply-face start (point) 'claude-agent-tool-face))
    ;; File path as clickable button
    (insert-text-button file-path
                        'action (lambda (_btn)
                                  (find-file-other-window
                                   (button-get _btn 'file-path)))
                        'file-path file-path
                        'face 'claude-agent-file-link
                        'help-echo "Click to open file"
                        'follow-link t)
    (let ((end-start (point)))
      (insert ")\n")
      (claude-agent--apply-face end-start (point) 'claude-agent-tool-face))
    ;; Update static marker
    (set-marker claude-agent--static-end-marker (point))
    ;; Rebuild dynamic section
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))
    (setq claude-agent--input-start-marker (point-marker))
    (insert saved-input)
    (claude-agent--update-read-only)
    (claude-agent--update-placeholder)
    (goto-char (if (and cursor-offset (>= cursor-offset 0))
                   (min (+ claude-agent--input-start-marker cursor-offset) (point-max))
                 claude-agent--input-start-marker))))

(defun claude-agent--insert-write-content (content)
  "Insert Write tool CONTENT with line numbers showing additions.
Follows same pattern as `claude-agent--insert-read-content'."
  (setq claude-agent--current-write-content content)
  (let* ((inhibit-read-only t)
         (saved-input (if (eq claude-agent--input-mode 'text)
                          (claude-agent--get-input-text)
                        claude-agent--saved-input))
         (cursor-offset (when (and (eq claude-agent--input-mode 'text)
                                   claude-agent--input-start-marker
                                   (marker-position claude-agent--input-start-marker)
                                   (>= (point) claude-agent--input-start-marker))
                          (- (point) claude-agent--input-start-marker)))
         (lines (split-string content "\n"))
         ;; Remove trailing empty lines
         (trimmed-lines (let ((result lines))
                          (while (and result (string-empty-p (car (last result))))
                            (setq result (butlast result)))
                          result))
         (line-num 0))
    ;; Delete dynamic section
    (delete-region claude-agent--static-end-marker (point-max))
    (goto-char claude-agent--static-end-marker)
    ;; Insert each line with line numbers and + prefix
    (dolist (line trimmed-lines)
      (cl-incf line-num)
      (let ((num-start (point)))
        (insert (format " %4d│+ " line-num))
        (claude-agent--apply-face num-start (point) 'claude-agent-diff-added))
      (insert line "\n"))
    ;; Update static marker
    (set-marker claude-agent--static-end-marker (point))
    ;; Rebuild dynamic section
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))
    (setq claude-agent--input-start-marker (point-marker))
    (insert saved-input)
    (claude-agent--update-read-only)
    (claude-agent--update-placeholder)
    (goto-char (if (and cursor-offset (>= cursor-offset 0))
                   (min (+ claude-agent--input-start-marker cursor-offset) (point-max))
                 claude-agent--input-start-marker))))

(defun claude-agent--append-to-log (text &optional face virtual-indent)
  "Append TEXT to the static section (conversation log).
If FACE is non-nil, apply it as an overlay to the inserted text.
If VIRTUAL-INDENT is non-nil, apply it as line-prefix/wrap-prefix."
  ;; We need to apply face/indent to the text being inserted.
  ;; Since append-to-static does complex operations, we'll handle styling here.
  (let* ((inhibit-read-only t)
         ;; In permission mode, use saved-input; in text mode, capture current input
         (saved-input (if (eq claude-agent--input-mode 'text)
                          (claude-agent--get-input-text)
                        claude-agent--saved-input))
         (cursor-offset (when (and (eq claude-agent--input-mode 'text)
                                   claude-agent--input-start-marker
                                   (marker-position claude-agent--input-start-marker)
                                   (>= (point) claude-agent--input-start-marker))
                          (- (point) claude-agent--input-start-marker))))
    ;; Delete everything from static-end to end
    (delete-region claude-agent--static-end-marker (point-max))
    ;; Insert new static content with styling
    (goto-char claude-agent--static-end-marker)
    (let ((start (point)))
      (insert text)
      ;; Apply face overlay if specified
      (when face
        (claude-agent--apply-face start (point) face))
      ;; Apply virtual indent if specified
      (when virtual-indent
        (put-text-property start (point) 'line-prefix virtual-indent)
        (put-text-property start (point) 'wrap-prefix virtual-indent)))
    (set-marker claude-agent--static-end-marker (point))
    ;; Insert status bar
    (when claude-agent--has-conversation
      (claude-agent--insert-status-bar))
    ;; Set input marker and restore input
    (setq claude-agent--input-start-marker (point-marker))
    (insert saved-input)
    ;; Finalize
    (claude-agent--update-read-only)
    (claude-agent--update-placeholder)
    ;; Restore cursor
    (goto-char (if (and cursor-offset (>= cursor-offset 0))
                   (min (+ claude-agent--input-start-marker cursor-offset) (point-max))
                 claude-agent--input-start-marker))))

;;;; Process filter - parsing NDJSON messages

(defun claude-agent--process-filter (proc output)
  "Process filter for agent PROC handling OUTPUT."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (claude-agent--handle-output output)))))

(defun claude-agent--handle-output (output)
  "Handle OUTPUT from the agent process, parsing NDJSON messages."
  (setq claude-agent--pending-output
        (concat claude-agent--pending-output output))

  ;; Process complete lines (each line is a JSON message)
  (while (string-match "\n" claude-agent--pending-output)
    (let ((line (substring claude-agent--pending-output 0 (match-beginning 0))))
      (setq claude-agent--pending-output
            (substring claude-agent--pending-output (match-end 0)))
      (claude-agent--process-json-line line))))

(defun claude-agent--process-json-line (line)
  "Process a single LINE of NDJSON output."
  (when (and line (not (string-empty-p (string-trim line))))
    (condition-case err
        (let* ((msg (json-read-from-string line))
               (msg-type (cdr (assq 'type msg))))
          (claude-agent--dispatch-message msg-type msg))
      (json-readtable-error
       (message "Claude agent: Invalid JSON: %s" line))
      (error
       (message "Claude agent: Error processing message: %s" (error-message-string err))))))

(defun claude-agent--dispatch-message (msg-type msg)
  "Dispatch message MSG based on MSG-TYPE."
  (pcase msg-type
    ;; Ready - clear thinking, send queued messages
    ("ready"
     (claude-agent--set-thinking nil)
     (when claude-agent--message-queue
       (claude-agent--send-next-queued)))

    ;; Session start
    ("session_start"
     nil)  ; Handled by buffer init

    ;; Session info - update model/session-id
    ("session_info"
     (when-let ((model (cdr (assq 'model msg))))
       (setq claude-agent--session-info
             (plist-put claude-agent--session-info :model model)))
     (when-let ((session-id (cdr (assq 'session_id msg))))
       (setq claude-agent--session-info
             (plist-put claude-agent--session-info :session-id session-id)))
     (claude-agent--render-dynamic-section))

    ;; Thinking status
    ("thinking"
     (let ((status (cdr (assq 'status msg))))
       (unless claude-agent--thinking-start-time
         (setq claude-agent--input-tokens 0
               claude-agent--output-tokens 0
               claude-agent--thinking-start-time (current-time)))
       (claude-agent--set-thinking (or status "Thinking..."))))

    ;; Progress - update token counts
    ("progress"
     (when-let ((input (cdr (assq 'input_tokens msg))))
       (setq claude-agent--input-tokens input))
     (when-let ((output (cdr (assq 'output_tokens msg))))
       (setq claude-agent--output-tokens output)))

    ;; Result - update cost
    ("result"
     (when-let ((cost (cdr (assq 'cost_usd msg))))
       (setq claude-agent--session-info
             (plist-put claude-agent--session-info :cost cost)))
     (claude-agent--render-dynamic-section))

    ;; MCP status
    ("mcp_status"
     (let ((servers (cdr (assq 'servers msg))))
       (setq claude-agent--mcp-server-status servers)
       (let ((failed (seq-filter
                      (lambda (s) (not (equal (cdr (assq 'status s)) "connected")))
                      servers)))
         (when failed
           (claude-agent--append-to-log
            (format "\n⚠ MCP server issue: %s\n"
                    (mapconcat (lambda (s)
                                 (format "%s (%s)"
                                         (cdr (assq 'name s))
                                         (cdr (assq 'status s))))
                               failed ", "))
            'claude-agent-error-face)))))

    ;; Permission request
    ("permission_request"
     (claude-agent--set-thinking "Awaiting permission...")
     (claude-agent--show-permission-prompt msg))

    ;; Permission granted (informational)
    ("permission_granted"
     nil)  ; Could show notification if desired

    ;; User message start
    ("user_start"
     (setq claude-agent--parse-state 'user)
     (setq claude-agent--has-conversation t)
     (claude-agent--append-to-log
      "\n━━━ You ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
      'claude-agent-user-header-face))

    ;; User message text
    ("user_text"
     (let ((text (cdr (assq 'text msg))))
       (claude-agent--append-to-log (concat text "\n") 'claude-agent-user-face "  ")))

    ;; User message end
    ("user_end"
     (setq claude-agent--parse-state nil))

    ;; Assistant message start
    ("assistant_start"
     (setq claude-agent--parse-state 'assistant)
     (claude-agent--append-to-log
      "\n━━━ Claude ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
      'claude-agent-assistant-header-face))

    ;; Assistant message text
    ("assistant_text"
     (let ((text (cdr (assq 'text msg))))
       (claude-agent--append-to-log (concat text "\n") nil "  ")))

    ;; Assistant message end
    ("assistant_end"
     (setq claude-agent--parse-state nil))

    ;; Tool call
    ("tool_call"
     (let* ((name (cdr (assq 'name msg)))
            (input (cdr (assq 'input msg)))
            (args-str (claude-agent--format-tool-input-for-display name input)))
       (setq claude-agent--parse-state (if (string= name "Read") 'read-tool 'tool))
       ;; Format based on tool type
       (cond
        ((string= name "Bash")
         (claude-agent--insert-bash-tool (cdr (assq 'command input))))
        ((string= name "Read")
         (claude-agent--insert-read-tool (cdr (assq 'file_path input))))
        (t
         (claude-agent--insert-tool-call name args-str)))))

    ;; Tool result
    ("tool_result"
     (let ((content (cdr (assq 'content msg)))
           (is-error (cdr (assq 'is_error msg))))
       (if (eq claude-agent--parse-state 'read-tool)
           ;; Read tool - use special formatted display
           (claude-agent--insert-read-content content)
         ;; Other tools - show in example block
         (progn
           (claude-agent--insert-tool-result-start)
           (claude-agent--append-to-log (concat content "\n") nil " ")
           (claude-agent--insert-tool-result-end)))))

    ;; Tool end
    ("tool_end"
     (setq claude-agent--parse-state nil)
     (claude-agent--set-thinking "Thinking..."))

    ;; Edit tool (special display)
    ("edit_tool"
     (let ((file-path (cdr (assq 'file_path msg)))
           (old-string (cdr (assq 'old_string msg)))
           (new-string (cdr (assq 'new_string msg))))
       (setq claude-agent--parse-state 'tool)
       (claude-agent--set-thinking (format "Editing: %s" (file-name-nondirectory file-path)))
       ;; Insert the diff display
       (let* ((inhibit-read-only t)
              (saved-input (claude-agent--get-input-text))
              (cursor-offset (when (and claude-agent--input-start-marker
                                        (marker-position claude-agent--input-start-marker)
                                        (>= (point) claude-agent--input-start-marker))
                               (- (point) claude-agent--input-start-marker))))
         (delete-region claude-agent--static-end-marker (point-max))
         (goto-char claude-agent--static-end-marker)
         (claude-agent--insert-diff file-path old-string new-string)
         (set-marker claude-agent--static-end-marker (point))
         (when claude-agent--has-conversation
           (claude-agent--insert-status-bar))
         (setq claude-agent--input-start-marker (point-marker))
         (insert saved-input)
         (claude-agent--update-read-only)
         (claude-agent--update-placeholder)
         (goto-char (if (and cursor-offset (>= cursor-offset 0))
                        (min (+ claude-agent--input-start-marker cursor-offset) (point-max))
                      claude-agent--input-start-marker)))))

    ;; Write tool (special display)
    ("write_tool"
     (let ((file-path (cdr (assq 'file_path msg)))
           (content (cdr (assq 'content msg))))
       (setq claude-agent--parse-state 'tool)
       (claude-agent--set-thinking (format "Writing: %s" (file-name-nondirectory file-path)))
       (claude-agent--insert-write-tool file-path)
       (claude-agent--insert-write-content content)))

    ;; Session message (system notifications)
    ("session_message_start"
     (setq claude-agent--parse-state 'session))

    ("session_message_text"
     (let ((text (cdr (assq 'text msg))))
       (claude-agent--append-to-log (concat text "\n") 'claude-agent-session-face)))

    ("session_message_end"
     (setq claude-agent--parse-state nil))

    ;; Error
    ("error"
     (let ((message-text (cdr (assq 'message msg)))
           (traceback (cdr (assq 'traceback msg))))
       (claude-agent--append-to-log
        (format "\n⚠ Error: %s\n" message-text)
        'claude-agent-error-face)
       (when traceback
         (claude-agent--append-to-log
          (format "Traceback:\n%s\n" traceback)
          'claude-agent-error-face))))

    ;; Unknown message type
    (_
     (message "Claude agent: Unknown message type: %s" msg-type))))

(defun claude-agent--format-tool-input-for-display (tool-name tool-input)
  "Format TOOL-INPUT for display based on TOOL-NAME."
  (cond
   ((member tool-name '("Read" "Write" "Edit"))
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Bash")
    (let ((cmd (cdr (assq 'command tool-input))))
      (if (> (length cmd) 60)
          (concat (substring cmd 0 57) "...")
        cmd)))
   ((string= tool-name "Glob")
    (cdr (assq 'pattern tool-input)))
   ((string= tool-name "Grep")
    (let ((pattern (cdr (assq 'pattern tool-input)))
          (path (cdr (assq 'path tool-input))))
      (if path (format "%s, %s" pattern path) pattern)))
   ((string= tool-name "WebFetch")
    (cdr (assq 'url tool-input)))
   ((string= tool-name "Task")
    (let ((desc (cdr (assq 'description tool-input))))
      (if (> (length desc) 40) (substring desc 0 40) desc)))
   (t
    (let ((first-val (cdar tool-input)))
      (if first-val
          (let ((val-str (format "%s" first-val)))
            (if (> (length val-str) 40) (substring val-str 0 40) val-str))
        "")))))

;;;; Permission prompt UI

(defvar-local claude-agent--permission-data nil
  "Current permission request data.")

(defvar-local claude-agent--permission-selection 0
  "Currently selected option in permission prompt (0-3).")

(defface claude-agent-permission-box-face
  '((t :foreground "#e5c07b" :background "#3e4451" :box (:line-width 1 :color "#5c6370")))
  "Face for permission dialog box."
  :group 'claude-agent)

(defface claude-agent-permission-selected-face
  '((t :foreground "#282c34" :background "#61afef" :weight bold))
  "Face for selected option in permission dialog."
  :group 'claude-agent)

(defface claude-agent-permission-option-face
  '((t :foreground "#abb2bf"))
  "Face for unselected options in permission dialog."
  :group 'claude-agent)

(defun claude-agent--format-tool-input (tool-name tool-input)
  "Format TOOL-INPUT for display based on TOOL-NAME."
  (cond
   ((string= tool-name "Read")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Write")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Edit")
    (cdr (assq 'file_path tool-input)))
   ((string= tool-name "Bash")
    (let ((cmd (cdr (assq 'command tool-input))))
      (if (> (length cmd) 50)
          (concat (substring cmd 0 47) "...")
        cmd)))
   (t (format "%s" tool-input))))

(defun claude-agent--generate-permission-pattern (tool-name tool-input scope)
  "Generate permission pattern for TOOL-NAME with TOOL-INPUT at SCOPE level."
  (pcase scope
    ('once
     (pcase tool-name
       ("Read" (format "Read(%s)" (cdr (assq 'file_path tool-input))))
       ("Write" (format "Write(%s)" (cdr (assq 'file_path tool-input))))
       ("Edit" (format "Edit(%s)" (cdr (assq 'file_path tool-input))))
       ("Bash" (format "Bash(%s)" (cdr (assq 'command tool-input))))
       (_ (format "%s" tool-name))))
    ('session
     (pcase tool-name
       ("Read" (format "Read(%s)" (cdr (assq 'file_path tool-input))))
       ("Write" (format "Write(%s)" (cdr (assq 'file_path tool-input))))
       ("Edit" (format "Edit(%s)" (cdr (assq 'file_path tool-input))))
       ("Bash"
        (let* ((cmd (cdr (assq 'command tool-input)))
               (first-word (car (split-string cmd))))
          (format "Bash(%s:*)" first-word)))
       (_ (format "%s(*)" tool-name))))
    ('always
     (pcase tool-name
       ("Read"
        (let* ((path (cdr (assq 'file_path tool-input)))
               (dir (file-name-directory path)))
          (format "Read(%s*)" (or dir "/"))))
       ("Write"
        (let* ((path (cdr (assq 'file_path tool-input)))
               (dir (file-name-directory path)))
          (format "Write(%s*)" (or dir "/"))))
       ("Edit"
        (let* ((path (cdr (assq 'file_path tool-input)))
               (dir (file-name-directory path)))
          (format "Edit(%s*)" (or dir "/"))))
       ("Bash"
        (let* ((cmd (cdr (assq 'command tool-input)))
               (first-word (car (split-string cmd))))
          (format "Bash(%s:*)" first-word)))
       (_ (format "%s(*)" tool-name))))))

(defvar-local claude-agent--permission-overlay-specs nil
  "List of (start end face) specs for permission dialog overlays.")

(defun claude-agent--apply-permission-overlays ()
  "Apply permission overlays in the current buffer using saved specs."
  (when claude-agent--permission-overlay-specs
    ;; Remove existing permission overlays in this buffer
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'claude-permission-face)
        (delete-overlay ov)))
    ;; Apply new overlays
    (dolist (spec claude-agent--permission-overlay-specs)
      (let ((ov (make-overlay (nth 0 spec) (nth 1 spec))))
        (overlay-put ov 'face (nth 2 spec))
        (overlay-put ov 'priority 1000)
        (overlay-put ov 'evaporate nil)
        (overlay-put ov 'claude-permission-face t)))))

(defun claude-agent--render-permission-content ()
  "Insert the permission dialog content at point.
Called by `render-dynamic-section' when in permission mode."
  (when claude-agent--permission-data
    (let* ((tool-name (cdr (assq 'tool_name claude-agent--permission-data)))
           (tool-input (cdr (assq 'tool_input claude-agent--permission-data)))
           (input-str (claude-agent--format-tool-input tool-name tool-input))
           (sel claude-agent--permission-selection)
           (inhibit-read-only t)
           (options '("Allow once" "Allow for this session" "Always allow" "Deny"))
           (overlay-specs nil))
      ;; Helper to insert and record overlay spec
      (cl-flet ((insert-styled (text face)
                  (let ((start (point)))
                    (insert text)
                    (push (list start (point) face) overlay-specs))))
        ;; Header
        (insert-styled "── Permission Request " 'claude-agent-input-header-face)
        (insert-styled (make-string 40 ?─) 'claude-agent-input-header-face)
        (insert "\n")
        ;; Tool info - now function-style
        (insert-styled " Claude wants to run:\n" 'claude-agent-session-face)
        (insert-styled (format " %s(%s)\n\n" tool-name input-str) 'claude-agent-tool-face)
        ;; Options
        (dotimes (i 4)
          (let* ((selected (= i sel))
                 (checkbox (if selected "[X]" "[ ]"))
                 (label (nth i options))
                 (face (if selected
                           'claude-agent-permission-selected-face
                         'claude-agent-permission-option-face)))
            (insert-styled (format " %d. %s %s\n" (1+ i) checkbox label) face)))
        ;; Footer
        (insert-styled (make-string 62 ?─) 'claude-agent-input-header-face)
        (insert "\n"))
      ;; Save overlay specs and apply
      (setq claude-agent--permission-overlay-specs (nreverse overlay-specs))
      (claude-agent--apply-permission-overlays)
      ;; Apply in indirect buffers too
      (let ((base (current-buffer)))
        (dolist (buf (buffer-list))
          (when (and (buffer-live-p buf)
                     (eq (buffer-base-buffer buf) base))
            (with-current-buffer buf
              (setq claude-agent--permission-overlay-specs
                    (buffer-local-value 'claude-agent--permission-overlay-specs base))
              (claude-agent--apply-permission-overlays))))))))

(defun claude-agent--render-permission-dialog ()
  "Re-render the permission dialog (updates selection state).
This is called when the user navigates options."
  (claude-agent--render-dynamic-section))

(defun claude-agent--show-permission-prompt (data)
  "Show permission prompt for DATA in the input area.
Saves current input text and switches to permission mode."
  ;; Save current input text before switching modes
  (setq claude-agent--saved-input (claude-agent--get-input-text))
  ;; Set permission state
  (setq claude-agent--permission-data data)
  (setq claude-agent--permission-selection 0)
  ;; Switch to permission mode
  (setq claude-agent--input-mode 'permission)
  ;; Render the dialog (which now uses render-dynamic-section)
  (claude-agent--render-permission-dialog)
  ;; Set up keyboard navigation
  (claude-agent--setup-permission-keymap))

(defun claude-agent--permission-select-next ()
  "Move selection down in permission dialog."
  (interactive)
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (setq claude-agent--permission-selection
           (mod (1+ claude-agent--permission-selection) 4))
     (claude-agent--render-permission-dialog))))

(defun claude-agent--permission-select-prev ()
  "Move selection up in permission dialog."
  (interactive)
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (setq claude-agent--permission-selection
           (mod (1- claude-agent--permission-selection) 4))
     (claude-agent--render-permission-dialog))))

(defun claude-agent--permission-confirm ()
  "Confirm the current selection in permission dialog."
  (interactive)
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (pcase claude-agent--permission-selection
       (0 (claude-agent--send-permission-response "allow_once"))
       (1 (claude-agent--send-permission-response "allow_session"))
       (2 (claude-agent--send-permission-response "allow_always"))
       (3 (claude-agent--send-permission-response "deny"))))))

;; Minor mode for permission dialog - takes precedence over evil-mode
(defvar claude-agent-permission-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Number keys for direct selection
    (define-key map (kbd "1") #'claude-agent-permit-once)
    (define-key map (kbd "2") #'claude-agent-permit-session)
    (define-key map (kbd "3") #'claude-agent-permit-always)
    (define-key map (kbd "4") #'claude-agent-deny)
    ;; Arrow keys for navigation
    (define-key map (kbd "<up>") #'claude-agent--permission-select-prev)
    (define-key map (kbd "<down>") #'claude-agent--permission-select-next)
    (define-key map (kbd "C-p") #'claude-agent--permission-select-prev)
    (define-key map (kbd "C-n") #'claude-agent--permission-select-next)
    (define-key map (kbd "k") #'claude-agent--permission-select-prev)
    (define-key map (kbd "j") #'claude-agent--permission-select-next)
    ;; Confirm selection
    (define-key map (kbd "RET") #'claude-agent--permission-confirm)
    (define-key map (kbd "SPC") #'claude-agent--permission-confirm)
    ;; Quick keys
    (define-key map (kbd "y") #'claude-agent-permit-once)
    (define-key map (kbd "n") #'claude-agent-deny)
    (define-key map (kbd "a") #'claude-agent-permit-always)
    (define-key map (kbd "q") #'claude-agent-deny)
    ;; Escape to deny (helpful for evil users)
    (define-key map (kbd "<escape>") #'claude-agent-deny)
    map)
  "Keymap for permission dialog mode.")

(define-minor-mode claude-agent-permission-mode
  "Minor mode for permission dialog interaction.
Takes precedence over evil-mode keybindings."
  :lighter " Permit"
  :keymap claude-agent-permission-mode-map
  (if claude-agent-permission-mode
      (progn
        ;; Switch to emacs state to use our keymap directly
        (when (bound-and-true-p evil-local-mode)
          (evil-emacs-state))
        (message "Permission: j/k to select, RET to confirm, 1-4 for direct choice"))
    ;; When disabling, return to normal state
    (when (bound-and-true-p evil-local-mode)
      (evil-normal-state))))

(defun claude-agent--setup-permission-keymap ()
  "Set up keymap for permission prompt interaction."
  ;; Enable permission mode in the base buffer
  (claude-agent-permission-mode 1)
  ;; For polymode: also enable in all indirect buffers sharing this base
  (let ((base (or (buffer-base-buffer) (current-buffer))))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (eq (buffer-base-buffer buf) base))
        (with-current-buffer buf
          (claude-agent-permission-mode 1))))))

(defun claude-agent--send-permission-response (action)
  "Send permission response with ACTION to the agent process.
Restores text input mode and any saved input."
  (claude-agent--in-base-buffer
   (when claude-agent--permission-data
     (let* ((tool-name (cdr (assq 'tool_name claude-agent--permission-data)))
            (tool-input (cdr (assq 'tool_input claude-agent--permission-data)))
            (scope (pcase action
                     ("allow_once" 'once)
                     ("allow_session" 'session)
                     ("allow_always" 'always)
                     (_ nil)))
            (pattern (when scope
                       (claude-agent--generate-permission-pattern
                        tool-name tool-input scope)))
            (response-msg `((type . "permission_response")
                            (action . ,action)
                            (pattern . ,pattern)))
            (saved-input claude-agent--saved-input))
       ;; Clear permission state and disable minor mode in all related buffers
       (setq claude-agent--permission-data nil)
       (setq claude-agent--permission-overlay-specs nil)
       (claude-agent-permission-mode -1)
       ;; For polymode: also disable in all indirect buffers sharing this base
       (let ((base (current-buffer)))
         (dolist (buf (buffer-list))
           (when (and (buffer-live-p buf)
                      (eq (buffer-base-buffer buf) base))
             (with-current-buffer buf
               (claude-agent-permission-mode -1)
               ;; Clear permission overlays in indirect buffers too
               (dolist (ov (overlays-in (point-min) (point-max)))
                 (when (overlay-get ov 'claude-permission-face)
                   (delete-overlay ov)))))))
       ;; Clear permission overlays in base buffer
       (dolist (ov (overlays-in (point-min) (point-max)))
         (when (overlay-get ov 'claude-permission-face)
           (delete-overlay ov)))
       ;; Switch to empty mode (clears input area completely)
       (setq claude-agent--input-mode 'empty)
       ;; Show thinking status (this rebuilds dynamic section with empty input)
       (claude-agent--set-thinking "Processing...")
       ;; Send JSON response to process
       (when (and claude-agent--process
                  (process-live-p claude-agent--process))
         (process-send-string claude-agent--process
                              (concat (json-encode response-msg) "\n")))))))

(defun claude-agent-permit-once ()
  "Allow the tool to run once."
  (interactive)
  (claude-agent--send-permission-response "allow_once"))

(defun claude-agent-permit-session ()
  "Allow the tool pattern for this session."
  (interactive)
  (claude-agent--send-permission-response "allow_session"))

(defun claude-agent-permit-always ()
  "Always allow this tool pattern (saves to settings)."
  (interactive)
  (claude-agent--send-permission-response "allow_always"))

(defun claude-agent-deny ()
  "Deny the permission request."
  (interactive)
  (claude-agent--send-permission-response "deny"))

;;;; Process management

(defun claude-agent--get-agent-dir ()
  "Get the directory containing the Python agent."
  (let ((this-file (or load-file-name
                       buffer-file-name
                       (locate-library "claude-agent")
                       (symbol-file 'claude-agent-run 'defun))))
    (when this-file
      (expand-file-name "claude_agent"
                        (file-name-directory this-file)))))

(defun claude-agent--start-process (work-dir buffer &optional resume-session continue-session)
  "Start the Python agent process for WORK-DIR with BUFFER.
Optional RESUME-SESSION is a session ID to resume.
Optional CONTINUE-SESSION, if non-nil, continues the most recent session."
  (let* ((agent-dir (claude-agent--get-agent-dir))
         (log-file (expand-file-name "claude-agent.log" work-dir))
         (args (list "run" "--directory" agent-dir
                     "python" "-u" "-m" "claude_agent"  ; -u for unbuffered
                     "--work-dir" work-dir
                     "--log-file" log-file)))
    ;; Add resume or continue flags
    (when resume-session
      (setq args (append args (list "--resume" resume-session))))
    (when continue-session
      (setq args (append args (list "--continue"))))
    (let ((process-connection-type t)  ; Use PTY for line-buffered output
          (process-environment (cons "PYTHONUNBUFFERED=1" process-environment))
          (proc (apply #'start-process
                       "claude-agent"
                       buffer
                       claude-agent-python-command
                       args)))
      (set-process-coding-system proc 'utf-8 'utf-8)
      (set-process-filter proc #'claude-agent--process-filter)
      (set-process-sentinel proc #'claude-agent--process-sentinel)
      proc)))

(defun claude-agent--process-sentinel (proc event)
  "Handle process PROC state change EVENT."
  (when (memq (process-status proc) '(exit signal))
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (claude-agent--set-thinking nil)
          (claude-agent--append-to-log
           (format "\n[Process %s]\n" (string-trim event))
           'claude-agent-session-face))))))

;;;; User commands

(defun claude-agent--is-busy-p ()
  "Return t if the agent is currently busy (thinking)."
  claude-agent--thinking-status)

(defun claude-agent--render-queue ()
  "Render queued messages by rebuilding the dynamic section."
  (claude-agent--render-dynamic-section))

(defun claude-agent--send-next-queued ()
  "Send the next queued message if any and not busy."
  (when (and claude-agent--message-queue
             (not (claude-agent--is-busy-p))
             claude-agent--process
             (process-live-p claude-agent--process))
    (let ((msg (pop claude-agent--message-queue)))
      ;; Send JSON message to process
      (process-send-string claude-agent--process
                           (concat (json-encode `((type . "message") (text . ,msg))) "\n")))))

(defun claude-agent-send ()
  "Send the current input to Claude, or queue if busy."
  (interactive)
  (claude-agent--in-base-buffer
   (when (and claude-agent--input-start-marker
              claude-agent--process
              (process-live-p claude-agent--process))
     (let* ((input (string-trim (buffer-substring-no-properties
                                  claude-agent--input-start-marker (point-max)))))
       ;; Ignore if empty
       (unless (string-empty-p input)
         ;; Clear the input area first
         (let ((inhibit-read-only t))
           (delete-region claude-agent--input-start-marker (point-max)))
         ;; Add to history
         (push input claude-agent--input-history)
         (setq claude-agent--input-history-index 0)
         ;; If busy, queue the message; otherwise send directly
         (if (claude-agent--is-busy-p)
             (progn
               (push input claude-agent--message-queue)
               (claude-agent--render-dynamic-section)
               (message "Message queued (agent is busy)"))
           ;; Send JSON message to process
           (process-send-string claude-agent--process
                                (concat (json-encode `((type . "message") (text . ,input))) "\n"))
           ;; Re-render dynamic section (input already cleared)
           (claude-agent--render-dynamic-section)))))))

(defun claude-agent-send-or-newline ()
  "Send input if on last line, otherwise insert newline."
  (interactive)
  (if (claude-agent--in-input-area-p)
      (if (save-excursion (end-of-line) (eobp))
          (claude-agent-send)
        (newline))
    (newline)))

(defun claude-agent-interrupt ()
  "Interrupt the current Claude operation."
  (interactive)
  (claude-agent--in-base-buffer
   (when (and claude-agent--process
              (process-live-p claude-agent--process))
     (process-send-string claude-agent--process
                          (concat (json-encode '((type . "interrupt"))) "\n")))))

(defun claude-agent-quit ()
  "Quit the Claude session."
  (interactive)
  (when (yes-or-no-p "Quit Claude session? ")
    (claude-agent--in-base-buffer
     (when (and claude-agent--process
                (process-live-p claude-agent--process))
       (process-send-string claude-agent--process
                            (concat (json-encode '((type . "quit"))) "\n"))))))

(defun claude-agent-previous-input ()
  "Recall previous input from history."
  (interactive)
  (claude-agent--in-base-buffer
   (when (and claude-agent--input-history
              (< claude-agent--input-history-index
                 (length claude-agent--input-history)))
     (let ((inhibit-read-only t))
       (delete-region claude-agent--input-start-marker (point-max))
       (goto-char claude-agent--input-start-marker)
       (insert (nth claude-agent--input-history-index
                    claude-agent--input-history))
       (cl-incf claude-agent--input-history-index)))))

(defun claude-agent-next-input ()
  "Recall next input from history."
  (interactive)
  (claude-agent--in-base-buffer
   (when (> claude-agent--input-history-index 0)
     (cl-decf claude-agent--input-history-index)
     (let ((inhibit-read-only t))
       (delete-region claude-agent--input-start-marker (point-max))
       (goto-char claude-agent--input-start-marker)
       (when (> claude-agent--input-history-index 0)
         (insert (nth (1- claude-agent--input-history-index)
                      claude-agent--input-history)))))))

;;;; MCP status functions

(defun claude-agent-mcp-server-status ()
  "Return list of MCP server statuses for current session.
Each element is an alist with keys: name, status."
  (claude-agent--in-base-buffer
   claude-agent--mcp-server-status))

;;;###autoload
(defun claude-agent-show-mcp-status ()
  "Display MCP server connection status for current Claude session."
  (interactive)
  (claude-agent--in-base-buffer
   (let ((status claude-agent--mcp-server-status))
     (if status
         (let ((msg (mapconcat
                     (lambda (s)
                       (let ((name (cdr (assq 'name s)))
                             (st (cdr (assq 'status s))))
                         (format "%s: %s" name
                                 (if (equal st "connected")
                                     (propertize st 'face 'success)
                                   (propertize st 'face 'error)))))
                     status "\n")))
           (message "MCP Servers:\n%s" msg))
       (message "No MCP status available (send a message first to initialize)")))))

;;;; Entry point

;;;###autoload
(defun claude-agent-run (work-dir &optional resume-session continue-session)
  "Start a Claude agent session for WORK-DIR.
Optional RESUME-SESSION is a session ID to resume.
Optional CONTINUE-SESSION, if non-nil, continues the most recent session."
  (interactive
   (list (read-directory-name "Project directory: "
                              (or (vc-git-root default-directory)
                                  default-directory))))
  (let* ((expanded-dir (expand-file-name work-dir))
         (short-name (file-name-nondirectory
                      (directory-file-name expanded-dir)))
         (buf-name (format "*claude:%s*" short-name))
         (buf (get-buffer-create buf-name)))

    ;; Set up buffer
    (with-current-buffer buf
      (claude-agent-mode)
      (claude-agent--init-buffer short-name)
      (setq claude-agent--parse-state nil
            claude-agent--pending-output ""
            claude-agent--session-info nil
            claude-agent--has-conversation nil))

    ;; Start process with optional resume/continue
    (let ((proc (claude-agent--start-process expanded-dir buf resume-session continue-session)))
      (with-current-buffer buf
        (setq claude-agent--process proc)))

    ;; Display buffer
    (pop-to-buffer buf)
    buf))

(provide 'claude-agent)
;;; claude-agent.el ends here
