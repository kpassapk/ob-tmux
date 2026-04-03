;;; ob-tmux.el --- Babel Support for Interactive Terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.
;; Copyright (C) 2017 Allard Hendriksen

;; Author: Allard Hendriksen
;; Keywords: literate programming, interactive shell, tmux
;; URL: https://github.com/ahendriksen/ob-tmux
;; Version: 0.1.5
;; Package-version: 0.1.5
;; Package-Requires: ((emacs "25.1") (seq "2.3") (s "1.9.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Org-Babel support for tmux.
;;
;; Heavily inspired by 'eev' from Eduardo Ochs and ob-screen.el from
;; Benjamin Andresen.
;;
;; See documentation on https://github.com/ahendriksen/ob-tmux
;;
;; You can test the default setup with
;; M-x org-babel-tmux-test RET

;;; Code:

(require 'ob)
(require 'org-id)
(require 'seq)
(require 's)


(defcustom org-babel-tmux-location "tmux"
  "The command location for tmux.
Change in case you want to use a different tmux than the one in your $PATH."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-tmux-session-prefix "org-babel-session-"
  "The string that will be prefixed to tmux session names started by ob-tmux."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-tmux-default-window-name "ob1"
  "This is the default tmux window name used for windows that are not explicitly named in an org session."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-tmux-terminal "gnome-terminal"
  "This is the terminal that will be spawned."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-tmux-terminal-opts '("--")
  "The list of options that will be passed to the terminal."
  :group 'org-babel
  :type 'list)

(defcustom org-babel-tmux-ssh-timeout 60
  "Timeout in seconds for server-side output polling over SSH."
  :group 'org-babel
  :type 'integer)

(defvar org-babel-default-header-args:tmux
  '((:results . "silent")
    (:session . "default")
    (:socket . nil)
    (:ssh . nil))
  "Default arguments to use when running tmux source blocks.")

(add-to-list 'org-src-lang-modes '("tmux" . sh))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-babel interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-babel-execute:tmux (body params)
  "Send a block of code via tmux to a terminal using Babel.
\"default\" session is used when none is specified.
Argument BODY the body of the tmux code block.
Argument PARAMS the org parameters of the code block."
  (message "Sending source code block to interactive terminal session...")
  (save-window-excursion
    (let* ((org-session (cdr (assq :session params)))
	   (org-header-terminal (cdr (assq :terminal params)))
	   (vars (mapcar
                  (lambda (y) (format "%s=\"%s\"" (cadr y) (cddr y)))
                  (seq-filter (lambda (x) (eq :var (car x))) params)))
	   (terminal (or org-header-terminal org-babel-tmux-terminal))
	   (socket (cdr (assq :socket params)))
	   (socket (when socket (expand-file-name socket)))
	   (ssh (cdr (assq :ssh params)))
	   (_ (when (and socket ssh)
		(user-error "ob-tmux: :ssh and :socket are mutually exclusive")))
	   (ob-session (ob-tmux--from-org-session org-session socket ssh))
	   (expanded-body (org-babel-expand-body:generic body params vars)))
      (if ssh
	  ;; === SSH PATH: fully non-blocking ===
	  (let ((result-params (cdr (assq :result-params params))))
	    (ob-tmux--ssh-dispatch ob-session expanded-body result-params))
	;; === LOCAL / SOCKET PATH: unchanged ===
	(let ((session-alive (ob-tmux--session-alive-p ob-session))
	      (window-alive (ob-tmux--window-alive-p ob-session)))
	  (unless session-alive (ob-tmux--create-session ob-session))
	  (unless window-alive (ob-tmux--create-window ob-session))
	  (unless session-alive
	    (ob-tmux--start-terminal-window ob-session terminal))
	  (while (not (ob-tmux--window-alive-p ob-session)))
	  (ob-tmux--disable-renaming ob-session)
	  (ob-tmux--send-body ob-session expanded-body)
	  (when org-header-terminal
	    (ob-tmux--deprecation-warning org-header-terminal))
	  nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ob-tmux object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defstruct (ob-tmux- (:constructor ob-tmux--create)
			(:copier ob-tmux--copy))
  session
  window
  socket
  ssh)

(defun ob-tmux--tmux-session (org-session)
  "Extract tmux session from ORG-SESSION string."
  (let* ((session (car (split-string org-session ":"))))
    (concat org-babel-tmux-session-prefix
	    (if (string-equal "" session) "default" session))))
(defun ob-tmux--tmux-window (org-session)
  "Extract tmux window from ORG-SESSION string."
  (let* ((window (cadr (split-string org-session ":"))))
    (if (string-equal "" window) nil window)))

(defun ob-tmux--from-org-session (org-session &optional socket ssh)
  "Create a new ob-tmux-session object from ORG-SESSION specification.
Optional argument SOCKET: the location of the tmux socket (only use if non-standard).
Optional argument SSH: SSH host for remote tmux execution."

  (ob-tmux--create
   :session (ob-tmux--tmux-session org-session)
   :window (ob-tmux--tmux-window org-session)
   :socket socket
   :ssh ssh))

(defun ob-tmux--window-default (ob-session)
  "Extracts the tmux window from the ob-tmux- object.
Returns `org-babel-tmux-default-window-name' if no window specified.

Argument OB-SESSION: the current ob-tmux session."
  (if (ob-tmux--window ob-session)
      (ob-tmux--window ob-session)
      org-babel-tmux-default-window-name))

(defun ob-tmux--target (ob-session)
  "Constructs a tmux target from the `ob-tmux-' object.

If no window is specified, use first window.

Argument OB-SESSION: the current ob-tmux session."
  (let* ((target-session (ob-tmux--session ob-session))
	 (window (ob-tmux--window ob-session))
	 (target-window (if window (concat "=" window) "^")))
    (concat target-session ":" target-window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process execution functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ob-tmux--execute (ob-session &rest args)
  "Execute a tmux command with arguments as given.

Argument OB-SESSION: the current ob-tmux session.
Optional command-line arguments can be passed in ARGS."
  (let ((ssh (ob-tmux--ssh ob-session))
	(socket (ob-tmux--socket ob-session)))
    (cond
     (ssh
      (apply 'start-process "ob-tmux" "*Messages*"
	     "ssh" ssh org-babel-tmux-location args))
     (socket
      (apply 'start-process "ob-tmux" "*Messages*"
	     org-babel-tmux-location "-S" socket args))
     (t
      (apply 'start-process "ob-tmux" "*Messages*"
	     org-babel-tmux-location args)))))

(defun ob-tmux--execute-string (ob-session &rest args)
  "Execute a tmux command with arguments as given.
Returns stdout as a string.

Argument OB-SESSION: the current ob-tmux session.  Optional
command-line arguments can be passed in ARGS and are
automatically space separated."
  (let* ((ssh (ob-tmux--ssh ob-session))
	 (socket (ob-tmux--socket ob-session))
	 (args (if socket (cons "-S" (cons socket args)) args))
	 (tmux-cmd (concat org-babel-tmux-location " "
			   (s-join " " args))))
    (shell-command-to-string
     (if ssh
	 (concat "ssh " (shell-quote-argument ssh) " " tmux-cmd)
       tmux-cmd))))

(defun ob-tmux--start-terminal-window (ob-session terminal)
  "Start a TERMINAL window with tmux attached to session.

  Argument OB-SESSION: the current ob-tmux session."
  (let ((start-process-mandatory-args `("org-babel: terminal"
					"*Messages*"
					,terminal))
	(tmux-cmd `(,org-babel-tmux-location
		    "attach-session"
		    "-t" ,(ob-tmux--target ob-session))))
    (unless (or (ob-tmux--socket ob-session)
		(ob-tmux--ssh ob-session))
      (apply 'start-process (append start-process-mandatory-args
				    org-babel-tmux-terminal-opts
				    tmux-cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tmux interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ob-tmux--create-session (ob-session)
  "Create a tmux session if it does not yet exist.

Argument OB-SESSION: the current ob-tmux session."
  (unless (ob-tmux--session-alive-p ob-session)
    (ob-tmux--execute ob-session
     "new-session"
     "-d" ;; just create the session, don't attach.
     "-c" (if (ob-tmux--ssh ob-session) "~" (expand-file-name "~"))
     "-s" (ob-tmux--session ob-session)
     "-n" (ob-tmux--window-default ob-session))))

(defun ob-tmux--create-window (ob-session)
  "Create a tmux window in session if it does not yet exist.

Argument OB-SESSION: the current ob-tmux session."
  (unless (ob-tmux--window-alive-p ob-session)
    (ob-tmux--execute ob-session
     "new-window"
     "-c" (if (ob-tmux--ssh ob-session) "~" (expand-file-name "~"))
     "-n" (ob-tmux--window-default ob-session)
     "-t" (ob-tmux--session ob-session))))

(defun ob-tmux--set-window-option (ob-session option value)
  "If window exists, set OPTION for window.

Argument OB-SESSION: the current ob-tmux session."
  (when (ob-tmux--window-alive-p ob-session)
    (ob-tmux--execute ob-session
     "set-window-option"
     "-t" (ob-tmux--target ob-session)
     option value)))

(defun ob-tmux--disable-renaming (ob-session)
  "Disable renaming features for tmux window.

Disabling renaming improves the chances that ob-tmux will be able
to find the window again later.

Argument OB-SESSION: the current ob-tmux session."
  (progn
    (ob-tmux--set-window-option ob-session "allow-rename" "off")
    (ob-tmux--set-window-option ob-session "automatic-rename" "off")))


(defun ob-tmux--format-keys (string)
  "Format STRING as a sequence of hexadecimal numbers, to be sent via the `send-keys' command."
  (mapcar (lambda (c) (format "0x%x" c))
	string))

(defun ob-tmux--send-keys (ob-session line)
  "If tmux window exists, send a LINE of text to it.

Argument OB-SESSION: the current ob-tmux session."
  (when (ob-tmux--window-alive-p ob-session)
    (let* ((hex-line (ob-tmux--format-keys line)))
      (apply 'ob-tmux--execute
	     ob-session
	     "send-keys"
	     "-t" (ob-tmux--target ob-session)
	     hex-line))))

(defun ob-tmux--send-body (ob-session body)
  "If tmux window (passed in OB-SESSION) exists, send BODY to it.

Argument OB-SESSION: the current ob-tmux session."
  (let ((lines (split-string body "[\n\r]+")))
    (when (ob-tmux--window-alive-p ob-session)
      (mapc (lambda (l)
	      (ob-tmux--send-keys ob-session (concat l "\n")))
	    lines))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tmux interrogation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ob-tmux--session-alive-p (ob-session)
  "Check if SESSION exists by parsing output of \"tmux ls\".

Argument OB-SESSION: the current ob-tmux session."
  (let* ((tmux-ls (ob-tmux--execute-string ob-session "ls -F '#S'"))
	 (tmux-session (ob-tmux--session ob-session)))
    (car
     (seq-filter (lambda (x) (string-equal tmux-session x))
		 (split-string tmux-ls "\n")))))

(defun ob-tmux--window-alive-p (ob-session)
  "Check if WINDOW exists in tmux session.

If no window is specified in OB-SESSION, returns 't."
  (let* ((window (ob-tmux--window ob-session))
	 (target (ob-tmux--target ob-session))
	 (output (ob-tmux--execute-string ob-session
		  "list-panes"
		  "-F 'yes_exists'"
		  "-t" (concat "'" target "'"))))
    (cond (window
	   (string-equal "yes_exists\n" output))
	  ((null window)
	   't))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSH async execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ob-tmux--ssh-script-output (session window uuid timeout body)
  "Generate a self-contained shell script for SSH async output capture.
SESSION and WINDOW: tmux target names.
UUID: unique marker identifier.
TIMEOUT: max seconds to poll.
BODY: the code to execute, embedded as a heredoc."
  (let ((heredoc-marker (format "OB_TMUX_BODY_%s" uuid)))
    (concat
     (format "SESSION=%s\nWINDOW=%s\nUUID=%s\nTIMEOUT=%s\n"
	     (shell-quote-argument session)
	     (shell-quote-argument window)
	     (shell-quote-argument uuid)
	     (number-to-string timeout))
     "TARGET=\"$SESSION:=$WINDOW\"\n"
     "tmux has-session -t \"$SESSION\" 2>/dev/null || \\\n"
     "  tmux new-session -d -c ~ -s \"$SESSION\" -n \"$WINDOW\"\n"
     "if ! tmux list-windows -t \"$SESSION\" -F '#W' | grep -qx \"$WINDOW\"; then\n"
     "  tmux new-window -c ~ -n \"$WINDOW\" -t \"$SESSION\"\nfi\n"
     "tmux set-window-option -t \"$TARGET\" allow-rename off 2>/dev/null\n"
     "tmux set-window-option -t \"$TARGET\" automatic-rename off 2>/dev/null\n"
     "tmux send-keys -t \"$TARGET\" \"echo 'OB_TMUX_START_'\"$UUID\"''\" Enter\n"
     (format "while IFS= read -r line || [ -n \"$line\" ]; do\n  tmux send-keys -l -t \"$TARGET\" -- \"$line\"\n  tmux send-keys -t \"$TARGET\" Enter\ndone <<'%s'\n" heredoc-marker)
     body "\n"
     heredoc-marker "\n"
     "tmux send-keys -t \"$TARGET\" \"echo 'OB_TMUX_END_'\"$UUID\"''\" Enter\n"
     "ELAPSED=0\n"
     "while [ $ELAPSED -lt $TIMEOUT ]; do\n"
     "  sleep 1\n"
     "  PANE=$(tmux capture-pane -p -t \"$TARGET\")\n"
     "  if echo \"$PANE\" | grep -qx \"OB_TMUX_END_$UUID\"; then\n"
     "    echo \"$PANE\" | sed -n \"/^OB_TMUX_START_${UUID}$/,/^OB_TMUX_END_${UUID}$/p\" | sed '1d;$d' | grep -v '^[^$]*\\$ ' | grep -v 'OB_TMUX_'\n"
     "    exit 0\n  fi\n"
     "  ELAPSED=$((ELAPSED + 1))\ndone\n"
     "echo \"ob-tmux: timeout after ${TIMEOUT}s\" >&2\nexit 1\n")))

(defun ob-tmux--ssh-script-silent (session window body)
  "Generate a self-contained shell script for SSH fire-and-forget execution.
SESSION and WINDOW: tmux target names.
BODY: the code to send, embedded as a heredoc."
  (let ((heredoc-marker "OB_TMUX_BODY_EOF"))
    (concat
     (format "SESSION=%s\nWINDOW=%s\n"
	     (shell-quote-argument session)
	     (shell-quote-argument window))
     "TARGET=\"$SESSION:=$WINDOW\"\n"
     "tmux has-session -t \"$SESSION\" 2>/dev/null || \\\n"
     "  tmux new-session -d -c ~ -s \"$SESSION\" -n \"$WINDOW\"\n"
     "if ! tmux list-windows -t \"$SESSION\" -F '#W' | grep -qx \"$WINDOW\"; then\n"
     "  tmux new-window -c ~ -n \"$WINDOW\" -t \"$SESSION\"\nfi\n"
     "tmux set-window-option -t \"$TARGET\" allow-rename off 2>/dev/null\n"
     "tmux set-window-option -t \"$TARGET\" automatic-rename off 2>/dev/null\n"
     (format "while IFS= read -r line || [ -n \"$line\" ]; do\n  tmux send-keys -l -t \"$TARGET\" -- \"$line\"\n  tmux send-keys -t \"$TARGET\" Enter\ndone <<'%s'\n" heredoc-marker)
     body "\n"
     heredoc-marker "\n")))

(defun ob-tmux--ssh-execute-async (ob-session script &optional callback)
  "Execute SCRIPT on remote host via SSH asynchronously.
OB-SESSION must have :ssh set.
SCRIPT: complete shell script string to pipe to bash -s.
CALLBACK: optional function called with (output exit-code) when done."
  (let* ((ssh-host (ob-tmux--ssh ob-session))
	 (process-name (format "ob-tmux-ssh-%s" (ob-tmux--session ob-session)))
	 (buf (generate-new-buffer (format " *%s*" process-name)))
	 (proc (start-process process-name buf "ssh" ssh-host "bash" "-s")))
    (set-process-coding-system proc 'utf-8 'utf-8)
    (process-send-string proc script)
    (process-send-eof proc)
    (when callback
      (set-process-sentinel
       proc
       (lambda (process _event)
	 (when (memq (process-status process) '(exit signal))
	   (let ((output (with-current-buffer (process-buffer process)
			   (buffer-string)))
		 (exit-code (process-exit-status process)))
	     (funcall callback output exit-code)
	     (kill-buffer (process-buffer process)))))))
    proc))

(defun ob-tmux--ssh-dispatch (ob-session body result-params)
  "Dispatch fully non-blocking SSH execution for OB-SESSION with BODY.
RESULT-PARAMS determines whether output is captured."
  (let* ((session-name (ob-tmux--session ob-session))
	 (window-name (ob-tmux--window-default ob-session))
	 (output-p (member "output" result-params)))
    (if output-p
	;; Output capture path
	(let* ((uuid (org-id-uuid))
	       (src-block-pos (org-babel-where-is-src-block-head))
	       (org-buffer (current-buffer))
	       (script (ob-tmux--ssh-script-output
			session-name window-name uuid
			org-babel-tmux-ssh-timeout body)))
	  ;; Insert UUID placeholder immediately (non-blocking)
	  (save-excursion
	    (goto-char src-block-pos)
	    (end-of-line)
	    (org-babel-insert-result uuid '("output" "replace")))
	  ;; Launch async SSH process
	  (ob-tmux--ssh-execute-async
	   ob-session script
	   (lambda (output exit-code)
	     (if (= exit-code 0)
		 (when (buffer-live-p org-buffer)
		   (with-current-buffer org-buffer
		     (save-excursion
		       (goto-char src-block-pos)
		       (end-of-line)
		       (org-babel-insert-result
			(string-trim-right output)
			'("output" "replace")))))
	       (message "ob-tmux SSH error (exit %d): %s" exit-code output))))
	  nil)
      ;; Silent path (fire-and-forget)
      (let ((script (ob-tmux--ssh-script-silent
		     session-name window-name body)))
	(ob-tmux--ssh-execute-async
	 ob-session script
	 (lambda (_output exit-code)
	   (unless (= exit-code 0)
	     (message "ob-tmux SSH error (exit %d)" exit-code))))
	(message "ob-tmux: code sent to %s via SSH." session-name)
	nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Warnings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ob-tmux--deprecation-warning (org-header-terminal)
  (let* ((message (format "DEPRECATION WARNING: Setting `:terminal` using org source block header arguments is deprecated.

Consider changing your ob-tmux configuration as follows:

(setq org-babel-default-header-args:tmux
      '((:results . \"\")
        (:session . \"\")
        (:terminal. \"%s\")         ; <--- REMOVE THIS LINE
        (:socket  . nil)))

;; You can now customize the terminal and its options as follows:
(setq org-babel-tmux-terminal \"%s\")
(setq org-babel-tmux-terminal-opts '(\"-T\" \"ob-tmux\" \"-e\"))
; The default terminal is \"gnome-terminal\" with options \"--\".

If you have any source blocks containing `:terminal`, please consider removing them:

    #+begin_src tmux :session test :terminal %s
    echo hello
    #+end_src

Becomes:

    #+begin_src tmux :session test
    echo hello
    #+end_src

End of warning. (See *Warnings* buffer for full message)
" org-header-terminal org-header-terminal org-header-terminal)))
    (display-warning 'deprecation-warning message :warning)
    message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ob-tmux--open-file (path)
  "Open file as string.

Argument PATH: the location of the file."
(with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-substring (point-min) (point-max))))

(defun ob-tmux--test ()
  "Test if the default setup works.  The terminal should shortly flicker."
  (interactive)
  (let* ((random-string (format "%s" (random 99999)))
         (tmpfile (org-babel-temp-file "ob-tmux-test-"))
         (body (concat "echo '" random-string "' > " tmpfile))
         tmp-string)
    (org-babel-execute:tmux body org-babel-default-header-args:tmux)
    ;; XXX: need to find a better way to do the following
    (while (or (not (file-readable-p tmpfile))
	       (= 0 (length (ob-tmux--open-file tmpfile))))
      ;; do something, otherwise this will be optimized away
      (format "org-babel-tmux: File not readable yet."))
    (setq tmp-string (ob-tmux--open-file tmpfile))
    (delete-file tmpfile)
    (message (concat "org-babel-tmux: Setup "
                     (if (string-match random-string tmp-string)
                         "WORKS."
		       "DOESN'T work.")))))

(provide 'ob-tmux)



;;; ob-tmux.el ends here
