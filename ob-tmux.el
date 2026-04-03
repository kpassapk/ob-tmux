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

(defcustom org-babel-tmux-poll-interval 1.0
  "Polling interval in seconds for async output capture."
  :group 'org-babel
  :type 'number)

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
           (session-alive (ob-tmux--session-alive-p ob-session))
	   (window-alive (ob-tmux--window-alive-p ob-session)))
      ;; Create tmux session and window if they do not yet exist
      (unless session-alive (ob-tmux--create-session ob-session))
      (unless window-alive (ob-tmux--create-window ob-session))
      ;; Start terminal window if the session does not yet exist
      (unless session-alive
	(ob-tmux--start-terminal-window ob-session terminal))
      ;; Wait until tmux window is available
      (while (not (ob-tmux--window-alive-p ob-session)))
      ;; Disable window renaming from within tmux
      (ob-tmux--disable-renaming ob-session)
      (let* ((expanded-body (org-babel-expand-body:generic body params vars))
	     (result-params (cdr (assq :result-params params)))
	     (output-p (and (member "output" result-params)
			    (ob-tmux--ssh ob-session))))
	(if output-p
	    ;; Async output capture: wrap with markers, poll for results
	    (let ((uuid (org-id-uuid))
		  (src-block-pos (org-babel-where-is-src-block-head)))
	      (ob-tmux--send-body-with-markers ob-session expanded-body uuid)
	      ;; Insert UUID placeholder as #+RESULTS
	      (save-excursion
		(goto-char src-block-pos)
		(end-of-line)
		(org-babel-insert-result uuid '("output" "replace")))
	      (ob-tmux--start-output-poll
	       ob-session uuid (current-buffer) src-block-pos)
	      ;; Return nil; we handle results ourselves
	      nil)
	  ;; Default: send body silently
	  (ob-tmux--send-body ob-session expanded-body)
	  ;; Warn that setting the terminal from the org source block
	  ;; header arguments is going to be deprecated.
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
;; Async output capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ob-tmux--capture-pane (ob-session)
  "Capture the contents of the tmux pane for OB-SESSION."
  (ob-tmux--execute-string ob-session
   "capture-pane" "-p" "-t" (concat "'" (ob-tmux--target ob-session) "'")))

(defun ob-tmux--send-body-with-markers (ob-session body uuid)
  "Send BODY to OB-SESSION wrapped with start/end markers identified by UUID.
All commands are concatenated into a single send-keys call to preserve ordering."
  (let* ((start-cmd (concat "echo 'OB_TMUX_START_" uuid "'"))
	 (end-cmd (concat "echo 'OB_TMUX_END_" uuid "'"))
	 (full-body (concat start-cmd "\n" body "\n" end-cmd "\n")))
    (ob-tmux--send-keys ob-session full-body)))

(defun ob-tmux--extract-output (pane-text uuid)
  "Extract command output from PANE-TEXT between markers identified by UUID.
Returns the text between the marker lines, excluding the markers themselves,
shell prompts, and the echo commands used for markers."
  (let* ((start-re (concat "^OB_TMUX_START_" (regexp-quote uuid) "$"))
	 (end-re (concat "^OB_TMUX_END_" (regexp-quote uuid) "$"))
	 (prompt-re "^[^$\n]*\\$ ")
	 (lines (split-string pane-text "\n"))
	 (collecting nil)
	 (result nil))
    (dolist (line lines)
      (cond
       ((string-match-p end-re line)
	(setq collecting nil))
       (collecting
	(unless (string-match-p prompt-re line)
	  (push line result)))
       ((string-match-p start-re line)
	(setq collecting t))))
    (let ((output (string-join (nreverse result) "\n")))
      (string-trim-right output))))

(defun ob-tmux--start-output-poll (ob-session uuid org-buffer src-block-pos)
  "Start polling for output from OB-SESSION.
Looks for end marker identified by UUID in the tmux pane.
When found, extracts the output and replaces the UUID placeholder in ORG-BUFFER."
  (let ((timer nil))
    (setq timer
	  (run-with-timer
	   org-babel-tmux-poll-interval
	   org-babel-tmux-poll-interval
	   (lambda ()
	     (condition-case err
		 (let* ((pane-text (ob-tmux--capture-pane ob-session))
			(end-marker (concat "OB_TMUX_END_" uuid))
			(found (seq-find
				(lambda (l) (string-equal l end-marker))
				(split-string pane-text "\n"))))
		   (when found
		     (cancel-timer timer)
		     (let ((output (ob-tmux--extract-output pane-text uuid)))
		       (when (buffer-live-p org-buffer)
			 (with-current-buffer org-buffer
			   (save-excursion
			     (goto-char src-block-pos)
			     (end-of-line)
			     (org-babel-insert-result
			      output '("output" "replace")))))
		       (message "ob-tmux: output captured."))))
	       (error
		(cancel-timer timer)
		(message "ob-tmux poll error: %S" err))))))))

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
