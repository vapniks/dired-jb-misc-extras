;;; dired-jb-misc-extras.el --- Miscellaneous extra dired related commands

;; Filename: dired-jb-misc-extras.el
;; Description: miscellaneous functions for `dired' and `image-dired'
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2015, Joe Bloggs, all rites reversed.
;; Created: 2015-08-28 15:30:22
;; Version: 0.1
;; Last-Updated: 2015-08-28 15:30:22
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/dired-jb-misc-extras
;; Keywords: unix
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires: 
;;
;; Features that might be required by this library:
;;
;; run-assoc
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 12k9zUo9Dgqk8Rary2cuzyvAQWD5EAuZ4q
;;
;; This library provides various miscellaneous `dired' related commands & functions
;; that I use occasionally. 
;;

;;; Commands:
;;
;; Below is a complete command list:
;;
;;  `dired-get-size'
;;    Get total size of marked files using linux du command. This only works on local directories.
;;  `dired-up-dir'
;;    In dired go up a directory and replace current buffer, instead of creating a new one.
;;  `dired-do-shell-command-regexp'
;;    Create and run shell commands from selected filenames which match REGEXP.
;;  `dired-find-file-other-window'
;;    Wrapper around dired-find-file-other-window.
;;  `image-dired-show-all-tags'
;;    Show all tags that have been used to tag files.
;;  `image-dired-display-thumbnail-original-image-fullsize'
;;    Display current thumbnail's original fullsize image in display buffer.
;;  `image-dired-rename-original'
;;    Rename original file corresponding to current thumbnail.
;;  `image-dired-copy-original'
;;    Copy original file corresponding to current thumbnail.
;;  `dired-copy-orglink-as-kill'
;;    Copy marked files in dired buffer to the ‘kill-ring’ as a list of org hyperlinks.
;;  `dired-copy-orglink-to-rectangle'
;;    Copy marked files in dired buffer to a rectangle (which can be yanked with ‘yank-rectangle’).
;;  `find-dired-preset'
;;    Find files in DIR using NAME args from `find-dired-presets'
;;
;; The following existing commands are advised:
;;
;;  `image-dired-display-thumbnail-original-image'
;;    Display current thumbnail's original image in display buffer.
;;  `image-dired-thumbnail-display-external'
;;    Display current thumbnail externally using `run-associated-program' (if installed).
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `find-dired-presets'
;;    Names sets of arguments for the find command when `find-dired-preset' is called.
;;  `dired-orglink-presets'
;;    Preset arguments for `file-name-as-orglink' used by `dired-copy-orglink-as-kill' and `dired-copy-orglink-to-rectangle'
;;
;;; Installation:
;;
;; Put dired-jb-misc-extras.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'dired-jb-misc-extras)

;;; Customize:
;;

;;; Require
(eval-when-compile (require 'cl))

;;;###autoload
(defun dired-get-size (&optional files bytesp)
  "Get total size of FILES (a list of filepaths) using linux du command.
When called interactively print the size of the marked files in
the message area.
If BYTESP is non-nil, or if called with a prefix arg return total
bytes, otherwise return in human readable form (e.g. 1K, 234M, 2G).
This only works on local directories."
  (interactive (list (dired-get-marked-files) current-prefix-arg))
  (let (num)
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil
	     (concat "-sc" (if bytesp "b" "h")) files)
      (setq num (progn (re-search-backward "\\(^[0-9.,]+.+\\).*total$")
		       (match-string 1)))
      (if (called-interactively-p 'any)
	  (message "Size of all marked files: %s" num)))
    num))

(cl-defun dired-convert-bytes (bytes &optional (precision 2))
  "Convert an integer number of BYTES to a human readable string.
Use PRECISION decimal places (default 2).
For example (dired-convert-bytes 10000 2) returns \"9.77KB\"."
  (if (< bytes 0) (error "Cant convert negative bytes arg"))
  (cond ((> (lsh bytes -30) 0)
	 (concat (format (concat "%." (number-to-string precision) "f")
			 (/ bytes 1073741824.0)) "GB"))
	((> (lsh bytes -20) 0)
	 (concat (format (concat "%." (number-to-string precision) "f")
			 (/ bytes 1048576.0)) "MB"))
	((> (lsh bytes -10) 0)
	 (concat (format (concat "%." (number-to-string precision) "f")
			 (/ bytes 1024.0)) "KB"))))

(defun dired-mark-until-size (size &optional movep)
  "Mark files & dirs from point onwards until their total size is >= SIZE, or there are no more.
If MOVEP is non-nil, or if called with a prefix argument, then move marker to the
end of the marked files, otherwise dont move it.
Return the total byte count of the marked files."
  (interactive (list (read-string "Size of files: ")
		     current-prefix-arg))
  (let* ((case-fold-search t)
	 (totalbytes (if (numberp size)
			 size
		       (* (string-to-number size)
			  (cond ((string-match "k" size) 1024)
				((string-match "m" size) 1048576)
				((string-match "g" size) 1073741824)
				(t 1)))))
	 (accum 0)
	 (count 0)
	 filename)
    (while (and (< accum totalbytes)
		(setq filename (ignore-errors (dired-filename-at-point))))
      (setq count (1+ count)
	    accum (+ accum
		     (string-to-number
		      (dired-get-size (list filename) t))))
      (dired-next-line 1))
    (let ((start (point)))
      (dired-mark (- count))
      (if movep (goto-char start)))
    (if (called-interactively-p 'any)
	(message "Total size of marked files = %s = %s bytes"
		 (dired-convert-bytes accum) accum))
    accum))

;;;###autoload
(defun dired-up-dir nil
  "In dired go up a directory and replace current buffer, instead of creating a new one."
  (interactive)
  (find-alternate-file ".."))

;;;###autoload
(defun dired-do-shell-command-regexp (regexp newname &optional arg whole-name)
  "Create and run shell commands from selected filenames which match REGEXP.
Shell command is created from prompted string, replacing \\=\\<n> or \\& as in `query-replace-regexp'.
REGEXP defaults to the last regexp used.
Output of shell commands (along with commands executed) will be displayed in `*Dired regexp shell commands output*'
which will pop-up underneath the dired buffer.

With non-zero prefix argument ARG, the command operates on the next ARG
files.  Otherwise, it operates on all the marked files, or the current
file if none are marked.

As each match is found, the user must type a character saying
  what to do with it.  For directions, type \\[help-command] at that time.

With a zero prefix arg, the regexp matches the absolute file name.
Normally, only the non-directory part of the file name is used.

Note: before running the shell command, it will cd into the directory containing the file,
      and this cd command will be displayed in the confirmation prompt."
  (interactive (dired-mark-read-regexp "Regexp shell command: "))
  (let* ((fn-list (dired-get-marked-files nil arg))
	 (operation-prompt "Do shell command: `%s'    ?")
	 (help-form "
  Type SPC or `y' to do shell command on this match, DEL or `n' to skip to next,
  `!' to do shell command on all remaining matches with no more questions.")
	 rename-regexp-query cdcmd maincmd cmd dir)
    (save-excursion
      (display-buffer (get-buffer-create "*Dired regexp shell commands output*"))
      (with-current-buffer "*Dired regexp shell commands output*"
	(goto-char (point-max))
	(insert "\n"))
      (dolist (from fn-list)
	(setq cdcmd (concat "cd " (file-name-directory from))
	      maincmd (if whole-name
			  (dired-string-replace-match regexp from newname)
			(dired-string-replace-match regexp (file-name-nondirectory from) newname))
	      cmd (concat cdcmd ";" maincmd))
	(if (dired-query 'rename-regexp-query operation-prompt (concat maincmd "\n"))
	    (progn (shell-command cmd)
		   (with-current-buffer "*Dired regexp shell commands output*"
		     (goto-char (point-max))
		     (insert "> " cmd "\n"))
		   (with-current-buffer "*Shell Command Output*"
		     (append-to-buffer "*Dired regexp shell commands output*" (point-min) (point-max))))
	  (dired-log "Shell command \"%s\" not executed\n" cmd))))
    (message "Shell commands completed")))

;;;###autoload
(defun dired-find-file-other-window (move)
  "Wrapper around dired-find-file-other-window.
  If called with a prefix arg then usual behaviour of moving point to window containing newly opened file will be performed.
  Otherwise point will be put back in the dired window."
  (interactive "P")
  (if move (dired-find-file-other-window)
    (progn (dired-find-file-other-window)
	   (other-window 1))))

;; The following functions/defadvice might not be necessary in future versions of image-dired
(defadvice image-dired-display-thumbnail-original-image (around editmode activate)
  (interactive)
  "Display current thumbnail's original image in display buffer."
  (if (equal (buffer-name) "*Image-Dired Edit Meta Data*")
      (let* ((thumb (plist-get
		     (cdr (get-text-property (point) 'display))
		     :file))
	     (thumb2 (if thumb (replace-regexp-in-string "\\.image-dired/" "" thumb)))
	     (file (if thumb2 (replace-regexp-in-string "\\.thumb" "" thumb2))))
	(if (not file)
	    (message "No original file name found")
	  (image-dired-create-display-image-buffer)
	  (display-buffer image-dired-display-image-buffer)
	  (image-dired-display-image file arg)))
    ad-do-it))

(if (featurep 'run-assoc)
    (defadvice image-dired-thumbnail-display-external (around editmode activate)
      "Display current thumbnail externally using `run-associated-program'."
      (interactive)
      (if (equal (buffer-name) "*Image-Dired Edit Meta Data*")
	  (let* ((thumb (plist-get
			 (cdr (get-text-property (point) 'display))
			 :file))
		 (thumb2 (if thumb (replace-regexp-in-string "\\.image-dired/" "" thumb)))
		 (file (if thumb2 (replace-regexp-in-string "\\.thumb" "" thumb2))))
	    (if (not file)
		(message "No original file name found")
	      (run-associated-program file)))
	ad-do-it)))

;;;###autoload
(defun image-dired-show-all-tags nil
  "Show all tags that have been used to tag files."
  (interactive)
  (image-dired-sane-db-file)
  (let ((dir (file-name-as-directory
	      (expand-file-name default-directory)))
	str tags)
    (image-dired--with-db-file
     ;; Collect tags
     (while (search-forward-regexp (concat dir "[^;]*;\\(.*\\)$") nil t)
       (setq str (concat str ";" (match-string 1)))))
    ;; Remove duplicates
    (if (not str)
	(message "No tags found!")
      (setq tags (split-string str "[;:]+" t)
	    tags (remove-duplicates tags :test 'equal)
	    tags (remove "comment" tags))
      (message "Tags: %s" (mapconcat 'identity tags " ")))))

;;;###autoload
(defun image-dired-display-thumbnail-original-image-fullsize nil
  "Display current thumbnail's original fullsize image in display buffer."
  (interactive)
  (progn (image-dired-display-thumbnail-original-image)
	 (other-window 1)
	 (image-dired-display-current-image-full)
	 (other-window 1)))

;;;###autoload
(defun image-dired-rename-original nil
  "Rename original file corresponding to current thumbnail."
  (interactive)
  (progn
    (display-buffer (image-dired-associated-dired-buffer))
    (image-dired-track-original-file)
    (image-dired-jump-original-dired-buffer)
    (dired-unmark-all-marks)
    (dired-do-rename)
    (revert-buffer)
    (other-window 1)
    (image-dired-delete-char)))

;;;###autoload
(defun image-dired-copy-original nil
  "Copy original file corresponding to current thumbnail."
  (interactive)
  (progn
    (display-buffer (image-dired-associated-dired-buffer))
    (image-dired-jump-original-dired-buffer)
    (dired-unmark-all-marks)
    (dired-do-copy)
    (revert-buffer)
    (other-window 1)
    (image-dired-delete-char)))

;;;###autoload
(cl-defun file-name-as-orglink (filepaths &optional dir (namefilter 'file-name-nondirectory)
					  prefix suffix)
  "Convert FILEPATHS to org hyperlink strings. FILEPATHS can be a single filepath or a list of them.
By default links will contain absolute filepaths, but if DIR is supplied, then links will
be relative to that directory.
The names of the links are created by passing the filepaths through the function supplied 
by NAMEFILTER which is `file-name-nondirectory' by default. 
Optional args PREFIX & SUFFIX are strings to prepend & append to the links, and will not 
become part of the link themselves, by default they are empty strings. 
Alternatively PREFIX & SUFFIX may also be functions used to generate prefix & suffix strings
from the filepath of each link (passed as an argument)."
  (let* ((paths (mapcar (if (and dir (not (equal dir "/")))
			    (lambda (x) (file-relative-name x dir)) 'identity)
			(if (stringp filepaths) (list filepaths) filepaths))))
    (mapcar (lambda (path) (concat (if (functionp prefix)
				       (funcall prefix path)
				     prefix)
				   "[[file:" path "]["
				   (funcall namefilter path)
				   "]]"
				   (if (functionp suffix)
				       (funcall suffix path)
				     suffix)))
	    paths)))
;;;###autoload
(defcustom dired-orglink-presets '(("space separated" relative file-name-nondirectory "" " ")
				   ("newline separated" relative file-name-nondirectory "" "\n")
				   ("org-table column" relative file-name-nondirectory "| " " |\n")
				   ("org-list" relative file-name-nondirectory " - " " \n")
				   ("org headers" relative file-name-nondirectory "** " " \n"))
  "Preset arguments for `file-name-as-orglink' used by `dired-copy-orglink-as-kill' and `dired-copy-orglink-to-rectangle'.
Each sublist takes the form (DESC DIRSYM NAMEFILTER PREFIX SUFFIX); DESC is a description of the preset,
DIRSYM is either 'relative, 'absolute or nil to indicate if links should be relative to `default-directory'
or absolute filepaths, or relative to a directory prompted from the user.
The remaining elements are used for arguments of the same name for `file-name-as-orglink'."
  :type '(repeat :tag "Presets" (list :tag "Presets"
				      (string :tag "Description")
				      (choice :tag "Link type"
					      (const :tag "Absolute" absolute)
					      (const :tag "Relative" relative)
					      (const :tag "Prompt relative dir" nil))
				      (function :tag "Link rename function"
						:help-echo "Function takes filepath as arg and returns linkname"
						:value file-name-nondirectory)
				      (string :tag "Prefix")
				      (string :tag "Suffix")))
  :group 'dired)

(defun dired-copy-orglink-get-args nil
  (let* ((descr (completing-read "Copy links as: "
				 (append (mapcar 'car dired-orglink-presets)
					 '("enter format manually"))))
	 (args (cdr (assoc descr dired-orglink-presets)))
	 (dir (cond ((equal current-prefix-arg '(16)) nil)
		    (current-prefix-arg (read-directory-name "Make links relative to dir: "))
		    ((eq (car args) 'relative) default-directory)
		    ((eq (car args) 'absolute) nil)
		    (t (read-directory-name "Make links relative to dir: ")))))
    (if (equal descr "enter format manually")
	(list
	 (read-directory-name "Make links relative to dir: ")
	 (read-from-minibuffer "Function to rename link (default 'file-name-nondirectory): "
			       nil nil t nil "file-name-nondirectory")
	 (read-from-minibuffer "Prefix string: ")
	 (read-from-minibuffer "Suffix string: "))
      (cons dir (cdr args)))))

;;;###autoload
(defun dired-copy-orglink-as-kill nil
  "Copy marked files in dired buffer to the `kill-ring' as a list of org hyperlinks.
Prompt the user for named preset arguments from `dired-orglink-presets' specifying the 
arguments to pass to `file-name-as-orglink'.
If a prefix key is used then a directory will be prompted for to make links relative to."  
  (interactive)
  (cl-destructuring-bind (dir namefilter prefix suffix)
      (dired-copy-orglink-get-args)
    (let* ((filepaths (or (dired-get-subdir)
			  (dired-get-marked-files)))
	   (links (file-name-as-orglink filepaths dir namefilter prefix suffix))
	   (linkstring (mapconcat 'identity links "")))
      (if (eq last-command 'kill-region)
	  (kill-append linkstring nil)
	(kill-new linkstring)))))

;;;###autoload
(defun dired-copy-orglink-to-rectangle nil
  "Like `dired-copy-orglink-as-kill' but save to rectangle instead of `kill-ring'.
The rectangle can be yanked with `yank-rectangle'."
  (interactive)
  (cl-destructuring-bind (dir namefilter prefix suffix)
      (dired-copy-orglink-get-args)
    (let* ((filepaths (or (dired-get-subdir)
			  (dired-get-marked-files)))
	   (links (file-name-as-orglink filepaths dir namefilter prefix suffix)))
      (setq killed-rectangle links))))

;;;###autoload
(defcustom find-dired-presets
  '(("images"
     "-type f -iregex '.*\\.\\(avif\\|bmp\\|eps\\|gif\\|heif\\|jpeg\\|jpg\\|png\\|svg\\|tiff\\|webp\\|xcf\\)$'")
    ("documents" "-type f -iregex '.*\\.\\(docx\\|doc\\|htm\\|html\\|odt\\|org\\|pdf\\|ppt\\|pptx\\|rtf\\|tex\\|txt\\)$'")
    ("data"
     "-type f -iregex '.*\\.\\(cdf\\|csv\\|dat\\|dkvp\\|h5\\|hdf5\\|json\\|mat\\|nc\\|Rdata\\|rds\\|sav\\|sdxf\\|sqlite\\|tsv\\|xls\\|xlsx\\|xml\\)$'")
    ("code"
     "-type f -iregex '.*\\.\\(ada\\|asm\\|awk\\|c\\|cl\\|clj\\|cljs\\|cpp\\|cs\\|do\\|el\\|exp\\|fasl\\|gawk\\|go\\|h\\|hpp\\|hs\\|java\\|js\\|k\\|l\\|lisp\\|lsp\\|lua\\|m\\|mlr\\|mod\\|php\\|pl\\|prg\\|py\\|r\\|rb\\|rs\\|scala\\|scm\\|sed\\|sh\\|sql\\|swift\\|tcl\\|txr\\|v\\|zsh\\)$'")
    ("configs"
     "-name '.*' -o \\( -type f -name '*config' \\) -o \\( -type f -name '*.yml' \\) ")
    ("files containing regexp"
     "-type f -a -exec grep -q '%1' {} \\\; " (lambda nil (read-regexp "grep regexp: ")))
    ("manual entry" "%1" (lambda nil (read-string "Run `find' (with args): "))))
  "Named sets of arguments for the find command when `find-dired-preset' is called.

Each element of this list is in the form (NAME ARGS PROMPT/FUNCTION...)
NAME is a name that the user can select when `find-dired-preset' is executed.
ARGS is a string of arguments for find which may contain placeholders %1, %2, etc.
and it should not contain the initial search directory, or the final \"-exec ls -ld {} \;\"
The PROMPT/FUNCTION elements correspond with the placeholders, and define what to replace
each placeholder with; either a string prompted from the user, or the return value of a FUNCTION."
  :type '(repeat
	  (list :tag "Named args"
		(string :tag "Name" :help-echo "Name for this set of args")
		(string :tag "find args"
			:help-echo "Arguments for find command excluding directory")
		(repeat :inline t
			(choice (string :tag "Prompt")
				(function :tag "Function"
					  :help-echo
					  "Function must return a string to replace placeholder in args")))))
  :group 'find-dired)
;;;###autoload
(defcustom find-dired-preset-ls-option
  '(("name" . ("-exec ls -ld {} +" . "-ld"))
    ("extension" . ("-exec ls -ldX {} +" . "-ldX"))
    ("version" . ("-exec ls -ldv {} +" . "-ldv"))
    ("modification time" . ("-exec ls -ldt {} +" . "-ldt"))
    ("status change time" . ("-exec ls -ldtc {} +" . "-ldtc"))
    ("access time" . ("-exec ls -ldtu {} +" . "-ldtu"))
    ("directories first" . ("-exec ls -ld --group-directories-first {} +" . "-ld --group-directories-first")))
  "Presets for `find-ls-option'. 
Each element is a cons cell whose key is a name that the user can select when `find-dired-preset' is run,
and whose value is a cons cell of the same form as `find-ls-option'."
  :type '(alist :key-type (string :tag "name")
		:value-type (cons :tag "value for `find-ls-option'"
				  (string :tag "final find option" :value "-exec ls -ld {} +")
				  (string :tag "ls switches" :value "-ld")))
  :group 'find-dired)

;;;###autoload
(defun find-dired-preset (dir name &optional lsopts)
  "Find files in DIR using NAME args from `find-dired-presets'.
When called interactively DIR & NAME will be prompted for. If a prefix arg is used then the user
is offered a list of recently visited directories to choose from."
  (interactive (list (if current-prefix-arg
			 (completing-read "Recent dir: "
					  (let ((items))
					    (dolist (item file-name-history)
					      (if (and (stringp item)
						       (not (string-match ":" item))
						       (> (length item) 0))
						  (let ((itemd (file-name-directory item)))
						    (if (and (stringp itemd)
							     (file-directory-p itemd)
							     (not (member itemd items)))
							(add-to-list 'items itemd t)))))
					    items))
		       (read-directory-name "Dir: " nil nil t))
		     (completing-read "File types: "
				      (mapcar 'car find-dired-presets))
		     (completing-read "Sort by: " find-dired-preset-ls-option)))
  (require 'find-dired)
  (let* ((args (assoc name find-dired-presets))
	 (argstr (cadr args))
	 (replacements (cddr args))
	 (find-ls-option (if (stringp lsopts)
			     (cdr (assoc lsopts find-dired-preset-ls-option))
			   lsopts)))
    (cl-loop for repl in replacements
	     for i from 1
	     do (setq argstr
		      (string-replace (concat "%" (number-to-string i))
				      (if (stringp repl)
					  (read-string repl)
					(funcall repl))
				      argstr)))
    (find-dired dir argstr)))

(when (boundp 'menu-bar-run-find-menu)
  (easy-menu-add-item menu-bar-run-find-menu nil
		      ["Find Files Using Preset..." find-dired-preset]
		      "`find' <anything>..."))

(easy-menu-add-item global-map '("menu-bar" "Tools") ["(unix) find files..." find-dired-preset t] "Compile...")
;;(easy-menu-remove-item global-map '("menu-bar" "Tools") "(unix) find files...")

(easy-menu-add-item dired-mode-map '("menu-bar" "operate")
		    ["Copy File Names As Orglinks (to kill-ring)" dired-copy-orglink-as-kill t]
		    "Copy File Names (to Paste)")
;;(easy-menu-remove-item dired-mode-map '("menu-bar" "operate") "Copy File Names As Orglinks (to kill-ring)")
(easy-menu-add-item dired-mode-map '("menu-bar" "operate") 
		    ["Copy File Names As Orglinks (to rectangle)" dired-copy-orglink-to-rectangle t]
		    "Copy File Names (to Paste)")
;;(easy-menu-remove-item dired-mode-map '("menu-bar" "operate") "Copy File Names As Orglinks (to rectangle)")

(provide 'dired-jb-misc-extras)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "dired-jb-misc-extras.el" (buffer-name) (buffer-string) "update")

;;; dired-jb-misc-extras.el ends here
