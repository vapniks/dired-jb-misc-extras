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
;;
;; The following existing commands are advised:
;;
;;  `image-dired-display-thumbnail-original-image'
;;    Display current thumbnail's original image in display buffer.
;;  `image-dired-thumbnail-display-external'
;;    Display current thumbnail externally using `run-associated-program' (if installed).

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

(provide 'dired-jb-misc-extras)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "dired-jb-misc-extras.el" (buffer-name) (buffer-string) "update")

;;; dired-jb-misc-extras.el ends here
