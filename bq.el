;;; bq.el --- interact with bq command               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defcustom bq-command "bq"
  "Executable for bq command location."
  :group 'bq)

(defcustom bq-switch-buffer-function #'switch-to-buffer-other-window
  "Function to change bq output buffer."
  :group 'bq)

(defcustom bq-dataset ""
  "Default dataset for bq."
  :group 'bq)

(defcustom bq-query-default-flags ""
  "Default flags for bq query command."
  :group 'bq)

(defcustom bq-query-default-global-flags ""
  "Default global flags for bq query command."
  :group 'bq)

(defcustom bq-ls-default-flags ""
  "Default option for bq ls command."
  :group 'bq)

(defvar bq-process)

(define-derived-mode bq-mode fundamental-mode "Bq"
  "")

(defun bq-get-buffer-create ()
  (let ((buf (get-buffer-create "*Bq - Output*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (bq-mode)
      (delete-region (point-min) (point-max)))
    buf))

(defun bq-process-start (cmd)
  (let ((buf (bq-get-buffer-create)))
    (with-current-buffer buf
      (goto-char (point-min))
      (insert (format "Executing: %s" cmd))
      (goto-char (point-max)))
    (let ((process (start-process-shell-command "bq" buf cmd)))
      (setq bq-process process)
      (funcall bq-switch-buffer-function
               (process-buffer process)))))

(defun bq-compose-command (&rest args)
  (let ((cmd (format "%s %s"
                     bq-command
                     (mapconcat #'shell-quote-argument
                                (cl-remove-if #'(lambda (str) (< (length str) 1))
                                              args)
                                " "))))
    cmd))

(defun bq-read-global-flags (default)
  (read-from-minibuffer "Global Flags: " default))

(defun bq-read-flags (default)
  (read-from-minibuffer "Command Flags: " default))

(defun bq-read-dataset ()
  (read-from-minibuffer "Dataset: "
                        bq-dataset))

(defun bq-read-table ()
  (read-from-minibuffer "Table: "))

(cl-defmacro bq-let-flags ((&optional (cmd-flags nil)
                                      (global-flags nil))
                           &body body)
  `(let ((global-flags (bq-read-global-flags ,global-flags))
         (flags (bq-read-flags ,cmd-flags)))
     ,@body))

;;;###autoload
(defun bq-help ()
  (interactive)
  (let* ((flags (bq-read-flags nil))
         (cmd (bq-compose-command "help"
                                  flags)))
    (bq-process-start cmd)))

;;;###autoload
(defun bq-ls ()
  (interactive)
  (bq-let-flags
   (bq-ls-default-flags)
   (let* ((dataset (bq-read-dataset))
          (cmd (bq-compose-command "ls"
                                   flags
                                   dataset)))
     (bq-process-start cmd))))

(defun bq--query (query)
  (bq-let-flags
   (bq-query-default-flags bq-query-default-global-flags)
   (let* ((cmd (bq-compose-command global-flags
                                   "query"
                                   flags
                                   query)))
     (bq-process-start cmd))))

;;;###autoload
(defun bq-query ()
  (interactive)
  (let* ((query (read-from-minibuffer "Query: ")))
    (bq--query query)))

;;;###autoload
(defun bq-query-from-region (beg end)
  (interactive "r")
  (let* ((query (buffer-substring-no-properties beg end)))
    (bq--query query)))

;;;###autoload
(defun bq-rm ()
  (interactive)
  (bq-let-flags
   ()
   (let* ((dataset (bq-read-dataset))
          (table (bq-read-table))
          (cmd (bq-compose-command "rm"
                                   flags
                                   (if table
                                       (format "%s.%s"
                                               dataset
                                               table)
                                     dataset))))
     (if (yes-or-no-p (format "Run command \"%s\" ?"
                              cmd))
         (bq-process-start cmd)))))

;;;###autoload
(defun bq-show ()
  (interactive)
  (bq-let-flags
   ()
   (let* ((dataset (bq-read-dataset))
          (table (bq-read-table))
          (cmd (bq-compose-command "show"
                                   flags
                                   (format "%s.%s"
                                           dataset
                                           table))))
     (bq-process-start cmd))))

(defcustom bq-load-default-flags ""
  "Default flags for bq load command."
  :group 'bq)


;; Examples:
;; bq load ds.new_tbl ./info.csv ./info_schema.json
;; bq load ds.new_tbl gs://mybucket/info.csv ./info_schema.json
;; bq load ds.small gs://mybucket/small.csv name:integer,value:string
;; bq load ds.small gs://mybucket/small.csv field1,field2,field3
;;;###autoload
(defun bq-load ()
  (interactive)
  (let ((use-local-file-as-source (yes-or-no-p "Use Local File as Source?")))
    (bq-let-flags (bq-load-default-flags)
                  (let* ((dataset (bq-read-dataset))
                         (table (bq-read-table))
                         (data (if use-local-file-as-source
                                   (car (find-file-read-args
                                         "Select Source File: "
                                         (confirm-nonexistent-file-or-buffer)))
                                 (read-from-minibuffer "Source Data URL: ")))
                         (schema (car (find-file-read-args
                                       "Select Schema: "
                                       (confirm-nonexistent-file-or-buffer))))
                         (cmd (bq-compose-command global-flags
                                                  "load"
                                                  flags
                                                  (format "%s.%s" dataset table)
                                                  data
                                                  schema)))
                    (bq-process-start cmd)))))


;; (find-file-read-args
;;  "Select File: "
;;  (confirm-nonexistent-file-or-buffer))



(provide 'bq)
;;; bq.el ends here
