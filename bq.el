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

(defcustom bq-ls-option ""
  "Default option for bq ls command."
  :group 'bq)

(defcustom bq-switch-buffer-function #'switch-to-buffer-other-window
  "Function to change bq output buffer."
  :group 'bq)

(defcustom bq-dataset ""
  "Default dataset for bq."
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
  (message "%s" cmd)
  (let* ((buf (bq-get-buffer-create))
         (process (start-process-shell-command "bq"
                                               buf
                                               cmd)))
    (setq bq-process process)
    (funcall bq-switch-buffer-function
             (process-buffer process))))

(defun bq-compose-command (&rest args)
  (let ((cmd (format "%s %s"
                     bq-command
                     (mapconcat #'shell-quote-argument
                                (cl-remove-if #'(lambda (str) (< (length str) 1))
                                              args)
                                " "))))
    cmd))

(defun bq-read-options ()
  (read-from-minibuffer "Options: "))

(defun bq-read-dataset ()
  (read-from-minibuffer "Dataset: "
                        bq-dataset))

(defun bq-read-table ()
  (read-from-minibuffer "Table: "))

(defun bq-help ()
  (interactive)
  (let* ((options (bq-read-options))
         (cmd (bq-compose-command "help"
                                  options)))
    (bq-process-start cmd)))

(defun bq-ls ()
  (interactive)
  (let* ((options (bq-read-options))
         (dataset (bq-read-dataset))
         (cmd (bq-compose-command "ls"
                                  bq-ls-option
                                  options
                                  dataset)))
    (bq-process-start cmd)))

(defun bq--query (query)
  (let* ((options (bq-read-options))
         (cmd (bq-compose-command "query"
                                  query)))
    (bq-process-start cmd)))

(defun bq-query ()
  (interactive)
  (let* ((options (bq-read-options))
         (query (read-from-minibuffer "Query: "))
         (cmd (bq-compose-command "query"
                                  query)))
    (bq-process-start cmd)))

(defun bq-qurery-from-region (beg end)
  (interactive "r")
  (let* ((query (buffer-substring-no-properties beg end)))
    (bq--query query)))

(defun bq-rm ()
  (interactive)
  (let* ((options (bq-read-options))
         (dataset (bq-read-dataset))
         (table (bq-read-table))
         (cmd (bq-compose-command "rm"
                                  options
                                  (if table
                                      (format "%s.%s"
                                              dataset
                                              table)
                                    dataset))))
    (if (yes-or-no-p (format "Run command \"%s\" ?"
                             cmd))
        (bq-process-start cmd))))


;; (find-file-read-args
;;  "Select File: "
;;  (confirm-nonexistent-file-or-buffer))



(provide 'bq)
;;; bq.el ends here
