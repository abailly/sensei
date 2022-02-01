;;; sensei.el --- An emacs sensei client  -*- lexical-binding: t; -*-

;; Copyright (c) Arnaud Bailly - 2022
;; Author: Arnaud Bailly <arnaud@pankzsoft.com>
;; Package-Requires: (package request)
;; Keywords: web
;; Version: 0.38.0

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials provided
;;       with the distribution.
;;
;;     * Neither the name of Author name here nor the names of other
;;       contributors may be used to endorse or promote products derived
;;       from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;;; Code:
(require 'package)

(use-package request
  :ensure t)

(defun sensei-insert-timestamp-iso ()
  "Insert the current timestamp (ISO 8601 format)."
  (format-time-string "%Y-%m-%dT%TZ"))

(defun sensei-read-config ()
  "Read sensei 'client.json' file from default XDG location."
  (json-read-file "~/.config/sensei/client.json")
  )

(defvar sensei-cur-directory
  "Used to set current directory when recording notes.

This variable is updated every time one starts recording a note,
according to what 'projectile-project-root' says for the current
directory.

TODO: Remove this global variable and find a way to pass the
project directory when starting note edition.
")

(defun sensei-send-event-note (directory)
  "Record notes in sensei from the current context.

DIRECTORY is the directory to record the note for."
  (let* ((config (sensei-read-config))
         (auth-token (cdr (assoc 'authToken config)))
         (username (cdr (assoc 'configUser config)))
         (server-uri (cdr (assoc 'serverUri config))))
    (request (concat server-uri "api/log/" username)
      :method "POST"
      :data (json-encode
             `((("tag" . "Note")
                ("_version" . 9)
                ("noteUser" . ,username)
                ("noteTimestamp" . ,(insert-timestamp-iso))
                ("noteDir" . ,directory)
                ("noteContent" . ,(buffer-substring-no-properties (point-min) (point-max))))))
      :headers `(("Content-Type" . "application/json")
                 ("X-API-Version" . "0.38.0")
                 ("Authorization" . ,(concat "Bearer " auth-token)))
      :parser 'json-read
      :error  (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                             (message "Got error: %S" error-thrown)))
      :success  (cl-function (lambda (&key data &allow-other-keys)
                               (message "Succesfully recorded note"))))))

(defun sensei-send-note-and-close ()
  "Record current buffer as note and cloes it.

DIRECTORY is the project to record note for."
  (interactive)
  (sensei-send-event-note sensei-cur-directory)
  (kill-buffer (current-buffer)))

(defun sensei-record-note ()
  "Interactive function to record some note."
  (interactive)
  (let ((buffer-name "*sensei-note*")
        (directory (projectile-project-root)))
    (get-buffer-create buffer-name)
    (message "In dir %S" directory)
    (setq sensei-cur-directory directory)
    (switch-to-buffer buffer-name)
    (use-local-map nil)
    (local-set-key
     (kbd "C-c C-c")
     'sensei-send-note-and-close)
  ))

(provide 'sensei)
;;; sensei.el ends here
