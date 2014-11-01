;;; edbi-auto-connect.el --- Automatically connect to DB

;; Copyright (C) 2014 by IMAKADO


;; Prefix: edbi-aconn:
;; Author: Kenji Imakado <ken.imakado -at- gmail.com>
;; Maintainer: imakado
;; Created: :2014-11-01
;; Keywords: 
;; URL:
;; Version: 0.0.1
;; Package-Requires: ((imakado "0.12")  (edbi "0.1.3"))

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

;; 1) Make .edbi-data-source.json file in the project root directory.
;; Following lines are sample config:

;; // .edbi-data-source.json
;; {
;;     "Data-Source": "dbi:mysql:database=wp",
;;     "User-Name": "root",
;;     "Auth": ""
;; }

;; 2) M-x `edbi-aconn:auto-connect-if-need' to connect to the DB.

;;; Code:

(require 'json)
(require 'imakado)
(eval-when-compile
  (require 'cl))

(require 'edbi)


(defvar edbi-aconn:auto-connect-dbview-open-function
  'edbi-aconn:auto-connect-dbview-open-function-default)

(defun edbi-aconn:auto-connect-dbview-open-function-default (conn)
  (save-window-excursion
    (edbi:dbview-open conn)))

(defvar edbi-aconn:auto-connect-dbview-quietly nil)
;; (setq edbi-aconn:auto-connect-dbview-open-function 'ignore)


(defun edbi-aconn:current-directory ()
  (file-name-directory
   (expand-file-name
    (or (buffer-file-name)
        default-directory))))

(defun edbi-aconn:read-connection-json (file)
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-file file)))


(defun* edbi-aconn:root-detector (current-dir)
  (let* ((current-dir (expand-file-name current-dir)))
    (loop for filename in (directory-files current-dir t)
          when (string-match (rx bol ".edbi-data-source.json" eol) (file-name-nondirectory filename))
          do (return filename))))

(defvar edbi-aconn:get-root-directory-limit 15)
(defun edbi-aconn:find-edbi-data-source-json-file ()
  (interactive)
  (let ((cur-dir (edbi-aconn:current-directory)))
    (ignore-errors
      (loop with count = 0
            for json-file = (edbi-aconn:root-detector cur-dir)
            until json-file
            if (= count edbi-aconn:get-root-directory-limit)
            do (return nil)
            else
            do (progn (incf count)
                      (setq cur-dir (expand-file-name (concat cur-dir "../"))))
            finally return json-file))))

(defmacro edbi-aconn:with-edbi-connection (&rest body)
  (declare (debug (body)))
  `(when (and edbi:dbview-buffer-name
              (buffer-live-p (get-buffer edbi:dbview-buffer-name)))
     (with-current-buffer edbi:dbview-buffer-name
       (when edbi:connection
         ,@body))))

(defun edbi-aconn:should-connect (uri-value-str)
  (let ((conn (edbi-aconn:with-edbi-connection edbi:connection)))
    (cond ((imakado-aand conn
                         (epc:manager-title
                          (edbi:connection-mngr conn))
                         (string-match (rx-to-string `(seq bol ,uri-value-str eol))
                                       it))
           nil)
          (t t))))

(defun edbi-aconn:auto-connect-if-need ()
  (interactive)
  (imakado-when-let (json-file (edbi-aconn:find-edbi-data-source-json-file))
    (imakado-awhen (edbi-aconn:read-connection-json json-file)
      (let* ((uri-value (assoc-default "Data-Source" it)))
        (when (edbi-aconn:should-connect uri-value)
          (edbi-aconn:auto-connect))))))

(defun edbi-aconn:auto-connect ()
  (interactive)
  (imakado-when-let (json-file (edbi-aconn:find-edbi-data-source-json-file))
    (imakado-awhen (edbi-aconn:read-connection-json json-file)
      (let* ((uri-value (assoc-default "Data-Source" it))
             (username (assoc-default "User-Name" it))
             (auth (assoc-default "Auth" it)))
        (let* ((data-source (edbi:data-source
                             uri-value
                             username
                             auth))
               (conn (edbi:start))
               (errmsg (condition-case err
                           (progn (edbi:connect conn data-source)
                                  nil)
                         (error (format "%s" err)))))
          ;; need this?
          (edbi:ds-history-add data-source)
          (if errmsg
              (message errmsg)
            (funcall edbi-aconn:auto-connect-dbview-open-function conn)
            (unless edbi-aconn:auto-connect-dbview-quietly
              (message "\"%s\": connect OK." uri-value))))))))



(provide 'edbi-auto-connect)

;;; edbi-auto-connect.el ends here
