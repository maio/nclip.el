;;; nclip.el --- Network (HTTP) Clipboard

;; Copyright 2013 Marian Schubert
;;
;; Author: Marian Schubert <marian.schubert@gmail.com>
;; URL: http://www.github.com/maio/nclip.el
;; Version: 1
;; Keywords: nclip, clipboard, network
;; Package-Requires: ()

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Useful when you work on remote host inside terminal and you want to
;; use your local clipboard.

;;; Installation:

;; Available as a package in http://melpa.milkbox.net

;;; Usage:

;; M-x turn-on-nclip

;;; Code:

(require 'url)

(defvar nclip-server "http://127.0.0.1:2547/")

(defvar nclip--last-selection nil
  "Helper variable that ensures that we don't mess emacs kill ring.
It seems that it contains some metadata about killed item so this variable
makes possible to update kill ring only when content of clipboard changes.")

(defun nclip--set-selection (data)
  (let ((url-request-method "POST")
        (url-request-data data))
    (url-retrieve-synchronously nclip-server)))

(defun nclip--get-selection ()
  (with-current-buffer (url-retrieve-synchronously nclip-server)
    ;; body contains clipboard content
    (goto-char url-http-end-of-headers)
    (forward-line)
    (buffer-substring (point) (point-max))))

(defun nclip-cut (text &optional push)
  (nclip--set-selection text)
  (setq nclip--last-selection text))

(defun nclip-paste ()
  (let ((clip-text (nclip--get-selection)))
    (cond
     ((string= clip-text "")
      (setq nclip--last-selection nil))
     ((string= clip-text nclip--last-selection)
      nil)
     (t
      (setq nclip--last-selection clip-text)))))

(defun turn-on-nclip ()
  (interactive)
  (setq interprogram-cut-function 'nclip-cut)
  (setq interprogram-paste-function 'nclip-paste))

(defun turn-off-nclip ()
  (interactive)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))

(provide 'nclip)
