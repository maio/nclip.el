;;; nclip.el --- Network (HTTP) Clipboard

;; Copyright 2013 Marian Schubert
;;
;; Author: Marian Schubert <marian.schubert@gmail.com>
;; URL: http://www.github.com/maio/nclip.el
;; Version: 2
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

;; nclip.el is network clipboard for Emacs which makes possible to use
;; your local clipboard even when you are running Emacs inside
;; terminal on remote server.
;;
;; When enabled, it will use remote HTTP server to fetch and update
;; clipboard content. Included HTTP server `nclip.rb` supports OSX
;; (pbcopy/pbpaste) but it should be pretty straightforward to support
;; other systems as well (e.g. Linux using xclip).

;;; Installation:

;; Available as a package in http://melpa.milkbox.net

;;; Usage:

;; Run `nclip.rb` on your local machine. Setup SSH port forwarding so
;; that when you SSH to remote server it will forward `2547` port on
;; remote server over SSH to `127.0.0.1:2547`.
;;
;; From command line:
;;
;;    ssh -R 2547:127.0.0.1:2547 some-server
;;
;; Or you can use ~/.ssh/config:

;;    Host some-server
;;      RemoteForward 127.0.0.1:2547 127.0.0.1:2547
;;

;; Run Emacs on remote server and enable nclip using:
;;
;;     M-x turn-on-nclip

;;; Code:

(require 'url)

(defvar nclip-auth-token (or (getenv "NCLIP_AUTH_TOKEN") "my-token"))
(defvar nclip-server "http://127.0.0.1:2547/")

(defvar nclip--last-selection nil
  "Helper variable that ensures that we don't mess emacs kill ring.
It seems that it contains some metadata about killed item so this variable
makes possible to update kill ring only when content of clipboard changes.")

(defun nclip--noop (status) nil)

(defun nclip--build-url ()
  (concat nclip-server "?" nclip-auth-token))

(defun nclip--set-selection (data)
  (let ((url-request-method "POST")
        (url-request-data data))
    (url-retrieve (nclip--build-url) 'nclip--noop)))

(defun nclip--get-selection ()
  (with-temp-buffer
    (url-insert-file-contents (nclip--build-url))
    (buffer-string)))

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

;;;###autoload
(defun turn-on-nclip ()
  (interactive)
  (setq interprogram-cut-function 'nclip-cut)
  (setq interprogram-paste-function 'nclip-paste))

(defun turn-off-nclip ()
  (interactive)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))

(provide 'nclip)
;;; nclip.el ends here
