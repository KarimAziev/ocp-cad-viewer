;;; ocp-cad-viewer.el --- Emacs control layer for OCP CAD Viewer -*- lexical-binding: t; -*-

;; Copyright (C) Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; Keywords: tools
;; Package-Requires: ((emacs "29.1") (websocket "1.16"))
;; URL: https://github.com/KarimAziev/ocp-cad-viewer
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Control a standalone OCP CAD Viewer running at
;; http://127.0.0.1:<port>/viewer from Emacs.  Commands are sent directly to
;; the viewer websocket using the C:<json> protocol implemented by ocp_vscode.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'websocket)
(require 'transient)

(defgroup ocp-cad-viewer nil
  "Control OCP CAD Viewer from Emacs."
  :group 'tools
  :prefix "ocp-cad-viewer-")

(defcustom ocp-cad-viewer-host "127.0.0.1"
  "Host where the standalone OCP CAD Viewer websocket listens."
  :type 'string
  :group 'ocp-cad-viewer)

(defcustom ocp-cad-viewer-port 3939
  "Port where the standalone OCP CAD Viewer websocket listens."
  :type 'integer
  :group 'ocp-cad-viewer)

(defcustom ocp-cad-viewer-rotate-step 10
  "Default rotate step, in degrees."
  :type 'number
  :group 'ocp-cad-viewer)

(defcustom ocp-cad-viewer-pan-step 10
  "Default pan step."
  :type 'number
  :group 'ocp-cad-viewer)

(defcustom ocp-cad-viewer-zoom-step 100
  "Default zoom command delta."
  :type 'number
  :group 'ocp-cad-viewer)

(defcustom ocp-cad-viewer-auto-connect t
  "Whether camera commands should connect automatically when needed."
  :type 'boolean
  :group 'ocp-cad-viewer)

(defcustom ocp-cad-viewer-message-functions nil
  "Hook run with decoded websocket status messages from the viewer.
Each function receives the decoded JSON object."
  :type 'hook
  :group 'ocp-cad-viewer)

(defvar ocp-cad-viewer--socket nil
  "Current websocket connection to the OCP CAD Viewer.")

(defvar ocp-cad-viewer--pending-frames nil
  "Frames queued while the websocket handshake is still in progress.")

(defvar ocp-cad-viewer--last-status nil
  "Most recent decoded status payload received from the viewer.")

(defun ocp-cad-viewer--viewer-url (&optional host port)
  "Return the standalone viewer URL for HOST and PORT."
  (format "http://%s:%d/viewer"
          (or host ocp-cad-viewer-host)
          (or port ocp-cad-viewer-port)))

(defun ocp-cad-viewer--websocket-url (&optional host port)
  "Return the websocket URL for HOST and PORT."
  (format "ws://%s:%d"
          (or host ocp-cad-viewer-host)
          (or port ocp-cad-viewer-port)))

(defun ocp-cad-viewer-connected-p ()
  "Return non-nil when the viewer websocket is currently open."
  (and ocp-cad-viewer--socket
       (websocket-openp ocp-cad-viewer--socket)))

(defun ocp-cad-viewer--decode-frame (frame)
  "Decode websocket FRAME as JSON when possible."
  (let ((text (websocket-frame-text frame)))
    (when (and text (not (string-empty-p text)))
      (condition-case nil
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-key-type 'symbol))
            (json-read-from-string text))
        (error text)))))

(defun ocp-cad-viewer--on-message (_socket frame)
  "Handle an incoming websocket FRAME."
  (setq ocp-cad-viewer--last-status (ocp-cad-viewer--decode-frame frame))
  (run-hook-with-args
   'ocp-cad-viewer-message-functions
   ocp-cad-viewer--last-status))

(defun ocp-cad-viewer--on-close (_socket)
  "Handle viewer websocket close."
  (setq ocp-cad-viewer--socket nil))

(defun ocp-cad-viewer--on-error (_socket callback error-data)
  "Handle websocket callback CALLBACK failure with ERROR-DATA."
  (message "OCP CAD Viewer websocket %s error: %S" callback error-data))

(defun ocp-cad-viewer--send-frame (socket frame)
  "Send protocol FRAME to SOCKET."
  (websocket-send-text socket frame))

(defun ocp-cad-viewer--flush-pending (&optional socket)
  "Send queued frames through SOCKET or the current viewer socket."
  (let ((target (or socket ocp-cad-viewer--socket)))
    (when (and target (websocket-openp target))
      (dolist (frame (nreverse ocp-cad-viewer--pending-frames))
        (ocp-cad-viewer--send-frame target frame))
      (setq ocp-cad-viewer--pending-frames nil))))

;;;###autoload
(defun ocp-cad-viewer-connect (&optional port host)
  "Connect to the OCP CAD Viewer websocket.
With prefix argument PORT, prompt for the port.  HOST defaults to
`ocp-cad-viewer-host'."
  (interactive
   (list (when current-prefix-arg
           (read-number "OCP CAD Viewer port: " ocp-cad-viewer-port))
         nil))
  (let* ((target-host (or host ocp-cad-viewer-host))
         (target-port (or port ocp-cad-viewer-port))
         (url (ocp-cad-viewer--websocket-url target-host target-port)))
    (when (ocp-cad-viewer-connected-p)
      (ocp-cad-viewer-disconnect))
    (setq ocp-cad-viewer-host target-host
          ocp-cad-viewer-port target-port
          ocp-cad-viewer--socket
          (websocket-open
           url
           :on-open (lambda (socket)
                      (setq ocp-cad-viewer--socket socket)
                      (ocp-cad-viewer--flush-pending socket)
                      (message "OCP CAD Viewer connected: %s" url))
           :on-message #'ocp-cad-viewer--on-message
           :on-close #'ocp-cad-viewer--on-close
           :on-error #'ocp-cad-viewer--on-error))
    ocp-cad-viewer--socket))

;;;###autoload
(defun ocp-cad-viewer-disconnect ()
  "Close the current OCP CAD Viewer websocket connection."
  (interactive)
  (when (ocp-cad-viewer-connected-p)
    (websocket-close ocp-cad-viewer--socket))
  (setq ocp-cad-viewer--socket nil
        ocp-cad-viewer--pending-frames nil))

;;;###autoload
(defun ocp-cad-viewer-reconnect (&optional port host)
  "Reconnect to the viewer websocket using optional PORT and HOST.

Optional argument PORT is an integer websocket port used for the
new connection; it defaults to `ocp-cad-viewer-port' (3939).

Optional argument HOST is a string websocket host used for the
new connection; it defaults to `ocp-cad-viewer-host' (\"127.0.0.1\")."
  (interactive
   (list
    (when current-prefix-arg
      (read-number "OCP CAD Viewer port: " ocp-cad-viewer-port))
    nil))
  (ocp-cad-viewer-disconnect)
  (ocp-cad-viewer-connect port host))

;;;###autoload
(defun ocp-cad-viewer-open (&optional port host)
  "Open the standalone OCP CAD Viewer in an xwidget browser.
With prefix argument PORT, prompt for the port.  HOST defaults to
`ocp-cad-viewer-host'.  The websocket connection is also opened."
  (interactive
   (list (when current-prefix-arg
           (read-number "OCP CAD Viewer port: " ocp-cad-viewer-port))
         nil))
  (let* ((target-host (or host ocp-cad-viewer-host))
         (target-port (or port ocp-cad-viewer-port))
         (url (ocp-cad-viewer--viewer-url target-host target-port)))
    (setq ocp-cad-viewer-host target-host
          ocp-cad-viewer-port target-port)
    (cond
     ((fboundp 'xwidget-webkit-browse-url)
      (xwidget-webkit-browse-url url))
     ((fboundp 'browse-url)
      (browse-url url))
     (t
      (user-error "No xwidget-webkit-browse-url or browse-url available")))
    (ocp-cad-viewer-connect target-port target-host)
    url))

(defun ocp-cad-viewer--encode-viewer-command (command &optional fields)
  "Encode viewer COMMAND and alist FIELDS as an OCP websocket frame."
  (concat
   "C:"
   (json-encode
    (append `((type . "viewer_command")
              (command . ,command))
            fields))))

(defun ocp-cad-viewer-send-frame (frame)
  "Send raw protocol FRAME to the viewer websocket.
When the socket is not yet open and `ocp-cad-viewer-auto-connect' is non-nil,
queue the frame and connect."
  (cond
   ((ocp-cad-viewer-connected-p)
    (ocp-cad-viewer--send-frame ocp-cad-viewer--socket frame))
   (ocp-cad-viewer-auto-connect
    (push frame ocp-cad-viewer--pending-frames)
    (unless ocp-cad-viewer--socket
      (ocp-cad-viewer-connect)))
   (t
    (user-error "OCP CAD Viewer websocket is not connected"))))

(defun ocp-cad-viewer-viewer-command (command &optional fields)
  "Send viewer COMMAND with alist FIELDS."
  (ocp-cad-viewer-send-frame
   (ocp-cad-viewer--encode-viewer-command command fields)))

(defun ocp-cad-viewer--scaled-step (prefix step)
  "Return STEP scaled by interactive PREFIX."
  (* step (prefix-numeric-value prefix)))

(defun ocp-cad-viewer--read-number (prompt default)
  "Read PROMPT as a number, using DEFAULT when the input is empty."
  (read-number (format "%s (%s): " prompt default) default))

(defun ocp-cad-viewer--camera-command (command &optional fields)
  "Send camera COMMAND with FIELDS."
  (ocp-cad-viewer-viewer-command command fields))

;;;###autoload
(defun ocp-cad-viewer-view-front ()
  "Switch the OCP CAD Viewer to front view."
  (interactive)
  (ocp-cad-viewer--camera-command "view" '((value . "front"))))

;;;###autoload
(defun ocp-cad-viewer-view-rear ()
  "Switch the OCP CAD Viewer to rear view."
  (interactive)
  (ocp-cad-viewer--camera-command "view" '((value . "rear"))))

;;;###autoload
(defun ocp-cad-viewer-view-left ()
  "Switch the OCP CAD Viewer to left view."
  (interactive)
  (ocp-cad-viewer--camera-command "view" '((value . "left"))))

;;;###autoload
(defun ocp-cad-viewer-view-right ()
  "Switch the OCP CAD Viewer to right view."
  (interactive)
  (ocp-cad-viewer--camera-command "view" '((value . "right"))))

;;;###autoload
(defun ocp-cad-viewer-view-top ()
  "Switch the OCP CAD Viewer to top view."
  (interactive)
  (ocp-cad-viewer--camera-command "view" '((value . "top"))))

;;;###autoload
(defun ocp-cad-viewer-view-bottom ()
  "Switch the OCP CAD Viewer to bottom view."
  (interactive)
  (ocp-cad-viewer--camera-command "view" '((value . "bottom"))))

;;;###autoload
(defun ocp-cad-viewer-view-iso ()
  "Switch the OCP CAD Viewer to isometric view."
  (interactive)
  (ocp-cad-viewer--camera-command "view" '((value . "iso"))))

;;;###autoload
(defun ocp-cad-viewer-reset ()
  "Reset the OCP CAD Viewer camera."
  (interactive)
  (ocp-cad-viewer--camera-command "reset"))

(defun ocp-cad-viewer--rotate (axis delta)
  "Rotate around AXIS by DELTA degrees."
  (ocp-cad-viewer--camera-command
   "rotate"
   `((axis . ,axis)
     (delta . ,delta))))

;;;###autoload
(defun ocp-cad-viewer-rotate-x+ (&optional prefix)
  "Rotate up around the viewer x axis.
PREFIX scales `ocp-cad-viewer-rotate-step'."
  (interactive "P")
  (ocp-cad-viewer--rotate
   "x" (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-rotate-step)))

;;;###autoload
(defun ocp-cad-viewer-rotate-x- (&optional prefix)
  "Rotate down around the viewer x axis.
PREFIX scales `ocp-cad-viewer-rotate-step'."
  (interactive "P")
  (ocp-cad-viewer--rotate
   "x" (- (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-rotate-step))))

;;;###autoload
(defun ocp-cad-viewer-rotate-z+ (&optional prefix)
  "Rotate left around the viewer z axis.
PREFIX scales `ocp-cad-viewer-rotate-step'."
  (interactive "P")
  (ocp-cad-viewer--rotate
   "z" (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-rotate-step)))

;;;###autoload
(defun ocp-cad-viewer-rotate-z- (&optional prefix)
  "Rotate right around the viewer z axis.
PREFIX scales `ocp-cad-viewer-rotate-step'."
  (interactive "P")
  (ocp-cad-viewer--rotate
   "z" (- (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-rotate-step))))

(defun ocp-cad-viewer--pan (direction step)
  "Pan DIRECTION by STEP."
  (ocp-cad-viewer--camera-command
   "pan"
   `((direction . ,direction)
     (step . ,step))))

;;;###autoload
(defun ocp-cad-viewer-pan-left (&optional prefix)
  "Pan left.  PREFIX scales `ocp-cad-viewer-pan-step'."
  (interactive "P")
  (ocp-cad-viewer--pan
   "left" (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-pan-step)))

;;;###autoload
(defun ocp-cad-viewer-pan-right (&optional prefix)
  "Pan right.  PREFIX scales `ocp-cad-viewer-pan-step'."
  (interactive "P")
  (ocp-cad-viewer--pan
   "right" (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-pan-step)))

;;;###autoload
(defun ocp-cad-viewer-pan-up (&optional prefix)
  "Pan up.  PREFIX scales `ocp-cad-viewer-pan-step'."
  (interactive "P")
  (ocp-cad-viewer--pan
   "up" (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-pan-step)))

;;;###autoload
(defun ocp-cad-viewer-pan-down (&optional prefix)
  "Pan down.  PREFIX scales `ocp-cad-viewer-pan-step'."
  (interactive "P")
  (ocp-cad-viewer--pan
   "down" (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-pan-step)))

;;;###autoload
(defun ocp-cad-viewer-pan-forward (&optional prefix)
  "Pan forward.  PREFIX scales `ocp-cad-viewer-pan-step'."
  (interactive "P")
  (ocp-cad-viewer--pan
   "forward" (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-pan-step)))

;;;###autoload
(defun ocp-cad-viewer-pan-backward (&optional prefix)
  "Pan backward.  PREFIX scales `ocp-cad-viewer-pan-step'."
  (interactive "P")
  (ocp-cad-viewer--pan
   "backward" (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-pan-step)))

(defun ocp-cad-viewer--zoom (delta)
  "Zoom by DELTA."
  (ocp-cad-viewer--camera-command "zoom" `((delta . ,delta))))

;;;###autoload
(defun ocp-cad-viewer-zoom-in (&optional prefix)
  "Zoom in.  PREFIX scales `ocp-cad-viewer-zoom-step'."
  (interactive "P")
  (ocp-cad-viewer--zoom
   (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-zoom-step)))

;;;###autoload
(defun ocp-cad-viewer-zoom-out (&optional prefix)
  "Zoom out.  PREFIX scales `ocp-cad-viewer-zoom-step'."
  (interactive "P")
  (ocp-cad-viewer--zoom
   (- (ocp-cad-viewer--scaled-step prefix ocp-cad-viewer-zoom-step))))

;;;###autoload
(defun ocp-cad-viewer-set-camera (&optional position target quaternion zoom)
  "Set absolute camera state.
POSITION, TARGET and QUATERNION are vectors or lists of numbers.  ZOOM is a
number.  Nil values are omitted from the command."
  (interactive
   (list nil nil nil (ocp-cad-viewer--read-number "Zoom" 1.0)))
  (let (fields)
    (when position
      (push (cons 'position position) fields))
    (when target
      (push (cons 'target target) fields))
    (when quaternion
      (push (cons 'quaternion quaternion) fields))
    (when zoom
      (push (cons 'zoom zoom) fields))
    (ocp-cad-viewer--camera-command "set" (nreverse fields))))

(defvar-keymap ocp-cad-viewer-mode-map
  :doc "Keymap for OCP CAD Viewer control buffers."
  "<right>" #'ocp-cad-viewer-rotate-z-
  "<left>" #'ocp-cad-viewer-rotate-z+
  "<up>" #'ocp-cad-viewer-rotate-x+
  "<down>" #'ocp-cad-viewer-rotate-x-
  "M-<left>" #'ocp-cad-viewer-pan-left
  "M-<right>" #'ocp-cad-viewer-pan-right
  "M-<up>" #'ocp-cad-viewer-pan-up
  "M-<down>" #'ocp-cad-viewer-pan-down
  "+" #'ocp-cad-viewer-zoom-in
  "-" #'ocp-cad-viewer-zoom-out
  "f" #'ocp-cad-viewer-view-front
  "t" #'ocp-cad-viewer-view-top
  "l" #'ocp-cad-viewer-view-left
  "r" #'ocp-cad-viewer-view-right
  "b" #'ocp-cad-viewer-view-rear
  "d" #'ocp-cad-viewer-view-bottom
  "i" #'ocp-cad-viewer-view-iso
  "0" #'ocp-cad-viewer-reset
  "?" #'ocp-cad-viewer-menu)

;;;###autoload
(define-minor-mode ocp-cad-viewer-mode
  "Minor mode for controlling an OCP CAD Viewer from Emacs."
  :lighter " OCP-View"
  :keymap ocp-cad-viewer-mode-map)

;;;###autoload
(define-derived-mode ocp-cad-viewer-control-mode special-mode "OCP-CAD/Viewer"
  "Major mode for an OCP CAD Viewer control buffer."
  (ocp-cad-viewer-mode 1))

;;;###autoload
(defun ocp-cad-viewer-control-buffer ()
  "Open an OCP CAD Viewer control buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*OCP CAD Viewer*")))
    (with-current-buffer buffer
      (ocp-cad-viewer-control-mode))
    (pop-to-buffer buffer)))

;;;###autoload (autoload 'ocp-cad-viewer-menu "ocp-cad-viewer" nil t)
(transient-define-prefix ocp-cad-viewer-menu ()
  "Transient menu for OCP CAD Viewer camera control."
  [["Connection"
    ("o" "Open xwidget" ocp-cad-viewer-open)
    ("c" "Connect" ocp-cad-viewer-connect)
    ("R" "Reconnect" ocp-cad-viewer-reconnect)
    ("q" "Disconnect" ocp-cad-viewer-disconnect)]
   ["Pan"
    ("M-<up>" "Up" ocp-cad-viewer-pan-up :transient t)
    ("M-<down>" "Down" ocp-cad-viewer-pan-down :transient t)
    ("M-<left>" "Left" ocp-cad-viewer-pan-left :transient t)
    ("M-<right>" "Right" ocp-cad-viewer-pan-right :transient t)
    ("F" "Forward" ocp-cad-viewer-pan-forward :transient t)
    ("B" "Backward" ocp-cad-viewer-pan-backward :transient t)]
   ["Rotate / Zoom"
    ("<up>" "Rotate x+" ocp-cad-viewer-rotate-x+ :transient t)
    ("<down>" "Rotate x-" ocp-cad-viewer-rotate-x- :transient t)
    ("<left>" "Rotate z+" ocp-cad-viewer-rotate-z+ :transient t)
    ("<right>" "Rotate z-" ocp-cad-viewer-rotate-z- :transient t)
    ("+" "Zoom in" ocp-cad-viewer-zoom-in :transient t)
    ("-" "Zoom out" ocp-cad-viewer-zoom-out :transient t)]
   ["Views"
    ("f" "Front" ocp-cad-viewer-view-front :transient t)
    ("b" "Rear" ocp-cad-viewer-view-rear :transient t)
    ("l" "Left" ocp-cad-viewer-view-left :transient t)
    ("r" "Right" ocp-cad-viewer-view-right :transient t)
    ("t" "Top" ocp-cad-viewer-view-top :transient t)
    ("d" "Bottom" ocp-cad-viewer-view-bottom :transient t)
    ("i" "Iso" ocp-cad-viewer-view-iso :transient t)
    ("0" "Reset" ocp-cad-viewer-reset :transient t)]])

(provide 'ocp-cad-viewer)
;;; ocp-cad-viewer.el ends here
