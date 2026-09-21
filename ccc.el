;;; ccc.el --- buffer local cursor color control library

;; Copyright (C) 2000 Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Copyright (C) 2001, 2002, 2004, 2005,
;;   2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
;;   2015, SKK Development Team

;; Author: Masatake YAMATO <masata-y@is.aist-nara.ac.jp>
;; Maintainer: SKK Development Team
;; URL: https://github.com/skk-dev/ddskk
;; URL: https://github.com/skk-dev/ddskk/blob/master/READMEs/README.ccc.org
;; Keywords: cursor

;; This file is part of Daredevil SKK.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Buffer local frame parameters
;; --- cursor, foreground, background
;; --- TODO: support other frame parameters
;;           should use uni prefix for functions and variables?

;;; Code:

(require 'faces)                        ; read-color, color-values

(eval-when-compile
  (require 'nadvice))

;; Internal variables.
(defvar ccc-buffer-local-cursor-color nil)
(make-variable-buffer-local 'ccc-buffer-local-cursor-color)

(defvar ccc-buffer-local-foreground-color nil)
(make-variable-buffer-local 'ccc-buffer-local-foreground-color)

(defvar ccc-buffer-local-background-color nil)
(make-variable-buffer-local 'ccc-buffer-local-background-color)

(defvar ccc-default-cursor-color nil)
(defvar ccc-default-foreground-color nil)
(defvar ccc-default-background-color nil)

(defvar ccc-frame-params-dirty t)
(defvar ccc-last-checked-buffer nil)
(defvar ccc-last-checked-frame nil)

;; Frame parameters.
(defsubst ccc-current-cursor-color ()
  (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))
(defsubst ccc-initial-cursor-color ()
  (cdr (assq 'cursor-color initial-frame-alist)))
(defsubst ccc-default-cursor-color ()
  (or ccc-default-cursor-color
      (cdr (assq 'cursor-color default-frame-alist))))
(defsubst ccc-fallback-cursor-color ()
  (if (eq frame-background-mode 'dark)
      "white"
    "black"))

(defsubst ccc-current-foreground-color ()
  (cdr (assq 'foreground-color (frame-parameters (selected-frame)))))
(defsubst ccc-initial-foreground-color ()
  (cdr (assq 'foreground-color initial-frame-alist)))
(defsubst ccc-default-foreground-color ()
  (or ccc-default-foreground-color
      (cdr (assq 'foreground-color default-frame-alist))))
(defsubst ccc-fallback-foreground-color ()
  (if (eq frame-background-mode 'dark)
      "white"
    "black"))

(defsubst ccc-current-background-color ()
  (cdr (assq 'background-color (frame-parameters (selected-frame)))))
(defsubst ccc-initial-background-color ()
  (cdr (assq 'background-color initial-frame-alist)))
(defsubst ccc-default-background-color ()
  (or ccc-default-background-color
      (cdr (assq 'background-color default-frame-alist))))
(defsubst ccc-fallback-background-color ()
  (if (eq frame-background-mode 'dark)
      "black"
    "white"))

(defsubst ccc-frame-cursor-color (&optional frame)
  (frame-parameter (or frame (selected-frame)) 'ccc-frame-cursor-color))
(defsubst ccc-set-frame-cursor-color (frame color)
  (modify-frame-parameters frame (list (cons 'ccc-frame-cursor-color color))))

(defsubst ccc-frame-foreground-color (&optional frame)
  (frame-parameter (or frame (selected-frame)) 'ccc-frame-foreground-color))
(defsubst ccc-set-frame-foreground-color (frame color)
  (when (eval-when-compile (>= emacs-major-version 23))
    (unless (window-system frame)
      (setq color "unspecified-fg")))
  (modify-frame-parameters frame (list (cons 'ccc-frame-foreground-color color))))

(defsubst ccc-frame-background-color (&optional frame)
  (frame-parameter (or frame (selected-frame)) 'ccc-frame-background-color))
(defsubst ccc-set-frame-background-color (frame color)
  (when (eval-when-compile (>= emacs-major-version 23))
    (unless (window-system frame)
      (setq color "unspecified-bg")))
  (modify-frame-parameters frame (list (cons 'ccc-frame-background-color color))))

;; Functions.
(defsubst ccc-read-color (prompt)
  (list (read-color prompt)))

(defsubst ccc-color-equal (a b)
  "Return t if colors A and B are the same color.
A and B should be strings naming colors.
This function queries the display system to find out what the color
names mean.  It returns nil if the colors differ or if it can't
determine the correct answer.

This function is the same as `facemenu-color-equal'"
  (cond
   ((equal a b) t)
   ((equal (color-values a) (color-values b)))))

(defun ccc-setup-new-frame (frame)
  (ccc-set-frame-cursor-color frame (or (ccc-default-cursor-color)
                                        (ccc-fallback-cursor-color)))
  (ccc-set-frame-foreground-color frame (or (ccc-default-foreground-color)
                                            (ccc-fallback-foreground-color)))
  (ccc-set-frame-background-color frame (or (ccc-default-background-color)
                                            (ccc-fallback-background-color))))

;;;###autoload
(defun ccc-setup ()
  (add-hook 'post-command-hook 'ccc-update-buffer-local-frame-params)
  (add-hook 'after-make-frame-functions 'ccc-setup-new-frame)
  ;; Determine default colors for frames other than the initial frame.
  (setq ccc-default-cursor-color (or (ccc-default-cursor-color)
                                     (ccc-current-cursor-color))
        ccc-default-foreground-color (or (ccc-default-foreground-color)
                                         (ccc-current-foreground-color))
        ccc-default-background-color (or (ccc-default-background-color)
                                         (ccc-current-background-color)))
  ;; Set up colors for the initial frame.
  (let ((frame (selected-frame)))
    (ccc-set-frame-cursor-color frame (or (ccc-initial-cursor-color)
                                          (ccc-default-cursor-color)
                                          (ccc-fallback-cursor-color)))
    (ccc-set-frame-foreground-color frame (or (ccc-initial-foreground-color)
                                              (ccc-default-foreground-color)
                                              (ccc-fallback-background-color)))
    (ccc-set-frame-background-color frame (or (ccc-initial-background-color)
                                              (ccc-default-background-color)
                                              (ccc-fallback-background-color)))))

;;;###autoload
(defun ccc-update-buffer-local-frame-params (&optional buffer)
  (let ((buf (if (buffer-live-p buffer)
                 buffer
               (window-buffer (selected-window))))
        (frame (selected-frame)))
    (with-current-buffer buf
      (when (or ccc-frame-params-dirty
                (not (eq buf ccc-last-checked-buffer))
                (not (eq frame ccc-last-checked-frame))
                ccc-buffer-local-cursor-color
                ccc-buffer-local-foreground-color
                ccc-buffer-local-background-color)
        (ccc-update-buffer-local-cursor-color)
        (ccc-update-buffer-local-foreground-color)
        (ccc-update-buffer-local-background-color)
        (setq ccc-frame-params-dirty nil)))
    (setq ccc-last-checked-buffer buf
          ccc-last-checked-frame frame)))

(defmacro ccc-define-buffer-local-color (kind prompt
                                         &optional window-system-only)
  "Define the buffer-local color functions for KIND (a symbol).
This defines `ccc-set-buffer-local-KIND-color',
`ccc-update-buffer-local-KIND-color' and
`ccc-set-KIND-color-buffer-local'."
  (let ((var (intern (format "ccc-buffer-local-%s-color" kind)))
        (frame-fn (intern (format "ccc-frame-%s-color" kind)))
        (current-fn (intern (format "ccc-current-%s-color" kind)))
        (set-fn (intern (format "set-%s-color" kind)))
        (setter (intern (format "ccc-set-buffer-local-%s-color" kind)))
        (updater (intern (format "ccc-update-buffer-local-%s-color" kind)))
        (localizer (intern (format "ccc-set-%s-color-buffer-local" kind))))
    `(progn
       (defun ,setter (color-name)
         (interactive (ccc-read-color ,prompt))
         ,@(when window-system-only
             '((unless window-system
                 (setq color-name nil))))
         (let ((local ,var))
           (setq ,var (or color-name
                          (,frame-fn)))
           (condition-case nil
               (,updater)
             (error
              (setq ,var local)))))
       (defun ,updater ()
         (let ((color (if (stringp ,var)
                          ,var
                        (,frame-fn))))
           (when (and ,@(when window-system-only '(window-system))
                      (stringp color)
                      (color-defined-p color)
                      (not (ccc-color-equal color (,current-fn))))
             (,set-fn color))))
       (defun ,localizer (arg)
         (if arg
             (setq ,var (,current-fn))
           (,set-fn (,frame-fn))
           (setq ,var nil))))))

;;
;; ccc-buffer-local-cursor-color
;;
(ccc-define-buffer-local-color cursor "Cursor color: ")

;;
;; ccc-buffer-local-foreground-color
;;
(ccc-define-buffer-local-color foreground "Foreground color: " window-system)

;;
;; ccc-buffer-local-background-color
;;
(ccc-define-buffer-local-color background "Background color: " window-system)

(defun ccc-setup-current-colors ()
  (setq ccc-default-cursor-color (ccc-current-cursor-color)
        ccc-default-foreground-color (ccc-current-foreground-color)
        ccc-default-background-color (ccc-current-background-color))
  (ccc-set-frame-cursor-color (selected-frame) (ccc-current-cursor-color))
  (ccc-set-frame-foreground-color (selected-frame) (ccc-current-foreground-color))
  (ccc-set-frame-background-color (selected-frame) (ccc-current-background-color)))

;; Advices.
(define-advice modify-frame-parameters (:after (frame alist) ccc-ad)
  (setq ccc-frame-params-dirty t)
  (dolist (spec '((cursor-color
                   ccc-buffer-local-cursor-color
                   . ccc-set-frame-cursor-color)
                  (foreground-color
                   ccc-buffer-local-foreground-color
                   . ccc-set-frame-foreground-color)
                  (background-color
                   ccc-buffer-local-background-color
                   . ccc-set-frame-background-color)))
    (let ((entry (assq (car spec) alist)))
      (when (and entry
                 (null (symbol-value (cadr spec))))
        (funcall (cddr spec) frame (cdr entry))))))

(define-advice custom-theme-checkbox-toggle
    (:after (widget &optional event) ccc-ad)
  (ccc-setup-current-colors))

(define-advice enable-theme (:after (theme) ccc-ad)
  (ccc-setup-current-colors))

(define-advice disable-theme (:after (theme) ccc-ad)
  (ccc-setup-current-colors))

(provide 'ccc)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ccc.el ends here
