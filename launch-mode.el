;;; launch-mode.el --- Major mode for launch-formatted text -*- lexical-binding: t; -*-

;; Author: iory <ab.ioryz@gmail.com>
;; URL: https://github.com/iory/launch-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (helm "2.0"))

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

;; launch-mode provides major mode.
;; And launch-mode provides goto-definition of other launch-files.

;;; Code:

(require 'launch-find-definition)


;;; Constants =================================================================

(defconst launch-mode-version "0.0.1"
  "launch mode version number.")

(defconst launch-output-buffer-name "*launch-output*"
  "Name of temporary buffer for launch command output.")

;;; Mode Definition  ==========================================================

(defun launch-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "launch-mode, version %s" launch-mode-version))

;;;###autoload
(define-derived-mode launch-mode nxml-mode "launch"
  "Major mode for editing launch files."
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.launch\\'" . launch-mode))
(add-to-list 'auto-mode-alist '("\\.launch.xml\\'" . launch-mode))
(add-to-list 'auto-mode-alist '("\\.test\\'" . launch-mode))

;;; Hook keymap ===============================================================

;;;###autoload
(defun launch-enable-keymap (&optional prefix)
  "Setup standard keybindings for the ros-launch file"
  (interactive)
  (if prefix
      (unless (string-match " $" prefix)
        (setq prefix (concat prefix " ")))
    (setq prefix "C-c r")
    )
  (define-key launch-mode-map (kbd (concat prefix ",")) 'launch-insert-node-name)
  (define-key launch-mode-map (kbd (concat prefix ".")) 'launch-goto-include-launch)
  (define-key launch-mode-map (kbd (concat prefix "[")) 'launch-location-stack-forward)
  (define-key launch-mode-map (kbd (concat prefix "]")) 'launch-location-stack-back)
  )

(add-hook 'launch-mode-hook 'launch-enable-keymap)


(provide 'launch-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; launch-mode.el ends here
