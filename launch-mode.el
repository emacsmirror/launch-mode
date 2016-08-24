;;; launch-mode.el --- Major mode for launch-formatted text -*- lexical-binding: t; -*-

;; Author: iory <ab.ioryz@gmail.com>
;; Maintainer: iory <ab.ioryz@gmail.com>
;; Created: August 24, 2016
;; Version: 0.0.1
;; Keywords: ROS, launch file, GitHub
;; URL: https://github.com/iory/launch-mode


;;; Constants =================================================================

(defconst launch-mode-version "0.0.1"
  "launch mode version number.")

(defconst launch-output-buffer-name "*launch-output*"
  "Name of temporary buffer for launch command output.")

(defconst launch-mode-source-dir
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory
    )
  "Source dir of launch-mode")

(load (concatenate #'string launch-mode-source-dir "launch-find-definition.el"))

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
(setq auto-mode-alist
      (cons (cons "\\.launch\\'" 'launch-mode) auto-mode-alist))

;;; Hook keymap ===============================================================

(defun launch-enable-keymap ()
  (define-key launch-mode-map "\C-c," 'launch-insert-node-name)
  (define-key launch-mode-map "\C-c." 'launch-goto-include-launch)
  )

(add-hook 'launch-mode-hook 'launch-enable-keymap)


(provide 'launch-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; launch-mode.el ends here
