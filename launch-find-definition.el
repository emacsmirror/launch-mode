;;; launch-find-definition.el --- find-definition of launch  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  iory

;; Author: iory
;; Keywords: launch

(require 'bookmark)
(require 'repeat)
(require 'cl-lib)


;;; Constants =================================================================

(defconst launch-mode-source-dir
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory
    )
  "Source dir of launch-mode")

(defconst launch-goto-definition-py "goto_launch.py"
  "Name of python script find launch file"
  )

(defconst launch-goto-include-launch-py-path
  (expand-file-name launch-goto-definition-py launch-mode-source-dir)
  "goto_launch.py's PATH"
  )

(defconst launch-insert-node-name-py "insert_node_name.py"
  "Name of python script find launch node name"
  )

(defconst launch-insert-node-name-py-path
  (expand-file-name launch-insert-node-name-py launch-mode-source-dir)
  "insert_node_name.py's PATH"
  )


;;; Variables =================================================================

(defvar launch-location-stack-index 0)
(defvar launch-location-stack nil)
(defvar launch-jump-hook nil)

(defcustom launch-max-bookmark-count 100
  "How many bookmarks to keep on the stack."
  :group 'launch
  :type 'integer
  :safe 'integerp)

(defcustom launch-after-find-file-hook nil
  "Run after Launch has jumped to a location possibly in a new file."
  :group 'launch
  :type 'hook)


;;; Definition of find-definition =============================================

(defun launch-current-location (&optional offset truename)
  (let ((filename (buffer-file-name)))
    (and filename
         (format "%s:%d:%d:"
                 (if truename (file-truename filename) filename)
                 (line-number-at-pos offset)
                 (1+ (- (or offset (point)) (point-at-bol)))))))

;;;###autoload
(defun launch-print-current-location ()
  (interactive)
  (message (launch-current-location)))

;;;###autoload
(defun launch-location-stack-forward ()
  (interactive)
  (launch-location-stack-jump -1))

;;;###autoload
(defun launch-location-stack-back ()
  (interactive)
  (launch-location-stack-jump 1))

(defun launch-find-file-or-buffer (file-or-buffer &optional other-window)
  (if (file-exists-p file-or-buffer)
      (if other-window
          (find-file-other-window file-or-buffer)
        (find-file file-or-buffer))
    (let ((buf (get-buffer file-or-buffer)))
      (cond ((not buf) (message "No buffer named \"%s\"" file-or-buffer))
            (other-window (switch-to-buffer-other-window file-or-buffer))
            (t (switch-to-buffer file-or-buffer))))))

(defun launch-buffer-is-multibyte ()
  (string-match "\\butf\\b" (symbol-name buffer-file-coding-system)))

(defun launch-goto-line-col (line column)
  (let ((old (point))
        (multibyte (launch-buffer-is-multibyte))
        (prev (buffer-local-value enable-multibyte-characters (current-buffer)))
        (ret t)
        (loc (local-variable-p enable-multibyte-characters)))
    (when multibyte
      (set-buffer-multibyte nil))
    (goto-char (point-min))
    (condition-case nil
        (progn
          (forward-line (1- line))
          (forward-char (1- column)))
      (error
       (setq ret nil)
       (goto-char old)))
    (when multibyte
      (set-buffer-multibyte prev))
    (unless loc
      (kill-local-variable enable-multibyte-characters))
    ret))


(defun launch-goto-location (location &optional nobookmark other-window skip-trampification)
  "Go to a stacked location."
  (message (format "launch-goto-location \"%s\"" location))

  (cond ((string-match "\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):?" location)
         (let ((line (string-to-number (match-string-no-properties 2 location)))
               (column (string-to-number (match-string-no-properties 3 location))))
           (launch-find-file-or-buffer (match-string-no-properties 1 location) other-window)
           (push-mark nil t)
           (launch-goto-line-col line column)
           (run-hooks 'launch-after-find-file-hook)
           t)
         )
        ((string-match "\\(.*?\\):\\([0-9]+\\):?" location)
         (let ((line (string-to-number (match-string-no-properties 2 location))))
           (launch-find-file-or-buffer (match-string-no-properties 1 location) other-window)
           (push-mark nil t)
           (goto-char (point-min))
           (forward-line (1- line))
           (run-hooks 'launch-after-find-file-hook)
           t))
        ((string-match "\\(.*?\\),\\([0-9]+\\)" location)
         (let ((offset (string-to-number (match-string-no-properties 2 location))))
           (launch-find-file-or-buffer (match-string-no-properties 1 location) other-window)
           (push-mark nil t)
           (launch-goto-offset offset)
           (run-hooks 'launch-after-find-file-hook)
           t))
        (t
         (when (string-match "^[ \t]+\\(.*\\)$" location)
           (setq location (match-string-no-properties 1 location)))
         (launch-find-file-or-buffer location other-window)))
  (unless nobookmark (launch-location-stack-push)))

(defun launch-location-stack-push (&optional location-arg)
  "Push current location into location stack.
If location-arg is non-nil, then push it instead."
  (let ((location (or location-arg (launch-current-location))))
    (while (> launch-location-stack-index 0)
      (cl-decf launch-location-stack-index)
      (pop launch-location-stack))
    (unless (string= location (car launch-location-stack))
      (push location launch-location-stack)
      (when (> (length launch-location-stack) launch-max-bookmark-count)
        (nbutlast launch-location-stack (- (length launch-location-stack) launch-max-bookmark-count)))
      (run-hooks 'launch-jump-hook))))

;;;###autoload
(defun launch-goto-offset (pos)
  (interactive "NOffset: ")
  (if (launch-buffer-is-multibyte)
      (let ((prev (buffer-local-value enable-multibyte-characters (current-buffer)))
            (loc (local-variable-p enable-multibyte-characters)))
        (set-buffer-multibyte nil)
        (goto-char (1+ pos))
        (set-buffer-multibyte prev)
        (unless loc
          (kill-local-variable enable-multibyte-characters)))
    (goto-char (1+ pos))))

(defun launch-location-stack-jump (by)
  (interactive)
  (let (;; copy of repeat-on-final-keystroke functionality from repeat.el
        (repeat-char
         (if (eq repeat-on-final-keystroke t)
             last-command-event
           (car (memq last-command-event
                      (listify-key-sequence
                       repeat-on-final-keystroke)))))
        (instack (nth launch-location-stack-index launch-location-stack))
        (cur (launch-current-location)))
    (if (not (string= instack cur))
        ;; location ring may contain locations from many sandboxes. In case current location is remote
        ;; and following is local one, we want following be visited as local file.
        ;; that's why 4th arg is t.
        (launch-goto-location instack t nil t)
      (let ((target (+ launch-location-stack-index by)))
        (when (and (>= target 0) (< target (length launch-location-stack)))
          (setq launch-location-stack-index target)
          (launch-goto-location (nth launch-location-stack-index launch-location-stack) t nil t))))
    (when repeat-char
      (let ((map (make-sparse-keymap)))
        (define-key map (vector repeat-char)
          `(lambda ()
             (interactive)
             (launch-location-stack-jump ,by)))
        (cond ((fboundp 'set-transient-map) (set-transient-map map))
              ((fboundp 'set-temporary-overlay-map) (set-temporary-overlay-map map))
              (t))))))


(defun launch-insert-node-name ()
  (interactive)
  (let (nodename-list
        selected-nodename)
    (setq nodename-list (split-string (shell-command-to-string
                                (cl-concatenate #'string "python " launch-insert-node-name-py-path " " (buffer-file-name))) "\n"))
    (setq selected-nodename (ido-completing-read "insert node_name" nodename-list))
    (let ((xlst (split-string selected-nodename "\t")))
      (insert (elt xlst 1)))
    )
  )

(defun launch-print-current-word ()
  "print current word."
  (interactive)
  (let (p1 p2)
    (save-excursion
      (skip-chars-backward "A-Z-a-z0-9/ ()_.")
      (setq p1 (point))
      (skip-chars-forward "A-Z-a-z0-9/ ()_.")
      (setq p2 (point))
      (message "%s" (buffer-substring-no-properties p1 p2)))))

(defun launch-goto-include-launch ()
  (interactive)
  (let ((launch-file-name (launch-print-current-word)))
    (when (not (or
                (and (> (length launch-file-name) 7)
                     (string-equal (substring launch-file-name -7) ".launch"))
                (and (> (length launch-file-name) 7)
                     (string-equal (substring launch-file-name -11) ".launch.xml"))
                ))
      (message "%s" "Must be launch file name")
      (cl-return-from launch-goto-include-launch)
      )
    (setq launch-file-name
          (replace-regexp-in-string
           "\n$" ""
           (shell-command-to-string
            (cl-concatenate #'string "python " launch-goto-include-launch-py-path " \"" launch-file-name "\""))
           )
          )
    (when (file-exists-p launch-file-name)
      (launch-location-stack-push)
      (find-file launch-file-name))
    )
  )


(provide 'launch-find-definition)

;;; launch-find-definition.el ends here
