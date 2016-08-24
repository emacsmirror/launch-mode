;;; find-definition.el --- find-definition of launch  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  iory

;; Author: iory
;; Keywords: launch


;;; Constants =================================================================

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


;;; Definition of find-definition =============================================

(defun launch-insert-node-name ()
  (interactive)
  (helm :sources '((name . "helm-launch-insert-nodename")
                   (buffer . "*helm-launch-insert-nodename*")
                   (candidates-in-buffer)
                   (migemo . t)
                   (init . (lambda ()
                             (helm-init-candidates-in-buffer 'global
                               (shell-command-to-string
                                (concatenate #'string "python " launch-insert-node-name-py-path " " (buffer-file-name))))))
                   (action . (lambda (x)
                               (let ((xlst (split-string x "\t")))
                                 (insert (elt xlst 1))
                                 )))
                   )
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
      (return-from launch-goto-include-launch)
      )
    (setq launch-file-name
          (replace-regexp-in-string
           "\n$" ""
           (shell-command-to-string
            (concatenate #'string "python " launch-goto-include-launch-py-path " \"" launch-file-name "\""))
           )
          )
    (when (file-exists-p launch-file-name)
      (find-file launch-file-name))
    )
  )


(provide 'launch-find-definition)
