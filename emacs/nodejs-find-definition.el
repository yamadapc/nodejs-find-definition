(require 'json)
(require 'projectile)

(defun nodejs-find-definition (line col)
  (interactive (list (count-lines 1 (point)) (current-column)))
  (let ((command (format "find-definition -l %d -c %d -n %s -r %s -f %s"
                         line col
                         (concat (projectile-project-root) "/node_modules")
                         (concat (projectile-project-root) "/core")
                         (buffer-file-name))))
  (message command)
  (let ((output (shell-command-to-string command)))
    (message output)
    (let* ((result (json-read-from-string output))
           (def-filepath (cdr (assoc 'path result)))
           (def-loc (cdr (assoc 'start (cdr (assoc 'loc result)))))
           (def-col (cdr (assoc 'column def-loc)))
           (def-line (cdr (assoc 'line def-loc))))
      (message (pp def-loc))
      (find-file def-filepath)
      (let ((current-prefix-arg def-line))
        (call-interactively 'goto-line))
      (move-to-column def-col)))))

(require 'evil-leader)
(evil-leader/set-key;; -for-mode 'js2-mode
  "md" 'nodejs-find-definition)
