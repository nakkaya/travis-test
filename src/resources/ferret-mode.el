(define-derived-mode ferret-mode
  clojure-mode "Ferret"
  "Major mode for Ferret.
\\{ferret-mode-map}"

  (define-key ferret-mode-map (kbd "C-c '") 'ferret-indent-cpp)
  (define-key ferret-mode-map (kbd "C-c c") 'compile)

  (require 'compile)
  (require 'ansi-color)

  (defun ferret-colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  
  (add-hook 'compilation-filter-hook 'ferret-colorize-compilation-buffer)
  
  (add-hook 'ferret-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "ferret -i %s" buffer-file-name)))))
  
  (defun ferret-indent-cpp ()
    (interactive)
    (let ((cursor-point (point)))
      (ignore-errors
       (let ((pps (parse-partial-sexp (point-min) (point))))
         (when (nth 3 pps)
           (let ((str-begin (nth 8 pps)))
             (goto-char str-begin)
             (forward-sexp)
             (backward-char)
             (let ((str-end (point)))
               (goto-char str-begin)
               (forward-char)
               (set-mark str-end)))
           (let ((col (save-excursion (goto-char (region-beginning))
                                      (current-column))))
             (kill-region (region-beginning) (region-end))
             (insert
              (with-temp-buffer
                (c++-mode)
                (yank)
                ;;un escape "
                (goto-char (point-min))
                (replace-string "\\\"" "\"")
                (indent-region (point-min) (point-max) nil)
                ;;escape "
                (goto-char (point-min))
                (replace-string "\"" "\\\"")
                ;;select all but first line
                (goto-char (point-min))
                (forward-line)
                (push-mark (point))
                (push-mark (point-max) nil t)
                ;;shift all text to col
                (indent-rigidly (region-beginning) (region-end) col)
                (buffer-string)))))))
      (goto-char cursor-point))))
