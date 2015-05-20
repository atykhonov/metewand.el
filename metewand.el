(defvar metewand-special-chars '("" "" "" "" "")
  "Special characters.")

(defun metewand--trim-string (string)
  "Remove whitespaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n\r]*" ""
                            (replace-regexp-in-string "[ \t\n\r]*\\'" "" string)))

(defun metewand-eval-current-buffer ()
  (interactive)
  (let ((instruction-list (list))
        (current-pos 1)
        (next-char-pos nil)
        (closest-char-pos nil)
        (instruction nil)
        (first-char nil)
        (continue t)
        (first t)
        (test-buffer "*test-buffer*")
        (temp-report-buffer " *temp report buffer*")
        (report-buffer "*report-buffer*")
        (test-description nil)
        (report nil)
        (curr-buffer (current-buffer)))
    (with-current-buffer (get-buffer-create test-buffer)
      (erase-buffer))
    (when (bufferp temp-report-buffer)
      (kill-buffer temp-report-buffer))
    (when (bufferp report-buffer)
      (kill-buffer report-buffer))
    (while continue
      (with-current-buffer curr-buffer
        (message "1")
        (dolist (char metewand-special-chars)
          (goto-char (+ current-pos 1))
          (setq next-char-pos (search-forward char nil t))
          ;; (when (equal next-char-pos (+ current-pos 1))
          ;;   (setq next-char-pos (search-forward char nil t)))
          (when (and (not (null next-char-pos))
                     (or (null closest-char-pos)
                         (< next-char-pos closest-char-pos)))
            (setq closest-char-pos (- next-char-pos 1))))
        (message "2")
        (when (null closest-char-pos)
          (setq continue nil)
          (setq closest-char-pos (point-max)))
        (message "3")
        (setq instruction (buffer-substring-no-properties
                           current-pos closest-char-pos)))
      (message "4")
      (setq current-pos closest-char-pos)
      (message "Instruction: %s" instruction)
      (setq current-pos closest-char-pos)
      (setq closest-char-pos nil)
      (setq instruction-id (substring instruction 0 1))
      (setq instruction-body (metewand--trim-string
                              (substring instruction 1)))
      (cond
       ((equal instruction-id "")
        (progn
          (with-current-buffer (get-buffer-create temp-report-buffer)
            (insert "Test: %s\n" instruction-body))))
       ((equal instruction-id "")
        (progn
          (message "Body: %s" instruction-body)
          (with-current-buffer (get-buffer-create test-buffer)
            (insert instruction-body)
            (goto-char 1)
            (when (search-forward "" nil t)
              (delete-char -1)
              ;; (when (search-forward "")
              ;;   (delete-char -1)
              ;;   (set-mark-command)
              ;;   (search-forward "")
              ;;   (delete-char -1))
              ))))
       ((equal instruction-id "")
        (progn
          (message "b")
          (switch-to-buffer test-buffer)
          (execute-kbd-macro
           (edmacro-parse-keys instruction-body) t)
          (switch-to-buffer curr-buffer)))
       ((equal instruction-id "")
        (setq continue nil)
        (message "Test separator"))
       ((equal instruction-id "")
        (progn
          (let ((expected-point-pos nil)
                (expected-result nil)
                (actual-point-pos nil)
                (actual-result nil))
            (setq expected-result
                  (with-temp-buffer
                    (insert instruction-body)
                    (goto-char 1)
                    (when (search-forward "" nil t)
                      (delete-char -1))
                    (setq expected-point-pos (point))
                    (buffer-substring-no-properties (point-min) (point-max))))
            (with-current-buffer test-buffer
              (setq actual-result (buffer-substring-no-properties (point-min) (point-max)))
              (setq actual-point-pos (point)))
            (if (and (equal actual-result expected-result)
                     (equal actual-point-pos expected-point-pos))
                (message "Test Passed!!!")
              (progn
                (message "Test Failed!")
                (when (not (equal actual-result expected-result))
                  (message "Expected Result: %s" expected-result)
                  (message "Actual Result: %s" actual-result))
                (when (not (equal actual-point-pos expected-point-pos))
                  (message "Expected point position: %s" expected-point-pos)
                  (message "Actual point position: %s" actual-point-pos))))
            (message "t")))))
      (message "Instruction ID: %s" instruction-id))
    ;; (with-current-buffer temp-report-buffer
    ;;   (setq report (buffer-substring (point-min) (point-max))))
    ;; (with-output-to-temp-buffer report-buffer
    ;;   (insert report)
    ;;   )
    ))

(provide 'metewand)
