(require 'f)

(defvar boron-path (f-dirname (f-this-file)))

(defvar boron-font-lock-keywords
  ;; Keywords
  `((,(rx symbol-start
          (group (or
                  (group "M-" (any print))
                  "M-"
                  "RET" "SPC" (group "C-" (any print)) "as" "elif" "global" "or" "with"
                  "assert" "else" "if" "pass" "yield" "break" "except" "import" "class"
                  "in" "raise" "continue" "finally" "is" "return" "def" "for" "lambda"
                  "try"
                  ;; Python 2:
                  "print" "exec"
                  ;; Python 3:
                  ;; False, None, and True are listed as keywords on the Python 3
                  ;; documentation, but since they also qualify as constants they are
                  ;; fontified like that in order to keep font-lock consistent between
                  ;; Python versions.
                  "nonlocal"
                  ;; Extra:
                  "self"))
          symbol-end)
     (1 font-lock-function-name-face))
    ;; Constants
    (,(rx symbol-start
          (or
           "Ellipsis" "False" "None" "NotImplemented" "True" "__debug__"
           ;; copyright, license, credits, quit and exit are added by the site
           ;; module and they are not intended to be used in programs
           "copyright" "credits" "exit" "license" "quit")
          symbol-end) . font-lock-constant-face)))

(defvar metewand-special-chars '("" "" "" "" "")
  "Special characters.")

(defun metewand--trim-string (string)
  "Remove whitespaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n\r]*" ""
                            (replace-regexp-in-string "[ \t\n\r]*\\'" "" string)))

(defun boron-eval-current-buffer ()
  (interactive)
  (let ((win (selected-window))
        (test-buffer (get-buffer-create "*test-buffer*"))
        (curr-buffer (current-buffer))
        (line nil)
        (continue t)
        (start nil)
        (end nil)
        (pos nil))
    (with-current-buffer test-buffer
      (erase-buffer))
    (with-current-buffer curr-buffer
      (goto-char (point-min))
      (while (not (eobp))
        (setq line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
        (message "String: %s" (substring line 0 3))
        (cond ((equal (substring line 0 3) "M-x")
               (let ((begin nil)
                     (end nil))
                 (while (string-match "\\(\".+?\"\\)" line)
                   (setq begin (match-beginning 1))
                   (setq end (match-end 1))
                   (setq line (concat
                               (substring line 0 begin)
                               (concat (apply (lambda (&rest args)
                                                (let ((result ""))
                                                  (dolist (arg args)
                                                    (when (equal arg " ")
                                                      (setq arg " SPC "))
                                                    (setq result (concat result arg)))
                                                  result))
                                              (split-string (substring line (+ begin 1) (- end 1))
                                                            "" t)))
                               (substring line end))))))
              ((equal (substring line 0 3) "M-:")
               (setq line
                     (concat
                      "M-: "
                      (apply (lambda (&rest args)
                               (let ((result ""))
                                 (dolist (arg args)
                                   (when (equal arg " ")
                                     (setq arg " SPC "))
                                   (setq result (concat result arg)))
                                 result))
                             (split-string (substring line 4) "" t))))))
        (when (not (equal (substring line 0 1)  "#"))
          (unwind-protect
              (with-current-buffer test-buffer
                (set-window-buffer win test-buffer t)
                (execute-kbd-macro
                 (edmacro-parse-keys line)))
            (set-window-buffer win curr-buffer t)))
        (search-forward "\n" nil t)))))

(defun boron-feature (text text2)
  (interactive "sText: \nsText2: ")
  (message "Feature: %s, %s" text text2))

(defun boron-scenario (text)
  (interactive "sText: ")
  (message "Feature: %s" text))

(defun boron-assert-equal (assertion)
  (interactive "sEqual to: ")
  (let ((test-buffer "*test-buffer*"))
    (if (equal assertion
               (with-current-buffer test-buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
        (progn
          (message "Test passed!")
          (run-hook-with-args boron-reporter-scenario-passed-hook "Test feature"))
      (progn
        (message "Test failed!")
        (run-hook-with-args boron-reporter-scenario-failed-hook "Test feature")))))

(defun boron-insert (text)
  (interactive "sText: ")
  (let ((test-buffer "*test-buffer*"))
    (with-current-buffer test-buffer
      (insert text))))

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

(defconst boron-syntax-propertize-function
  (syntax-propertize-rules
   ((rx "(.*)")
    (0 (ignore (python-syntax-stringify))))))

(defvar boron-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table.")

;;;###autoload
(define-derived-mode boron-mode fundamental-mode "Boron"
  "Major mode for editing boron files.

\\{boron-mode-map}"
  ;; (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  (set (make-local-variable 'comment-start) "#")
  ;; (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  
  ;; (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  ;; (setq-local comment-add 1)		;default to `;;' in comment-region
  ;; (setq-local comment-column 40)
  ;; (setq-local comment-use-syntax t)
  
  (set (make-local-variable 'font-lock-defaults)
       '(boron-font-lock-keywords nil nil nil nil))

  (set (make-local-variable 'syntax-propertize-function)
       boron-syntax-propertize-function)

  (set-syntax-table boron-syntax-table)

  
  ;; (set (make-local-variable 'paragraph-start) "\\s-*$")
  ;; (set (make-local-variable 'fill-paragraph-function)
  ;;      #'python-fill-paragraph)
  
  (set (make-local-variable 'paragraph-start) "\\s-*$"))

(provide 'boron)
