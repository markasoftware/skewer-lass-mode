;; -*- lexical-binding: t -*-

;; Copyright (c) Mark Polyakov 2021
;; Released under the MIT license.

(require 'skewer-mode)
(require 'slime)

(defun skewer-lass-compile-and-write (str)
  (unless (slime-connected-p)
    (error "Cannot compile LASS without a Swank connection."))
  (when
      (slime-eval '(cl:null (cl:find-package :lass)))
    (if (slime-eval '(cl:not (cl:null (cl:find-package :ql))))
        (error "The LASS package was not found, and Quicklisp is not available to install it.")
      (slime-eval '(ql:quickload :lass))))
  ;; if the expression is quoted, we want to un-quote it. TODO: gensym?
  (slime-eval `(cl:let ((lass-form (cl:read-from-string ,str)))
                       (cl:when (cl:and (cl:consp lass-form) (cl:eq 'cl:quote (cl:car lass-form)))
                                (cl:setf lass-form (cl:cadr lass-form)))
                       (lass:compile-and-write lass-form))))

(defun skewer-lass-eval-defun ()
  "Convert the defun at point to CSS, and send it to the browser."
  (interactive)
  (let* ((slime-defun-region (slime-region-for-defun-at-point))
         (start (car slime-defun-region))
         (end (cadr slime-defun-region))
         (rule (buffer-substring-no-properties start end)))
    (skewer-flash-region start end)
    (skewer-css (skewer-css-trim (skewer-lass-compile-and-write rule)))))

(defun skewer-lass-eval-last-sexp ()
  "Convert the sexp before point to CSS, and send it to the browser."
  (interactive)
  (let* ((start (save-excursion (backward-sexp) (point)))
         (end (point))
         (rule (buffer-substring-no-properties start end)))
    (skewer-flash-region start end)
    (skewer-css (skewer-css-trim (skewer-lass-compile-and-write rule)))))

(defun skewer-lass-eval-buffer ()
  "Convert the whole buffer from LASS to CSS, and send it to the browser."
  (interactive)
  (skewer-css (skewer-lass-compile-and-write (buffer-substring-no-properties (point-min) (point-max)))))

(defvar skewer-lass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-M-x") 'skewer-lass-eval-defun)
    (define-key map (kbd "C-c C-e") 'skewer-lass-eval-last-sexp)
    (define-key map (kbd "C-c C-k") 'skewer-lass-eval-buffer)
    (define-key map (kbd "C-c C-c") 'skewer-css-clear-all)
    map)
  "Keymap for skewer-lass-mode.")

;;;###autoload
(define-minor-mode skewer-lass-mode
  "Minor mode for sending LASS-generated CSS to a browser."
  :lighter " skewer-lass")

(provide 'skewer-lass)
