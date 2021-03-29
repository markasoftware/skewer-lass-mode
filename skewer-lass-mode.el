;; -*- lexical-binding: t -*-

;; Copyright (c) Mark Polyakov 2021
;; Released under the MIT license.

(require 'skewer-mode)
(require 'slime)

(defcustom skewer-lass-eval :auto
  "Whether to evaluate sexps before converting them from LASS to CSS. Can be t, nil, or the default,
  :auto, which evaluates forms when they're quoted or quasiquoted. This facilitates development
  when your LASS code is not stored in a standalone file but is mixed with other Lisp."
  :type '(choice (const :tag "Never evaluate" nil)
                 (const :tag "Always evaluate" t)
                 (const :tag "Evaluate when quoted" :auto))
  :group 'skewer)

(defun skewer-lass-feature-symbol (feature package name)
  "Return an s-exp for a (cond) clause that checks if the given feature is present in *features*,
  and if so returns the named symbol in the given package. This makes up for the difficulty of
  read-time conditionals in slime-eval's argument"
  `((cl:member ,feature cl:*features*) (cl:find-symbol ,name (cl:find-package ,package))))

(defun skewer-lass-compile-and-write (str)
  (unless (slime-connected-p)
    (error "Cannot compile LASS without a Swank connection."))
  (when
      (slime-eval '(cl:null (cl:find-package :lass)))
    (if (slime-eval '(cl:not (cl:null (cl:find-package :ql))))
        (error "The LASS package was not found, and Quicklisp is not available to install it.")
      (slime-eval '(ql:quickload :lass))))
  ;; TODO: gensym for lass-form and eof?
  (slime-eval
   `(cl:with-input-from-string
     (stream ,str)
     (cl:apply #'lass:compile-and-write
            (cl:loop
             :for lass-form := (cl:read stream nil 'eof)
             :until (cl:eq lass-form 'eof)
             :collect
             ,(cl-ecase skewer-lass-eval
                (nil `lass-form)
                ((t) `(cl:eval lass-form))
                (:auto
                 `(cl:if (cl:and (cl:consp lass-form)
                                 (cl:member
                                  (cl:car lass-form)
                                  (cl:list
                                   'cl:quote
                                   (cl:cond
                                    ,(skewer-lass-feature-symbol :sbcl "SB-INT" "QUASIQUOTE")
                                    ,(skewer-lass-feature-symbol :clisp "SYSTEM" "BACKQUOTE")
                                    ,(skewer-lass-feature-symbol :ecl "SI" "QUASIQUOTE")
                                    ,(skewer-lass-feature-symbol :abcl "SYSTEM" "BACKQ-LIST"))
                                   ;; CCL appears expands backquotes early
                                   )))
                         (cl:eval lass-form)
                         lass-form))))))))

(defun skewer-lass-eval-region (start end)
  "Convert the region to CSS, and send it to the browser."
  (interactive "r")
  (skewer-flash-region start end)
  (skewer-css (skewer-css-trim (skewer-lass-compile-and-write
                                (buffer-substring-no-properties start end)))))

(defun skewer-lass-eval-defun ()
  "Convert the defun at point to CSS, and send it to the browser."
  (interactive)
  (let* ((slime-defun-region (slime-region-for-defun-at-point))
         (start (car slime-defun-region))
         (end (cadr slime-defun-region)))
    (skewer-lass-eval-region start end)))

(defun skewer-lass-eval-last-sexp ()
  "Convert the sexp before point to CSS, and send it to the browser."
  (interactive)
  (let* ((start (save-excursion (backward-sexp) (point)))
         (end (point)))
    (skewer-lass-eval-region start end)))

(defun skewer-lass-eval-buffer ()
  "Convert the whole buffer from LASS to CSS, and send it to the browser."
  (interactive)
  (skewer-lass-eval-region (point-min) (point-max)))

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
