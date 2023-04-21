;;; conf-data-toml.el --- A conf-aode for Data.toml files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 TEC
;;
;; Author: TEC <contact@tecosaur.net>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: November 05, 2022
;; Modified: November 05, 2022
;; Version: 0.0.1
;; Keywords: data files tools
;; Homepage: https://github.com/tecosaur/conf-data-toml
;; Package-Requires: ((emacs "26.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'conf-mode)
(require 'imenu)

(with-eval-after-load 'consult-imenu
  (add-to-list
   'consult-imenu-config
   '(conf-data-toml-mode :types
     ((?c "Configuration")
      (?d "Datasets")))))

(defun conf-data-toml--create-index ()
  "Scan the buffer for relevant entries."
  (let ((start-time (float-time))
        (pos (point-max))
        next-pos
        index-alist)
    (goto-char pos)
    (while (and pos (< (- (float-time) start-time) imenu-max-index-time))
      (setq next-pos (re-search-backward
                      "^\\[\\[\\([A-Za-z0-9_]+\\|\"[^\"]+\"\\)\\]\\]" nil t))
      (when next-pos
        (let* ((name (propertize (string-trim
                                  (substring-no-properties (match-string 1))
                                  "\"" "\"")
                                 'face 'font-lock-function-name-face))
               (transformers
                (list (cons (propertize "*" 'face 'font-lock-keyword-face)
                            (if imenu-use-markers (point-marker) (point))))))
          (save-excursion
            (while (re-search-forward "\\[\\[.*\\.\\(storage\\|loader\\|writer\\)\\]\\]" pos t)
              (let ((trans-type (propertize (substring-no-properties (match-string 1))
                                            'face 'font-lock-type-face))
                    (trans-marker (save-excursion
                                    (goto-char (match-beginning 0))
                                    (if imenu-use-markers (point-marker) (point)))))
                (push (cons (if (re-search-forward "\\=\n[\t ]*driver ?= ?\"\\([a-z_]+\\)\"" nil t)
                                (propertize
                                 (substring-no-properties (match-string 1))
                                 'face 'font-lock-string-face)
                              (propertize "unknown" 'face '(warning italic)))
                            trans-marker)
                      (alist-get trans-type transformers nil nil #'equal)))))
          (setq transformers (nreverse transformers))
          (and transformers
               (push (cons name transformers)
                     (alist-get "Datasets" index-alist nil nil #'equal)))))
      (setq pos next-pos))
    (setq pos (re-search-backward "^\\s-*\\[?config\\.\\([A-Za-z0-9_.]+\\)" nil t))
    (while (and pos (< (- (float-time) start-time) imenu-max-index-time))
      (push (cons (match-string 1) (if imenu-use-markers (point-marker) (point)))
            (alist-get "Configuration" index-alist nil nil #'equal))
      (setq pos (re-search-backward "^\\s-*\\[?config\\.\\([A-Za-z0-9_.]+\\)" nil t)))
    (when (re-search-backward "^plugins = \\[" nil t)
      (push (cons "Plugins" (if imenu-use-markers (point-marker) (point)))
            index-alist))
    (when (re-search-backward "^name = \"" nil t)
      (push (cons "Name" (if imenu-use-markers (point-marker) (point)))
            index-alist))
    index-alist))

(defun conf-data-toml--fontify-section ()
  "Fontify a TOML section representing a dataset or configuration."
  (let ((start (point))
        (end (save-excursion
               (backward-char 1)
               (condition-case nil
                   (progn
                     (forward-list)
                     (1- (point)))
                 (scan-error
                  (end-of-line)
                  (point))))))
    (if (= (char-after (point)) ?\")
        (condition-case nil
            (goto-char (scan-sexps (point) 1))
          (scan-error))
      (re-search-forward "[].]" nil t)
      (backward-char 1))
    (cond
     ((string= (buffer-substring start (point)) "config")
      (put-text-property start end 'face 'font-lock-type-face))
     ((or (string-match-p "\\[\\["
                          (buffer-substring (line-beginning-position) start))
          (save-excursion
            (and (re-search-backward "^\\s-*\\[+" nil t)
                 (re-search-backward "^\\s-*\\[+" nil t)
                 (progn
                   (goto-char (match-end 0))
                   (forward-char -1)
                   (string-match-p
                    (regexp-quote
                     (buffer-substring
                      (1+ (point))
                      (condition-case nil
                          (1- (scan-sexps (point) 1))
                        (scan-error (1+ (point))))))
                    (buffer-substring start end))))))
      (put-text-property start (point) 'face 'font-lock-function-name-face)
      (put-text-property (point) end 'face 'font-lock-type-face))
     (t
      (put-text-property start end 'face 'error)))
    (goto-char end)
    nil))

(defun conf-data-toml--fontify-description ()
  "Fontify a (possibly multi-line) data set description."
  (let ((start (save-excursion
                 (skip-chars-backward "\"")
                 (point)))
        (end (save-excursion
               (forward-char -1)
               (condition-case nil
                   (goto-char (scan-sexps (point) 1))
                 (scan-error))
               (skip-chars-forward "\"")
               (point))))
    (put-text-property start end 'face 'font-lock-doc-face)
    nil))

(defvar conf-data-toml-font-lock-keywords
  '(("^\\s-*\\(uuid\\)\\s-*=\\s-*\\(\"[a-z0-9-]+\"\\)"
     (1 'font-lock-builtin-face prepend)
     (2 'shadow prepend))
    ("^\\s-*\\(description\\)\\s-*=\\s-*\\(\"+\\)"
     (1 'font-lock-builtin-face prepend)
     (2 (conf-data-toml--fontify-description)))
    ("^\\s-*\\(driver\\|type\\)"
     1 'font-lock-builtin-face prepend)
    ("^\\(data_config_version\\|name\\|plugins\\)"
     1 'font-lock-builtin-face prepend)
    ("[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\([T ][0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\(?:\\.[0-9]+\\)?\\(?:Z\\|[+-][0-9][0-9]:[0-9][0-9]\\)\\)?$" ; rfc3339 timestamp
     (0 'font-lock-constant-face prepend))
    ("^\\s-*\\(config\\)\\(.+?\\)\\s-*="
     (1 'font-lock-type-face)
     (2 'font-lock-variable-name-face nil t))
    ("^\\s-*\\(.+?\\)\\(?:\\[\\(.*?\\)\\]\\)?\\s-*="
     (1 'font-lock-variable-name-face)
     (2 'font-lock-constant-face nil t))
    ("\\_<false\\|true\\_>" 0 'font-lock-keyword-face)
    ("^\\s-*\\[+"
     (0 (conf-data-toml--fontify-section))))
  "Font-lock keywords for `conf-data-toml-mode'.")

;;;###autoload
(define-derived-mode conf-data-toml-mode conf-mode "Conf[TOML:Data]"
  "Conf Mode starter for Data.toml files."
  (conf-mode-initialize "#" 'conf-data-toml-font-lock-keywords)
  (setq-local conf-assignment-column 0
              conf-assignment-sign ?=
              imenu-create-index-function #'conf-data-toml--create-index))

(provide 'conf-data-toml)
;;; conf-data-toml.el ends here
