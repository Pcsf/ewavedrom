;;; ob-ewavedrom.el --- Org Babel support for WaveDrom blocks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pcsf

;; Author: Pcsf <7274075+Pcsf@users.noreply.github.com>
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Evaluates #+begin_src wavedrom blocks with ewavedrom.  With :file the
;; diagram is written there and linked; without it the SVG markup is the
;; result, which `:results html' carries straight into an HTML export.

;;; Code:

(require 'ob)
(require 'ewavedrom)

(defvar org-babel-default-header-args:wavedrom
  '((:results . "file graphics") (:exports . "results"))
  "Default header arguments for wavedrom blocks.")

(defun org-babel-execute:wavedrom (body params)
  "Render the WaveJSON BODY; write it to :file in PARAMS when given."
  (let ((svg (ewavedrom-render-string body))
        (file (cdr (assq :file params))))
    (if (not file)
        svg
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file file (insert svg)))
      nil)))

(defun org-babel-prep-session:wavedrom (_session _params)
  "WaveDrom blocks have no sessions."
  (error "WaveDrom blocks do not support sessions"))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("wavedrom" . ewavedrom)))

(provide 'ob-ewavedrom)
;;; ob-ewavedrom.el ends here
