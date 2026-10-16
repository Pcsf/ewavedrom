;;; ewavedrom-test.el --- ERT suite for ewavedrom  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l test/ewavedrom-test.el -f ert-run-tests-batch-and-exit
;;
;; test/reference/*.svg is what WaveDrom itself draws for test/cases/*.json
;; (tools/gen-reference.ts); the golden tests demand the same bytes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewavedrom)

(defconst ewavedrom-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun ewavedrom-test--slurp (file)
  (with-temp-buffer
    (set-buffer-multibyte t)
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (buffer-string)))

(defun ewavedrom-test--cases ()
  (directory-files (expand-file-name "cases" ewavedrom-test--dir) t "\\.json\\'"))

;;;; Reader

(ert-deftest ewavedrom-test-read-js-literal ()
  (let ((v (ewavedrom-read-string
            "// lead comment
{ signal: [ { name: 'a\\'b', wave: \"0\\\"1\", }, /* gap */ ],
  'quoted': -1.5e1, n: .5, t: true, f: false, z: null, i: 0x1F, }")))
    (should (ewavedrom--obj-p v))
    (should (equal (ewavedrom--keys v) '("signal" "quoted" "n" "t" "f" "z" "i")))
    (let ((lane (aref (ewavedrom--get v "signal") 0)))
      (should (equal (ewavedrom--get lane "name") "a'b"))
      (should (equal (ewavedrom--get lane "wave") "0\"1")))
    (should (= (ewavedrom--get v "quoted") -15))
    (should (= (ewavedrom--get v "n") 0.5))
    (should (eq (ewavedrom--get v "t") t))
    (should (eq (ewavedrom--get v "f") :false))
    (should (eq (ewavedrom--get v "z") :null))
    (should (= (ewavedrom--get v "i") 31))
    (should (= (length (ewavedrom--get v "signal")) 1))))

(ert-deftest ewavedrom-test-read-escapes-and-unicode ()
  (let ((v (ewavedrom-read-string "{a: 'tab\\tnl\\nu\\u00b5 µ', b: [1,,2]}")))
    (should (equal (ewavedrom--get v "a") "tab\tnl\nuµ µ"))
    (should (= (length (ewavedrom--get v "b")) 3))))

(ert-deftest ewavedrom-test-read-error-names-position ()
  (let ((err (should-error (ewavedrom-read-string "{ signal: [\n  { name: 'a' wave: '0' }\n] }")
                           :type 'user-error)))
    (should (string-match-p "line 2, column 15" (cadr err)))))

(ert-deftest ewavedrom-test-read-error-unterminated ()
  (let ((err (should-error (ewavedrom-read-string "{ a: 'open")
                           :type 'user-error)))
    (should (string-match-p "line 1" (cadr err)))))

(ert-deftest ewavedrom-test-render-rejects-non-signal ()
  (should-error (ewavedrom-render-string "{ reg: [] }") :type 'user-error)
  (should-error (ewavedrom-render-string "[1, 2]") :type 'user-error))

;;;; Number formatting, the one place JS and Lisp disagree by default

(ert-deftest ewavedrom-test-js-number-strings ()
  (should (equal (ewavedrom--js-str 40.0) "40"))
  (should (equal (ewavedrom--js-str -0.0) "0"))
  (should (equal (ewavedrom--js-str 14.21) "14.21"))
  (should (equal (ewavedrom--js-str (* 0.7 40)) "28"))
  (should (equal (ewavedrom--js-str (+ 0.1 0.2)) "0.30000000000000004"))
  (should (equal (ewavedrom--js-str 1e-7) "1e-7"))
  (should (equal (ewavedrom--js-str 1e21) "1e+21"))
  (should (equal (ewavedrom--js-str 3) "3")))

;;;; Golden: same bytes as WaveDrom

(dolist (case (ewavedrom-test--cases))
  (let* ((name (file-name-base case))
         (ref (expand-file-name (concat "reference/" name ".svg") ewavedrom-test--dir)))
    (eval
     `(ert-deftest ,(intern (concat "ewavedrom-test-golden-" name)) ()
        ,(format "Rendering %s matches WaveDrom byte for byte." name)
        (let ((got (ewavedrom-render-string (ewavedrom-test--slurp ,case)))
              (want (ewavedrom-test--slurp ,ref)))
          (unless (equal got want)
            (let ((i (or (cl-mismatch got want) 0)))
              (ert-fail (list :first-difference i
                              :got (substring got (max 0 (- i 60)) (min (length got) (+ i 60)))
                              :want (substring want (max 0 (- i 60)) (min (length want) (+ i 60)))))))))
     t)))

;;;; Corpus coverage: the golden tests prove only what the corpus exercises

(defun ewavedrom-test--corpus ()
  (mapcar (lambda (f) (ewavedrom-read-string (ewavedrom-test--slurp f)))
          (ewavedrom-test--cases)))

(defun ewavedrom-test--lanes (node)
  "Every lane object reachable from a signal array NODE."
  (cond ((vectorp node) (apply #'append (mapcar #'ewavedrom-test--lanes node)))
        ((ewavedrom--obj-p node) (list node))))

(ert-deftest ewavedrom-test-corpus-covers-features ()
  (let* ((docs (ewavedrom-test--corpus))
         (lanes (apply #'append (mapcar (lambda (d) (ewavedrom-test--lanes (ewavedrom--get d "signal"))) docs)))
         (waves (mapconcat (lambda (l) (let ((w (ewavedrom--get l "wave"))) (if (stringp w) w ""))) lanes ""))
         (missing nil))
    (dolist (c (string-to-list "pnPNhlHL01xz=23456789du.|<>"))
      (unless (memq c (string-to-list waves)) (push (string c) missing)))
    (dolist (key '("period" "phase" "data" "node" "over" "under"))
      (unless (cl-some (lambda (l) (ewavedrom--has l key)) lanes) (push key missing)))
    (unless (cl-some (lambda (l) (stringp (ewavedrom--get l "data"))) lanes) (push "data-string" missing))
    (unless (cl-some (lambda (l) (vectorp (ewavedrom--get l "data"))) lanes) (push "data-array" missing))
    (unless (cl-some (lambda (l) (vectorp (ewavedrom--get l "wave"))) lanes) (push "piecewise" missing))
    (unless (cl-some (lambda (l) (null (ewavedrom--keys l))) lanes) (push "spacer" missing))
    (dolist (key '("head" "foot" "edge" "gaps"))
      (unless (cl-some (lambda (d) (ewavedrom--has d key)) docs) (push key missing)))
    (dolist (key '("text" "tick" "tock" "every"))
      (unless (cl-some (lambda (d) (let ((h (ewavedrom--get d "head")) (f (ewavedrom--get d "foot")))
                                     (or (and (ewavedrom--obj-p h) (ewavedrom--has h key))
                                         (and (ewavedrom--obj-p f) (ewavedrom--has f key)))))
                       docs)
        (push (concat "head/foot " key) missing)))
    (dolist (key '("hscale" "hbounds" "marks" "skin" "arcFontSize"))
      (unless (cl-some (lambda (d) (let ((c (ewavedrom--get d "config"))) (and c (ewavedrom--has c key)))) docs)
        (push (concat "config " key) missing)))
    (unless (cl-some (lambda (d) (let ((s (ewavedrom--get d "signal")))
                                   (cl-some (lambda (e) (and (vectorp e) (vectorp (cl-find-if #'vectorp e)))) s)))
                     docs)
      (push "nested group" missing))
    (should (equal missing nil))))

(ert-deftest ewavedrom-test-corpus-covers-every-arc-shape ()
  (let* ((edges (apply #'append
                       (mapcar (lambda (d) (append (ewavedrom--get d "edge") nil))
                               (ewavedrom-test--corpus))))
         (shapes (mapcar (lambda (e) (let ((w (car (split-string e))))
                                       (substring w 1 -1)))
                         edges))
         (labelled (cl-count-if (lambda (e) (cdr (split-string e))) edges)))
    (dolist (s '("-" "~" "-~" "~-" "-|" "|-" "-|-" "->" "~>" "-~>" "~->" "-|>" "|->"
                 "-|->" "<->" "<~>" "<-~>" "<-|>" "<-|->" "+"))
      (should (member s shapes)))
    (should (cl-some (lambda (s) (not (member s '("-" "~" "-~" "~-" "-|" "|-" "-|-" "->" "~>" "-~>" "~->" "-|>" "|->"
                                                  "-|->" "<->" "<~>" "<-~>" "<-|>" "<-|->" "+"))))
                     shapes))
    (should (> labelled 5))
    (should (< labelled (length edges)))))

(ert-deftest ewavedrom-test-corpus-covers-every-skin ()
  (let ((used (delq nil (mapcar (lambda (d) (let ((c (ewavedrom--get d "config")))
                                              (and c (ewavedrom--get c "skin"))))
                                (ewavedrom-test--corpus)))))
    (dolist (s (mapcar #'car ewavedrom-skins))
      (should (member s used)))))

;;;; Files, preview and Org Babel

(ert-deftest ewavedrom-test-json-file-detected-by-content ()
  (dolist (text '("{ signal: [] }"
                  "\n  {\"signal\": []}"
                  "// timing of the bus\n/* rev 2 */ { 'signal' : [] }"))
    (should (string-match-p ewavedrom-magic-regexp text)))
  (dolist (text '("{\"name\": \"x\", \"signal\": 1}" "[{signal: []}]" "{ signals: [] }"))
    (should-not (string-match-p ewavedrom-magic-regexp text)))
  (let ((file (make-temp-file "ewavedrom" nil ".json" "{ signal: [{wave: '01'}] }")))
    (unwind-protect
        (let ((buf (find-file-noselect file)))
          (unwind-protect
              (should (eq (buffer-local-value 'major-mode buf) 'ewavedrom-mode))
            (kill-buffer buf)))
      (delete-file file))))

(ert-deftest ewavedrom-test-render-file-writes-svg ()
  (let ((out (make-temp-file "ewavedrom" nil ".svg")))
    (unwind-protect
        (progn
          (ewavedrom-render-file (expand-file-name "cases/basic-tutorial.json" ewavedrom-test--dir) out)
          (should (equal (ewavedrom-test--slurp out)
                         (ewavedrom-test--slurp (expand-file-name "reference/basic-tutorial.svg"
                                                                  ewavedrom-test--dir)))))
      (delete-file out))))

(ert-deftest ewavedrom-test-preview-keeps-last-good-image ()
  (let ((src (generate-new-buffer "wave.json"))
        (ewavedrom-preview-display-function #'ignore))
    (unwind-protect
        (with-current-buffer src
          (insert "{signal: [{name: 'a', wave: '01'}]}")
          (ewavedrom-preview)
          (let* ((out (get-buffer (ewavedrom--preview-buffer-name src)))
                 (good (with-current-buffer out ewavedrom--preview-svg)))
            (should (string-prefix-p "<svg" good))
            (erase-buffer)
            (insert "{signal: [{name: 'a' wave: '01'}]}")
            (let ((msg (ewavedrom-preview)))
              (should (string-match-p "line 1" msg)))
            (should (equal (with-current-buffer out ewavedrom--preview-svg) good))))
      (let ((out (get-buffer (ewavedrom--preview-buffer-name src))))
        (when out (kill-buffer out)))
      (kill-buffer src))))

(ert-deftest ewavedrom-test-preview-mode-renders-on-save ()
  (let* ((dir (make-temp-file "ewavedrom" t))
         (file (expand-file-name "w.json" dir))
         (ewavedrom-preview-display-function #'ignore)
         buf)
    (unwind-protect
        (progn
          (setq buf (find-file-noselect file))
          (with-current-buffer buf
            (ewavedrom-preview-mode 1)
            (insert "{signal: [{name: 'save', wave: '0110'}]}")
            (save-buffer)
            (with-current-buffer (ewavedrom--preview-buffer-name buf)
              (should (string-match-p ">save<" ewavedrom--preview-svg)))))
      (when buf
        (let ((out (get-buffer (ewavedrom--preview-buffer-name buf))))
          (when out (kill-buffer out)))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest ewavedrom-test-babel-file-and-inline ()
  (require 'org)
  (require 'ob-ewavedrom)
  (let* ((dir (make-temp-file "ewavedrom" t))
         (default-directory dir)
         (org-confirm-babel-evaluate nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#+begin_src wavedrom :file out.svg\n{signal: [{name: 'b', wave: '01'}]}\n#+end_src\n")
          (goto-char (point-min))
          (org-babel-execute-src-block)
          (should (file-exists-p (expand-file-name "out.svg" dir)))
          (should (string-prefix-p "<svg" (ewavedrom-test--slurp (expand-file-name "out.svg" dir))))
          (should (string-match-p "\\[\\[file:out.svg\\]\\]" (buffer-string)))
          (should (string-prefix-p
                   "<svg"
                   (org-babel-execute:wavedrom "{signal: [{name: 'c', wave: '10'}]}" nil))))
      (delete-directory dir t))))

(provide 'ewavedrom-test)
;;; ewavedrom-test.el ends here
