;;; ewavedrom.el --- WaveDrom timing diagrams in pure Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pcsf
;; Rendering logic ported from WaveDrom, Copyright (c) 2011-2024 Aliaksei
;; Chapyzhenka, under the MIT licence (see LICENSE).

;; Author: Pcsf <7274075+Pcsf@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: hardware, tools, multimedia
;; URL: https://github.com/Pcsf/ewavedrom

;;; Commentary:

;; Renders WaveDrom WaveJSON timing diagrams to SVG without any external
;; program: the reader, the renderer and the skins are all Lisp.  The output
;; is the same SVG, byte for byte, that WaveDrom produces for the same input,
;; including edges and arrows.  `reg' and `assign' diagrams are not supported.
;;
;; Entry points:
;;   `ewavedrom-render-string'  WaveJSON text -> SVG string
;;   `ewavedrom-render-file'    WaveJSON file -> SVG file
;;   `ewavedrom-preview'        show the current buffer's diagram
;;   `ewavedrom-preview-mode'   re-render the preview on every save
;;   `ewavedrom-mode'           major mode for .wjson files

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ewavedrom-data)

(defgroup ewavedrom nil
  "WaveDrom timing diagrams rendered in Emacs Lisp."
  :group 'tools
  :prefix "ewavedrom-")

(defcustom ewavedrom-default-skin "default"
  "Skin used when the WaveJSON does not name one in `config.skin'."
  :type 'string)

(defcustom ewavedrom-dark-skin-fix t
  "Non-nil makes the `dark' skin readable.
WaveDrom draws that skin on a white canvas, so its white lines and text
vanish, and edge labels sit on white boxes.  The fix paints the canvas
`ewavedrom-dark-background' and recolours edges with
`ewavedrom-dark-accent'.  Nil gives WaveDrom's output unchanged."
  :type 'boolean)

(defcustom ewavedrom-dark-background "#000000"
  "Canvas colour for the `dark' skin when `ewavedrom-dark-skin-fix' is on."
  :type 'color)

(defcustom ewavedrom-dark-accent "#58a6ff"
  "Edge, arrow and group colour for the `dark' skin when the fix is on."
  :type 'color)

;;;; Values
;;
;; JS objects are (:obj (KEY . VAL)...) so key order follows insertion order
;; as it does in JS.  Arrays are vectors.  undefined is nil, and null, false
;; are :null, :false.

(defun ewavedrom--obj-p (x)
  "Non-nil if X is a JS object."
  (eq (car-safe x) :obj))

(defun ewavedrom--obj (&rest kvs)
  "Make a JS object from alternating keys and values KVS."
  (let ((obj (list :obj)))
    (while kvs
      (ewavedrom--put obj (car kvs) (cadr kvs))
      (setq kvs (cddr kvs)))
    obj))

(defun ewavedrom--get (obj key)
  "Property KEY of OBJ, or nil."
  (and (ewavedrom--obj-p obj) (cdr (assoc key (cdr obj)))))

(defun ewavedrom--has (obj key)
  "Non-nil if OBJ has own property KEY."
  (and (ewavedrom--obj-p obj) (assoc key (cdr obj)) t))

(defun ewavedrom--keys (obj)
  "Own keys of OBJ in insertion order."
  (mapcar #'car (cdr obj)))

(defun ewavedrom--put (obj key val)
  "Set KEY of OBJ to VAL; a new key goes last, as in JS."
  (let ((cell (assoc key (cdr obj))))
    (if cell
        (setcdr cell val)
      (setcdr (last obj) (list (cons key val))))
    val))

(defconst ewavedrom--nan (/ 0.0 0.0))

(defun ewavedrom--nan-p (x)
  (and (floatp x) (/= x x)))

(defun ewavedrom--truthy (v)
  "JS truthiness of V."
  (not (or (memq v '(nil :null :false))
           (equal v "")
           (and (numberp v) (or (= v 0) (ewavedrom--nan-p v))))))

(defun ewavedrom--num (v)
  "JS Number(V)."
  (cond ((numberp v) v)
        ((eq v t) 1)
        ((memq v '(:false :null)) 0)
        ((stringp v)
         (let ((s (string-trim v)))
           (cond ((string= s "") 0)
                 ((string-match "\\`[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'" s)
                  (float (string-to-number s)))
                 ((string-match "\\`0[xX]\\([0-9a-fA-F]+\\)\\'" s)
                  (float (string-to-number (match-string 1 s) 16)))
                 ((string-match "\\`\\([-+]?\\)Infinity\\'" s)
                  (if (string= (match-string 1 s) "-") -1.0e+INF 1.0e+INF))
                 (t ewavedrom--nan))))
        ((vectorp v)
         (cond ((= (length v) 0) 0)
               ((= (length v) 1) (ewavedrom--num (ewavedrom--str (aref v 0))))
               (t ewavedrom--nan)))
        (t ewavedrom--nan)))

(defun ewavedrom--or (a b)
  "JS A || B."
  (if (ewavedrom--truthy a) a b))

(defun ewavedrom--nor (v default)
  "Number(V || DEFAULT)."
  (ewavedrom--num (ewavedrom--or v default)))

;;;; JS number and string conversion

(defun ewavedrom--js-str (x)
  "JS Number.prototype.toString of the number X."
  (cond
   ((integerp x) (number-to-string x))
   ((ewavedrom--nan-p x) "NaN")
   ((= x 1.0e+INF) "Infinity")
   ((= x -1.0e+INF) "-Infinity")
   ((= x 0) "0")
   ((and (= x (ffloor x)) (< (abs x) 1e21)) (format "%d" (truncate x)))
   (t
    ;; Emacs prints the shortest round-trip digits; only the layout differs.
    (let* ((s (number-to-string (abs x)))
           (sign (if (< x 0) "-" ""))
           digits exp)
      (if (string-match "\\`\\([0-9]\\)\\(?:\\.\\([0-9]+\\)\\)?e\\([-+]?[0-9]+\\)\\'" s)
          (setq digits (concat (match-string 1 s) (or (match-string 2 s) ""))
                exp (1+ (string-to-number (match-string 3 s))))
        (string-match "\\`\\([0-9]*\\)\\.\\([0-9]*\\)\\'" s)
        (let ((int (match-string 1 s)) (frac (match-string 2 s)))
          (if (string-match-p "\\`0*\\'" int)
              (let ((nz (or (string-match "[1-9]" frac) 0)))
                (setq digits (substring frac nz) exp (- nz)))
            (setq digits (concat int frac) exp (length int)))))
      (setq digits (replace-regexp-in-string "0+\\'" "" digits))
      (let ((k (length digits)) (n exp))
        (concat
         sign
         (cond
          ((and (<= k n) (<= n 21)) (concat digits (make-string (- n k) ?0)))
          ((and (< 0 n) (<= n 21)) (concat (substring digits 0 n) "." (substring digits n)))
          ((and (< -6 n) (<= n 0)) (concat "0." (make-string (- n) ?0) digits))
          (t (concat (substring digits 0 1)
                     (if (> k 1) (concat "." (substring digits 1)) "")
                     "e" (if (>= (1- n) 0) "+" "-")
                     (number-to-string (abs (1- n))))))))))))

(defun ewavedrom--str (v)
  "JS String(V)."
  (cond ((stringp v) v)
        ((numberp v) (ewavedrom--js-str v))
        ((eq v t) "true")
        ((eq v :false) "false")
        ((eq v :null) "null")
        ((null v) "undefined")
        ((vectorp v) (mapconcat (lambda (e) (if (memq e '(nil :null)) "" (ewavedrom--str e))) v ","))
        (t "[object Object]")))

(defun ewavedrom--to-fixed (x dp)
  "JS X.toFixed(DP): exact decimal value, ties rounded up."
  (if (or (ewavedrom--nan-p x) (>= (abs x) 1e21))
      (ewavedrom--js-str x)
    (let* ((neg (< x 0))
           (exact (format "%.60f" (abs x)))
           (dot (string-match "\\." exact))
           (int (substring exact 0 dot))
           (frac (substring exact (1+ dot)))
           (keep (concat int (substring frac 0 dp)))
           (up (>= (aref frac dp) ?5))
           (n (string-to-number keep)))
      (when up (setq n (1+ n)))
      (let* ((s (format (format "%%0%dd" (1+ dp)) n))
             (body (if (> dp 0)
                       (concat (substring s 0 (- (length s) dp)) "." (substring s (- (length s) dp)))
                     s)))
        (concat (if (and neg (string-match-p "[1-9]" body)) "-" "") body)))))

(defun ewavedrom--round (x)
  "JS Math.round."
  (floor (+ x 0.5)))

(defun ewavedrom--count (len)
  "How many integers i satisfy 0 <= i < LEN."
  (if (> len 0) (ceiling len) 0))

;;;; Reader

(defun ewavedrom--fail (fmt &rest args)
  (user-error "WaveJSON line %d, column %d: %s"
              (line-number-at-pos)
              (1+ (- (point) (line-beginning-position)))
              (apply #'format fmt args)))

(defun ewavedrom--skip ()
  "Skip whitespace and comments."
  (while (progn
           (skip-chars-forward " \t\n\r\f\v\u00a0\ufeff")
           (cond ((looking-at "//") (forward-line 1) t)
                 ((looking-at "/\\*")
                  (unless (search-forward "*/" nil t)
                    (ewavedrom--fail "unterminated comment"))
                  t)))))

(defun ewavedrom--describe ()
  (if (eobp) "end of input" (format "`%c'" (char-after))))

(defun ewavedrom--expect (char)
  (ewavedrom--skip)
  (if (eq (char-after) char)
      (forward-char)
    (ewavedrom--fail "expected `%c' but found %s" char (ewavedrom--describe))))

(defun ewavedrom--read-string-literal ()
  (let ((quote (char-after))
        (start (point))
        (chars nil)
        (pending-high nil))
    (forward-char)
    (while (not (eq (char-after) quote))
      (let ((c (char-after)))
        (cond
         ((or (null c) (eq c ?\n))
          (goto-char start)
          (ewavedrom--fail "unterminated string"))
         ((eq c ?\\)
          (forward-char)
          (let ((e (char-after)))
            (forward-char)
            (pcase e
              ('nil (goto-char start) (ewavedrom--fail "unterminated string"))
              (?n (push ?\n chars)) (?t (push ?\t chars)) (?r (push ?\r chars))
              (?b (push ?\b chars)) (?f (push ?\f chars)) (?v (push ?\v chars))
              (?0 (push 0 chars))
              (?\n nil)
              (?x (if (looking-at "[0-9a-fA-F]\\{2\\}")
                      (progn (push (string-to-number (match-string 0) 16) chars)
                             (goto-char (match-end 0)))
                    (ewavedrom--fail "bad \\x escape")))
              (?u (let ((code
                         (cond ((looking-at "{\\([0-9a-fA-F]+\\)}")
                                (prog1 (string-to-number (match-string 1) 16)
                                  (goto-char (match-end 0))))
                               ((looking-at "[0-9a-fA-F]\\{4\\}")
                                (prog1 (string-to-number (match-string 0) 16)
                                  (goto-char (match-end 0))))
                               (t (ewavedrom--fail "bad \\u escape")))))
                    (cond ((and (>= code #xD800) (<= code #xDBFF))
                           (setq pending-high code))
                          ((and pending-high (>= code #xDC00) (<= code #xDFFF))
                           (push (+ #x10000 (ash (- pending-high #xD800) 10) (- code #xDC00)) chars)
                           (setq pending-high nil))
                          (t (push code chars)))))
              (_ (push e chars)))))
         (t (push c chars) (forward-char)))))
    (forward-char)
    (apply #'string (nreverse chars))))

(defun ewavedrom--read-number ()
  (if (looking-at "\\([-+]?\\)\\(?:0[xX]\\([0-9a-fA-F]+\\)\\|\\(\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\)\\|\\(Infinity\\)\\)")
      (let* ((neg (string= (match-string 1) "-"))
             (v (cond ((match-string 2) (float (string-to-number (match-string 2) 16)))
                      ((match-string 3) (float (string-to-number (match-string 3))))
                      (t 1.0e+INF))))
        (goto-char (match-end 0))
        (if neg (- v) v))
    (ewavedrom--fail "bad number")))

(defun ewavedrom--read-key ()
  (ewavedrom--skip)
  (let ((c (char-after)))
    (cond ((memq c '(?\" ?\')) (ewavedrom--read-string-literal))
          ((looking-at "[[:alpha:]_$][[:alnum:]_$]*")
           (goto-char (match-end 0))
           (match-string-no-properties 0))
          ((looking-at "[-+.0-9]")
           (ewavedrom--js-str (ewavedrom--read-number)))
          (t (ewavedrom--fail "expected a property name but found %s" (ewavedrom--describe))))))

(defun ewavedrom--read-object ()
  (forward-char)
  (let ((obj (list :obj)) (done nil))
    (while (not done)
      (ewavedrom--skip)
      (if (eq (char-after) ?\})
          (progn (forward-char) (setq done t))
        (let ((key (ewavedrom--read-key)))
          (ewavedrom--expect ?:)
          (ewavedrom--put obj key (ewavedrom--read-value))
          (ewavedrom--skip)
          (pcase (char-after)
            (?, (forward-char))
            (?\} (forward-char) (setq done t))
            (_ (ewavedrom--fail "expected `,' or `}' but found %s" (ewavedrom--describe)))))))
    obj))

(defun ewavedrom--read-array ()
  (forward-char)
  (let ((items nil) (done nil))
    (while (not done)
      (ewavedrom--skip)
      (pcase (char-after)
        (?\] (forward-char) (setq done t))
        (?, (forward-char) (push nil items))
        (_ (push (ewavedrom--read-value) items)
           (ewavedrom--skip)
           (pcase (char-after)
             (?, (forward-char))
             (?\] (forward-char) (setq done t))
             (_ (ewavedrom--fail "expected `,' or `]' but found %s" (ewavedrom--describe)))))))
    (vconcat (nreverse items))))

(defun ewavedrom--read-value ()
  (ewavedrom--skip)
  (let ((c (char-after)))
    (cond
     ((null c) (ewavedrom--fail "unexpected end of input"))
     ((eq c ?\{) (ewavedrom--read-object))
     ((eq c ?\[) (ewavedrom--read-array))
     ((memq c '(?\" ?\')) (ewavedrom--read-string-literal))
     ((looking-at "\\(true\\|false\\|null\\|undefined\\|NaN\\)\\_>")
      (goto-char (match-end 0))
      (pcase (match-string 1)
        ("true" t) ("false" :false) ("null" :null) ("undefined" nil) (_ ewavedrom--nan)))
     ((looking-at "[-+.0-9]\\|[-+]?Infinity") (ewavedrom--read-number))
     (t (ewavedrom--fail "unexpected %s" (ewavedrom--describe))))))

(defun ewavedrom-read-string (string)
  "Read the WaveJSON (a JavaScript object literal) in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((v (ewavedrom--read-value)))
      (ewavedrom--skip)
      (unless (eobp)
        (ewavedrom--fail "unexpected %s after the value" (ewavedrom--describe)))
      v)))

;;;; SVG serialisation (onml stringify)

(defun ewavedrom--attr-str (v)
  (if (vectorp v)
      (mapconcat (lambda (e) (if (memq e '(nil :null)) "" (ewavedrom--str e))) v " ")
    (ewavedrom--str v)))

(defun ewavedrom--clean (txt)
  (mapconcat #'identity
             (cl-remove-if (lambda (l) (string= (string-trim l) "")) (split-string txt "\n"))
             "\n"))

(defun ewavedrom--stringify (node)
  "Serialise the onml NODE, a list or vector (TAG [ATTRS] CHILD...)."
  (let* ((a (if (vectorp node) (append node nil) node))
         (len (length a))
         (tag (ewavedrom--str (car a)))
         (res (concat "<" tag))
         (parts nil)
         (flat t)
         (empty (= len 1))
         (i 1))
    (unless empty
      (dolist (e (cdr a))
        (let ((skip nil))
          (when (= i 1)
            (if (ewavedrom--obj-p e)
                (progn
                  (dolist (kv (cdr e))
                    (setq res (concat res " " (car kv) "=\"" (ewavedrom--attr-str (cdr kv)) "\"")))
                  (if (= len 2) (setq empty t) (setq res (concat res ">")))
                  (setq skip t))
              (setq res (concat res ">"))))
          (unless (or skip
                      (not (or (ewavedrom--truthy e) (equal e 0) (equal e 0.0) (equal e ""))))
            (if (or (stringp e) (numberp e) (eq e t))
                (push (ewavedrom--str e) parts)
              (setq flat nil)
              (push (ewavedrom--stringify e) parts))))
        (setq i (1+ i))))
    (cond (empty (concat res "/>"))
          (flat (concat res (ewavedrom--clean (apply #'concat (nreverse parts))) "</" tag ">"))
          (t (concat res (apply #'concat (nreverse parts)) "</" tag ">")))))

(defun ewavedrom--tt (x y &rest kvs)
  "onml tt: a translate transform for X, Y, then attributes KVS."
  (let ((obj (list :obj)))
    (when (or (ewavedrom--truthy x) (ewavedrom--truthy y))
      (ewavedrom--put obj "transform"
                      (concat "translate("
                              (ewavedrom--str (ewavedrom--or x 0))
                              (if (ewavedrom--truthy y) (concat "," (ewavedrom--str y)) "")
                              ")")))
    (while kvs
      (ewavedrom--put obj (car kvs) (cadr kvs))
      (setq kvs (cddr kvs)))
    obj))

;;;; Text

(defconst ewavedrom--tspan-token
  "<o>\\|<ins>\\|<s>\\|<sub>\\|<sup>\\|<b>\\|<i>\\|<tt>\\|</o>\\|</ins>\\|</s>\\|</sub>\\|</sup>\\|</b>\\|</i>\\|</tt>")

(defconst ewavedrom--tspan-trans
  '(("<o>" add "text-decoration" "overline") ("</o>" del "text-decoration" "overline")
    ("<ins>" add "text-decoration" "underline") ("</ins>" del "text-decoration" "underline")
    ("<s>" add "text-decoration" "line-through") ("</s>" del "text-decoration" "line-through")
    ("<b>" add "font-weight" "bold") ("</b>" del "font-weight" "bold")
    ("<i>" add "font-style" "italic") ("</i>" del "font-style" "italic")
    ("<sub>" add "baseline-shift" "sub" "font-size" ".7em") ("</sub>" del "baseline-shift" "sub" "font-size" ".7em")
    ("<sup>" add "baseline-shift" "super" "font-size" ".7em") ("</sup>" del "baseline-shift" "super" "font-size" ".7em")
    ("<tt>" add "font-family" "monospace") ("</tt>" del "font-family" "monospace")))

(defun ewavedrom--xscape (s)
  (replace-regexp-in-string
   "[&\"<>]"
   (lambda (m) (pcase m ("&" "&amp;") ("\"" "&quot;") ("<" "&lt;") (_ "&gt;")))
   s t t))

(defun ewavedrom--tspan (str)
  "tspan.parse: STR with <b>-style markup as a list of onml nodes."
  (cond
   ((null str) nil)
   ((numberp str) (list (ewavedrom--js-str str)))
   ((not (stringp str)) (list str))
   (t
    (let ((state (mapcar #'list '("text-decoration" "font-weight" "font-style"
                                  "baseline-shift" "font-size" "font-family")))
          (res nil)
          (case-fold-search nil)
          (done nil))
      (cl-flet ((dump ()
                  (let ((obj (list :obj)))
                    (dolist (p state)
                      (when (cdr p)
                        (ewavedrom--put obj (car p) (mapconcat #'identity (cdr p) " "))))
                    obj)))
        (while (not done)
          (let ((i (string-match ewavedrom--tspan-token str)))
            (if (null i)
                (progn (push (list "tspan" (dump) (ewavedrom--xscape str)) res)
                       (setq done t))
              (when (> i 0)
                (push (list "tspan" (dump) (ewavedrom--xscape (substring str 0 i))) res))
              (let* ((m (match-string 0 str))
                     (tr (cdr (assoc m ewavedrom--tspan-trans)))
                     (op (car tr))
                     (pairs (cdr tr)))
                (while pairs
                  (let ((cell (assoc (car pairs) state)) (val (cadr pairs)))
                    (if (eq op 'add)
                        (unless (member val (cdr cell)) (setcdr cell (append (cdr cell) (list val))))
                      (setcdr cell (delete val (cdr cell)))))
                  (setq pairs (cddr pairs)))
                (setq str (substring str (+ i (length m))))
                (when (string= str "") (setq done t)))))))
      (nreverse res)))))

(defun ewavedrom--text-width (str &optional size)
  "Width of STR in pixels at font SIZE, as WaveDrom estimates it."
  (let ((size (or size 11)) (w 0) (n (length ewavedrom-char-widths)))
    (dolist (c (string-to-list str))
      (setq w (+ w (if (> c #xFFFF)
                       (* 2 ewavedrom-char-width-other)
                     (if (< c n) (aref ewavedrom-char-widths c) ewavedrom-char-width-other)))))
    (/ (* (float w) size) 100)))

;;;; Lane state (WaveDrom's lane.js)

(cl-defstruct (ewavedrom--lane (:constructor ewavedrom--make-lane))
  (xs 20) (ys 20) (xg 120) (yh0 0) (yh1 0) (yf0 0) (yf1 0) (y0 5) (yo 30)
  (tgo -10) (ym 15) (xlabel 6) (xmax 1) (hscale 1) (head nil) (foot nil)
  (xmin-cfg 0) (xmax-cfg 1e12) (period 1) (phase 0))

(defun ewavedrom--config (source key)
  (let ((c (ewavedrom--get source "config")))
    (and (ewavedrom--truthy c) (ewavedrom--get c key))))

(defun ewavedrom--parse-config (source lane)
  (let ((hscale (ewavedrom--config source "hscale")))
    (setf (ewavedrom--lane-hscale lane) 1)
    (when (ewavedrom--truthy hscale)
      (let* ((x (ewavedrom--num hscale))
             (h (ewavedrom--round (if (> x 0) (ewavedrom--round x) 1))))
        (when (> h 0)
          (setf (ewavedrom--lane-hscale lane) (min h 100))))))
  (let ((hb (ewavedrom--config source "hbounds")))
    (when (and (vectorp hb) (= (length hb) 2))
      (aset hb 0 (ffloor (ewavedrom--num (aref hb 0))))
      (aset hb 1 (fceiling (ewavedrom--num (aref hb 1))))
      (when (< (aref hb 0) (aref hb 1))
        (setf (ewavedrom--lane-xmin-cfg lane) (* 2 (floor (aref hb 0)))
              (ewavedrom--lane-xmax-cfg lane) (* 2 (floor (aref hb 1)))))))
  (dolist (end '("head" "foot"))
    (let ((obj (ewavedrom--get source end))
          (head (string= end "head")))
      (if head
          (setf (ewavedrom--lane-yh0 lane) 0 (ewavedrom--lane-yh1 lane) 0
                (ewavedrom--lane-head lane) obj)
        (setf (ewavedrom--lane-yf0 lane) 0 (ewavedrom--lane-yf1 lane) 0
              (ewavedrom--lane-foot lane) obj))
      (when (ewavedrom--truthy obj)
        (let ((tick (ewavedrom--get obj "tick"))
              (tock (ewavedrom--get obj "tock"))
              (shift (/ (ewavedrom--lane-xmin-cfg lane) 2.0)))
          (when (or (ewavedrom--truthy tick) (equal tick 0.0) (equal tick 0)
                    (ewavedrom--truthy tock) (equal tock 0.0) (equal tock 0))
            (if head (setf (ewavedrom--lane-yh0 lane) 20) (setf (ewavedrom--lane-yf0 lane) 20)))
          (dolist (k '("tick" "tock"))
            (let ((v (ewavedrom--get obj k)))
              (when (or (ewavedrom--truthy v) (equal v 0.0) (equal v 0))
                (ewavedrom--put obj k (ewavedrom--js-add v shift)))))
          (when (ewavedrom--truthy (ewavedrom--get obj "text"))
            (if head (setf (ewavedrom--lane-yh1 lane) 46) (setf (ewavedrom--lane-yf1 lane) 46))))))))

(defun ewavedrom--js-add (a b)
  "JS A + B, where B is a number."
  (if (or (numberp a) (memq a '(t :false :null)))
      (+ (ewavedrom--num a) b)
    (concat (ewavedrom--str a) (ewavedrom--js-str b))))

;;;; Wave strings to bricks

(defun ewavedrom--gen-brick (texts extra times)
  (let ((texts (if (listp texts) texts (list texts)))
        (r nil))
    (if (= (length texts) 4)
        (dotimes (_ (ewavedrom--count times))
          (push (nth 0 texts) r)
          (dotimes (_ (ewavedrom--count extra)) (push (nth 1 texts) r))
          (push (nth 2 texts) r)
          (dotimes (_ (ewavedrom--count extra)) (push (nth 3 texts) r)))
      (when (= (length texts) 1)
        (setq texts (list (car texts) (car texts))))
      (push (car texts) r)
      (dotimes (_ (ewavedrom--count (- (* times (* 2 (+ extra 1))) 1)))
        (push (cadr texts) r)))
    (nreverse r)))

(defconst ewavedrom--first-brick
  '(("p" "pclk" "111" "nclk" "000") ("n" "nclk" "000" "pclk" "111")
    ("P" "Pclk" "111" "nclk" "000") ("N" "Nclk" "000" "pclk" "111")
    ("l" "000") ("L" "000") ("0" "000") ("h" "111") ("H" "111") ("1" "111")
    ("=" "vvv-2") ("2" "vvv-2") ("3" "vvv-3") ("4" "vvv-4") ("5" "vvv-5")
    ("6" "vvv-6") ("7" "vvv-7") ("8" "vvv-8") ("9" "vvv-9")
    ("d" "ddd") ("u" "uuu") ("z" "zzz")))

(defun ewavedrom--gen-first-wave-brick (text extra times)
  (ewavedrom--gen-brick (or (cdr (assoc text ewavedrom--first-brick)) '("xxx")) extra times))

(defun ewavedrom--tbl (alist key)
  (cdr (assoc key alist)))

(defconst ewavedrom--x1 '(("p" . "pclk") ("n" . "nclk") ("P" . "Pclk") ("N" . "Nclk")
                          ("h" . "pclk") ("l" . "nclk") ("H" . "Pclk") ("L" . "Nclk")))
(defconst ewavedrom--x2 '(("0" . "0") ("1" . "1") ("x" . "x") ("d" . "d") ("u" . "u") ("z" . "z")
                          ("=" . "v") ("2" . "v") ("3" . "v") ("4" . "v") ("5" . "v")
                          ("6" . "v") ("7" . "v") ("8" . "v") ("9" . "v")))
(defconst ewavedrom--x3 '(("0" . "") ("1" . "") ("x" . "") ("d" . "") ("u" . "") ("z" . "")
                          ("=" . "-2") ("2" . "-2") ("3" . "-3") ("4" . "-4") ("5" . "-5")
                          ("6" . "-6") ("7" . "-7") ("8" . "-8") ("9" . "-9")))
(defconst ewavedrom--y1 '(("p" . "0") ("n" . "1") ("P" . "0") ("N" . "1") ("h" . "1") ("l" . "0")
                          ("H" . "1") ("L" . "0") ("0" . "0") ("1" . "1") ("x" . "x") ("d" . "d")
                          ("u" . "u") ("z" . "z") ("=" . "v") ("2" . "v") ("3" . "v") ("4" . "v")
                          ("5" . "v") ("6" . "v") ("7" . "v") ("8" . "v") ("9" . "v")))
(defconst ewavedrom--y2 '(("p" . "") ("n" . "") ("P" . "") ("N" . "") ("h" . "") ("l" . "")
                          ("H" . "") ("L" . "") ("0" . "") ("1" . "") ("x" . "") ("d" . "")
                          ("u" . "") ("z" . "") ("=" . "-2") ("2" . "-2") ("3" . "-3") ("4" . "-4")
                          ("5" . "-5") ("6" . "-6") ("7" . "-7") ("8" . "-8") ("9" . "-9")))
(defconst ewavedrom--x4 '(("p" . "111") ("n" . "000") ("P" . "111") ("N" . "000") ("h" . "111")
                          ("l" . "000") ("H" . "111") ("L" . "000") ("0" . "000") ("1" . "111")
                          ("x" . "xxx") ("d" . "ddd") ("u" . "uuu") ("z" . "zzz")
                          ("=" . "vvv-2") ("2" . "vvv-2") ("3" . "vvv-3") ("4" . "vvv-4")
                          ("5" . "vvv-5") ("6" . "vvv-6") ("7" . "vvv-7") ("8" . "vvv-8") ("9" . "vvv-9")))
(defconst ewavedrom--x5 '(("p" . "nclk") ("n" . "pclk") ("P" . "nclk") ("N" . "pclk")))
(defconst ewavedrom--x6 '(("p" . "000") ("n" . "111") ("P" . "000") ("N" . "111")))
(defconst ewavedrom--xclude '(("hp" . "111") ("Hp" . "111") ("ln" . "000") ("Ln" . "000")
                              ("nh" . "111") ("Nh" . "111") ("pl" . "000") ("Pl" . "000")))

(defun ewavedrom--gen-wave-brick (text extra times)
  (let* ((a0 (substring text 0 1))
         (a1 (substring text 1 2))
         (tmp0 (ewavedrom--tbl ewavedrom--x4 a1))
         (tmp1 (ewavedrom--tbl ewavedrom--x1 a1)))
    (if (null tmp1)
        (let ((tmp2 (ewavedrom--tbl ewavedrom--x2 a1)))
          (if (null tmp2)
              (ewavedrom--gen-brick "xxx" extra times)
            (let ((tmp3 (ewavedrom--tbl ewavedrom--y1 a0)))
              (if (null tmp3)
                  (ewavedrom--gen-brick "xxx" extra times)
                (ewavedrom--gen-brick
                 (list (concat tmp3 "m" tmp2 (ewavedrom--tbl ewavedrom--y2 a0)
                               (ewavedrom--tbl ewavedrom--x3 a1))
                       tmp0)
                 extra times)))))
      (let ((tmp4 (ewavedrom--tbl ewavedrom--xclude text)))
        (when tmp4 (setq tmp1 tmp4))
        (let ((tmp5 (ewavedrom--tbl ewavedrom--x5 a1)))
          (if (null tmp5)
              (ewavedrom--gen-brick (list tmp1 tmp0) extra times)
            (ewavedrom--gen-brick (list tmp1 tmp0 tmp5 (ewavedrom--tbl ewavedrom--x6 a1))
                                  extra times)))))))

(defun ewavedrom--marker-p (brick)
  (and (stringp brick) (string-match-p "\\`vvv-[2-9]\\'" brick)))

(defun ewavedrom--find-lane-markers (bricks)
  "Centre positions of each run of data bricks in BRICKS."
  (let ((g 0) (l 0) (ret nil))
    (dolist (e bricks)
      (if (ewavedrom--marker-p e)
          (setq l (1+ l))
        (when (/= l 0)
          (push (- g (/ (+ l 1) 2.0)) ret)
          (setq l 0)))
      (setq g (1+ g)))
    (when (/= l 0)
      (push (- g (/ (+ l 1) 2.0)) ret))
    (nreverse ret)))

(defun ewavedrom--parse-wave-lane (src extra lane)
  "Bricks for wave string SRC and the number of data markers the phase hides."
  (let* ((stack (mapcar #'string (string-to-list src)))
         (next (pop stack))
         (repeats 1)
         (r nil)
         (sub nil)
         top)
    (while (member (car stack) '("." "|"))
      (pop stack)
      (setq repeats (1+ repeats)))
    (setq r (ewavedrom--gen-first-wave-brick next extra repeats))
    (while stack
      (setq top next
            next (pop stack))
      (when (equal next "<") (setq sub t next (pop stack)))
      (when (equal next ">") (setq sub nil next (pop stack)))
      (setq repeats 1)
      (while (member (car stack) '("." "|"))
        (pop stack)
        (setq repeats (1+ repeats)))
      (let ((text (concat (or top "undefined") (or next "undefined"))))
        (setq r (append r (if sub
                              (ewavedrom--gen-wave-brick text 0 (- repeats (ewavedrom--lane-period lane)))
                            (ewavedrom--gen-wave-brick text extra repeats))))))
    (let ((unseen nil))
      (dotimes (_ (ewavedrom--count (ewavedrom--lane-phase lane)))
        (push (pop r) unseen))
      (setq unseen (nreverse unseen))
      (list r
            (if unseen
                (let ((n (length (ewavedrom--find-lane-markers unseen))))
                  (if (and (= 1 (length (ewavedrom--find-lane-markers (last unseen))))
                           (= 1 (length (ewavedrom--find-lane-markers (list (car r))))))
                      (1- n)
                    n))
              0)))))

(defun ewavedrom--data-extract (sig unseen)
  (let ((data (ewavedrom--get sig "data")))
    (cond ((null data) nil)
          (t (when (stringp data)
               (setq data (vconcat (split-string (string-trim data) "[ \t\n\r\f\v]+"))))
             (if (vectorp data)
                 (let ((n (length data)) (start (or unseen 0)))
                   (when (< start 0) (setq start (max 0 (+ n start))))
                   (if (>= start n) [] (substring data start)))
               data)))))

(defun ewavedrom--parse-wave-lanes (sigs lane)
  "One entry (NAME PHASE BRICKS DATA SIG) per lane object in SIGS."
  (mapcar
   (lambda (sig)
     (let ((phase (ewavedrom--get sig "phase"))
           (xmin (ewavedrom--lane-xmin-cfg lane))
           bricks unseen)
       (setf (ewavedrom--lane-period lane) (ewavedrom--nor (ewavedrom--get sig "period") 1)
             (ewavedrom--lane-phase lane) (+ (if (ewavedrom--truthy phase) (* (ewavedrom--num phase) 2) 0)
                                             xmin))
       (when (stringp (ewavedrom--get sig "wave"))
         (let ((p (ewavedrom--parse-wave-lane
                   (ewavedrom--get sig "wave")
                   (- (* (ewavedrom--lane-period lane) (ewavedrom--lane-hscale lane)) 1)
                   lane)))
           (setq bricks (car p) unseen (cadr p))
           (unless bricks (setq bricks 'empty))))
       (list (ewavedrom--or (ewavedrom--get sig "name") " ")
             (+ (ewavedrom--nor phase 0) (/ xmin 2.0))
             bricks
             (ewavedrom--data-extract sig unseen)
             sig)))
   sigs))

;;;; Rendering

(cl-defstruct (ewavedrom--walk (:constructor ewavedrom--make-walk))
  (x 0) (y 0) (xx 0) (name nil) (lanes nil) (width nil) (groups nil))

(defun ewavedrom--rec (tmp st)
  "Walk the signal array TMP, collecting lanes, offsets and groups in ST.
Lists in ST are built in reverse."
  (let ((delta 10) name)
    (when (and (> (length tmp) 0) (or (stringp (aref tmp 0)) (numberp (aref tmp 0))))
      (setq name (aref tmp 0) delta 25))
    (cl-incf (ewavedrom--walk-x st) delta)
    (mapc (lambda (e)
            (cond ((vectorp e)
                   (let ((old-y (ewavedrom--walk-y st)))
                     (ewavedrom--rec e st)
                     (push (list (ewavedrom--walk-xx st) old-y
                                 (- (ewavedrom--walk-y st) old-y) (ewavedrom--walk-name st))
                           (ewavedrom--walk-groups st))))
                  ((or (ewavedrom--obj-p e) (eq e :null))
                   (push (if (eq e :null) (list :obj) e) (ewavedrom--walk-lanes st))
                   (push (ewavedrom--walk-x st) (ewavedrom--walk-width st))
                   (cl-incf (ewavedrom--walk-y st)))))
          tmp)
    (setf (ewavedrom--walk-xx st) (ewavedrom--walk-x st))
    (cl-decf (ewavedrom--walk-x st) delta)
    (setf (ewavedrom--walk-name st) name)
    st))

(defconst ewavedrom--ou-colors
  '(("1" . "#000000") ("2" . "#e90000") ("3" . "#3edd00") ("4" . "#0074cd")
    ("5" . "#ff15db") ("6" . "#af9800") ("7" . "#00864f") ("8" . "#a076ff")))

(defun ewavedrom--render-over-under (sig key lane)
  (let* ((xs (ewavedrom--lane-xs lane))
         (period (* (ewavedrom--nor (ewavedrom--get sig "period") 1) 2 xs))
         (xoffset (- (* (ewavedrom--nor (ewavedrom--get sig "phase") 0) 2 xs)))
         (gap1 12) (serif 7)
         (y (if (string= key "under") (ewavedrom--lane-ys lane) 0))
         (spec (ewavedrom--get sig key))
         start color)
    (when (ewavedrom--truthy spec)
      (let ((res (list "g" (ewavedrom--tt xoffset y "style" "stroke-width:3")))
            (arr (mapcar #'string (string-to-list (ewavedrom--str spec))))
            (i 0))
        (cl-flet ((line (x)
                    (when start
                      (setq res (append res (list (list "line" (ewavedrom--obj
                                                               "style" (concat "stroke:" color)
                                                               "x1" (+ (* period start) gap1)
                                                               "x2" (* period x)))))))))
          (dolist (dot arr)
            (when (and (not (string= dot ".")) start)
              (line i)
              (when (string= key "over")
                (setq res (append res (list (list "path" (ewavedrom--obj
                                                          "style" (concat "stroke:none;fill:" color)
                                                          "d" (concat "m" (ewavedrom--js-str (- (* period i) serif))
                                                                      " 0 l" (ewavedrom--js-str serif) " "
                                                                      (ewavedrom--js-str serif) " v-"
                                                                      (ewavedrom--js-str serif) " z"))))))))
            (cond ((string= dot "0") (setq start nil))
                  ((not (string= dot "."))
                   (setq start i
                         color (or (cdr (assoc dot ewavedrom--ou-colors)) "#000000"))))
            (setq i (1+ i)))
          (when start (line (length arr))))
        (list res)))))

(defun ewavedrom--render-lane-uses (el lane)
  (let ((bricks (nth 2 el)) (data (nth 3 el)) (res nil) (i 0))
    (when bricks
      (let ((bricks (if (eq bricks 'empty) nil bricks)))
        (dolist (ref bricks)
          (push (list "use" (ewavedrom--tt (* i (ewavedrom--lane-xs lane)) 0
                                           "xlink:href" (concat "#" (ewavedrom--str ref))))
                res)
          (setq i (1+ i)))
        (when (and (vectorp data) (> (length data) 0))
          (let ((j 0))
            (dolist (label (ewavedrom--find-lane-markers bricks))
              (when (< j (length data))
                (let ((d (aref data j)))
                  (when d
                    (push (append (list "text" (ewavedrom--obj
                                                "x" (+ (* label (ewavedrom--lane-xs lane)) (ewavedrom--lane-xlabel lane))
                                                "y" (ewavedrom--lane-ym lane)
                                                "text-anchor" "middle"
                                                "xml:space" "preserve"))
                                  (ewavedrom--tspan d))
                          res))))
              (setq j (1+ j)))))))
    (nreverse res)))

(defun ewavedrom--render-wave-lane (content index lane)
  "Lane groups for CONTENT, plus the label width of each lane."
  (let ((xmax 0) (glengths nil) (res nil) (j 0))
    (dolist (el content)
      (let ((name (nth 0 el)))
        (when (ewavedrom--truthy name)
          (let* ((x (nth 1 el))
                 (xoffset (if (> x 0) (- (fceiling (* 2 x)) (* 2 x)) (- (* 2 x)))))
            (push (append
                   (list "g" (ewavedrom--tt 0 (+ (ewavedrom--lane-y0 lane) (* j (ewavedrom--lane-yo lane)))
                                            "id" (format "wavelane_%d_%d" j index)))
                   (list (append (list "text" (ewavedrom--obj "x" (ewavedrom--lane-tgo lane)
                                                              "y" (ewavedrom--lane-ym lane)
                                                              "class" "info"
                                                              "text-anchor" "end"
                                                              "xml:space" "preserve"))
                                 (ewavedrom--tspan name)))
                   (list (append (list "g" (ewavedrom--tt (* xoffset (ewavedrom--lane-xs lane)) 0
                                                          "id" (format "wavelane_draw_%d_%d" j index)))
                                 (ewavedrom--render-lane-uses el lane)))
                   (ewavedrom--render-over-under (nth 4 el) "over" lane)
                   (ewavedrom--render-over-under (nth 4 el) "under" lane))
                  res)
            (let ((b (nth 2 el)))
              (setq xmax (max xmax (if (listp b) (length b) 0))))
            (push (if (stringp name) (ewavedrom--text-width name 11) 0) glengths))))
      (setq j (1+ j)))
    (setf (ewavedrom--lane-xmax lane)
          (min xmax (- (ewavedrom--lane-xmax-cfg lane) (ewavedrom--lane-xmin-cfg lane))))
    (list (nreverse glengths) (nreverse res))))

(defun ewavedrom--render-groups (groups index lane)
  (let ((res (list "g")) (i 0)
        (yo (ewavedrom--lane-yo lane))
        (top (+ (ewavedrom--lane-yh0 lane) (ewavedrom--lane-yh1 lane))))
    (dolist (g groups)
      (cl-destructuring-bind (x y height name) g
        (setq res (append res (list (list "path" (ewavedrom--obj
                                                  "id" (format "group_%d_%d" i index)
                                                  "d" (concat "m " (ewavedrom--js-str (+ x 0.5)) ","
                                                              (ewavedrom--js-str (+ (* y yo) 3.5 top))
                                                              " c -3,0 -5,2 -5,5 l 0,"
                                                              (ewavedrom--js-str (- (* height yo) 16))
                                                              " c 0,3 2,5 5,5")
                                                  "style" "stroke:#0041c4;stroke-width:1;fill:none")))))
        (when name
          (setq res (append res (list (list "g" (ewavedrom--tt (- x 10) (+ (* yo (+ y (/ height 2.0))) top))
                                            (list "g" (ewavedrom--obj "transform" "rotate(270)")
                                                  (append (list "text" (ewavedrom--obj "text-anchor" "middle"
                                                                                       "class" "info"
                                                                                       "xml:space" "preserve"))
                                                          (ewavedrom--tspan name)))))))))
      (setq i (1+ i)))
    res))

(defun ewavedrom--captext (lane end y)
  (let ((obj (if (string= end "head") (ewavedrom--lane-head lane) (ewavedrom--lane-foot lane))))
    (when (and (ewavedrom--truthy obj) (ewavedrom--truthy (ewavedrom--get obj "text")))
      (list (append (list "text" (ewavedrom--obj "x" (/ (* (ewavedrom--lane-xmax lane) (ewavedrom--lane-xs lane)) 2.0)
                                                 "y" y "fill" "#000"
                                                 "text-anchor" "middle" "xml:space" "preserve"))
                    (ewavedrom--tspan (ewavedrom--get obj "text")))))))

(defun ewavedrom--ticktock (lane end key x dx y len)
  (let* ((obj (if (string= end "head") (ewavedrom--lane-head lane) (ewavedrom--lane-foot lane)))
         (n (ewavedrom--count len))
         (val (and (ewavedrom--obj-p obj) (ewavedrom--get obj key)))
         offset labels)
    (when (and (ewavedrom--obj-p obj) (ewavedrom--has obj key) val)
      (cond ((stringp val)
             (setq val (vconcat (split-string (string-trim val) "[ \t\n\r\f\v]+"))))
            ((or (numberp val) (memq val '(t :false)))
             (setq offset (ewavedrom--num val))
             (setq val (vconcat (mapcar (lambda (i) (+ i offset)) (number-sequence 0 (1- n)))))))
      (when (vectorp val)
        (let ((vlen (length val)))
          (cond
           ((= vlen 0) (setq val 'none))
           ((= vlen 1)
            (setq offset (ewavedrom--num (aref val 0)))
            (setq labels (if (ewavedrom--nan-p offset)
                             val
                           (vconcat (mapcar (lambda (i) (+ i offset)) (number-sequence 0 (1- n)))))))
           ((= vlen 2)
            (setq offset (ewavedrom--num (aref val 0)))
            (let* ((step (ewavedrom--num (aref val 1)))
                   (parts (split-string (ewavedrom--str (aref val 1)) "\\."))
                   (dp (if (= (length parts) 2) (length (nth 1 parts)) 0)))
              (if (or (ewavedrom--nan-p offset) (ewavedrom--nan-p step))
                  (setq labels val)
                (setq offset (* step offset))
                (setq labels (vconcat (mapcar (lambda (i) (ewavedrom--to-fixed (+ (* step i) offset) dp))
                                              (number-sequence 0 (1- n))))))))
           (t (setq labels val))))
        (unless (eq val 'none)
          (let ((res (list "g" (ewavedrom--obj "class" "muted" "text-anchor" "middle" "xml:space" "preserve")))
                (every (ewavedrom--get obj "every")))
            (dotimes (i n)
              (unless (and (ewavedrom--truthy every)
                           (let ((m (if offset
                                        (let ((a (+ i offset)) (b (ewavedrom--num every)))
                                          (if (= b 0) ewavedrom--nan (- a (* b (ftruncate (/ a b))))))
                                      ewavedrom--nan)))
                             (or (ewavedrom--nan-p m) (/= m 0))))
                (setq res (append res (list (append (list "text" (ewavedrom--obj "x" (+ (* i dx) x) "y" y))
                                                    (ewavedrom--tspan (and (< i (length labels)) (aref labels i)))))))))
            (list res)))))))

(defun ewavedrom--render-marks (content index lane source)
  (let* ((mstep (* 2 (ewavedrom--lane-hscale lane)))
         (mmstep (* mstep (ewavedrom--lane-xs lane)))
         (marks (/ (float (ewavedrom--lane-xmax lane)) mstep))
         (gy (* (length content) (ewavedrom--lane-yo lane)))
         (res (list "g" (ewavedrom--obj "id" (format "gmarks_%d" index)))))
    (unless (eq (ewavedrom--config source "marks") :false)
      (let ((lines (list "g" (ewavedrom--obj "style" "stroke:#888;stroke-width:0.5;stroke-dasharray:1,3"))))
        (dotimes (i (ewavedrom--count (+ marks 1)))
          (setq lines (append lines (list (list "line" (ewavedrom--obj
                                                        "id" (format "gmark_%d_%d" i index)
                                                        "x1" (* i mmstep) "y1" 0
                                                        "x2" (* i mmstep) "y2" gy))))))
        (setq res (append res (list lines)))))
    (append res
            (ewavedrom--captext lane "head" (if (/= (ewavedrom--lane-yh0 lane) 0) -33 -13))
            (ewavedrom--captext lane "foot" (+ gy (if (/= (ewavedrom--lane-yf0 lane) 0) 45 25)))
            (ewavedrom--ticktock lane "head" "tick" 0 mmstep -5 (+ marks 1))
            (ewavedrom--ticktock lane "head" "tock" (/ mmstep 2.0) mmstep -5 marks)
            (ewavedrom--ticktock lane "foot" "tick" 0 mmstep (+ gy 15) (+ marks 1))
            (ewavedrom--ticktock lane "foot" "tock" (/ mmstep 2.0) mmstep (+ gy 15) marks))))

;;;; Edges and arrows

(defconst ewavedrom--arrow "marker-end:url(#arrowhead);stroke:#0041c4;stroke-width:1;fill:none")
(defconst ewavedrom--arrow2
  "marker-end:url(#arrowhead);marker-start:url(#arrowtail);stroke:#0041c4;stroke-width:1;fill:none")

(defun ewavedrom--arc-shape (shape labelled from to)
  "Path, style and label position (D STYLE LX LY) for an edge of SHAPE."
  (let* ((fx (car from)) (fy (cdr from)) (tx (car to)) (ty (cdr to))
         (dx (- tx fx)) (dy (- ty fy))
         (lx (/ (+ fx tx) 2.0)) (ly (/ (+ fy ty) 2.0))
         (s #'ewavedrom--js-str)
         (curve (lambda (c1 c2)
                  (concat "M " (funcall s fx) "," (funcall s fy) " c " (funcall s c1) ", 0 "
                          (funcall s c2) ", " (funcall s dy) " " (funcall s dx) ", " (funcall s dy))))
         (hv (concat "m " (funcall s fx) "," (funcall s fy) " " (funcall s dx) ",0 0," (funcall s dy)))
         (vh (concat "m " (funcall s fx) "," (funcall s fy) " 0," (funcall s dy) " " (funcall s dx) ",0"))
         (hvh (concat "m " (funcall s fx) "," (funcall s fy) " " (funcall s (/ dx 2.0)) ",0 0,"
                      (funcall s dy) " " (funcall s (/ dx 2.0)) ",0"))
         (at (lambda (f) (+ fx (* (- tx fx) f))))
         d style)
    (pcase shape
      ("-" nil)
      ("~" (setq d (funcall curve (* 0.7 dx) (* 0.3 dx))))
      ("-~" (setq d (funcall curve (* 0.7 dx) dx)) (when labelled (setq lx (funcall at 0.75))))
      ("~-" (setq d (funcall curve 0 (* 0.3 dx))) (when labelled (setq lx (funcall at 0.25))))
      ("-|" (setq d hv) (when labelled (setq lx tx)))
      ("|-" (setq d vh) (when labelled (setq lx fx)))
      ("-|-" (setq d hvh))
      ("->" (setq style ewavedrom--arrow))
      ("~>" (setq style ewavedrom--arrow d (funcall curve (* 0.7 dx) (* 0.3 dx))))
      ("-~>" (setq style ewavedrom--arrow d (funcall curve (* 0.7 dx) dx))
       (when labelled (setq lx (funcall at 0.75))))
      ("~->" (setq style ewavedrom--arrow d (funcall curve 0 (* 0.3 dx)))
       (when labelled (setq lx (funcall at 0.25))))
      ("-|>" (setq style ewavedrom--arrow d hv) (when labelled (setq lx tx)))
      ("|->" (setq style ewavedrom--arrow d vh) (when labelled (setq lx fx)))
      ("-|->" (setq style ewavedrom--arrow d hvh))
      ("<->" (setq style ewavedrom--arrow2))
      ("<~>" (setq style ewavedrom--arrow2 d (funcall curve (* 0.7 dx) (* 0.3 dx))))
      ("<-~>" (setq style ewavedrom--arrow2 d (funcall curve (* 0.7 dx) dx))
       (when labelled (setq lx (funcall at 0.75))))
      ("<-|>" (setq style ewavedrom--arrow2 d hv) (when labelled (setq lx tx)))
      ("<-|->" (setq style ewavedrom--arrow2 d hvh))
      ("+" (setq style "marker-end:url(#tee);marker-start:url(#tee);fill:none;stroke:#00F;stroke-width:1"))
      (_ (setq style "fill:none;stroke:#F00;stroke-width:1")))
    (list d style lx ly)))

(defun ewavedrom--render-label (x y text size)
  (let ((w (+ (ewavedrom--text-width text size) 2)))
    (list "g" (ewavedrom--tt x y)
          (list "rect" (ewavedrom--obj "x" (- (ash (truncate w) -1))
                                       "y" (- (ash (truncate size) -1))
                                       "width" w "height" size
                                       "style" "fill:#FFF;"))
          (append (list "text" (ewavedrom--obj "text-anchor" "middle"
                                               "y" (ewavedrom--round (* 0.3 size))
                                               "style" (concat "font-size:" (ewavedrom--str size) "px;")))
                  (ewavedrom--tspan text)))))

(defun ewavedrom--event-keys (events)
  "Keys of EVENTS in JS Object.keys order: array indices first, ascending."
  (let ((keys (mapcar #'car events)))
    (append (sort (cl-remove-if-not (lambda (k) (string-match-p "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" k)) keys)
                  (lambda (a b) (< (string-to-number a) (string-to-number b))))
            (cl-remove-if (lambda (k) (string-match-p "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" k)) keys))))

(defun ewavedrom--render-arcs (lanes index source lane)
  (let* ((afs (ewavedrom--config source "arcFontSize"))
         (size (if (ewavedrom--truthy afs) afs 11))
         (res (list "g" (ewavedrom--obj "id" (format "wavearcs_%d" index))))
         (events nil)
         (i 0))
    (dolist (el lanes)
      (let ((text (ewavedrom--get el "node"))
            (period (ewavedrom--get el "period"))
            (phase (ewavedrom--get el "phase")))
        (setf (ewavedrom--lane-period lane) (if (ewavedrom--truthy period) (ewavedrom--num period) 1)
              (ewavedrom--lane-phase lane) (+ (if (ewavedrom--truthy phase) (* (ewavedrom--num phase) 2) 0)
                                              (ewavedrom--lane-xmin-cfg lane)))
        (when (ewavedrom--truthy text)
          (let ((pos 0))
            (dolist (c (string-to-list (ewavedrom--str text)))
              (let ((name (string c)))
                (unless (string= name ".")
                  (let ((p (cons (+ (* (ewavedrom--lane-xs lane)
                                       (- (* 2 pos (ewavedrom--lane-period lane) (ewavedrom--lane-hscale lane))
                                          (ewavedrom--lane-phase lane)))
                                    (ewavedrom--lane-xlabel lane))
                                 (+ (* i (ewavedrom--lane-yo lane)) (ewavedrom--lane-y0 lane)
                                    (* (ewavedrom--lane-ys lane) 0.5))))
                        (cell (assoc name events)))
                    (if cell (setcdr cell p) (setq events (append events (list (cons name p))))))))
              (setq pos (1+ pos))))))
      (setq i (1+ i)))
    (let ((edges (ewavedrom--get source "edge")))
      (when (vectorp edges)
        (mapc (lambda (e)
                (when (stringp e)
                  (let* ((words (split-string (string-trim e) "[ \t\n\r\f\v]+"))
                         (w0 (car words))
                         (label (let ((rest (substring e (min (length e) (length w0)))))
                                  (if (> (length rest) 0) (substring rest 1) "")))
                         (from (cdr (assoc (substring w0 0 (min 1 (length w0))) events)))
                         (to (cdr (assoc (substring w0 (max 0 (1- (length w0)))) events))))
                    (when (and from to)
                      (let* ((shape (if (>= (length w0) 2) (substring w0 1 -1) ""))
                             (props (ewavedrom--arc-shape shape (> (length label) 0) from to)))
                        (setq res (append res
                                          (list (list "path"
                                                      (ewavedrom--obj
                                                       "id" (concat "gmark_" (substring w0 0 1) "_"
                                                                    (substring w0 (1- (length w0))))
                                                       "d" (or (nth 0 props)
                                                               (concat "M " (ewavedrom--js-str (car from)) ","
                                                                       (ewavedrom--js-str (cdr from)) " "
                                                                       (ewavedrom--js-str (car to)) ","
                                                                       (ewavedrom--js-str (cdr to))))
                                                       "style" (or (nth 1 props)
                                                                   "fill:none;stroke:#00F;stroke-width:1"))))))
                        (when (> (length label) 0)
                          (setq res (append res (list (ewavedrom--render-label
                                                       (nth 2 props) (nth 3 props) label size))))))))))
              edges)))
    (dolist (k (ewavedrom--event-keys events))
      (when (string= k (downcase k))
        (let ((p (cdr (assoc k events))))
          (when (> (car p) 0)
            (setq res (append res (list (ewavedrom--render-label (car p) (cdr p) k size))))))))
    res))

;;;; Gaps and piece-wise lanes

(defun ewavedrom--render-gap-uses (text lane)
  (let ((stack (mapcar #'string (string-to-list (or text ""))))
        (pos 0) (sub nil) (res nil)
        (period (ewavedrom--lane-period lane)))
    (while stack
      (let ((next (pop stack)))
        (when (equal next "<") (setq sub t next (pop stack)))
        (when (equal next ">") (setq sub nil next (pop stack)))
        (setq pos (+ pos (if sub 1 (* 2 period))))
        (when (equal next "|")
          (push (list "use" (ewavedrom--tt (* (ewavedrom--lane-xs lane)
                                              (- (* (- pos (if sub 0 period)) (ewavedrom--lane-hscale lane))
                                                 (ewavedrom--lane-phase lane)))
                                           0 "xlink:href" "#gap"))
                res))))
    (nreverse res)))

(defun ewavedrom--render-gaps (lanes index source lane)
  (let* ((n (length lanes))
         (h (* n (ewavedrom--lane-yo lane)))
         (s #'ewavedrom--js-str)
         (style "fill:none;stroke:#000;stroke-width:1px")
         (vline (lambda (x) (list "line" (ewavedrom--obj "x1" x "x2" x "y2" h
                                                         "style" "stroke:#000;stroke-width:1px"))))
         (path (lambda (d) (list "path" (ewavedrom--obj "d" d "style" style))))
         (backdrop (lambda (w) (list "rect" (ewavedrom--obj "x" (/ (- w) 2.0) "width" w "height" h
                                                            "style" "fill:#ffffffcc;stroke:none"))))
         (v9 (funcall s (- h 9)))
         (res nil))
    (when (stringp (ewavedrom--get source "gaps"))
      (let ((scale (* (ewavedrom--lane-hscale lane) (ewavedrom--lane-xs lane) 2))
            (x 0))
        (dolist (c (split-string (string-trim (ewavedrom--get source "gaps")) "[ \t\n\r\f\v]+"))
          (unless (string= c ".")
            (let ((offset (if (string= c (downcase c)) 0.5 0))
                  (marks
                   (pcase c
                     ("0" (list (funcall backdrop 4)))
                     ((or "1" "|") (list (funcall backdrop 4) (funcall vline 0)))
                     ("2" (list (funcall backdrop 4) (funcall vline -2) (funcall vline 2)))
                     ("3" (list (funcall backdrop 6) (funcall vline -3) (funcall vline 0) (funcall vline 3)))
                     ("[" (list (funcall backdrop 4) (funcall path (concat "M  2 0 h -4 v " (funcall s (- h 1)) " h  4"))))
                     ("]" (list (funcall backdrop 4) (funcall path (concat "M -2 0 h  4 v " (funcall s (- h 1)) " h -4"))))
                     ("(" (list (funcall backdrop 4)
                                (funcall path (concat "M  2 0 a 4 4 0 0 0 -4 4 v " v9 " a 4 4 0 0 0  4 4"))))
                     (")" (list (funcall backdrop 4)
                                (funcall path (concat "M -2 0 a 4 4 1 0 1  4 4 v " v9 " a 4 4 1 0 1 -4 4"))))
                     (")(" (list (funcall backdrop 8)
                                 (funcall path (concat "M -5 0 a 4 4 1 0 1  4 4 v " v9 " a 4 4 1 0 1 -4 4"
                                                       "M  5 0 a 4 4 0 0 0 -4 4 v " v9 " a 4 4 0 0 0  4 4"))))
                     ("((" (list (funcall backdrop 8)
                                 (funcall path (concat "M  2 0 a 4 4 0 0 0 -4 4 v " v9 " a 4 4 0 0 0  4 4"
                                                       "M  5 1 a 3 3 0 0 0 -3 3 v " v9 " a 3 3 0 0 0  3 3"))))
                     ("))" (list (funcall backdrop 8)
                                 (funcall path (concat "M -5 1 a 3 3 1 0 1  3 3 v " v9 " a 3 3 1 0 1 -3 3"
                                                       "M -2 0 a 4 4 1 0 1  4 4 v " v9 " a 4 4 1 0 1 -4 4"))))
                     ("s" (let ((m nil) (idx 0))
                            (dolist (l lanes)
                              (let ((w (ewavedrom--get l "wave")))
                                (when (and (ewavedrom--truthy w) (> (length w) x))
                                  (push (list "use" (ewavedrom--tt 2 (+ 5 (* (ewavedrom--lane-yo lane) idx))
                                                                   "xlink:href" "#gap"))
                                        m)))
                              (setq idx (1+ idx)))
                            (nreverse m))))))
              (push (append (list "g" (ewavedrom--tt (* scale (+ x offset)) nil)) marks) res)))
          (setq x (1+ x)))))
    (let ((idx 0))
      (dolist (val lanes)
        (let ((period (ewavedrom--get val "period"))
              (phase (ewavedrom--get val "phase")))
          (setf (ewavedrom--lane-period lane) (if (ewavedrom--truthy period) (ewavedrom--num period) 1)
                (ewavedrom--lane-phase lane) (+ (if (ewavedrom--truthy phase) (* (ewavedrom--num phase) 2) 0)
                                                (ewavedrom--lane-xmin-cfg lane)))
          (when (stringp (ewavedrom--get val "wave"))
            (push (append (list "g" (ewavedrom--tt 0 (+ (ewavedrom--lane-y0 lane) (* idx (ewavedrom--lane-yo lane)))
                                                   "id" (format "wavegap_%d_%d" idx index)))
                          (ewavedrom--render-gap-uses (ewavedrom--get val "wave") lane))
                  res)))
        (setq idx (1+ idx))))
    (append (list "g" (ewavedrom--obj "id" (format "wavegaps_%d" index))) (nreverse res))))

(defun ewavedrom--pw-scale (d sx sy)
  "Scale the tokens of piece-wise path D as WaveDrom does, in place."
  (let ((i 0)
        (nanp (lambda (j) (or (>= j (length d)) (ewavedrom--nan-p (ewavedrom--num (aref d j))))))
        (mul (lambda (j f)
               (when (>= j (length d))
                 (setq d (vconcat d (make-vector (- (1+ j) (length d)) nil))))
               (aset d j (* (ewavedrom--num (aref d j)) f)))))
    (while (< i (length d))
      (pcase (downcase (ewavedrom--str (aref d i)))
        ("h" (while (and (< i (length d)) (not (funcall nanp (1+ i))))
               (funcall mul (1+ i) sx) (setq i (1+ i))))
        ("v" (while (and (< i (length d)) (not (funcall nanp (1+ i))))
               (funcall mul (1+ i) sy) (setq i (1+ i))))
        ((or "m" "l" "t")
         (while (and (< (1+ i) (length d)) (not (funcall nanp (1+ i))))
           (funcall mul (1+ i) sx) (funcall mul (+ i 2) sy) (setq i (+ i 2))))
        ("q"
         (while (and (< (+ i 3) (length d)) (not (funcall nanp (1+ i))))
           (funcall mul (1+ i) sx) (funcall mul (+ i 2) sy)
           (funcall mul (+ i 3) sx) (funcall mul (+ i 4) sy) (setq i (+ i 4))))
        ("a"
         (while (and (< (+ i 6) (length d)) (not (funcall nanp (1+ i))))
           (funcall mul (1+ i) sx) (funcall mul (+ i 2) sy)
           (funcall mul (+ i 6) sx) (funcall mul (+ i 7) sy) (setq i (+ i 7)))))
      (setq i (1+ i)))
    d))

(defun ewavedrom--render-piece-wise-lane (wave idx lane)
  "Group for a piece-wise WAVE, [\"pw\", {d: ...}], or nil if it is not one."
  (let ((tag (and (> (length wave) 0) (aref wave 0)))
        (attr (and (> (length wave) 1) (aref wave 1))))
    (when (and (equal tag "pw") (ewavedrom--obj-p attr))
      (let* ((d (ewavedrom--get attr "d"))
             (d (cond ((stringp d) (vconcat (split-string (string-trim d) "[ \t\n\r\f\v,]+")))
                      ((vectorp d) (copy-sequence d)))))
        (list "g" (ewavedrom--tt 0 (+ (* (ewavedrom--lane-yo lane) idx)
                                      (ewavedrom--lane-ys lane)
                                      (ewavedrom--lane-y0 lane)))
              (list "path" (ewavedrom--obj
                            "style" "fill:none;stroke:#000;stroke-width:1px;"
                            "d" (and d (ewavedrom--pw-scale d (* 2 (ewavedrom--lane-xs lane))
                                                            (- (ewavedrom--lane-ys lane)))))))))))

(defun ewavedrom--render-piece-wise (lanes lane)
  (let ((res (list "g")) (idx 0))
    (dolist (row lanes)
      (let ((wave (ewavedrom--get row "wave")))
        (when (vectorp wave)
          (setq res (append res (list (ewavedrom--render-piece-wise-lane wave idx lane))))))
      (setq idx (1+ idx)))
    res))

;;;; Whole diagram

(defun ewavedrom--skin (source)
  (or (assoc (ewavedrom--config source "skin") ewavedrom-skins)
      (assoc ewavedrom-default-skin ewavedrom-skins)
      (assoc "default" ewavedrom-skins)))

(defun ewavedrom--render-signal (source index)
  "SVG text for the signal diagram SOURCE."
  (let* ((skin (ewavedrom--skin source))
         (socket (plist-get (cdr skin) :socket))
         (lane (ewavedrom--make-lane :xs (nth 0 socket) :ys (nth 1 socket)
                                     :xlabel (nth 2 socket) :ym (nth 3 socket))))
    (ewavedrom--parse-config source lane)
    (let* ((st (ewavedrom--rec (ewavedrom--get source "signal") (ewavedrom--make-walk)))
           (lanes (reverse (ewavedrom--walk-lanes st)))
           (widths (reverse (ewavedrom--walk-width st)))
           (groups (reverse (ewavedrom--walk-groups st)))
           (content (ewavedrom--parse-wave-lanes lanes lane))
           (wave-lanes (ewavedrom--render-wave-lane content index lane))
           (wave-groups (ewavedrom--render-groups groups index lane))
           (xmax (let ((m 0))
                   (cl-mapc (lambda (len w) (setq m (max m (+ len w)))) (car wave-lanes) widths)
                   m)))
      (setf (ewavedrom--lane-xg lane)
            (* (ceiling (/ (float (- xmax (ewavedrom--lane-tgo lane))) (ewavedrom--lane-xs lane)))
               (ewavedrom--lane-xs lane)))
      (let* ((body (append (list (ewavedrom--render-marks content index lane source))
                           (cadr wave-lanes)
                           (list (ewavedrom--render-arcs lanes index source lane)
                                 (ewavedrom--render-gaps lanes index source lane)
                                 (ewavedrom--render-piece-wise lanes lane))))
             (xs (ewavedrom--lane-xs lane))
             (width (+ (ewavedrom--lane-xg lane) (* xs (+ (ewavedrom--lane-xmax lane) 1))))
             (height (+ (* (length content) (ewavedrom--lane-yo lane))
                        (ewavedrom--lane-yh0 lane) (ewavedrom--lane-yh1 lane)
                        (ewavedrom--lane-yf0 lane) (ewavedrom--lane-yf1 lane)))
             (head (cons :obj (copy-alist (plist-get (cdr skin) :head))))
             (waves (list "g" (ewavedrom--obj "id" (format "waves_%d" index))
                          (list "rect" (ewavedrom--obj "width" width "height" height
                                                       "style" "stroke:none;fill:white"))
                          (append (list "g" (ewavedrom--tt (+ (ewavedrom--lane-xg lane) 0.5)
                                                           (+ (ewavedrom--lane-yh0 lane) (ewavedrom--lane-yh1 lane) 0.5)
                                                           "id" (format "lanes_%d" index)))
                                  body)
                          (list "g" (ewavedrom--obj "id" (format "groups_%d" index)) wave-groups))))
        (ewavedrom--put head "id" (format "svgcontent_%d" index))
        (ewavedrom--put head "xmlns" "http://www.w3.org/2000/svg")
        (ewavedrom--put head "xmlns:xlink" "http://www.w3.org/1999/xlink")
        (ewavedrom--put head "height" height)
        (ewavedrom--put head "width" width)
        (ewavedrom--put head "viewBox" (concat "0 0 " (ewavedrom--js-str width) " " (ewavedrom--js-str height)))
        (ewavedrom--put head "overflow" "hidden")
        (ewavedrom--put head "class" "WaveDrom")
        (let ((open (ewavedrom--stringify (list "svg" head ""))))
          (concat (substring open 0 (- (length open) (length "</svg>")))
                  (plist-get (cdr skin) :defs)
                  (ewavedrom--stringify waves)
                  "</svg>"))))))

(defun ewavedrom--fix-dark (svg)
  "Recolour the light-canvas parts WaveDrom leaves in a dark-skin SVG."
  (let ((bg ewavedrom-dark-background)
        (accent ewavedrom-dark-accent)
        (case-fold-search nil))
    (dolist (rule (list (cons "style=\"stroke:none;fill:white\"" (concat "style=\"stroke:none;fill:" bg "\""))
                        (cons "style=\"fill:#FFF;\"" (concat "style=\"fill:" bg ";\""))
                        (cons "#0041c4" accent)
                        (cons "stroke:#00F;" (concat "stroke:" accent ";"))
                        (cons "</style>" (concat ".s6{fill:" bg "}</style>"))))
      (setq svg (replace-regexp-in-string (regexp-quote (car rule)) (cdr rule) svg t t)))
    svg))

;;;###autoload
(defun ewavedrom-render-string (string)
  "Render the WaveJSON in STRING and return the SVG text."
  (let ((source (ewavedrom-read-string string)))
    (unless (ewavedrom--obj-p source)
      (user-error "WaveJSON: the root has to be an object, {signal: [...]}"))
    (let ((signal (ewavedrom--get source "signal")))
      (cond ((vectorp signal)
             (let ((svg (ewavedrom--render-signal source 0)))
               (if (and ewavedrom-dark-skin-fix (equal (car (ewavedrom--skin source)) "dark"))
                   (ewavedrom--fix-dark svg)
                 svg)))
            (signal (user-error "WaveJSON: `signal' has to be an array"))
            ((or (ewavedrom--has source "reg") (ewavedrom--has source "assign"))
             (user-error "WaveJSON: only `signal' diagrams are supported, not `reg' or `assign'"))
            (t (user-error "WaveJSON: `signal: [...]' is missing from the root object"))))))

(defun ewavedrom--read-file (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (buffer-string)))

;;;###autoload
(defun ewavedrom-render-file (file &optional out)
  "Render WaveJSON FILE to the SVG file OUT, by default FILE with .svg.
Return the name of the file written."
  (interactive "fWaveJSON file: ")
  (let ((out (or out (concat (file-name-sans-extension file) ".svg")))
        (svg (ewavedrom-render-string (ewavedrom--read-file file))))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file out (insert svg)))
    (when (called-interactively-p 'interactive)
      (message "Wrote %s" out))
    out))

;;;; Preview

(defcustom ewavedrom-preview-display-function #'ewavedrom--display-side
  "Function called with the preview buffer to show it."
  :type 'function)

(defcustom ewavedrom-preview-fit-width t
  "Non-nil shrinks a preview wider than its window to the window width."
  :type 'boolean)

(defvar-local ewavedrom--preview-svg nil
  "SVG text of the image currently shown in a preview buffer.")

(defun ewavedrom--preview-buffer-name (source)
  (format "*ewavedrom: %s*" (buffer-name (get-buffer source))))

(defun ewavedrom--display-side (buffer)
  (display-buffer buffer '((display-buffer-reuse-window display-buffer-in-side-window)
                           (side . right) (window-width . 0.5))))

(defun ewavedrom--show-svg (buffer svg &optional window)
  "Show SVG in BUFFER, fitted to WINDOW when that is set."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if (and (display-graphic-p) (image-type-available-p 'svg))
          (insert-image (apply #'create-image svg 'svg t
                               (when (and (windowp window) ewavedrom-preview-fit-width)
                                 (list :max-width (window-body-width window t)))))
        (insert svg))
      (setq ewavedrom--preview-svg svg)
      (setq buffer-read-only t)
      (goto-char (point-min)))))

;;;###autoload
(defun ewavedrom-preview ()
  "Render the current buffer's WaveJSON and show it in a side window.
On an error the previous image stays; the error message is shown and
returned."
  (interactive)
  (let* ((source (current-buffer))
         (out (get-buffer-create (ewavedrom--preview-buffer-name source))))
    (condition-case err
        (let ((svg (ewavedrom-render-string
                    (buffer-substring-no-properties (point-min) (point-max)))))
          (ewavedrom--show-svg out svg (funcall ewavedrom-preview-display-function out))
          nil)
      (user-error
       (let ((msg (error-message-string err)))
         (message "%s" msg)
         msg)))))

(defun ewavedrom--after-save ()
  (ewavedrom-preview))

;;;###autoload
(define-minor-mode ewavedrom-preview-mode
  "Re-render the WaveDrom preview each time the buffer is saved."
  :lighter " Wave"
  (if ewavedrom-preview-mode
      (add-hook 'after-save-hook #'ewavedrom--after-save nil t)
    (remove-hook 'after-save-hook #'ewavedrom--after-save t)))

(defvar ewavedrom-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ewavedrom-preview)
    (define-key map (kbd "C-c C-s") #'ewavedrom-render-file-of-buffer)
    map)
  "Keymap for `ewavedrom-mode'.")

(defun ewavedrom-render-file-of-buffer ()
  "Write the current buffer's diagram next to its file, as .svg."
  (interactive)
  (unless buffer-file-name (user-error "Buffer is not visiting a file"))
  (let ((out (concat (file-name-sans-extension buffer-file-name) ".svg")))
    (let ((coding-system-for-write 'utf-8-unix)
          (svg (ewavedrom-render-string (buffer-substring-no-properties (point-min) (point-max)))))
      (with-temp-file out (insert svg)))
    (message "Wrote %s" out)))

;;;###autoload
(define-derived-mode ewavedrom-mode js-mode "WaveDrom"
  "Major mode for WaveJSON timing diagrams.
\\{ewavedrom-mode-map}"
  (ewavedrom-preview-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wjson\\'" . ewavedrom-mode))

;;;###autoload
(defconst ewavedrom-magic-regexp
  "\\`\\(?:[[:space:]]\\|//[^\n]*\n\\|/\\*\\(?:[^*]\\|\\*+[^*/]\\)*\\*+/\\)*{[[:space:]]*[\"']?signal[\"']?[[:space:]]*:"
  "Matches the start of a WaveJSON signal diagram, whatever the file is called.")

;;;###autoload
(add-to-list 'magic-mode-alist (cons ewavedrom-magic-regexp #'ewavedrom-mode))

(provide 'ewavedrom)
;;; ewavedrom.el ends here
