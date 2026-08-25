;;; pdf-text.el --- Reflowed plain-text reading view for PDFs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: August 16, 2026
;; Version: 0.1.0
;; Keywords: files, multimedia
;; Homepage: https://github.com/agzam/pdf-text
;; Package-Requires: ((emacs "29.1") (pdf-tools "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; `pdf-view-as-text' reads the PDF of the current `pdf-view-mode' buffer as
;; reflowed text in a companion buffer, landing on the page the image view
;; shows and as far into it as the window sits down the image.  Text and glyph
;; geometry both come from the already-running epdfinfo (`pdf-info-charlayout'
;; per page, `pdf-info-gettext' as the fallback for a page without one), and
;; the document outline (`pdf-info-outline') becomes foldable org headings.
;; `pdf-text-show-in-pdf' jumps the PDF back to the page at point;
;; `pdf-text-sync-mode' keeps the two on the same page in both directions.
;;
;; poppler hands out lines, never blocks: it computes paragraphs and columns
;; internally and then discards them while serving text, so the reflow has to
;; rebuild that structure from the glyph boxes - where a line ends, how far
;; the next one starts in, how much air sits between their baselines.
;;
;; Everything below the entry commands is pure transformation, testable
;; without a PDF or pdf-tools on the load path.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tq)

;; pdf-tools is a runtime dependency only: every symbol below resolves when a
;; command runs, so the transforms load and test without it.  org arrives with
;; the derived mode.
(declare-function pdf-info-charlayout "ext:pdf-info")
(declare-function pdf-info-gettext "ext:pdf-info")
(declare-function pdf-info-number-of-pages "ext:pdf-info")
(declare-function pdf-info-outline "ext:pdf-info")
(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function image-mode-window-get "image-mode")
(declare-function pdf-view-image-size "ext:pdf-view")
(declare-function org-cycle-overview "org-cycle")
(declare-function org-fold-show-set-visibility "org-fold")
(declare-function org-fold-show-entry "org-fold")
(declare-function pdf-info-process-assert-running "ext:pdf-info")
(declare-function pdf-info-process "ext:pdf-info")
(defvar pdf-info--queue)
(defvar pdf-info-epdfinfo-program)

;;; Charlayout off the wire

;; A charlayout response is a fixed grammar: "OK\n", one record
;; "X0 Y0 X1 Y1:GLYPH\n" per glyph - %f coordinates, the glyph escaped
;; only as \\ \: or \n - then ".\n".  pdf-info reads it through a
;; generic per-character walk that costs more than the server's own
;; work, and one server lays out pages one at a time however many
;; cores the machine has.  Reading the wire directly and fanning a
;; whole book over a few worker servers turns extraction time into
;; the time it takes to drain the answers.

(defvar pdf-text-charlayout-workers 4
  "Worker servers a whole-book extraction may spawn.
Capped by `num-processors'.  The measured knee sits at four: past it,
draining and parsing the responses saturates the Emacs side.")

(defvar pdf-text-charlayout-pool-min 16
  "Pages below which extraction stays on the main server.
Spawning workers and opening the document in each is a fixed cost a
short window never earns back.")

(defvar pdf-text--charlayout-buffer nil
  "Work buffer `pdf-text--charlayout-parse' reuses across pages.")

(defun pdf-text--query-escape (file)
  "FILE escaped for the wire: backslash and colon prefixed, newline as \\n."
  (string-replace "\n" "\\n" (replace-regexp-in-string "[\\:]" "\\\\\\&" file)))

(defun pdf-text--charlayout-parse (response)
  "Charlayout RESPONSE string as (GLYPH (X0 Y0 X1 Y1)) records.
The shape `pdf-info-charlayout' returns, read off the wire grammar
instead: coordinates never contain a colon and every literal backslash
arrives escaped, so one colon-to-space pass leaves the four floats to
the native reader - no intermediate strings - and \"\\ \" can only
name the colon glyph.  Signals on an ERR or malformed response."
  (with-current-buffer
      (or (and (buffer-live-p pdf-text--charlayout-buffer)
               pdf-text--charlayout-buffer)
          (setq pdf-text--charlayout-buffer
                (with-current-buffer
                    (generate-new-buffer " *pdf-text-charlayout*" t)
                  (buffer-disable-undo)
                  (current-buffer))))
    (erase-buffer)
    (insert response)
    (goto-char (point-min))
    (cond
     ((looking-at "OK\n")
      (forward-line 1)
      (subst-char-in-region (point) (point-max) ?: ?\s)
      (let ((buf (current-buffer))
            result)
        (while (not (eq (char-after) ?.))
          (let* ((x0 (read buf)) (y0 (read buf)) (x1 (read buf)) (y1 (read buf))
                 (glyph (progn
                          (forward-char 1)
                          (let ((c (char-after)))
                            (if (eq c ?\\)
                                (progn
                                  (forward-char 1)
                                  (let ((e (char-after)))
                                    (cond ((eq e ?n) ?\n)
                                          ((eq e ?\s) ?:)
                                          (t e))))
                              c)))))
            (unless (numberp y1)
              (error "Malformed charlayout record"))
            ;; past the glyph's last character and the record's newline
            (forward-char 2)
            (push (list glyph (list x0 y0 x1 y1)) result)))
        (nreverse result)))
     ((looking-at "ERR\n")
      (forward-line 1)
      (error "epdfinfo: %s"
             (buffer-substring-no-properties
              (point) (progn (re-search-forward "^\\.\n")
                             (1- (match-beginning 0))))))
     (t (error "Invalid charlayout response")))))

(defun pdf-text--charlayout-pool (n)
  "N transaction queues over worker epdfinfo servers of our own."
  (cl-loop repeat n
           collect (let* ((process-connection-type nil)
                          (default-directory temporary-file-directory)
                          (proc (start-process "pdf-text-epdfinfo" nil
                                               pdf-info-epdfinfo-program)))
                     (set-process-query-on-exit-flag proc nil)
                     (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
                     (tq-create proc))))

(defun pdf-text--charlayout-pool-stop (pool)
  "Quit and reap POOL's workers; their queue buffers go with them."
  (dolist (tq pool)
    (ignore-errors (process-send-string (tq-process tq) "quit\n"))
    (ignore-errors (tq-close tq))))

(defun pdf-text--charlayouts (file pages)
  "Charlayout of FILE's PAGES, a list in that order, nil where a page fails.
Every query goes down the wire before the first answer is read, so the
server never waits on a round trip; past `pdf-text-charlayout-pool-min'
pages the queries fan out over `pdf-text-charlayout-workers' worker
servers, which parallelizes poppler across cores.  A page that fails
any of it - an ERR response, a malformed parse, a worker death -
retries once through `pdf-info-charlayout' on the main server, so a
password the main server holds or a crashed worker costs speed, never
a page."
  (pdf-info-process-assert-running)
  (let* ((file (expand-file-name file))
         (total (length pages))
         (workers (max 1 (min pdf-text-charlayout-workers (num-processors))))
         (pool (and (< 1 workers)
                    (<= pdf-text-charlayout-pool-min total)
                    (stringp pdf-info-epdfinfo-program)
                    (file-executable-p pdf-info-epdfinfo-program)
                    (condition-case nil (pdf-text--charlayout-pool workers)
                      (error nil))))
         (queues (or pool (list pdf-info--queue)))
         (results (make-vector total nil))
         (got 0))
    (unwind-protect
        (progn
          (cl-loop for p in pages
                   for i from 0
                   do (let ((slot i))
                        (tq-enqueue (nth (mod i (length queues)) queues)
                                    (concat "charlayout:"
                                            (pdf-text--query-escape file)
                                            ":" (number-to-string p)
                                            ":0 0 1 1\n")
                                    "^\\.\n" nil
                                    (lambda (_ r)
                                      (aset results slot r)
                                      (setq got (1+ got))))))
          ;; a dead worker never answers; wait only while a live queue
          ;; still owes something, and let the fallback cover the rest
          (while (and (< got total)
                      (cl-some (lambda (tq)
                                 (and (eq (process-status (tq-process tq)) 'run)
                                      (not (tq-queue-empty tq))))
                               queues))
            (accept-process-output nil 0.05)))
      (when pool (pdf-text--charlayout-pool-stop pool)))
    (cl-loop for r across results
             for p in pages
             collect (or (and r (condition-case nil
                                    (pdf-text--charlayout-parse r)
                                  (error nil)))
                         (condition-case nil
                             (pdf-info-charlayout p nil file)
                           (error nil))))))

;;; Statistics over glyph measurements

(defun pdf-text--quantile (values fraction)
  "Order statistic of VALUES at FRACTION; nil when VALUES is empty."
  (when values
    (let* ((sorted (sort (copy-sequence values) #'<))
           (index (min (1- (length sorted))
                       (floor (* fraction (length sorted))))))
      (nth index sorted))))

(defun pdf-text--variation (values)
  "Coefficient of variation of VALUES; nil below two values."
  (when (< 1 (length values))
    (let ((mean (/ (apply #'+ values) (float (length values)))))
      (when (< 0 mean)
        (/ (sqrt (/ (apply #'+ (mapcar (lambda (v) (expt (- v mean) 2)) values))
                    (float (length values))))
           mean)))))

(defun pdf-text--mode-value (values bucket &optional weights)
  "Most common of VALUES once rounded into steps BUCKET wide.
The mode, not an extreme: a page number outside the column, a heading
at twice the body size, or a stray wide line cannot move it.  WEIGHTS,
a parallel list, says how much each value counts - by ink width, a
column of page numbers no longer outvotes a column of prose it happens
to match line for line."
  (let ((counts (make-hash-table :test #'eql)) (best 0) mode)
    (cl-loop for v in values
             for w in (or weights (make-list (length values) 1))
             do (let* ((key (round (/ v bucket)))
                       (n (+ w (gethash key counts 0))))
                  (puthash key n counts)
                  (when (< best n) (setq best n mode key))))
    (and mode (* mode bucket))))

(defvar pdf-text-column-strength 0.25
  "Ink weight, as a fraction of the strongest edge mode's, a column needs.
The facing column of a two-column document carries about as much ink
as its twin; paragraph indents, list hangs and margin notes recur far
below that.")

(defun pdf-text--strong-edges (values bucket weights)
  "The extreme strong modes of VALUES, as (LEFTMOST . RIGHTMOST), or nil.
Rounded into steps BUCKET wide and weighted by WEIGHTS like
`pdf-text--mode-value'; a mode is strong once it carries
`pdf-text-column-strength' of the strongest mode's weight.  The
strongest mode names one column of a multicolumn document, and the
text area runs to the farthest mode that could be a column in its own
right."
  (let ((counts (make-hash-table :test #'eql)) (best 0))
    (cl-loop for v in values
             for w in weights
             do (let ((key (round (/ v bucket))))
                  (cl-callf + (gethash key counts 0) w)
                  (when (< best (gethash key counts))
                    (setq best (gethash key counts)))))
    (let (lo hi)
      (maphash (lambda (key n)
                 (when (<= (* pdf-text-column-strength best) n)
                   (when (or (null lo) (< key lo)) (setq lo key))
                   (when (or (null hi) (< hi key)) (setq hi key))))
               counts)
      (and lo (cons (* lo bucket) (* hi bucket))))))

;;; Line records

(cl-defstruct (pdf-text-line (:constructor pdf-text-line-create)
                             (:copier nil))
  "One line of a page: its text and the geometry of the glyphs that drew it.
Coordinates are page-relative.  The geometry slots are nil for a line
built without a layout, and every rule that reads one falls back to
character heuristics."
  text
  kind                                  ; nil prose, `mono', `math' or `row'
  align                                 ; nil, `right' or `center'
  x0 x1                                 ; ink edges, left and right
  top bot                               ; ink extent, highest and lowest
  base                                  ; median glyph bottom: the baseline
  height                                ; glyph height, upper quantile of the ink
  space                                 ; median width of the line's spaces
  cv                                    ; advance variation; ~0 is monospaced
  first-width                           ; width of the first word
  claimed)                              ; owned by a lane region; zones keep out

(defun pdf-text--layout-lines (layout)
  "Charlayout LAYOUT split at newline glyphs into per-line glyph lists.
Entries are (CHAR (X0 Y0 X1 Y1)); the newline glyphs poppler emits
mirror the line breaks of the gettext stream."
  (let (lines cur)
    (dolist (e layout)
      (if (eq (car e) ?\n)
          (progn (push (nreverse cur) lines) (setq cur nil))
        (push e cur)))
    (when cur (push (nreverse cur) lines))
    (nreverse lines)))

(defvar pdf-text-script-size 0.78
  "Glyph height, in body heights, at and under which a glyph can be a script.
Super- and subscripts are set at 0.5-0.7 of their body size; the
impostors run larger - an inline code font at 0.85 of the serif body,
small caps at 0.8 - and the gap between the two is real.")

(defvar pdf-text-script-raise 0.25
  "Baseline raise, in body heights, past which a smaller glyph is a superscript.
Real superscripts sit 0.35-0.5 body heights up.  An inline code font
whose boxes anchor a shade high sits under 0.2, and stays text.")

(defvar pdf-text-script-drop 0.06
  "Baseline drop, in body heights, past which a smaller glyph is a subscript.
First-level subscripts drop as little as 0.1 body heights, and glyphs
of one font on one baseline agree on their box bottoms exactly, so the
floor guards against float dust rather than typography.")

(defvar pdf-text-script-reach 0.8
  "How far, in body heights, a script baseline can sit from the typeset one.
Scripts are offset by a fraction of the glyph size - the deepest
observed drop is 0.22, the highest raise 0.5.  A glyph past the reach
is another typeset line poppler folded into the record - a drop cap,
a merged pair - and both the baseline vote and the per-glyph
classification must leave it alone.")

(defun pdf-text--escape-literals (text)
  "TEXT with a zero-width space breaking every org-parseable literal.
The reflow writes org sub- and superscripts as ^{...}/_{...}, org
footnotes as [fn:...], and org table rows as |-opened lines, and org
must parse only what the reflow generated: the same forms extracted
from the page itself - a line of LaTeX or org source in a listing, a
bar-ruled page footer - stay plain text."
  (replace-regexp-in-string
   "\\`\\([ \t]*\\)|" "\\1\u200B|"
   (replace-regexp-in-string
    "\\[fn:" "[\u200Bfn:"
    (replace-regexp-in-string "\\([_^]\\){" "\\1\u200B{" text))))

(defun pdf-text--glyph-baseline (glyphs)
  "The typeset baseline of GLYPHS and its body height, as (BASE . HEIGHT).
Letters and digits anchor their boxes at the pen position, so their
box bottoms cluster on the baselines the line was set at; symbol
fonts hang their boxes below the same pen and stay out of the vote.
The widest cluster names the baseline, but every cluster within a
raise of it - each measured by its own glyph size - contests, and the
highest wins: a fragment set mostly in subscript cedes to its few
full-size glyphs, and a line of math-font variables, whose alphabetic
boxes hang below the pen like their operators, cedes to the words set
beside them.  Nothing legitimate sits below a baseline within a
raise's reach of it.  Nil without alphanumeric ink."
  (let (boxes)
    (dolist (g glyphs)
      (when (and (not (memq (car g) '(?\s ?\t)))
                 (string-match-p "[[:alnum:]]" (string (car g))))
        (push (cadr g) boxes)))
    (when boxes
      (let* ((sorted (sort boxes (lambda (a b) (< (nth 3 a) (nth 3 b)))))
             (heights (mapcar (lambda (b) (- (nth 3 b) (nth 1 b))) sorted))
             (gap (* 0.04 (pdf-text--quantile heights 0.5)))
             (levels nil)
             (current (list (car sorted))))
        (dolist (b (cdr sorted))
          (if (< (- (nth 3 b) (nth 3 (car current))) gap)
              (push b current)
            (push current levels)
            (setq current (list b))))
        (push current levels)
        (let* ((stats (mapcar
                       (lambda (level)
                         (list (nth 3 (car (last level)))
                               (pdf-text--quantile
                                (mapcar (lambda (b) (- (nth 3 b) (nth 1 b)))
                                        level)
                                0.5)
                               (apply #'+ (mapcar (lambda (b)
                                                    (- (nth 2 b) (nth 0 b)))
                                                  level))))
                       levels))
               (widest (cl-reduce (lambda (a b) (if (< (nth 2 a) (nth 2 b)) b a))
                                  stats))
               (contest (cl-remove-if-not
                         (lambda (level)
                           (<= (abs (- (nth 0 level) (nth 0 widest)))
                               (* pdf-text-script-raise (nth 1 level))))
                         stats))
               (ref (cl-reduce (lambda (a b) (if (< (nth 0 b) (nth 0 a)) b a))
                               (or contest (list widest)))))
          (cons (nth 0 ref) (nth 1 ref)))))))

(defun pdf-text--script-dir (glyph base height)
  "Which script GLYPH reads as against baseline BASE and body HEIGHT.
`up', `down', or nil for a glyph on the baseline, too large to be a
script, or past `pdf-text-script-reach' - another typeset line the
record carries, not a script of this one.  Box bottoms are baselines:
a descender does not lower its glyph's box, so any real offset is
typeset, not ink."
  (let* ((box (cadr glyph))
         (h (- (nth 3 box) (nth 1 box)))
         (offset (- (nth 3 box) base)))
    (when (and (< h (* pdf-text-script-size height))
               (< (abs offset) (* pdf-text-script-reach height)))
      (cond ((< offset (* (- pdf-text-script-raise) height)) 'up)
            ((< (* pdf-text-script-drop height) offset) 'down)))))

(defvar pdf-text-script-symbol-offset 0.25
  "Baseline offset, in body heights, a run of pure symbols needs to be a script.
A footnote asterisk sits half a body height up; an operator font whose
boxes anchor a shade off the baseline - a midline ellipsis - sits
within 0.15 of it, and stays text.")

(defun pdf-text--script-segments (glyphs base height)
  "GLYPHS split into script runs and plain text, as (DIR . GLYPHS) segments.
DIR is `up', `down' or nil, each glyph read against the typeset
baseline BASE and body HEIGHT.  A space joins the run around it only
when the ink on both sides continues it."
  (let ((vec (vconcat glyphs))
        (segments nil))
    (cl-flet ((dir-at (i)
                (let ((g (aref vec i)))
                  (unless (memq (car g) '(?\s ?\t))
                    (pdf-text--script-dir g base height))))
              (next-ink (i)
                (cl-loop for j from i below (length vec)
                         unless (memq (car (aref vec j)) '(?\s ?\t))
                         return j)))
      (let ((i 0))
        (while (< i (length vec))
          (let* ((g (aref vec i))
                 (space (memq (car g) '(?\s ?\t)))
                 (dir (unless space (dir-at i)))
                 (current (car segments)))
            (cond
             ;; a space continues the segment around it only when the
             ;; ink on both sides agrees; otherwise it reads as plain
             (space
              (let* ((j (next-ink i))
                     (ahead (and j (dir-at j))))
                (if (and current (car current) (eq ahead (car current)))
                    (push g (cdr current))
                  (if (and current (null (car current)))
                      (push g (cdr current))
                    (push (cons nil (list g)) segments)))))
             ((and current (eq dir (car current)))
              (push g (cdr current)))
             (t (push (cons dir (list g)) segments)))
            (setq i (1+ i))))))
    (mapcar (lambda (segment)
              (cons (car segment) (nreverse (cdr segment))))
            (nreverse segments))))

(defun pdf-text--script-markup (glyphs base height space)
  "The text of GLYPHS with script runs wrapped as org ^{...}/_{...} markup.
BASE and HEIGHT are the line's typeset baseline and body height, SPACE
its median word gap.  A run's edge spaces stay outside the wrap; a
lone space poppler set between a run and its host at a fraction of a
word gap - a font switch, not a space the page set - goes, so the
markup attaches where the page attaches.  A run that fails its own
test - symbols alone without a symbol's offset, a brace among the
glyphs - reads as the plain text it was, and literal ^{ and _{ pairs
from the page are broken so org never parses them."
  (let ((out nil)
        (last-ink nil))
    (dolist (segment (pdf-text--script-segments glyphs base height))
      (let* ((dir (car segment))
             (glyphs (cdr segment))
             (text (apply #'string (mapcar #'car glyphs)))
             (ink (cl-remove-if (lambda (g) (memq (car g) '(?\s ?\t))) glyphs)))
        (if (null dir)
            (push (pdf-text--escape-literals text) out)
          (let* ((trimmed (string-trim text))
                 (symbols (not (string-match-p "[[:alnum:]]" trimmed)))
                 (weak (and symbols
                            (cl-some
                             (lambda (g)
                               (< (abs (- (nth 3 (cadr g)) base))
                                  (* pdf-text-script-symbol-offset height)))
                             ink))))
            (if (or weak (string-match-p "[{}]" trimmed) (string-empty-p trimmed))
                (push (pdf-text--escape-literals text) out)
              ;; a lone space before the run at under half a word gap
              ;; is the font switch showing, not a space the page set
              (when (and (stringp (car out))
                         (string-suffix-p " " (car out))
                         (not (string-suffix-p "  " (car out)))
                         space last-ink ink
                         (< (- (nth 0 (cadr (car ink))) (nth 2 last-ink))
                            (* 0.5 space)))
                (setcar out (substring (car out) 0 -1)))
              (push (concat (if (eq dir 'up) "^{" "_{") trimmed "}") out))))
        (when ink (setq last-ink (cadr (car (last ink)))))))
    (apply #'concat (nreverse out))))

(defun pdf-text--glyph-line (glyphs)
  "Line record for GLYPHS, one line of `pdf-info-charlayout' output.
The text carries org script markup for glyph runs set smaller than and
offset from the line's baseline - the raised exponent, the subscript
index - which only exists here, while the per-glyph boxes still do."
  (let* ((text (apply #'string (mapcar #'car glyphs)))
         (ink (cl-remove-if (lambda (g) (memq (car g) '(?\s ?\t))) glyphs))
         (boxes (mapcar #'cadr ink))
         (spaces (cl-loop for g in glyphs
                          when (eq (car g) ?\s)
                          collect (- (nth 2 (cadr g)) (nth 0 (cadr g)))))
         (advances (cl-loop for (a b) on glyphs while b
                            for step = (- (nth 0 (cadr b)) (nth 0 (cadr a)))
                            when (< 0 step) collect step))
         (opening (cl-position-if (lambda (g) (not (eq (car g) ?\s))) glyphs))
         (gap (and opening (cl-position ?\s glyphs :key #'car :start opening))))
    (if (null boxes)
        (pdf-text-line-create :text (pdf-text--escape-literals text))
      (let ((ref (pdf-text--glyph-baseline glyphs))
            (space (pdf-text--quantile spaces 0.5)))
        (pdf-text-line-create
         :text (if ref
                   (pdf-text--script-markup glyphs (car ref) (cdr ref) space)
                 (pdf-text--escape-literals text))
         :x0 (apply #'min (mapcar (lambda (b) (nth 0 b)) boxes))
         :x1 (apply #'max (mapcar (lambda (b) (nth 2 b)) boxes))
         :top (apply #'min (mapcar (lambda (b) (nth 1 b)) boxes))
         :bot (apply #'max (mapcar (lambda (b) (nth 3 b)) boxes))
         ;; the baseline the line's letters anchor at; a line with no
         ;; letters falls back on the median glyph bottom, which
         ;; descenders and superscripts are too few to move
         :base (if ref
                   (car ref)
                 (pdf-text--quantile (mapcar (lambda (b) (nth 3 b)) boxes) 0.5))
         ;; the upper quantile of ink heights tracks the font size, where
         ;; the median would only report the x-height of the line's vowels
         :height (pdf-text--quantile
                  (mapcar (lambda (b) (- (nth 3 b) (nth 1 b))) boxes) 0.8)
         :space space
         :cv (pdf-text--variation advances)
         :first-width (- (nth 2 (cadr (nth (1- (or gap (length glyphs))) glyphs)))
                         (nth 0 (cadr (nth opening glyphs)))))))))

(defun pdf-text--layout-text (layout)
  "The plain text of LAYOUT's glyph stream, newlines included."
  (apply #'string (delq nil (mapcar #'car layout))))

(defun pdf-text--strip-unprinted (text)
  "TEXT without the characters the page never prints, spaces normalized.
A soft hyphen marks where a word may break, and prints only where the
break happened - the end of the line - so everywhere else it goes,
the head of a continuation line included (some typesetters leave the
discretionary marker there).  The line-final one stays: the wrap join
reads it, and whatever survives to the rendered page becomes a plain
hyphen.  C0 control characters are extraction garbage - a BELL inside
a heading, a unit separator inside a formula - and go wherever they
sit; the tab stays, preformatted text indents with it.  The typographic
space family - en, em, figure, thin, no-break and their kin - prints
as the same blank a space does, and Emacs highlights the characters
themselves, so they all read as plain spaces."
  (replace-regexp-in-string
   "[\u00A0\u1680\u2000-\u200A\u202F\u205F\u3000]" " "
   (replace-regexp-in-string
    "\u00AD+\\(.\\)" "\\1"
    (replace-regexp-in-string "[\x00-\x08\x0B-\x1F\x7F]+" "" text))))

(defun pdf-text--page-lines (text &optional layout)
  "Line records for one page, from LAYOUT when it exists, else from TEXT.
Taking the text from the same glyph stream as its geometry is what
keeps the two aligned; the fallback path reflows on character
heuristics alone."
  (let ((lines (if layout
                   (mapcar #'pdf-text--glyph-line (pdf-text--layout-lines layout))
                 (mapcar (lambda (line)
                           (pdf-text-line-create
                            :text (pdf-text--escape-literals line)))
                         (split-string text "\n")))))
    (dolist (line lines lines)
      (setf (pdf-text-line-text line)
            (pdf-text--strip-unprinted (pdf-text-line-text line))))))

(defun pdf-text--profile (pages)
  "Modal body geometry of PAGES, each a list of `pdf-text-line'.
A plist: :height glyph height, :leading baseline step, :left and
:right column edges, :space word gap.  Document-wide, because a page
of listings or a page of table rows has no representative body
geometry of its own."
  (let (heights lefts rights widths spaces leadings)
    (dolist (lines pages)
      (let (prev)
        (dolist (line lines)
          (when (pdf-text-line-x0 line)
            (push (pdf-text-line-height line) heights)
            (push (pdf-text-line-x0 line) lefts)
            (push (pdf-text-line-x1 line) rights)
            (push (- (pdf-text-line-x1 line) (pdf-text-line-x0 line)) widths)
            (when (pdf-text-line-space line)
              (push (pdf-text-line-space line) spaces))
            (when-let* ((base (pdf-text-line-base line))
                        (previous (and prev (pdf-text-line-base prev)))
                        (step (- base previous))
                        ((< 0 step))
                        ((< step 0.1)))
              (push step leadings))
            (setq prev line)))))
    (let ((height (pdf-text--mode-value heights 0.001)))
      (list :height height
            :leading (pdf-text--mode-value leadings 0.002)
            :left (pdf-text--mode-value lefts 0.005 widths)
            :right (pdf-text--mode-value rights 0.005 widths)
            ;; the modal column is ONE column of a two-column document;
            ;; the text area spans them all, which is what tells a
            ;; facing column's cells from a margin note's
            :text-left (car (pdf-text--strong-edges lefts 0.005 widths))
            :text-right (cdr (pdf-text--strong-edges rights 0.005 widths))
            :space (let ((median (pdf-text--quantile spaces 0.5)))
                     (cond ((and median height) (max median (/ height 3.0)))
                           (median)
                           (height (/ height 3.0))))))))

(defvar pdf-text-extra-profile nil
  "Document profile carried into a render of a window of the document.
`pdf-text--profile' output.  Modal body geometry is defined over the
whole book, and a window dominated by exercises or listings measures
a different body than the book does - the drift the corpus-add
refusal names.")

(defvar pdf-text-page-profile-min-lines 8
  "Body lines a page needs before its own column edges are trusted.")

(defun pdf-text--page-profile (lines profile)
  "PROFILE with the column edges this page's own body LINES establish.
Mirrored margins put the body column at a different offset on facing
pages, so one document-wide edge is wrong for half the book.  Only
lines set at the body size have a say: running heads, margin notes and
figure captions have margins of their own.  A page too thin to speak
for itself keeps the document's edges."
  (let* ((height (plist-get profile :height))
         (body (cl-remove-if-not
                (lambda (line)
                  (and (pdf-text-line-x0 line)
                       (or (null height)
                           (null (pdf-text-line-height line))
                           (< (abs (- (pdf-text-line-height line) height))
                              (* 0.15 height)))))
                lines)))
    (if (< (length body) pdf-text-page-profile-min-lines)
        profile
      (let ((page (copy-sequence profile))
            (widths (mapcar (lambda (line)
                              (- (pdf-text-line-x1 line) (pdf-text-line-x0 line)))
                            body)))
        (plist-put page :left
                   (pdf-text--mode-value (mapcar #'pdf-text-line-x0 body)
                                         0.005 widths))
        (plist-put page :right
                   (pdf-text--mode-value (mapcar #'pdf-text-line-x1 body)
                                         0.005 widths))
        page))))

;;; Reading-order repair

(defvar pdf-text-margin-band 0.12
  "Fraction of the page, at top and bottom, where running heads sit.")

(defvar pdf-text-fragment-step 0.5
  "Baseline step, in modal leadings, under which two records share a typeset line.
Poppler breaks a line at a super- or subscript because the script's
baseline is offset, and the offset is a fraction of the leading where
a real next line is a whole leading down.")

(defvar pdf-text-fragment-gap 3
  "Horizontal gap, in modal spaces, above which same-baseline records stay apart.
A table-of-contents entry and its page number, or a margin note beside
its paragraph, share a baseline without sharing a line.")

(defvar pdf-text-zone-height 0.35
  "Fraction of the page's ink height a reassembled zone may span.
A backward baseline jump inside the column marks an aligned array the
reading order scattered; one that spans most of the page is a page of
two columns, or the jump up to the folio, and reassembly would
interleave what are really parallel flows.")

(defun pdf-text--x-gap (a b)
  "Horizontal gap between the ink of lines A and B, zero where they overlap."
  (max 0.0
       (- (pdf-text-line-x0 b) (pdf-text-line-x1 a))
       (- (pdf-text-line-x0 a) (pdf-text-line-x1 b))))

(defun pdf-text--wider-line (a b)
  "The wider-inked of lines A and B."
  (if (< (- (pdf-text-line-x1 a) (pdf-text-line-x0 a))
         (- (pdf-text-line-x1 b) (pdf-text-line-x0 b)))
      b
    a))

(defun pdf-text--merge-records (records text)
  "One line record spanning RECORDS, carrying TEXT.
The baseline and spacing come from the widest record: a script
fragment's own baseline is offset by design, and letting it speak for
the merged line would skew every leading measured against it."
  (let ((widest (cl-reduce #'pdf-text--wider-line records)))
    (pdf-text-line-create
     :text text
     :x0 (apply #'min (mapcar #'pdf-text-line-x0 records))
     :x1 (apply #'max (mapcar #'pdf-text-line-x1 records))
     :top (apply #'min (mapcar #'pdf-text-line-top records))
     :bot (apply #'max (mapcar #'pdf-text-line-bot records))
     :base (pdf-text-line-base widest)
     :height (apply #'max (mapcar #'pdf-text-line-height records))
     :space (pdf-text-line-space widest)
     :cv (pdf-text-line-cv widest)
     :first-width (pdf-text-line-first-width (car records)))))

(defvar pdf-text-script-fragment-chars 8
  "Characters an undissected fragment may hold and still wrap as one script.
The limits poppler splits off an operator are a few glyphs; a longer
record at a slight offset is a row of an aligned array, whose fonts
skew the size comparison, and it must stay text.")

(defun pdf-text--merge-script-text (prev line)
  "The text a merge of records PREV and LINE carries, scripts attached.
The wider record is the typeset line, the narrower the fragment
poppler split off it.  A trailing fragment the glyph pass already
dissected opens with its own ^{}/_{} markup and only needs the space
dropped, so org attaches it to what it follows; a short one it could
not dissect - every glyph one size, nothing in-record to contrast
against - wraps whole, and the side its baseline is offset to picks
  the marker.  A raised marker-shaped leading fragment - the symbol or
number a page sets before its footnote's first word - prefixes the
line, so the note block opens with its marker; only the footnote
token shapes qualify, because a raised letter served before its line
is a math limit the flat join serves better.  Anything else joins
with a space, as a plain split line."
  (let* ((prev-text (pdf-text-line-text prev))
         (line-text (string-trim (pdf-text-line-text line)))
         (host (pdf-text--wider-line prev line))
         (base (pdf-text-line-base host))
         (height (pdf-text-line-height host))
         (offset (and base (pdf-text-line-base line)
                      (- (pdf-text-line-base line) base)))
         (prev-offset (and base (pdf-text-line-base prev)
                           (- (pdf-text-line-base prev) base))))
    (cond
     ((string-match-p "\\`[_^]{" line-text)
      (concat (string-trim-right prev-text) line-text))
     ((and (eq host prev) offset height
           (pdf-text-line-height line)
           (< (pdf-text-line-height line) (* pdf-text-script-size height))
           (<= (length line-text) pdf-text-script-fragment-chars)
           (pdf-text--wordless-p line-text)
           (not (string-match-p "[{}]" line-text))
           (or (< (* pdf-text-script-drop height) offset)
               (< offset (* (- pdf-text-script-raise) height))))
      (concat (string-trim-right prev-text)
              (if (< offset 0) "^{" "_{") line-text "}"))
     ((and (eq host line) prev-offset height
           (pdf-text-line-height prev)
           (< (pdf-text-line-height prev) (* pdf-text-script-size height))
           (string-match-p "\\`\\(?:[*†‡§¶]\\|[0-9]\\{1,2\\}\\)\\'"
                           (string-trim prev-text))
           (< prev-offset (* (- pdf-text-script-raise) height)))
      (concat "^{" (string-trim prev-text) "}" line-text))
     (t (concat prev-text " " line-text)))))

(defun pdf-text--merge-script-fragments (lines profile)
  "LINES with script fragments rejoined to the typeset line they came from.
A record whose baseline sits within `pdf-text-fragment-step' of its
stream neighbour's, close enough in x to touch it, is the piece of a
super- or subscript stack - the limits of a big operator, a lone
raised exponent - that poppler emitted as a line of its own.  The text
joins in stream order, the fragment as the script its offset says it
is, by `pdf-text--merge-script-text'; PROFILE's leading and space are
the measures.  Rejoining is what keeps the sentence around an inline
formula whole."
  (let ((leading (plist-get profile :leading))
        (space (or (plist-get profile :space) 0.005)))
    (if (null leading)
        lines
      (let (out)
        (dolist (line lines)
          (let ((prev (car out)))
            (if (and prev
                     (pdf-text-line-base prev) (pdf-text-line-base line)
                     (pdf-text-line-x0 prev) (pdf-text-line-x0 line)
                     ;; not in the margin bands: a running head and its
                     ;; folio share a baseline too, and merging them would
                     ;; hide the page marker from the marginal-line rules
                     (< pdf-text-margin-band (pdf-text-line-base prev))
                     (< (pdf-text-line-base prev) (- 1 pdf-text-margin-band))
                     (< (abs (- (pdf-text-line-base line)
                                (pdf-text-line-base prev)))
                        (* pdf-text-fragment-step leading))
                     (< (pdf-text--x-gap prev line)
                        (* pdf-text-fragment-gap space)))
                (setcar out (pdf-text--merge-records
                             (list prev line)
                             (pdf-text--merge-script-text prev line)))
              (push line out))))
        (nreverse out)))))

(defun pdf-text--zone-row (rows base leading)
  "The row of ROWS that BASE belongs to, within half a LEADING."
  (cl-find-if (lambda (row)
                (< (abs (- base (car row))) (* pdf-text-fragment-step leading)))
              rows))

(defun pdf-text--collect-zone (vec start limit leading space left right)
  "The zone opening at index START of VEC, as (END . ROWS), or nil.
Records accrete into baseline rows while they stay inside the column
and above LIMIT - the bottom of the run the first jump returned from,
which every parallel run of the zone revisits.  A record that
overlaps the ink already on its row is the resuming flow, not another
cell, and closes the zone there.  ROWS come back with their cells in
stream order."
  (let ((i start)
        (rows nil)
        (open t))
    (while (and open (< i (length vec)))
      (let* ((line (aref vec i))
             (base (pdf-text-line-base line))
             (x0 (pdf-text-line-x0 line))
             (x1 (pdf-text-line-x1 line)))
        (if (not (and base x0
                      (<= base limit)
                      (<= (- left (* 2 space)) x0)
                      (<= x1 (+ right (* 2 space)))))
            (setq open nil)
          (let ((row (pdf-text--zone-row rows base leading)))
            (cond
             ((null row)
              (push (cons base (list line)) rows)
              (setq i (1+ i)))
             ((cl-some (lambda (cell)
                         (and (< (pdf-text-line-x0 cell) x1)
                              (< x0 (pdf-text-line-x1 cell))))
                       (cdr row))
              (setq open nil))
             (t (push line (cdr row))
                (setq i (1+ i))))))))
    (when rows
      (cons i (mapcar (lambda (row) (nreverse (cdr row)))
                      (sort rows (lambda (a b) (< (car a) (car b)))))))))

(defun pdf-text--page-ink-span (lines)
  "Vertical distance between the first and the last ink of LINES."
  (let ((tops (delq nil (mapcar #'pdf-text-line-top lines)))
        (bots (delq nil (mapcar #'pdf-text-line-bot lines))))
    (if (and tops bots)
        (- (apply #'max bots) (apply #'min tops))
      0)))

(defun pdf-text--reassemble-zones (lines profile)
  "LINES with the aligned arrays reading order scattered put back in rows.
Poppler serves an aligned display - a rewrite-rule table, an equation
array - column run by column run, jumping back up the page between
runs.  The jump marks the zone: its records re-sort into baseline
rows, each row's cells in x order, one line per row.  A zone that
runs past `pdf-text-zone-height' of the page is parallel columns or
the folio and stays untouched, as does one with nothing to merge.
PROFILE's leading, space and column edges are the measures."
  (let ((leading (plist-get profile :leading))
        (space (or (plist-get profile :space) 0.005))
        (left (plist-get profile :left))
        (right (plist-get profile :right)))
    (if (not (and leading left right))
        lines
      (let* ((vec (vconcat lines))
             (span (pdf-text--page-ink-span lines))
             (out nil)
             (i 0))
        (while (< i (length vec))
          (let* ((line (aref vec i))
                 (prev (and (< 0 i) (aref vec (1- i))))
                 (jump (and prev
                            (pdf-text-line-base line) (pdf-text-line-base prev)
                            ;; a lane region owns its records: parallel
                            ;; flows keep their served order, and the
                            ;; jumps between lane runs are that order,
                            ;; not a scattered array
                            (not (pdf-text-line-claimed line))
                            (not (pdf-text-line-claimed prev))
                            (< (pdf-text-line-base line)
                               (- (pdf-text-line-base prev)
                                  (* pdf-text-fragment-step leading)))
                            ;; a drop cap's baseline sits at the last
                            ;; line it spans while poppler emits it
                            ;; first: the step back to the paragraph's
                            ;; first line is typesetting, not a column
                            ;; run
                            (not (pdf-text--drop-cap-p prev profile)))))
            (if (not jump)
                (progn (push line out) (setq i (1+ i)))
              ;; walk back to where the run this jump returns to began
              (let ((start (1- i))
                    (limit (+ (pdf-text-line-base prev)
                              (* pdf-text-fragment-step leading)))
                    (floor (- (pdf-text-line-base line)
                              (* pdf-text-fragment-step leading))))
                (while (and (< 0 start)
                            (pdf-text-line-base (aref vec (1- start)))
                            (<= floor (pdf-text-line-base (aref vec (1- start)))))
                  (setq start (1- start)))
                (let* ((zone (pdf-text--collect-zone vec start limit leading space
                                                     left right))
                       (rows (cdr zone))
                       (cells (apply #'+ 0 (mapcar #'length rows)))
                       (bases (mapcar (lambda (row)
                                        (pdf-text-line-base (car row)))
                                      rows)))
                  (if (and zone
                           ;; a zone that closed before its own jump is
                           ;; no zone, and taking it would walk the scan
                           ;; backwards over the same jump forever
                           (< i (car zone))
                           (<= 2 (length rows))
                           (< (length rows) cells)
                           (< 0 span)
                           (<= (- (apply #'max bases) (apply #'min bases))
                               (* pdf-text-zone-height span)))
                      (progn
                        ;; unwind what the zone claims back off OUT
                        (dotimes (_ (- i start)) (pop out))
                        (dolist (row rows)
                          (let ((sorted (sort (copy-sequence row)
                                              (lambda (a b)
                                                (< (pdf-text-line-x0 a)
                                                   (pdf-text-line-x0 b))))))
                            (push (if (cdr sorted)
                                      (pdf-text--merge-records
                                       sorted
                                       (mapconcat (lambda (cell)
                                                    (string-trim
                                                     (pdf-text-line-text cell)))
                                                  sorted " "))
                                    (car sorted))
                                  out)))
                        (setq i (car zone)))
                    (push line out)
                    (setq i (1+ i))))))))
        (nreverse out)))))

;;; Multicolumn lanes

(defvar pdf-text-gap-factor)
(defvar pdf-text-monospace-variation)
(defvar pdf-text-monospace-min-glyphs)
(defvar pdf-text-footnote-size)

(defvar pdf-text-lane-gutter 3
  "Width, in space widths, a gutter must keep clear to separate lanes.
Word gaps in justified prose stretch to a space or two; the white
channel between the columns of a table runs wider on every row that
has one.")

(defvar pdf-text-lane-min-rows 3
  "Rows of side-by-side cells a lane region must show.
A running head shares its baseline with a folio, and two aligned rows
can meet by accident; three are set that way.")

(defvar pdf-text-lane-flows-min-rows 4
  "Side-by-side rows a region must show before it reorders as flows.
Rendering rows in place is cheap to be wrong about; reading lanes to
their ends moves records past their neighbours, and three rows of
accidental alignment - an exercise header, a fragment of a matching
list - scramble more than they save.")

(defvar pdf-text-lane-math-density 0.2
  "Fraction of math glyphs above which a lane region is mathematics.
The zone repair owns aligned maths - a rewrite-rule table, an
equation array - and renders it verbatim; a lane of phonetic symbols
beside prose descriptions stays a table.")

(defvar pdf-text-lane-ragged 4
  "Right-edge spread, in space widths, that reads as ragged cells.
Facing columns of justified prose end flush; translation pairs end
where their words do.  The spread of a lane's right edges is what
tells the two apart when both arrive column by column.")

(defun pdf-text--lane-tabbed-p (line)
  "Whether LINE carries tab-joined segments."
  (string-match-p "\t" (pdf-text-line-text line)))

(defun pdf-text--lane-segments (line)
  "LINE's text split at its tab runs, trimmed, empties dropped."
  (split-string (pdf-text-line-text line) "\t+" t "[ \t]+"))

(defun pdf-text--lane-rows (lines leading)
  "LINES clustered into baseline rows, half a LEADING wide, top to bottom.
Each row is (BASE . CELLS) with CELLS as (INDEX . LINE) in x order,
INDEX the position in the LINES stream.  Records with no geometry
stay out; the caller routes them past the regions untouched."
  (let ((indexed (cl-loop for line in lines
                          for i from 0
                          when (and (pdf-text-line-base line)
                                    (pdf-text-line-x0 line))
                          collect (cons i line)))
        rows)
    (dolist (entry (sort (copy-sequence indexed)
                         (lambda (a b) (< (pdf-text-line-base (cdr a))
                                          (pdf-text-line-base (cdr b))))))
      (let ((base (pdf-text-line-base (cdr entry))))
        (if (and rows (< (abs (- base (caar rows)))
                         (* pdf-text-fragment-step leading)))
            (push entry (cdar rows))
          (push (cons base (list entry)) rows))))
    (mapcar (lambda (row)
              (cons (car row)
                    (sort (nreverse (cdr row))
                          (lambda (a b) (< (pdf-text-line-x0 (cdr a))
                                           (pdf-text-line-x0 (cdr b)))))))
            (nreverse rows))))

(defun pdf-text--lane-row-gaps (cells)
  "The x intervals between consecutive CELLS, as ((START . END)...)."
  (cl-loop for (a b) on cells while b
           collect (cons (pdf-text-line-x1 (cdr a))
                         (pdf-text-line-x0 (cdr b)))))

(defun pdf-text--lane-intersect (gutters gaps min-width)
  "GUTTERS cut down to what GAPS keep open, MIN-WIDTH survivors only."
  (let (out)
    (dolist (g gutters (nreverse out))
      (dolist (gap gaps)
        (let ((start (max (car g) (car gap)))
              (end (min (cdr g) (cdr gap))))
          (when (< (+ start min-width) end)
            (push (cons start end) out)))))))

(defun pdf-text--lane-crosses-p (line gutters)
  "Whether LINE's ink reaches inside one of GUTTERS."
  (cl-some (lambda (g)
             (and (< (pdf-text-line-x0 line) (cdr g))
                  (< (car g) (pdf-text-line-x1 line))))
           gutters))

(defun pdf-text--lane-inside-p (line profile)
  "Whether LINE's ink sits inside PROFILE's text area, margin slack allowed.
The area is the column of a one-column document; on a two-column
document the modal column is one of the two, and the area runs to the
farthest strong edge, so the facing column's cells do not read as
margin notes."
  (let ((space (or (plist-get profile :space) 0.005))
        (left (or (plist-get profile :text-left) (plist-get profile :left)))
        (right (or (plist-get profile :text-right) (plist-get profile :right))))
    (and left right
         (<= (- left (* 2 space)) (pdf-text-line-x0 line))
         (<= (pdf-text-line-x1 line) (+ right (* 2 space))))))

(defun pdf-text--lane-clean-row-p (row profile min-width)
  "Whether ROW is side-by-side cells inside PROFILE's column.
Cells split by tabs disqualify - their ink spans lanes - as does a
cell outside the column: a margin note shares a baseline with the
body line beside it, and that pair is the page's geometry, not a
table's.  MIN-WIDTH is the gutter the cells must leave open."
  (and (<= 2 (length (cdr row)))
       (cl-every (lambda (cell)
                   (and (not (pdf-text--lane-tabbed-p (cdr cell)))
                        (pdf-text--lane-inside-p (cdr cell) profile)))
                 (cdr row))
       (cl-some (lambda (gap) (< (+ (car gap) min-width) (cdr gap)))
                (pdf-text--lane-row-gaps (cdr row)))))

(defun pdf-text--lane-runs (rows profile)
  "Maximal runs of ROWS whose cells keep a common gutter open.
Each region comes back as (:rows ROWS :gutters GUTTERS :clean N):
the run's rows, the white channels its side-by-side rows agree on,
and how many rows brought such cells.  A run opens at a clean
multi-cell row, takes tab-joined rows and single cells nested in a
lane as it goes, and closes on a row that crosses the gutters, walks
outside the column, or steps more than a paragraph gap down the
page.  The step is the air between the run's lowest cell and the
row's highest: row bases carry the intra-row jitter of facing
baselines, and a jittered row must not read as a gap.  PROFILE gives
the measures."
  (let* ((space (or (plist-get profile :space) 0.005))
         (leading (or (plist-get profile :leading) 0.02))
         (min-width (* pdf-text-lane-gutter space))
         regions run gutters clean last-base)
    (cl-flet ((close-run ()
                (when (and run (<= pdf-text-lane-min-rows clean) gutters)
                  (push (list :rows (nreverse run) :gutters gutters :clean clean)
                        regions))
                (setq run nil gutters nil clean 0 last-base nil)))
      (dolist (row rows)
        (let* ((cells (cdr row))
               (near (cl-loop for cell in cells
                              minimize (pdf-text-line-base (cdr cell))))
               (far (cl-loop for cell in cells
                             maximize (pdf-text-line-base (cdr cell))))
               (stepped (and last-base
                             (< (* pdf-text-gap-factor leading)
                                (- near last-base)))))
          (cl-flet ((sink () (setq last-base (if last-base (max last-base far)
                                               far))))
            (when stepped (close-run))
            (cond
             ((null run)
              (when (pdf-text--lane-clean-row-p row profile min-width)
                (setq run (list row)
                      gutters (cl-remove-if
                               (lambda (gap)
                                 (<= (cdr gap) (+ (car gap) min-width)))
                               (pdf-text--lane-row-gaps cells))
                      clean 1)
                (sink)))
             ((pdf-text--lane-clean-row-p row profile min-width)
              (let ((cut (pdf-text--lane-intersect
                          gutters (pdf-text--lane-row-gaps cells) min-width)))
                (if cut
                    (progn (push row run)
                           (setq gutters cut
                                 clean (1+ clean))
                           (sink))
                  (close-run)
                  (when (pdf-text--lane-clean-row-p row profile min-width)
                    (setq run (list row)
                          gutters (cl-remove-if
                                   (lambda (gap)
                                     (<= (cdr gap) (+ (car gap) min-width)))
                                   (pdf-text--lane-row-gaps cells))
                          clean 1)
                    (sink)))))
             ((cl-some (lambda (cell) (pdf-text--lane-tabbed-p (cdr cell))) cells)
              (push row run)
              (sink))
             ((and (null (cdr cells))
                   (pdf-text--lane-inside-p (cdr (car cells)) profile)
                   (not (pdf-text--lane-crosses-p (cdr (car cells)) gutters)))
              (push row run)
              (sink))
             (t (close-run))))))
      (close-run))
    (nreverse regions)))

(defun pdf-text--lane-spans (region profile)
  "The lane x intervals of REGION, between column edge and gutter.
Ordered left to right, one interval more than REGION has gutters.
PROFILE's text-area edges close the two open ends."
  (let* ((space (or (plist-get profile :space) 0.005))
         (gutters (sort (copy-sequence (plist-get region :gutters))
                        (lambda (a b) (< (car a) (car b)))))
         (left (- (or (plist-get profile :text-left)
                      (plist-get profile :left) 0.0)
                  (* 2 space)))
         (right (+ (or (plist-get profile :text-right)
                       (plist-get profile :right) 1.0)
                   (* 2 space)))
         (starts (cons left (mapcar #'cdr gutters)))
         (ends (append (mapcar #'car gutters) (list right))))
    (cl-mapcar #'cons starts ends)))

(defun pdf-text--lane-of (line spans &optional by-end)
  "Index of the lane of SPANS holding LINE, or nil.
The ink's left edge decides; BY-END reads the right edge instead, for
a tab-prefixed record whose leading tab glyphs lie about its start."
  (let ((x (if by-end (pdf-text-line-x1 line) (pdf-text-line-x0 line))))
    (and x (cl-position-if (lambda (span)
                             (and (<= (car span) x) (< x (cdr span))))
                           spans))))

(defun pdf-text--lane-region-cells (region)
  "Every record of REGION, cells and continuations alike."
  (cl-loop for row in (plist-get region :rows)
           append (mapcar #'cdr (cdr row))))

(defun pdf-text--lane-mathish-p (region)
  "Whether REGION's glyphs read as mathematics rather than a table.
Char-weighted over every cell: a lane of one-glyph phonetic symbols
cannot outvote the prose beside it, an equation array with prose-free
cells cannot hide behind its numbers."
  (let ((mathy 0) (wordy 0))
    (dolist (line (pdf-text--lane-region-cells region))
      (pcase-let ((`(,m . ,w) (pdf-text--math-chars (pdf-text-line-text line))))
        (cl-incf mathy m)
        (cl-incf wordy w)))
    (and (< 0 (+ mathy wordy))
         (<= pdf-text-lane-math-density (/ (float mathy) (+ mathy wordy))))))

(defun pdf-text--lane-mono-p (region)
  "Whether REGION's cells are mostly monospaced - a listing, not a table.
Only cells long enough to judge get a vote: a listing whose every code
line pairs with a bare comment dash would otherwise split its vote
down the middle and reflow as a table."
  (let* ((cells (cl-remove-if
                 (lambda (line)
                   (< (length (string-trim (pdf-text-line-text line)))
                      pdf-text-monospace-min-glyphs))
                 (pdf-text--lane-region-cells region)))
         (mono (cl-count-if (lambda (line)
                              (let ((cv (pdf-text-line-cv line)))
                                (and cv (< cv pdf-text-monospace-variation))))
                            cells)))
    (and cells (< (/ (length cells) 2) mono))))

(defun pdf-text--lane-column-served-p (region spans)
  "Whether poppler served REGION lane by lane rather than row by row.
Measured as the fraction of consecutive stream records that stay in
the lane of SPANS the previous one occupied: lane runs push it toward
one, row-interleaved cells toward zero.  Tab-joined records span
lanes and stay out of the vote."
  (let* ((ordered (cl-loop for row in (plist-get region :rows)
                           append (cl-remove-if
                                   (lambda (cell)
                                     (pdf-text--lane-tabbed-p (cdr cell)))
                                   (cdr row))))
         (stream (sort ordered (lambda (a b) (< (car a) (car b)))))
         (same 0) (pairs 0))
    (cl-loop for (a b) on stream while b
             for la = (pdf-text--lane-of (cdr a) spans)
             for lb = (pdf-text--lane-of (cdr b) spans)
             when (and la lb)
             do (cl-incf pairs)
             and do (when (eql la lb) (cl-incf same)))
    (and (< 0 pairs) (<= 0.5 (/ (float same) pairs)))))

(defun pdf-text--lane-numeric (region spans)
  "The lane of SPANS that holds REGION's bare numbers, or nil.
Three cells and more, digits alone in every one: the page-number
column of a table of contents."
  (let ((counts (make-vector (length spans) 0))
        (breakers (make-vector (length spans) nil)))
    (dolist (row (plist-get region :rows))
      (dolist (cell (cdr row))
        (when-let* ((lane (and (not (pdf-text--lane-tabbed-p (cdr cell)))
                               (pdf-text--lane-of (cdr cell) spans))))
          (if (string-match-p "\\`[0-9]+\\'"
                              (string-trim (pdf-text-line-text (cdr cell))))
              (cl-incf (aref counts lane))
            (aset breakers lane t)))))
    (cl-loop for lane from 0 below (length spans)
             when (and (<= 3 (aref counts lane))
                       (not (aref breakers lane)))
             return lane)))

(defun pdf-text--lane-wordless-lanes (region spans)
  "Lane indices of SPANS whose REGION cells mostly carry no word.
A lane of bare digits, brackets or operator debris is a truth table,
an equation column or a listing's punctuation, not entries a reader
follows."
  (let ((wordless (make-vector (length spans) 0))
        (total (make-vector (length spans) 0)))
    (dolist (row (plist-get region :rows))
      (dolist (cell (cdr row))
        (when-let* ((lane (and (not (pdf-text--lane-tabbed-p (cdr cell)))
                               (pdf-text--lane-of (cdr cell) spans))))
          (cl-incf (aref total lane))
          (when (pdf-text--wordless-p (pdf-text-line-text (cdr cell)))
            (cl-incf (aref wordless lane))))))
    (cl-loop for lane from 0 below (length spans)
             when (and (< 0 (aref total lane))
                       (< (aref total lane) (* 2 (aref wordless lane))))
             collect lane)))

(defun pdf-text--lane-enumerated-p (region spans)
  "Whether two and more lanes of REGION run one enumerated list.
Cells opening on list markers in half a lane's rows, twice over
SPANS, are a numbered or bulleted list set two-up to save paper: one
flow wrapped into lanes, not rows that pair."
  (let ((marked (make-vector (length spans) 0))
        (total (make-vector (length spans) 0)))
    (dolist (row (plist-get region :rows))
      (dolist (cell (cdr row))
        (when-let* ((lane (and (not (pdf-text--lane-tabbed-p (cdr cell)))
                               (pdf-text--lane-of (cdr cell) spans))))
          (cl-incf (aref total lane))
          (when (pdf-text--list-marker
                 (string-trim (pdf-text-line-text (cdr cell))) t)
            (cl-incf (aref marked lane))))))
    (<= 2 (cl-loop for lane from 0 below (length spans)
                   count (and (< 0 (aref total lane))
                              (<= (aref total lane)
                                  (* 2 (aref marked lane))))))))

(defun pdf-text--lane-ragged-p (region spans profile)
  "Whether every lane of REGION ends where its words do, not flush.
SPANS name the lanes; PROFILE's space width scales the measures.
A lane is ragged unless it reads as a justified prose column: a
majority of its right edges agree on the lane's own rightmost edge -
paragraph-final lines fall short but stay a minority - and a majority
of its cells open in lowercase, the wrapped tail of a sentence.
Sentence-length coincidence lines up a handful of pair edges, and the
openers are what tell those pairs from prose; a min-max spread could
tell neither apart."
  (let ((space (or (plist-get profile :space) 0.005))
        (case-fold-search nil)
        (edges (make-vector (length spans) nil))
        (lowers (make-vector (length spans) 0)))
    (dolist (row (plist-get region :rows))
      (when (<= 2 (length (cdr row)))
        (dolist (cell (cdr row))
          (when-let* ((lane (and (not (pdf-text--lane-tabbed-p (cdr cell)))
                                 (pdf-text--lane-of (cdr cell) spans))))
            (push (pdf-text-line-x1 (cdr cell)) (aref edges lane))
            (when (string-match-p "\\`[[:lower:]]"
                                  (string-trim
                                   (pdf-text-line-text (cdr cell))))
              (cl-incf (aref lowers lane)))))))
    (cl-loop for lane from 0 below (length spans)
             for xs = (aref edges lane)
             always (or (null (cdr xs))
                        (let* ((bucket (max space 0.001))
                               (top (apply #'max xs))
                               (mode (pdf-text--mode-value xs bucket))
                               (agree (cl-count-if
                                       (lambda (x)
                                         (< (abs (- x mode)) (* 2 bucket)))
                                       xs)))
                          (not (and (< (length xs) (* 2 agree))
                                    (<= (- top mode)
                                        (* pdf-text-lane-ragged space))
                                    (< (length xs)
                                       (* 2 (aref lowers lane))))))))))

(defun pdf-text--lane-classify (region spans profile)
  "How REGION, its lanes SPANS, renders: `rows', `flows' or nil.
Cells served row by row are rows - the page was written that way.
Cells served lane by lane are rows only when a numeric lane pairs
every entry with its number, or when two lanes ragged by PROFILE's
measure pair one to one; anything else - parallel item lists, facing
columns of justified prose - reads each lane to its end, which for
lane-served records means leaving them exactly as they came.

Nil declines the region.  Lanes that are all wordless are a truth
table or an equation array in digits the math gate cannot see, and a
flows region with even one wordless lane is tabular debris - a
statistics block's bracket column - not lists a reader follows."
  (let ((wordless (pdf-text--lane-wordless-lanes region spans)))
    (cond
     ((eql (length wordless) (length spans)) nil)
     ((not (pdf-text--lane-column-served-p region spans)) 'rows)
     ((pdf-text--lane-numeric region spans) 'rows)
     ((pdf-text--lane-enumerated-p region spans) 'flows)
     ((and (= 2 (length spans))
           (let ((rows (cl-remove-if-not
                        (lambda (row) (<= 2 (length (cdr row))))
                        (plist-get region :rows))))
             (<= 0.8 (/ (float (cl-count-if
                                (lambda (row) (= 2 (length (cdr row))))
                                rows))
                        (max 1 (length rows)))))
           (pdf-text--lane-ragged-p region spans profile))
      'rows)
     (wordless nil)
     (t 'flows))))

(defun pdf-text--lane-continuation-p (row spans refs profile)
  "Whether ROW continues the cells of the row above rather than starting one.
A continuation cell leads with the whitespace its page indented it by,
or sits in from the left edge REFS records for its lane of SPANS.
Every cell must read so - a row mixing fresh cells with indented ones
is a new row whose lanes happen to differ.  PROFILE's space width is
the indent tolerance."
  (let ((space (or (plist-get profile :space) 0.005)))
    (cl-every
     (lambda (cell)
       (let ((line (cdr cell)))
         (or (string-match-p "\\`[ \t]" (pdf-text-line-text line))
             (when-let* ((lane (pdf-text--lane-of line spans))
                         (ref (aref refs lane)))
               (< (+ ref space) (pdf-text-line-x0 line))))))
     (cdr row))))

(defun pdf-text--lane-append-cell (cells lane text)
  "Join TEXT onto the LANE cell of CELLS, a vector of strings."
  (let ((prev (aref cells lane)))
    (aset cells lane
          (if (or (null prev) (string-empty-p prev))
              text
            (pdf-text--join-lines prev text)))))

(defun pdf-text--lane-row-fill (row spans refs cells)
  "Distribute ROW's records over CELLS, one string per lane of SPANS.
Tab-joined records contribute their segments to consecutive lanes
from the lane their ink opens in - or, with one segment, the lane
their ink ends in, since leading tab glyphs lie about the start.
REFS learns each lane's left edge from the cells that land there."
  (dolist (cell (cdr row))
    (let* ((line (cdr cell))
           (text (string-trim (pdf-text-line-text line))))
      (if (pdf-text--lane-tabbed-p line)
          (let* ((segments (pdf-text--lane-segments line))
                 (lane (or (if (cdr segments)
                               (pdf-text--lane-of line spans)
                             (pdf-text--lane-of line spans 'by-end))
                           0)))
            (dolist (segment segments)
              (pdf-text--lane-append-cell cells (min lane (1- (length spans)))
                                          segment)
              (setq lane (1+ lane))))
        (let ((lane (or (pdf-text--lane-of line spans) 0)))
          (when (or (null (aref refs lane))
                    (< (pdf-text-line-x0 line) (aref refs lane)))
            (aset refs lane (pdf-text-line-x0 line)))
          (pdf-text--lane-append-cell cells lane text))))))

(defun pdf-text--lane-row-records (region spans refs profile)
  "REGION's rows assembled into cell vectors, as ((CELLS . RECORDS)...).
CELLS is one string per lane of SPANS; RECORDS the member lines the
row and its continuations were drawn from.  REFS accumulates lane
left edges for the continuation test; PROFILE scales its indent."
  (let (rows)
    (dolist (row (plist-get region :rows))
      (if (and rows
               (pdf-text--lane-continuation-p row spans refs profile))
          (progn
            (pdf-text--lane-row-fill row spans refs (caar rows))
            (setf (cdar rows)
                  (append (cdar rows) (mapcar #'cdr (cdr row)))))
        (let ((cells (make-vector (length spans) nil)))
          (pdf-text--lane-row-fill row spans refs cells)
          (push (cons cells (mapcar #'cdr (cdr row))) rows))))
    (nreverse rows)))

(defun pdf-text--lane-table (region spans profile)
  "REGION rendered as org table row records, one `pdf-text-line' per row.
Cells pad to their lane's widest entry, so the rows align as plain
text exactly as org would align them; a literal bar inside a cell
becomes a broken bar, the one glyph org cannot mistake for a column.
SPANS name the lanes and PROFILE the measures."
  (let* ((refs (make-vector (length spans) nil))
         (rows (pdf-text--lane-row-records region spans refs profile))
         (widths (make-vector (length spans) 0)))
    (dolist (row rows)
      (dotimes (lane (length spans))
        (let ((text (aref (car row) lane)))
          (when text
            (aset (car row) lane
                  (setq text (replace-regexp-in-string "|" "\u00A6" text)))
            (aset widths lane (max (aref widths lane) (string-width text)))))))
    (mapcar
     (lambda (row)
       (let* ((cells (cl-loop for lane from 0 below (length spans)
                              for text = (or (aref (car row) lane) "")
                              collect (concat text
                                              (make-string
                                               (- (aref widths lane)
                                                  (string-width text))
                                               ?\s))))
              (record (pdf-text--merge-records
                       (cdr row)
                       (concat "| " (mapconcat #'identity cells " | ") " |"))))
         (setf (pdf-text-line-kind record) 'row)
         (setf (pdf-text-line-cv record) nil)
         (setf (pdf-text-line-claimed record) t)
         record))
     rows)))

(defun pdf-text--lane-adopt (rows spans regions profile min-width)
  "Stray ROWS that fit the lanes a numeric-lane region establishes.
A table of contents runs its entries in blocks; a pair cut off from
the block by prose - the first entry of the page, above the chapter
line - is still an entry.  The row must be clean side-by-side cells
that keep clear of SPANS' gutters, one of them bare digits in the
numeric lane.  REGIONS' member rows stay out.  PROFILE and MIN-WIDTH
are the run measures.  Returns each adopted row as its own region."
  (let ((members (make-hash-table :test 'eq))
        (numeric (cl-loop for region in regions
                          for rs = (cdr (assq region spans))
                          when (and rs (eq 'rows (plist-get region :class))
                                    (pdf-text--lane-numeric region rs))
                          return (cons region rs))))
    (dolist (region regions)
      (dolist (row (plist-get region :rows))
        (puthash row t members)))
    (when numeric
      (let* ((region (car numeric))
             (rs (cdr numeric))
             (lane (pdf-text--lane-numeric region rs))
             (gutters (plist-get region :gutters))
             adopted)
        (dolist (row rows)
          (when (and (not (gethash row members))
                     (pdf-text--lane-clean-row-p row profile min-width)
                     (cl-notany (lambda (cell)
                                  (pdf-text--lane-crosses-p (cdr cell) gutters))
                                (cdr row))
                     (cl-some (lambda (cell)
                                (and (eql lane (pdf-text--lane-of (cdr cell) rs))
                                     (string-match-p
                                      "\\`[0-9]+\\'"
                                      (string-trim
                                       (pdf-text-line-text (cdr cell))))))
                              (cdr row)))
            (push (list :rows (list row) :gutters gutters :clean 1
                        :class 'rows)
                  adopted)))
        (nreverse adopted)))))

(defun pdf-text--lane-evict-foot-blocks (region profile min-width)
  "REGION without the foot block a lane resumes in smaller type, or nil.
An author note or an unmarked footnote sits at the foot of one column,
set under `pdf-text-footnote-size' of the body height; its lines share
baseline rows with the facing column's foot, so the run swallows them,
and a lane reorder would then read the note before the column whose
sentence it interrupts.  A cell that resumes its lane after more than
the paragraph gap, set that much smaller, leaves the region and stays
in the stream - which serves it where the page reads it, after the
columns.  Returns nil when the surgery leaves fewer than
`pdf-text-lane-min-rows' clean rows; PROFILE gives the body height
and the leading, MIN-WIDTH the gutter measure."
  (let* ((height (plist-get profile :height))
         (leading (or (plist-get profile :leading) 0.02))
         (gap (* pdf-text-gap-factor leading))
         (spans (pdf-text--lane-spans region profile))
         (seen (make-vector (length spans) nil))
         evicted rows)
    (if (null height)
        region
      (dolist (row (plist-get region :rows))
        (let (kept)
          (dolist (cell (cdr row))
            (let* ((line (cdr cell))
                   (h (pdf-text-line-height line))
                   (base (pdf-text-line-base line))
                   (lane (pdf-text--lane-of line spans))
                   (last (and lane (aref seen lane))))
              (if (and h base last
                       (< h (* pdf-text-footnote-size height))
                       (< gap (- base last)))
                  (setq evicted t)
                (push cell kept)
                (when (and lane base) (aset seen lane base)))))
          (when kept
            (push (cons (car row) (nreverse kept)) rows))))
      (if (not evicted)
          region
        (plist-put region :rows (nreverse rows))
        (plist-put region :clean
                   (cl-count-if (lambda (row)
                                  (pdf-text--lane-clean-row-p
                                   row profile min-width))
                                (plist-get region :rows)))
        (and (<= pdf-text-lane-min-rows (plist-get region :clean))
             region)))))

(defun pdf-text--lane-unfold (ordered spans profile)
  "Records ORDERED lane-major remapped into the modal column's frame.
Facing columns of justified prose are one flow folded to fit the page;
once the reorder unrolls the fold, the geometry unrolls with it - the
lanes shift onto one measure and their baselines run on where the lane
before left off - so every downstream rule reads the page as the
single column the author wrote, and the sentence a column seam split
rejoins there.  The frame is the lane holding PROFILE's modal left
edge: the profile of a two-column document IS one of its columns, and
landing the unfold there keeps the profile a downstream pass measures
over the output equal to the one this pass measured over the input -
a seeded window render and its book agree only then.  SPANS name the
lanes, PROFILE the leading and the modal column."
  (let ((leading (or (plist-get profile :leading) 0.02))
        (left (plist-get profile :left))
        (groups (make-vector (length spans) nil)))
    (dolist (line ordered)
      (push line (aref groups (or (pdf-text--lane-of line spans) 0))))
    (let* ((lanes (cl-loop for span in spans
                           for i from 0
                           for members = (nreverse (aref groups i))
                           when members collect (cons span members)))
           (target (or (and left
                            (cl-find-if (lambda (lane)
                                          (and (<= (car (car lane)) left)
                                               (< left (cdr (car lane)))))
                                        lanes))
                       (car lanes)))
           (ref-left (and target
                          (cl-loop for line in (cdr target)
                                   minimize (pdf-text-line-x0 line))))
           (tail (and lanes
                      (cl-loop for line in (cdr (car lanes))
                               maximize (pdf-text-line-base line)))))
      (dolist (lane lanes)
        (let ((shift (- ref-left
                        (cl-loop for line in (cdr lane)
                                 minimize (pdf-text-line-x0 line))))
              (rebase (if (eq lane (car lanes))
                          0.0
                        (- (+ tail leading)
                           (cl-loop for line in (cdr lane)
                                    minimize (pdf-text-line-base line))))))
          (dolist (line (cdr lane))
            (cl-incf (pdf-text-line-x0 line) shift)
            (cl-incf (pdf-text-line-x1 line) shift)
            (cl-incf (pdf-text-line-base line) rebase)
            (when (pdf-text-line-top line)
              (cl-incf (pdf-text-line-top line) rebase))
            (when (pdf-text-line-bot line)
              (cl-incf (pdf-text-line-bot line) rebase)))
          (setq tail (cl-loop for line in (cdr lane)
                              maximize (pdf-text-line-base line))))))))

(defun pdf-text--mark-lanes (lines profile)
  "LINES with multicolumn regions read as the page set them.
Rows of side-by-side cells become org table rows; parallel lane
flows keep their served order and are claimed against the zone
repair, whose row-sort is what braided them into pseudo-prose.
PROFILE gives the column and the measures."
  (let ((leading (plist-get profile :leading)))
    (if (not (and leading (plist-get profile :left) (plist-get profile :right)))
        lines
      (let* ((min-width (* pdf-text-lane-gutter
                           (or (plist-get profile :space) 0.005)))
             (rows (pdf-text--lane-rows lines leading))
             (regions (cl-remove-if
                       (lambda (region)
                         (or (pdf-text--lane-mathish-p region)
                             (pdf-text--lane-mono-p region)))
                       (delq nil
                             (mapcar (lambda (region)
                                       (pdf-text--lane-evict-foot-blocks
                                        region profile min-width))
                                     (pdf-text--lane-runs rows profile)))))
             (spans (mapcar (lambda (region)
                              (cons region
                                    (pdf-text--lane-spans region profile)))
                            regions)))
        (dolist (region regions)
          (plist-put region :class
                     (pdf-text--lane-classify region (cdr (assq region spans))
                                              profile)))
        (setq regions
              (cl-remove-if (lambda (region)
                              (pcase (plist-get region :class)
                                ('flows (< (plist-get region :clean)
                                           pdf-text-lane-flows-min-rows))
                                ('rows nil)
                                (_ t)))
                            regions))
        (setq regions
              (append regions
                      (pdf-text--lane-adopt rows spans regions profile
                                            min-width)))
        (if (null regions)
            lines
          (let ((replacement (make-hash-table :test 'eq))
                (skip (make-hash-table :test 'eq)))
            (dolist (region regions)
              (let* ((members (pdf-text--lane-region-cells region))
                     (rs (or (cdr (assq region spans))
                             (pdf-text--lane-spans region profile)))
                     ;; splice where the stream first touches the
                     ;; region, whichever lane was served first
                     (first (cl-loop with best = nil with at = nil
                                     for row in (plist-get region :rows)
                                     do (dolist (cell (cdr row))
                                          (when (or (null at)
                                                    (< (car cell) at))
                                            (setq at (car cell)
                                                  best (cdr cell))))
                                     finally return best)))
                (pcase (plist-get region :class)
                  ('rows
                   (let ((records (pdf-text--lane-table region rs profile)))
                     (dolist (line members) (puthash line t skip))
                     (puthash first records replacement)))
                  ('flows
                   ;; each lane read to its end, left to right: the
                   ;; stream may serve the right lane first, and the
                   ;; reader gets the lanes in page order either way.
                   ;; the reordered records are copies: the caller's
                   ;; records keep their served order and geometry, so
                   ;; a second pass over the same page reads the same
                   ;; page - the unfold rewrites coordinates
                   (let* ((fixed (pdf-text--lane-ragged-p region rs profile))
                          (ordered
                           (mapcar
                            (lambda (cell)
                              (let ((copy (copy-sequence (cdr cell))))
                                (setf (pdf-text-line-claimed copy) t)
                                ;; ragged lanes are item lists, one
                                ;; entry per typeset line; reflowing
                                ;; them would only braid neighbours
                                ;; back together.  flush lanes are
                                ;; facing prose columns and reflow
                                (when (and fixed
                                           (null (pdf-text-line-kind copy)))
                                  (setf (pdf-text-line-kind copy) 'fixed))
                                copy))
                            (sort (cl-loop for row in (plist-get region :rows)
                                           append (copy-sequence (cdr row)))
                                  (lambda (a b)
                                    (let ((la (or (pdf-text--lane-of (cdr a) rs)
                                                  0))
                                          (lb (or (pdf-text--lane-of (cdr b) rs)
                                                  0)))
                                      (if (eql la lb)
                                          (< (car a) (car b))
                                        (< la lb))))))))
                     (unless fixed
                       (pdf-text--lane-unfold ordered rs profile))
                     (dolist (line members)
                       (puthash line t skip))
                     (puthash first ordered replacement))))))
            (cl-loop for line in lines
                     for records = (gethash line replacement)
                     append (cond (records records)
                                  ((gethash line skip) nil)
                                  (t (list line))))))))))

(defun pdf-text-reading-order (pages)
  "PAGES of `pdf-text-line' records, cleaned and in repaired reading order.
Script fragments rejoin the typeset line poppler split them from, by
`pdf-text--merge-script-fragments'; multicolumn regions come back as
table rows or stay lane-ordered flows, by `pdf-text--mark-lanes'; and
the aligned arrays reading order scattered come back row by row, by
`pdf-text--reassemble-zones'.  These are the lines the reflow reads,
and the lines the corpus measures survival against: the repairs
merge and reorder records, never drop one."
  (let* ((page-lines (pdf-text-clean-pages pages))
         ;; the seeded document profile reaches the repairs too: the
         ;; merge and zone thresholds are leadings and spaces, and a
         ;; window measures both differently than its book does
         (profile (or pdf-text-extra-profile (pdf-text--profile page-lines))))
    (mapcar (lambda (lines)
              (pdf-text--reassemble-zones
               (pdf-text--mark-lanes
                (pdf-text--merge-script-fragments lines profile)
                profile)
               profile))
            page-lines)))

;;; Line classification

(defvar pdf-text-monospace-variation 0.05
  "Advance variation below which a line reads as monospaced.
A listing's glyph advances are identical, proportional type varies by
0.2 and more, so the two never meet in the middle.")

(defvar pdf-text-monospace-min-glyphs 8
  "Glyphs a line needs before its advances can call it monospaced.
Proportional fonts set digits to one width, so a page number or a
table cell of figures reads as monospaced until enough letters have
had their say.")

(defun pdf-text--similar-height-p (a b)
  "Whether lines A and B were set in the same size."
  (let ((ha (pdf-text-line-height a))
        (hb (pdf-text-line-height b)))
    (and ha hb (< 0 (max ha hb))
         (< (/ (abs (- ha hb)) (max ha hb)) 0.15))))

(defun pdf-text--mark-monospace (lines)
  "Tag the monospaced LINES, which are listings and must not reflow.
A line too short to judge on its own takes the tag from a neighbour of
the same size: a closing brace alone on its line belongs to the
listing above it."
  (dolist (line lines)
    (when-let* ((cv (pdf-text-line-cv line))
                ((null (pdf-text-line-kind line))))
      (when (and (< cv pdf-text-monospace-variation)
                 (<= pdf-text-monospace-min-glyphs
                     (length (string-trim (pdf-text-line-text line)))))
        (setf (pdf-text-line-kind line) 'mono))))
  (let ((vec (vconcat lines)))
    (dotimes (i (length vec))
      (let ((line (aref vec i)))
        (unless (or (pdf-text-line-kind line)
                    (<= pdf-text-monospace-min-glyphs
                        (length (string-trim (pdf-text-line-text line)))))
          (when (cl-some (lambda (j)
                           (and (<= 0 j) (< j (length vec))
                                (eq 'mono (pdf-text-line-kind (aref vec j)))
                                (pdf-text--similar-height-p line (aref vec j))))
                         (list (1- i) (1+ i)))
            (setf (pdf-text-line-kind line) 'mono))))))
  lines)

(defun pdf-text--aligned-pair (line other profile)
  "How LINE sits against neighbouring OTHER: `right', `center' or nil.
A run whose right edges agree while its left edges move is set flush
right; one whose centres agree while both edges move is centred.
Either way the left edge means nothing about paragraphs.  Justified
prose looks flush right too, so the run must also stay clear of the
right edge PROFILE gives the column."
  (let ((space (or (plist-get profile :space) 0.005))
        (column-right (plist-get profile :right))
        (column-left (plist-get profile :left))
        (leading (or (plist-get profile :leading) 0.02)))
    (when (and (pdf-text-line-x0 other)
               (pdf-text--similar-height-p line other)
               (pdf-text-line-base line) (pdf-text-line-base other)
               (< (abs (- (pdf-text-line-base line) (pdf-text-line-base other)))
                  (* 2 leading))
               column-right
               (< (pdf-text-line-x1 line) (- column-right (* 2 space))))
      (let ((left-step (abs (- (pdf-text-line-x0 line) (pdf-text-line-x0 other))))
            (right-step (abs (- (pdf-text-line-x1 line) (pdf-text-line-x1 other))))
            (centre-step (abs (- (+ (pdf-text-line-x0 line) (pdf-text-line-x1 line))
                                 (+ (pdf-text-line-x0 other) (pdf-text-line-x1 other))))))
        (cond
         ((and (< right-step space) (< space left-step)) 'right)
         ((and (< centre-step (* 2 space))
               (< space left-step)
               (< space right-step)
               column-left
               (< (+ column-left (* 2 space)) (pdf-text-line-x0 line)))
          'center))))))

(defun pdf-text--shares-measure-p (line other align profile)
  "Whether LINE is set to the same ALIGN measure as neighbouring OTHER."
  (let ((space (or (plist-get profile :space) 0.005))
        (leading (or (plist-get profile :leading) 0.02)))
    (and (pdf-text-line-x0 line) (pdf-text-line-x0 other)
         (pdf-text--similar-height-p line other)
         (pdf-text-line-base line) (pdf-text-line-base other)
         (< (abs (- (pdf-text-line-base line) (pdf-text-line-base other)))
            (* 2 leading))
         (pcase align
           ('right (< (abs (- (pdf-text-line-x1 line) (pdf-text-line-x1 other)))
                      space))
           ('center (< (abs (- (+ (pdf-text-line-x0 line) (pdf-text-line-x1 line))
                               (+ (pdf-text-line-x0 other) (pdf-text-line-x1 other))))
                       (* 2 space)))))))

(defun pdf-text--mark-alignment (lines profile)
  "Tag the LINES of right-aligned and centred runs, measured against PROFILE.
A pair of lines establishes the run, then it spreads to the neighbours
sharing its measure: one line of a flush-right note can start where
the line above it did by coincidence, and that is no reason to read it
as prose."
  (let ((vec (vconcat lines)))
    (dotimes (i (length vec))
      (let ((line (aref vec i)))
        (when (pdf-text-line-x0 line)
          (dolist (j (list (1- i) (1+ i)))
            (when (and (<= 0 j) (< j (length vec))
                       (null (pdf-text-line-align line)))
              (setf (pdf-text-line-align line)
                    (pdf-text--aligned-pair line (aref vec j) profile)))))))
    (dolist (step '(1 -1))
      (dotimes (k (length vec))
        (let* ((i (if (< 0 step) k (- (length vec) 1 k)))
               (j (- i step))
               (line (aref vec i)))
          (when (and (<= 0 j) (< j (length vec))
                     (null (pdf-text-line-align line)))
            (let ((align (pdf-text-line-align (aref vec j))))
              (when (and align
                         (pdf-text--shares-measure-p line (aref vec j) align profile))
                (setf (pdf-text-line-align line) align))))))))
  lines)

(defun pdf-text--math-char-p (ch)
  "Whether CH is a glyph mathematics is written in.
Operators, arrows, Greek, letterlike symbols, the mathematical
alphanumerics, and the private-use area math fonts map their glyphs
to.  ASCII comparison and relation characters count too; parentheses,
digits and Latin letters do not."
  (or (memq ch '(?= ?+ ?< ?> ?| ?\\ ?^ ?~ ?`))
      (memq ch '(?\N{NOT SIGN} ?\N{PLUS-MINUS SIGN}
                 ?\N{MULTIPLICATION SIGN} ?\N{DIVISION SIGN}
                 ?\N{FRACTION SLASH}))
      (<= #x0370 ch #x03FF)             ; Greek
      (<= #x2032 ch #x2037)             ; primes
      (<= #x2070 ch #x209F)             ; superscripts and subscripts
      (<= #x2100 ch #x214F)             ; letterlike
      (<= #x2190 ch #x21FF)             ; arrows
      (<= #x2200 ch #x23FF)             ; operators, misc technical
      (<= #x27C0 ch #x27EF)             ; misc mathematical A
      (<= #x27F0 ch #x27FF)             ; supplemental arrows A
      (<= #x2900 ch #x297F)             ; supplemental arrows B
      (<= #x2980 ch #x29FF)             ; misc mathematical B
      (<= #x2A00 ch #x2AFF)             ; supplemental operators
      (<= #xE000 ch #xF8FF)             ; private use
      (<= #x1D400 ch #x1D7FF)))        ; mathematical alphanumerics

(defun pdf-text--math-chars (text)
  "Counts of TEXT's mathematical and word glyphs, as (MATHY . WORDY)."
  (let ((mathy 0) (wordy 0))
    (dotimes (i (length text))
      (let ((ch (aref text i)))
        (cond ((pdf-text--math-char-p ch) (cl-incf mathy))
              ((if (< ch 128)
                   (or (<= ?a ch ?z) (<= ?A ch ?Z))
                 (string-match-p "[[:alpha:]]" (char-to-string ch)))
               (cl-incf wordy)))))
    (cons mathy wordy)))

(defun pdf-text--wordless-p (text)
  "Whether TEXT carries no word - no run of three letters or more.
A bare variable, a rule number, a lone bracket."
  (not (string-match-p "[[:alpha:]]\\{3,\\}" text)))

(defun pdf-text-mathish-text-p (text)
  "Whether TEXT reads as mathematics rather than prose.
Either its glyphs are substantially operators and symbols, or what
letters it has are single-letter variables next to at least one
operator.  Words of three letters and more read as prose and vote
against."
  (pcase-let ((`(,mathy . ,wordy) (pdf-text--math-chars text)))
    (or (and (<= 2 mathy) (<= wordy (* 3 mathy)))
        (and (<= 1 mathy)
             (let* ((words (seq-filter (lambda (w) (string-match-p "[[:alpha:]]" w))
                                       (split-string text "[^[:alnum:]]+" t)))
                    (short (seq-count (lambda (w) (<= (length w) 2)) words)))
               (and words (<= (* 3 (length words)) (* 5 short))))))))

(defun pdf-text--displayed-p (line profile)
  "Whether the page sets LINE apart from PROFILE's column, as display maths.
In from the left margin and short of the right, or centred - the
setting a displayed equation gets, where prose is flush or justified."
  (let ((space (or (plist-get profile :space) 0.005))
        (left (plist-get profile :left))
        (right (plist-get profile :right))
        (x0 (pdf-text-line-x0 line))
        (x1 (pdf-text-line-x1 line)))
    (and x0 x1 left right
         (or (eq 'center (pdf-text-line-align line))
             (and (< (+ left (* 3 space)) x0)
                  (< x1 (- right (* 6 space))))))))

(defun pdf-text--mark-math (lines profile)
  "Tag the LINES set as display mathematics, which render verbatim.
A line is display maths when its glyphs read as mathematics -
operators and single-letter variables rather than words - and PROFILE
says the page sets it apart from the column.  A displayed line with no
words at all - a lone bracket, a bare variable under an operator run -
joins a neighbouring display, the way a short brace joins its
listing."
  (dolist (line lines)
    (when (and (null (pdf-text-line-kind line))
               (pdf-text--displayed-p line profile)
               (pdf-text-mathish-text-p (pdf-text-line-text line)))
      (setf (pdf-text-line-kind line) 'math)))
  (let ((vec (vconcat lines))
        (changed t))
    (while changed
      (setq changed nil)
      (dotimes (i (length vec))
        (let ((line (aref vec i)))
          (when (and (null (pdf-text-line-kind line))
                     (pdf-text--displayed-p line profile)
                     (pdf-text--wordless-p (pdf-text-line-text line))
                     (not (string-blank-p (pdf-text-line-text line)))
                     (cl-some (lambda (j)
                                (and (<= 0 j) (< j (length vec))
                                     (eq 'math (pdf-text-line-kind (aref vec j)))))
                              (list (1- i) (1+ i))))
            (setf (pdf-text-line-kind line) 'math)
            (setq changed t))))))
  lines)

(defvar pdf-text-preformatted-indent 4
  "Leading spaces at which a line counts as preformatted.")

(defvar pdf-text-preformatted-space-run 3
  "Interior space-run length that marks a line preformatted.
Three, not two: justified PDFs emit double spaces inside ordinary
sentences, and those lines must stay joinable.")

(defun pdf-text--preformatted-p (line)
  "Whether LINE looks preformatted: code, table, or aligned layout."
  (or (string-match-p (format "\\`\\(?: \\{%d\\}\\|\t\\)"
                              pdf-text-preformatted-indent)
                      line)
      (string-match-p (format " \\{%d\\}" pdf-text-preformatted-space-run)
                      (string-trim line))))

(defconst pdf-text-bullet-re
  "[•‣▪▫◦∙·◆◇○●□■▶▸✓✔➤]"
  "Glyphs that open a list item and mean nothing else.")

(defconst pdf-text-weak-bullet-re
  "[-–—*+]"
  "Glyphs that open a list item only where the line is indented.
A dash or an asterisk at the margin is far more often a footnote
marker or a wrapped clause than a bullet.")

(defconst pdf-text-numeral-re
  "(?[0-9]\\{1,3\\}[.)]\\|(?[ivxIVX]\\{1,5\\})\\|(?[ivx]\\{1,5\\}[.)]\\|([a-zA-Z])"
  "Enumerators that open a list item: 1., 2), (3), iv., (a).")

(defun pdf-text--list-marker (text &optional indented)
  "The list marker TEXT opens with, else nil.
INDENTED reports that the line starts in from the column margin, which
is what tells a bullet dash from a wrapped clause."
  (let ((trimmed (string-trim-left text)))
    (when (string-match (format "\\`\\(%s\\|%s%s\\)[ \t]+[^ \t]"
                                pdf-text-bullet-re
                                (if indented
                                    (format "%s\\|" pdf-text-weak-bullet-re)
                                  "")
                                pdf-text-numeral-re)
                        trimmed)
      (match-string 1 trimmed))))

;;; Titles the outline names

(defconst pdf-text-ligature-alist
  '((?\ﬀ . "ff") (?\ﬁ . "fi") (?\ﬂ . "fl") (?\ﬃ . "ffi") (?\ﬄ . "ffl")
    (?\ﬅ . "st") (?\ﬆ . "st"))
  "Ligature glyphs and the letters they stand for.
A font substitutes them for the letter pairs it sets, so the line
carries the glyph where the outline entry carries the letters.")

(defun pdf-text--normalize-title (text)
  "TEXT reduced to the letters and digits it is made of, downcased.
An outline entry and the line it names agree on the words and on
little else: the line may be set in small caps, spaced out by the
extraction, or carry the ligatures its font substitutes."
  (let ((folded (mapconcat (lambda (char)
                             (or (cdr (assq char pdf-text-ligature-alist))
                                 (char-to-string char)))
                           text "")))
    (downcase (replace-regexp-in-string "[^[:alnum:]]" "" folded))))

(defun pdf-text--heading-title (head)
  "The title the org heading line HEAD carries, without its markers."
  (string-trim (replace-regexp-in-string "\\`\\(?:\\*+\\|#\\+TITLE:\\)[ \t]*" ""
                                         head)))

(defconst pdf-text-section-number-re
  "\\`\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\.?[ \t]+"
  "A section number opening a line: \"1 \", \"3.1 \", \"10. \".
The page numbers its sections where the outline names them bare, at
every depth a paper or a textbook uses.")

(defun pdf-text--section-number (text)
  "The section number TEXT opens with, or nil."
  (and (string-match pdf-text-section-number-re text)
       (match-string 1 text)))

(defun pdf-text--unnumbered-title (text)
  "TEXT without the section number it opens with."
  (replace-regexp-in-string pdf-text-section-number-re "" text))

(defun pdf-text--title-key (text)
  "TEXT as the key a title is looked up by: normalised, unnumbered."
  (pdf-text--normalize-title (pdf-text--unnumbered-title text)))

(defun pdf-text--heading-alist (headings)
  "HEADINGS as an alist from the normalised title to the heading line.
The book's own title renders as a keyword rather than a headline and
names no line of the page, so it stays out."
  (delq nil
        (mapcar (lambda (head)
                  (when (string-prefix-p "*" head)
                    (cons (pdf-text--normalize-title
                           (pdf-text--heading-title head))
                          head)))
                headings)))

;;; Footnote markers

(defvar pdf-text-footnote-size 0.9
  "Glyph height, in body heights, under which a block can be a footnote.
Footnote blocks are set at 0.7-0.8 of the body size; body text never
dips under 1, so the gap between the two is real.")

(defvar pdf-text-footnote-size-slack 0.95
  "The footnote size gate for a block that opens with a marker.
A paper can set its notes a bare hair over `pdf-text-footnote-size'
of the modal body height - the alignment paper's notes reach 0.906 of
their own page's body - and the marker at the page's foot is the
stronger signal, so it buys the gate its slack.  Body text never dips
under 1; a block without a marker gets no slack at all.")

(defvar pdf-text-footnote-foot 0.6
  "How far down the page an unmarked block must sit to read as a note.
An author note or an imprint line lives at the page's foot; a page
set in small type from the top - a notes chapter, a copyright page -
is body text that happens to be small, and dimming it would paint
the whole page.")

(defconst pdf-text-footnote-symbols
  '((?* . "star") (?† . "dagger") (?‡ . "ddagger") (?§ . "sect") (?¶ . "par"))
  "Footnote marker symbols and the org label names they take.
An org footnote label is word characters, hyphens and underscores
only, so the symbol itself cannot serve.")

(defun pdf-text--footnote-open (text)
  "The footnote marker TEXT opens with, as (TOKEN . BODY-START), or nil.
A footnote block leads with its own marker: a symbol or a small
number, flat (`*A word', `1. Of course') or superscripted the way the
body writes it (`^{*}').  A flat number needs its closing period or
parenthesis - a bare one opens a table-of-contents entry or a merged
folio line as often as a footnote - and every form must lead into
text, not stand alone.  BODY-START is where the note's own words
begin."
  (let ((case-fold-search nil))
    (cond
     ((string-match "\\`\\^{\\([*†‡§¶]\\|[0-9]\\{1,2\\}\\)}[ \t]*\\([^ \t\n]\\)"
                    text)
      (cons (match-string 1 text) (match-beginning 2)))
     ((string-match "\\`\\([*†‡§¶]\\)[ \t]*\\([[:alpha:]“”\"‘’']\\)" text)
      (cons (match-string 1 text) (match-beginning 2)))
     ((string-match "\\`\\([0-9]\\{1,2\\}\\)[.)][ \t]+\\([^ \t\n]\\)" text)
      (cons (match-string 1 text) (match-beginning 2))))))

(defun pdf-text--footnote-label (page token)
  "The org label for the footnote TOKEN marks on PAGE.
A numeric marker keeps its numeral, because the digit is the page's
own text and the label is what gives it back to the reader; a symbol
takes its name from `pdf-text-footnote-symbols'.  Pages number their
markers independently, so the label carries the page to stay unique
across the book."
  (format "%d-%s" page
          (or (cdr (assq (aref token 0) pdf-text-footnote-symbols)) token)))

(defun pdf-text--footnote-marker-re (token)
  "Regexp matching TOKEN as a rendered footnote reference in body text.
The body writes the marker as generated superscript markup - literal
^{ pairs off the page carry a zero-width space and can never match.
A numeric marker must not hang off a digit: 10^{3} is an exponent,
not a third footnote.  Group 1 holds what precedes the marker, so a
replacement can keep it."
  (concat "\\(" (and (string-match-p "\\`[0-9]" token) "[^0-9]") "\\)"
          (regexp-quote (concat "^{" token "}"))))

(defun pdf-text--footnote-flat-re (token)
  "Regexp for TOKEN cited as the flat symbol the text layer wrote, or nil.
A title can carry its footnote symbol in-record with no size contrast
for the glyph pass to dissect - the alignment paper's \"Dynamic
Programming*\" - so a symbol also cites as its word-attached literal
closing the text.  Group 1 holds what precedes it, like
`pdf-text--footnote-marker-re'; the open brace is excluded so a
generated ^{...} never half-matches.  A numeric token has no flat
form: a trailing digit is a quantity far more often than a citation."
  (unless (string-match-p "\\`[0-9]" token)
    (concat "\\([^ \t\n{]\\)" (regexp-quote token) "\\'")))

;;; Cleanups over line records

(defun pdf-text--normalize-line (line)
  "LINE with digit runs collapsed to #, for header/footer matching.
\"INTRODUCTION │ 7\" and \"INTRODUCTION │ 9\" must count as one form."
  (string-trim (replace-regexp-in-string "[0-9]+" "#" line)))

(defun pdf-text--edge-lines (lines)
  "First and last non-blank line of LINES, once each.
The fallback for a page with no geometry: running heads open and close
the plain text stream."
  (let ((nb (cl-remove-if (lambda (line)
                            (string-blank-p (pdf-text-line-text line)))
                          lines)))
    (cl-remove-duplicates (delq nil (list (car nb) (car (last nb)))))))

(defvar pdf-text-recurring-min-count 3
  "Occurrences in the margin band before a line counts as a running head.")

(defvar pdf-text-extra-recurring-forms nil
  "Recurring margin forms known from outside the pages being rendered.
The document decides what recurs - a running head repeats across a
book, not across a window of it - and `pdf-text-remove-marginal-lines'
counts only the pages it is handed.  A corpus case carries the
digit-normalised forms its own window cannot show often enough.")

(defvar pdf-text-extra-folio-merged 0
  "Folio-merged margin candidates known from outside the rendered pages.
The folio-merged style is a book-wide reading: heads that rotate per
section never recur as forms, so what recurs is the shape, counted
over the whole document.  A window's own count sits under the book's;
the larger of the two is the document's.")



(defvar pdf-text-margin-detachment 1.8
  "Leadings between a margin line and the body before it stands apart.
Dense layouts set the running head under two leadings off the body -
the Spanish grammar runs at 1.9 - while a footnote block's own lines
sit a single leading apart, which is what keeps the block out however
low this goes.")

(defvar pdf-text-heading-height 1.15
  "Glyph height, in body heights, at which a line reads as a heading.")

(defun pdf-text--folio-merged-p (text)
  "Whether TEXT reads as a running head with its folio set into the line.
Poppler joins the page number to the head when the gap between them
is small, and the result opens or closes on a bare number the words
do not own.  Such a line runs as wide as its title and the narrowness
test never sees it."
  (let ((trimmed (string-trim text)))
    (or (string-match-p "\\`[0-9]\\{1,4\\}[ \t]" trimmed)
        (string-match-p "[ \t][0-9]\\{1,4\\}\\'" trimmed))))

(defun pdf-text--neighbour-gap (vec index step)
  "Baseline distance from line INDEX of VEC to its neighbour along STEP.
Lines sharing a baseline are one visual line that poppler split at a
wide gap - a page number and its running head - so the scan walks past
them.  Nil at the end of the page."
  (let* ((line (aref vec index))
         (base (pdf-text-line-base line))
         (i (+ index step))
         found)
    (while (and base (<= 0 i) (< i (length vec)) (not found))
      (when-let* ((other (pdf-text-line-base (aref vec i)))
                  ((< 0.0001 (abs (- other base)))))
        (setq found (abs (- other base))))
      (setq i (+ i step)))
    found))

(defun pdf-text--margin-candidates (lines profile)
  "LINES of one page that sit apart in the top or bottom margin band.
An alist of (LINE . NARROW-P).  A candidate sits inside the band, cut
off from the body by `pdf-text-margin-detachment' of PROFILE's
leadings, and is either narrow or a folio-merged head running at full
measure - a footnote block fails the band and detachment tests, which
is what keeps it out of the running-head count.  NARROW-P carries
which width test passed: the narrow forms are what the
drop-anywhere recurrence may trust (a two-up scan embeds them
mid-text), the folio-merged ones only ever go from the band itself."
  (let ((leading (plist-get profile :leading))
        (left (plist-get profile :left))
        (right (plist-get profile :right)))
    (if (not (and leading left right (cl-some #'pdf-text-line-base lines)))
        (mapcar (lambda (line) (cons line t)) (pdf-text--edge-lines lines))
      (let ((vec (vconcat lines))
            (width (- right left))
            candidates)
        (dotimes (i (length vec))
          (let* ((line (aref vec i))
                 (base (pdf-text-line-base line))
                 (before (pdf-text--neighbour-gap vec i -1))
                 (after (pdf-text--neighbour-gap vec i 1))
                 (gap (* pdf-text-margin-detachment leading))
                 ;; a record with no ink - a tab-only line - has nothing
                 ;; to measure and can never sit in a band
                 (narrow (and (pdf-text-line-x0 line) (pdf-text-line-x1 line)
                              (< (- (pdf-text-line-x1 line) (pdf-text-line-x0 line))
                                 (* 0.6 width)))))
            (when (and base
                       ;; a lane region proved its rows aligned three
                       ;; deep; page furniture never does
                       (not (pdf-text-line-claimed line))
                       (or (< base pdf-text-margin-band)
                           (< (- 1.0 pdf-text-margin-band) base))
                       (or narrow
                           (pdf-text--folio-merged-p (pdf-text-line-text line)))
                       (or (null before) (< gap before))
                       (or (null after) (< gap after)))
              (push (cons line narrow) candidates))))
        (nreverse candidates)))))

(defun pdf-text--recurring-facts (candidates)
  "What CANDIDATES - per-page `pdf-text--margin-candidates' - recur as.
A cons of (FORMS . FOLIO-MERGED): the digit-normalised forms whose
narrow candidates reach `pdf-text-recurring-min-count', and how many
candidates read as folio-merged heads.  Raw counts over the pages
given; the seeding by `pdf-text-extra-recurring-forms' and
`pdf-text-extra-folio-merged' is the caller's."
  (let ((counts (make-hash-table :test #'equal))
        (folio-merged 0)
        recurring)
    (dolist (page-candidates candidates)
      (dolist (candidate page-candidates)
        (let ((line (car candidate)))
          (when (and line (not (string-blank-p (pdf-text-line-text line))))
            (when (pdf-text--folio-merged-p (pdf-text-line-text line))
              (cl-incf folio-merged))
            ;; a running head carries words and a folio digits; a lone
            ;; brace or bar at a page edge recurs like a head in a
            ;; listings book but is the page's own text, and must not
            ;; arm the drop-anywhere rule against every copy of itself
            (when (and (cdr candidate)
                       (string-match-p "[[:alnum:]]"
                                       (pdf-text-line-text line)))
              (cl-incf (gethash (pdf-text--normalize-line (pdf-text-line-text line))
                                counts 0)))))))
    (maphash (lambda (form n)
               (when (<= pdf-text-recurring-min-count n) (push form recurring)))
             counts)
    (cons recurring folio-merged)))

(defun pdf-text--page-marker-p (text)
  "Whether TEXT is a bare page marker: a number, a numeral, or a rule."
  (let ((trimmed (string-trim text)))
    (and (not (string-blank-p trimmed))
         (string-match-p
          "\\`\\(?:[Pp]age[ \t]*\\)?\\(?:[0-9]+\\|[ivxlcdmIVXLCDM]+\\|[|·—–-]+\\)\\'"
          trimmed))))

(defun pdf-text-remove-marginal-lines (pages profiles &optional headings)
  "PAGES without running heads, footers, and page numbers.
PROFILES holds each page's own layout profile.
A margin line goes when its digit-normalised form recurs across pages,
when it is nothing but a page marker, or when it shares a baseline
with one - the folio and the running head are set as one line, which
poppler splits at the gap between them.  Recurring forms are then
dropped wherever they appear: a two-up scan embeds whole book pages,
running heads included, in the middle of the text.

HEADINGS carries one entry per page: the heading lines the outline
puts on it.  A book that runs its section title in the page head
makes that title a recurring form, and the section's own heading line
is then dropped along with the head - so a line down in the body that
the outline names is spared.  The head itself sits in the margin band
and goes as it should.

A footnote is spared the same way: a line in the bottom band, set
smaller than the body, opening with a footnote marker and leading
into words, is the page's own text however often its digit-normalised
form recurs and whatever baseline the folio beside it holds.  And a
worded line set over `pdf-text-heading-height' body heights is spared
wherever it sits, named or not: a chapter title opening its page at
the top, a paper's own title over its abstract - the running heads
that echo them are set at the body size or under it, so display type
is never furniture.  Furniture set large stays furniture: a page
marker at any size, and a folio or a unit digit carrying no word at
all.

Only the narrow candidates feed the drop-anywhere recurrence - they
are what a two-up scan embeds mid-text.  A folio-merged head cannot
lean on recurrence at all: a book that titles its heads by section
rotates them before any form recurs.  What recurs is the style - a
book showing `pdf-text-recurring-min-count' folio-merged candidates
anywhere runs its heads that way, and then every folio-merged
candidate goes from the band, display type excepted.

Both readings are document-wide, and PAGES may be a window of the
document: `pdf-text-extra-recurring-forms' and
`pdf-text-extra-folio-merged' carry what the surrounding pages
established, the way `pdf-text-extra-vocabulary' does for hyphens."
  (let* ((tolerance (* 0.5 (or (plist-get (car profiles) :leading) 0.01)))
         (candidates (cl-loop for lines in pages
                              for profile in profiles
                              collect (pdf-text--margin-candidates lines profile)))
         (facts (pdf-text--recurring-facts candidates))
         (recurring (cl-union (car facts) pdf-text-extra-recurring-forms
                              :test #'equal))
         (folio-merged (max (cdr facts) pdf-text-extra-folio-merged)))
    (cl-loop
     for lines in pages
     for marginal in candidates
     for profile in profiles
     for heads = headings then (cdr heads)
     collect
     (let ((folios (delq nil
                         (mapcar (lambda (candidate)
                                   (let ((line (car candidate)))
                                     (and line
                                          (pdf-text--page-marker-p (pdf-text-line-text line))
                                          (pdf-text-line-base line))))
                                 marginal)))
           (body (plist-get profile :height))
           (titles (mapcar #'car (pdf-text--heading-alist (car heads)))))
       (cl-remove-if
        (lambda (line)
          (let* ((text (pdf-text-line-text line))
                 (base (pdf-text-line-base line))
                 (height (pdf-text-line-height line))
                 (in-margin (assq line marginal))
                 (tall (and body height
                            (< (* pdf-text-heading-height body) height)))
                 ;; the title may carry its chapter number ("10 GETTING
                 ;; THE LEAD OUT"), which the outline entry does not
                 (titled (or (member (pdf-text--normalize-title text) titles)
                             (member (pdf-text--title-key text) titles)))
                 ;; display type is the page's own: a book sets its
                 ;; running head at the body size or under it, never
                 ;; over it, so a line set this large is the title the
                 ;; head echoes - the paper's own, which the outline
                 ;; never names - and not a copy of it.  A title is
                 ;; made of words; a workbook's giant unit digit and a
                 ;; folio dressed in rules are display type too
                 (display (and tall
                               (string-match-p "[[:alpha:]]" text)
                               (not (pdf-text--page-marker-p text))))
                 ;; an in-band named line is the chapter's own opener
                 ;; when it stands alone in the book; the recurring
                 ;; copies are the running heads that echo it (DSB),
                 ;; and those still go
                 (named (or display
                            (and titled
                                 (or (not in-margin)
                                     (not (member (pdf-text--normalize-line text)
                                                  recurring))))))
                 (footnote (and base height body
                                (< (- 1.0 pdf-text-margin-band) base)
                                (< height (* pdf-text-footnote-size body))
                                (pdf-text--footnote-open text))))
            (and (not (string-blank-p text))
                 (not named)
                 (not footnote)
                 (or (member (pdf-text--normalize-line text) recurring)
                     (and in-margin
                          (or (pdf-text--page-marker-p text)
                              (and (<= pdf-text-recurring-min-count folio-merged)
                                   (not tall)
                                   (pdf-text--folio-merged-p text))
                              (and base
                                   (cl-some (lambda (folio)
                                              (< (abs (- base folio)) tolerance))
                                            folios))))))))
        lines)))))

(defun pdf-text--collapse-doubled (line)
  "Collapse LINE when it reads as the same string twice.
Slide-style titles painted twice for a shadow effect reach gettext as
\"PATTERNS OF CONFLICT PATTERNS OF CONFLICT\"; a body line that is one
string doubled around a single space is that artifact, not prose."
  (let* ((trimmed (string-trim line))
         (len (length trimmed))
         (mid (/ len 2)))
    (if (and (< 2 len)
             (cl-oddp len)
             (eq ?\s (aref trimmed mid))
             (equal (substring trimmed 0 mid) (substring trimmed (1+ mid))))
        (substring trimmed 0 mid)
      line)))

(defun pdf-text--echo-band-p (line piece)
  "Whether PIECE sits where a shadow echo of LINE would: on its line.
The second paint lands a point lower, never a leading down, so a
piece with a baseline of its own must hold it near LINE's.  Records
without geometry cannot argue and pass."
  (let ((base (pdf-text-line-base line))
        (other (pdf-text-line-base piece))
        (height (pdf-text-line-height line)))
    (or (null base) (null other)
        (< (abs (- other base)) (* 0.5 (or height 0.02))))))

(defun pdf-text--dedup-adjacent (lines)
  "LINES with runs of identical non-blank neighbours collapsed to one.
The shadow-draw artifact: the second paint lands a point lower, so
gettext emits the same title on two adjacent lines.  Two records whose
baselines sit a real line step apart are not an echo - a column of an
aligned array repeats its operator on every row - so geometry, where
a line carries it, has the veto."
  (let (out)
    (dolist (line lines (nreverse out))
      (unless (and out
                   (not (string-blank-p (pdf-text-line-text line)))
                   (equal (string-trim (pdf-text-line-text line))
                          (string-trim (pdf-text-line-text (car out))))
                   (pdf-text--echo-band-p (car out) line))
        (push line out)))))

(defun pdf-text--drop-split-echoes (lines)
  "LINES without runs that only repeat the preceding line in pieces.
The shadow paint's second copy can also split across lines: after
\"PATTERNS OF CONFLICT\" come \"PATTERNS OF\" and \"CONFLICT\".  When
the space-join of the following lines equals the previous line, they
are that echo, not text.  A blank line ends the candidate run, and so
does a piece set a whole line step down the page - the operator column
of an aligned array repeats its glyph on every row, and rows are not
echoes of one another."
  (let (out)
    (while lines
      (let* ((line (pop lines))
             (trimmed (string-trim (pdf-text-line-text line))))
        (push line out)
        (unless (string-blank-p trimmed)
          (let ((acc "") (rest lines) (n 0) matched)
            (while (and rest
                        (not matched)
                        (not (string-blank-p (pdf-text-line-text (car rest))))
                        (pdf-text--echo-band-p line (car rest))
                        (< (length acc) (length trimmed)))
              (setq acc (string-trim
                         (concat acc " " (string-trim (pdf-text-line-text (car rest)))))
                    n (1+ n)
                    rest (cdr rest))
              (when (equal acc trimmed) (setq matched t)))
            (when matched (setq lines (nthcdr n lines)))))))
    (nreverse out)))

(defun pdf-text-join-small-caps (line)
  "LINE with small-caps extraction gaps closed.
A word typeset in small caps reaches gettext as its full-size initial,
a spurious space from the font-size change, then the tail: \"S ECOND
E DITION\".  Word gaps look identical, so joining takes line-level
evidence: no lowercase anywhere (small caps extract as capitals), and
the initial-gap pattern either twice or filling the whole line.  A
lone \"A\" or \"I\" pair stays: those are the two real one-letter
words (\"A DISCOURSE\"), while a heading like \"P REFACE\" cannot be
anything but the artifact."
  (let* ((case-fold-search nil)         ; [A-Z] must not match lowercase
         (trimmed (string-trim line))
         (pairs (let ((n 0) (start 0))
                  (while (string-match "\\b[A-Z] [A-Z]\\{2,\\}\\b" trimmed start)
                    (setq n (1+ n) start (match-end 0)))
                  n)))
    (if (and (not (string-match-p "[a-z]" trimmed))
             (or (<= 2 pairs)
                 (string-match-p "\\`[B-HJ-Z] [A-Z]\\{2,\\}\\'" trimmed)))
        (replace-regexp-in-string "\\b\\([A-Z]\\) \\([A-Z]+\\)\\b" "\\1\\2" line)
      line)))

(defun pdf-text-clean-pages (pages)
  "PAGES of line records with paint artifacts and small-caps gaps gone."
  (mapcar (lambda (lines)
            (let ((kept (pdf-text--drop-split-echoes
                         (pdf-text--dedup-adjacent lines))))
              (dolist (line kept kept)
                (setf (pdf-text-line-text line)
                      (pdf-text-join-small-caps (pdf-text-line-text line))))))
          pages))

;;; Joining lines into blocks

(defconst pdf-text-wrap-hyphen-re "[[:alnum:]][-\u2010\u2011\u00AD]\\'"
  "A word-attached hyphen at a line's end: the renderer split a word.
Books use the ASCII hyphen, the typographic one, and the soft hyphen
interchangeably for this.")

(defconst pdf-text-closed-dash-re "[[:alnum:]][\u2013\u2014]\\'"
  "An en or em dash at a line's end.
English typography sets both closed up against their neighbours, so
the wrap put no space there and the join must not add one - but the
dash itself is text and stays.")

(defun pdf-text--wrap-hyphen-p (text)
  "Whether TEXT ends in a hyphen that a line wrap put there."
  (string-match-p pdf-text-wrap-hyphen-re text))

(defvar pdf-text-extra-vocabulary nil
  "Hyphenated words known from outside the pages being rendered.
The document decides whether a wrap hyphen closes up or stays, and a
window of pages is not the document: a book can hyphenate
\"well-known\" once in chapter one and wrap it in chapter nine.  A
corpus case carries the compounds its own pages cannot show.")

(defvar pdf-text-extra-heading-levels nil
  "Height clusters carried into a render whose pages cannot establish them.
The shape `pdf-text--heading-clusters' returns.  A corpus window
seeds the clusters its book computed, because the rank of a size -
which org level it maps to - depends on every style the book uses,
and seven pages need not show them all.")

(defun pdf-text--hyphenated-words (pages)
  "Words PAGES writes with an internal hyphen, downcased, as a set.
A wrap hyphen is ambiguous - \"well-\" plus \"known\" is a compound,
\"informa-\" plus \"tion\" is one split word - and the document itself
settles it: a compound it hyphenates elsewhere keeps its hyphen here."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (word pdf-text-extra-vocabulary)
      (puthash (downcase word) t table))
    (dolist (lines pages table)
      (dolist (line lines)
        (dolist (word (split-string (pdf-text-line-text line) "[ \t]+" t))
          (when (string-match "[[:alnum:]]-[[:alnum:]]" word)
            (puthash (downcase (string-trim word "[^[:alnum:]]+" "[^[:alnum:]]+"))
                     t table)))))))

(defun pdf-text--join-lines (para line &optional vocabulary)
  "Append LINE to PARA, absorbing a wrap hyphen or a drop cap.
A word-attached trailing hyphen means the wrap split mid-word: a
lowercase continuation is the split word's tail, so the hyphen goes -
unless VOCABULARY, the document's own hyphenated words, shows the two
halves belong to a compound.  Any other continuation is a compound
broken at its own hyphen and keeps it; neither wants a space.  An en
or em dash closes up the same way but survives, being text rather
than a wrap artifact.  A dangling hyphen is ordinary text.  A PARA
that is one capital letter
is a drop cap - the oversized initial extracts as its own line - and
rejoins its word without a space."
  (let ((case-fold-search nil))         ; [[:lower:]] must not match S
    (cond
     ((string-match-p "\\`[[:upper:]]\\'" para)
      (concat para line))
     ((string-match-p pdf-text-closed-dash-re para)
      (concat para line))
     ((not (pdf-text--wrap-hyphen-p para))
      (concat para " " line))
     ((not (string-match-p "\\`[[:lower:]]" line))
      (concat para line))
     ((and vocabulary
           (gethash (downcase
                     (concat (car (last (split-string (substring para 0 -1) "[ \t]+" t)))
                             "-"
                             (string-trim (car (split-string line "[ \t]+" t))
                                          "" "[^[:alnum:]]+")))
                    vocabulary))
      (concat (substring para 0 -1) "-" line))
     (t (concat (substring para 0 -1) line)))))

;;; Blocks

(cl-defstruct (pdf-text-block (:constructor pdf-text-block-create)
                              (:copier nil))
  "A run of lines that belong together: a paragraph, a list item, a listing."
  kind                                  ; para, item, mono, table, fixed, blank
  reason                                ; why the block before it ended
  lines                                 ; in reverse while it grows
  left body-x marker drop-cap)

(defvar pdf-text-gap-factor 1.35
  "Baseline step, in modal leadings, that opens a new block.
Renderers pad between paragraphs and around headings; within a
paragraph the step is the leading itself.")

(defvar pdf-text-blank-factor 1.25
  "Baseline step, in modal leadings, that renders as a blank line.
Below it the page put no air between the two blocks either, so a table
of contents or a stanza stays tight.")

(defvar pdf-text-size-tolerance 0.2
  "Relative glyph-height change that separates two blocks.")

(defvar pdf-text-full-line-fraction 0.7
  "Fraction of the page's widest line at which a line counts as full.
The fallback for a page with no geometry, where glyph edges cannot say
whether the renderer wrapped the line or the paragraph ended.")

(defun pdf-text--page-width (lines)
  "Widest trimmed line length in LINES, the page's wrap-column estimate.
Preformatted lines are layout, not prose, and stay out of the
estimate."
  (apply #'max 0
         (mapcar (lambda (line) (length (string-trim (pdf-text-line-text line))))
                 (cl-remove-if (lambda (line)
                                 (pdf-text--preformatted-p (pdf-text-line-text line)))
                               lines))))

(defun pdf-text--mono-p (line)
  "Whether LINE was set in a monospaced face."
  (eq 'mono (pdf-text-line-kind line)))

(defun pdf-text--drop-cap-p (line profile)
  "Whether LINE is an initial standing alone, oversized against PROFILE's body."
  (let ((case-fold-search nil)
        (height (pdf-text-line-height line))
        (body (plist-get profile :height)))
    (and (string-match-p "\\`[[:upper:]]\\'" (string-trim (pdf-text-line-text line)))
         (or (null height) (null body) (< (* 1.4 body) height)))))

(defun pdf-text--gap-break-p (line prev profile)
  "Whether the step from PREV's baseline to LINE's opens a new block."
  (when-let* ((leading (plist-get profile :leading))
              (base (pdf-text-line-base line))
              (previous (pdf-text-line-base prev))
              ((< 0 leading)))
    (let ((step (- base previous)))
      (or (< step (- (* 0.5 leading)))  ; back up the page: another column
          (< (* pdf-text-gap-factor leading) step)))))

(defun pdf-text--size-break-p (line prev profile)
  "Whether LINE and PREV differ enough in glyph size to be separate blocks.
The difference is measured against PROFILE's body height."
  (when-let* ((body (plist-get profile :height))
              (this (pdf-text-line-height line))
              (that (pdf-text-line-height prev))
              ((< 0 body)))
    (< pdf-text-size-tolerance (/ (abs (- this that)) body))))

(defun pdf-text--supports-break-p (prev line)
  "Whether the words meeting at PREV and LINE read as a break in the text.
Sentence punctuation closing one line and a capital, a digit or an
opening quote starting the next is what a paragraph boundary looks
like in words.  Flush-right and centred runs need the confirmation,
because every line of theirs falls short of a margin without ending
anything."
  (let ((case-fold-search nil))
    (and (string-match-p "[.!?:;)”’\"']\\'" (string-trim (pdf-text-line-text prev)))
         (string-match-p "\\`[[:upper:][:digit:]“\"'(]"
                         (string-trim (pdf-text-line-text line))))))

(defun pdf-text--ends-short-p (line prev right profile page-width)
  "Whether PREV ended its block instead of wrapping into LINE.
The renderer breaks a line when the next word no longer fits, so a
line that left room for LINE's first word ended for a reason of its
own: the paragraph, the list item or the entry stopped there.  RIGHT
is the margin they wrap against.  PAGE-WIDTH carries the
character-count fallback for a page with no geometry."
  (let ((space (or (plist-get profile :space) 0))
        (x1 (pdf-text-line-x1 prev))
        (width (pdf-text-line-first-width line)))
    (if (and right x1 width)
        (< (+ x1 space width) right)
      (< (length (string-trim (pdf-text-line-text prev)))
         (* pdf-text-full-line-fraction page-width)))))

(defun pdf-text--indent-break-p (line block profile)
  "Whether LINE starts in from BLOCK's body margin: a new paragraph.
The step is measured in PROFILE's space widths, and the rule is silent
over a right-aligned or centred run, where the left edge moves for
reasons of typesetting."
  (let ((space (or (plist-get profile :space) 0))
        (body (pdf-text-block-body-x block))
        (x0 (pdf-text-line-x0 line)))
    (cond
     ((pdf-text-line-align line) nil)
     ((and body x0) (< (+ body space) x0))
     (t (and (null x0) (string-match-p "\\`[ \t]" (pdf-text-line-text line)))))))

(defun pdf-text--dedent-break-p (line block profile)
  "Whether LINE falls back left of BLOCK's body margin, ending it.
A quotation, a listing or a list item runs inset; prose resuming at
PROFILE's column margin is no longer part of it.  A drop cap is the
exception: it holds the first lines of its own paragraph inset, and
the paragraph really does go on once the text clears the initial."
  (let* ((space (or (plist-get profile :space) 0))
         (column (plist-get profile :left))
         (body (pdf-text-block-body-x block))
         (x0 (pdf-text-line-x0 line)))
    (and body x0 (< x0 (- body space))
         (null (pdf-text-line-align line))
         (not (and (pdf-text-block-drop-cap block)
                   column
                   (< (abs (- x0 column)) (* 2 space)))))))

(defun pdf-text--item-break-p (line prev block profile page-width)
  "Whether LINE opens a list item.
The marker alone is not enough - a wrapped clause can start with a
dash - so LINE must also stand apart from PREV: a step in from
PROFILE's margin, extra air above, a predecessor that ended short of
PAGE-WIDTH's measure, or BLOCK already running as a list."
  (let* ((space (or (plist-get profile :space) 0))
         (x0 (pdf-text-line-x0 line))
         (left (plist-get profile :left))
         (indented (and x0 left (< (+ left space) x0))))
    (and (pdf-text--list-marker (pdf-text-line-text line) (or indented (null x0)))
         (or (eq 'item (pdf-text-block-kind block))
             (and x0 (pdf-text-line-x0 prev)
                  (< space (abs (- x0 (pdf-text-line-x0 prev)))))
             (pdf-text--gap-break-p line prev profile)
             (pdf-text--ends-short-p line prev (plist-get profile :right)
                                     profile page-width)))))

(defun pdf-text--break-reason (line block profile page-width)
  "Why LINE cannot continue BLOCK, or nil when it can.
Ordered by strength of evidence: a wrap hyphen or a drop cap forces
the join, a change of face or a list marker forces the break, and the
geometry - PROFILE's body measures, PAGE-WIDTH - decides the rest."
  (let ((prev (car (pdf-text-block-lines block)))
        (kind (pdf-text-block-kind block)))
    (cond
     ((string-blank-p (pdf-text-line-text line)) 'blank)
     ((eq 'blank kind) 'text)
     ((not (eq (pdf-text-line-kind line)
               (pcase kind ('table 'row) ((or 'mono 'math) kind) (_ nil))))
      'face)
     ;; a wrapped table row stands two typeset lines tall, so the step
     ;; between row records exceeds any gap factor; consecutive rows
     ;; are one table regardless
     ((eq 'table kind) nil)
     ((memq kind '(mono math))
      (and (pdf-text--gap-break-p line prev profile) 'gap))
     ((eq 'fixed kind)
      (if (pdf-text--preformatted-p (pdf-text-line-text line))
          (and (pdf-text--gap-break-p line prev profile) 'gap)
        'fixed))
     ((pdf-text--preformatted-p (pdf-text-line-text line)) 'fixed)
     ((pdf-text--drop-cap-p prev profile) nil)
     ;; a change of glyph size outranks the hyphen join: a body line
     ;; ending on a wrap hyphen above a smaller-type footnote block is
     ;; two blocks, not a continuation (DSB page 19)
     ((pdf-text--size-break-p line prev profile) 'size)
     ((pdf-text--wrap-hyphen-p (pdf-text-line-text prev)) nil)
     ((pdf-text--item-break-p line prev block profile page-width) 'item)
     ((pdf-text--gap-break-p line prev profile) 'gap)
     ((pdf-text--indent-break-p line block profile) 'indent)
     ((pdf-text--dedent-break-p line block profile) 'dedent))))

(defun pdf-text--block-margin (block profile)
  "Right margin BLOCK's lines wrap against.
A block starting at the column margin wraps against PROFILE's own
right edge.  One starting elsewhere - a margin note, a pull quote, an
indented list item - wraps against its own widest line instead;
measured against the column's, every line of it would read as a
paragraph end.  Knowing this takes the whole block, which is why the
short-line rule runs over grouped blocks rather than line by line."
  (let* ((edges (delq nil (mapcar #'pdf-text-line-x1 (pdf-text-block-lines block))))
         (widest (and edges (apply #'max edges)))
         (left (pdf-text-block-left block))
         (column-left (plist-get profile :left))
         (column-right (plist-get profile :right))
         (space (or (plist-get profile :space) 0)))
    (cond ((null widest) column-right)
          ((null column-right) widest)
          ((or (null left) (null column-left)) column-right)
          ((< (abs (- left column-left)) (* 2 space)) column-right)
          (t widest))))

(defun pdf-text--start-block (line reason profile)
  "A block opened by LINE, which broke off the one before it for REASON.
PROFILE gives the column margin the block's own is measured from."
  (let* ((text (pdf-text-line-text line))
         (space (or (plist-get profile :space) 0))
         (left (pdf-text-line-x0 line))
         (column-left (plist-get profile :left))
         (indented (or (null left)
                       (null column-left)
                       (< (+ column-left space) left)))
         (marker (pdf-text--list-marker text indented))
         (kind (cond ((string-blank-p text) 'blank)
                     ((eq 'row (pdf-text-line-kind line)) 'table)
                     ((eq 'fixed (pdf-text-line-kind line)) 'fixed)
                     ((pdf-text--mono-p line) 'mono)
                     ((eq 'math (pdf-text-line-kind line)) 'math)
                     ((pdf-text--preformatted-p text) 'fixed)
                     (marker 'item)
                     (t 'para))))
    (pdf-text-block-create
     :kind kind
     :reason reason
     :lines (list line)
     :left left
     ;; every block, list item included, finds its body margin on its
     ;; second line: an item's continuation hangs under the marker in
     ;; one book and returns to the column margin in the next
     :marker marker
     :drop-cap (pdf-text--drop-cap-p line profile))))

(defun pdf-text--extend-block (block line)
  "Add LINE to BLOCK, tracking the margins its later lines establish."
  (push line (pdf-text-block-lines block))
  (when-let* ((x0 (pdf-text-line-x0 line)))
    (unless (pdf-text-block-body-x block)
      (setf (pdf-text-block-body-x block) x0))
    (when (or (null (pdf-text-block-left block))
              (< x0 (pdf-text-block-left block)))
      (setf (pdf-text-block-left block) x0)))
  block)

(defun pdf-text--group-lines (lines profile page-width)
  "LINES of one page grouped into blocks by every rule but the short line.
PROFILE and PAGE-WIDTH are what those rules measure against."
  (let (blocks current)
    (dolist (line lines)
      (let ((reason (and current
                         (pdf-text--break-reason line current profile page-width))))
        (if (and current (not reason))
            (pdf-text--extend-block current line)
          (when current
            (setf (pdf-text-block-lines current)
                  (nreverse (pdf-text-block-lines current)))
            (push current blocks))
          (setq current (pdf-text--start-block line (or reason 'start) profile)))))
    (when current
      (setf (pdf-text-block-lines current)
            (nreverse (pdf-text-block-lines current)))
      (push current blocks))
    (nreverse blocks)))

(defun pdf-text--split-short-lines (block profile page-width)
  "BLOCK split wherever one of its lines ended short of its own margin.
A short line is the end of a paragraph, an entry or an item; the rule
needs the block's measure - PROFILE's column edge only where the block
starts at it, PAGE-WIDTH throughout - so it runs once the grouping
settled it."
  (if (or (memq (pdf-text-block-kind block) '(mono math table fixed blank))
          (< (length (pdf-text-block-lines block)) 2))
      (list block)
    (let* ((right (pdf-text--block-margin block profile))
           ;; lines set flush right or centred fall short of any margin
           ;; by design, so their geometry alone cannot end a paragraph
           (aligned (cl-some #'pdf-text-line-align (pdf-text-block-lines block)))
           (lines (pdf-text-block-lines block))
           parts current)
      (dolist (line lines)
        (if (null current)
            (setq current (pdf-text--start-block
                           line (pdf-text-block-reason block) profile))
          (let ((prev (car (pdf-text-block-lines current))))
            (if (and (not (pdf-text--wrap-hyphen-p (pdf-text-line-text prev)))
                     (not (pdf-text--drop-cap-p prev profile))
                     (pdf-text--ends-short-p line prev right profile page-width)
                     (or (not aligned) (pdf-text--supports-break-p prev line)))
                (progn
                  (setf (pdf-text-block-lines current)
                        (nreverse (pdf-text-block-lines current)))
                  (push current parts)
                  (setq current (pdf-text--start-block line 'short profile)))
              (pdf-text--extend-block current line)))))
      (when current
        (setf (pdf-text-block-lines current)
              (nreverse (pdf-text-block-lines current)))
        (push current parts))
      (nreverse parts))))

(defun pdf-text--blocks (lines profile)
  "LINES of one page grouped into `pdf-text-block' records against PROFILE."
  (let ((page-width (pdf-text--page-width lines))
        (marked (pdf-text--mark-math
                 (pdf-text--mark-alignment (pdf-text--mark-monospace lines) profile)
                 profile)))
    (cl-mapcan (lambda (block)
                 (pdf-text--split-short-lines block profile page-width))
               (pdf-text--group-lines marked profile page-width))))

;;; Rendering blocks back to text

(defun pdf-text--join-block (block vocabulary)
  "BLOCK's lines joined into one paragraph line, de-hyphenated by VOCABULARY."
  (let (para)
    (dolist (line (pdf-text-block-lines block) (or para ""))
      (let ((text (string-trim (pdf-text-line-text line))))
        (setq para (if para
                       (pdf-text--join-lines para text vocabulary)
                     text))))))

(defun pdf-text--render-item (block vocabulary indent)
  "BLOCK rendered as an org list item at INDENT columns, joined by VOCABULARY.
A bullet glyph becomes org's own dash, which costs nothing to read and
buys real list structure; an enumerator is already org syntax and
stays as the document wrote it."
  (let* ((text (pdf-text--join-block block vocabulary))
         (marker (pdf-text-block-marker block))
         (pad (make-string indent ?\s)))
    (if (and marker (string-match-p (concat "\\`" pdf-text-bullet-re) marker))
        (concat pad "- " (string-trim (substring text (length marker))))
      (concat pad text))))

(defun pdf-text--render-mono (block profile left)
  "BLOCK's lines, verbatim, with their own indentation restored.
Listings and display mathematics render this way: one source line per
rendered line, never joined into prose.  LEFT is the margin of the run
this block belongs to, which spans every block the vertical gaps
inside it split it into - measure each block against itself and the
second half of a listing loses its nesting.  The step is the run's own
space width, PROFILE's where its lines carry none."
  (let* ((lines (pdf-text-block-lines block))
         (unit (or (car (delq nil (mapcar #'pdf-text-line-space lines)))
                   (plist-get profile :space)
                   0.005)))
    (mapconcat (lambda (line)
                 (let ((step (if (and left (pdf-text-line-x0 line))
                                 (round (/ (- (pdf-text-line-x0 line) left) unit))
                               0)))
                   (concat "  " (make-string (max 0 step) ?\s)
                           (string-trim (pdf-text-line-text line)))))
               lines "\n")))

(defun pdf-text--inset-p (block profile)
  "Whether BLOCK is a passage set in from PROFILE's column, as a quotation is.
One line in from the margin is a centred heading or an attribution and
reads better flush; the inset only means something over a passage."
  (let ((left (pdf-text-block-left block))
        (column (plist-get profile :left))
        (space (or (plist-get profile :space) 0)))
    (and left column
         (< 1 (length (pdf-text-block-lines block)))
         (< (+ column (* 2 space)) left))))

(defun pdf-text--block-height (block)
  "The glyph height BLOCK is set at, or nil where no line measured one."
  (when-let* ((heights (delq nil (mapcar #'pdf-text-line-height
                                         (pdf-text-block-lines block)))))
    (apply #'max heights)))

(defun pdf-text--sidebar-title-p (block next profile)
  "Whether BLOCK is the title of the boxed passage NEXT opens.
A sidebar is set in from PROFILE's column and in smaller type than its body,
and its first line is its title.  On its own that line reads as a
centred heading and prints flush, which loses the box: the title
starts where the box starts and is set as the box is set, so it is
part of it."
  (let ((space (or (plist-get profile :space) 0))
        (column (plist-get profile :left))
        (body (plist-get profile :height))
        (left (pdf-text-block-left block))
        (other (pdf-text-block-left next))
        (height (pdf-text--block-height block))
        (box (pdf-text--block-height next)))
    (and left other column body height box
         (< (+ column (* 2 space)) left)
         (< (abs (- left other)) space)
         (< height body)
         (< box body)
         (pdf-text--inset-p next profile))))

(defun pdf-text--inset-blocks (blocks profile)
  "The blocks of BLOCKS that render set in from PROFILE's column margin.
A passage of more than one line that runs inset is a quotation or the
body of a boxed sidebar; the sidebar's title line joins it, so the box
reads as the one unit the page sets."
  (let (out)
    (cl-loop for (block next) on blocks
             do (when (or (pdf-text--inset-p block profile)
                          (and next (pdf-text--sidebar-title-p block next profile)))
                  (push block out)))
    (nreverse out)))

(defun pdf-text--item-indent (block stack profile)
  "Indent columns for item BLOCK, and the nesting STACK it leaves behind.
Deeper markers nest, a marker back at an earlier column closes the
levels it left; PROFILE's space width is the step."
  (let ((left (pdf-text-block-left block))
        (space (or (plist-get profile :space) 0)))
    (if (null left)
        (cons 0 stack)
      (while (and stack (< left (- (car stack) space)))
        (pop stack))
      (when (or (null stack) (< (+ (car stack) space) left))
        (push left stack))
      (cons (* 2 (1- (length stack))) stack))))

(defun pdf-text--blank-between-p (block previous profile)
  "Whether a blank line belongs between PREVIOUS and BLOCK.
The page itself decides: blocks the renderer set apart get one, blocks
it stacked at the plain leading - a table of contents, a stanza, a
tight list - stay together."
  (let ((leading (plist-get profile :leading))
        (last (car (last (pdf-text-block-lines previous))))
        (first (car (pdf-text-block-lines block))))
    (cond
     ;; two lane-flow records render verbatim back to back; only the
     ;; page's own air separates the items of a list read lane-wise
     ((and last first
           (eq 'fixed (pdf-text-line-kind last))
           (eq 'fixed (pdf-text-line-kind first))
           leading
           (pdf-text-line-base last) (pdf-text-line-base first))
      (< (* pdf-text-blank-factor leading)
         (- (pdf-text-line-base first) (pdf-text-line-base last))))
     ((memq (pdf-text-block-reason block) '(indent dedent size face fixed gap)) t)
     ((not (and leading last first
                (pdf-text-line-base last) (pdf-text-line-base first)))
      t)
     (t (< (* pdf-text-blank-factor leading)
           (- (pdf-text-line-base first) (pdf-text-line-base last)))))))

(defvar pdf-text-heading-max-words 14
  "Words a block may carry and still read as a heading rather than prose.")

(defun pdf-text--heading-block-p (block profile text)
  "Whether BLOCK, joined up as TEXT, reads as a heading by how it is set.
Bigger type than PROFILE's body, few words, nothing closing the line: what a
section title looks like on a page that spells it differently from the
outline - a display face the extraction reads letter by letter, a
title the page carries with its chapter number."
  (let ((body (plist-get profile :height))
        (heights (delq nil (mapcar #'pdf-text-line-height
                                   (pdf-text-block-lines block)))))
    (and body heights
         (< (* pdf-text-heading-height body) (apply #'max heights))
         (<= (length (split-string text)) pdf-text-heading-max-words)
         (not (string-match-p "[.,;:]\\'" (string-trim text))))))

(defun pdf-text--numbered-head (head number)
  "HEAD with the section NUMBER the page gives it set in front of its title."
  (if (string-match "\\`\\(\\*+\\|#\\+TITLE:\\)[ \t]*" head)
      (concat (match-string 1 head) " " number " " (substring head (match-end 0)))
    head))

(defun pdf-text--title-placement (text pending)
  "The entry of PENDING that the block text TEXT names, and how to head it.
A cons (ENTRY . HEAD), nil when TEXT names none of them.  A page
numbers its sections where the outline names them bare - \"1
Introduction\" for \"Introduction\", \"3.1 Baseline Distance
Measure\" for its child - so a title is looked for behind a section
number too.  The number is the page's own word and papers cite it, so
the heading keeps it."
  (if-let* ((entry (assoc (pdf-text--normalize-title text) pending)))
      (cons entry (cdr entry))
    (when-let* ((number (pdf-text--section-number text))
                (entry (assoc (pdf-text--title-key text) pending)))
      (cons entry (pdf-text--numbered-head (cdr entry) number)))))

(defun pdf-text--assign-headings (blocks profile vocabulary headings)
  "Where each of HEADINGS goes among BLOCKS, as an alist.
Every entry is (BLOCK HEAD REPLACE).  A title names a line, and the
block whose words are that title - behind the section number the page
sets in front of it, if any - is where its section starts: the
heading is that line, so it replaces it.  A title no block spells out
falls back on the way the page is set - the blocks that read as
headings take what is left over, both in the order they run down the
page - and there the heading goes above the block, because the words
differ and the page's own are not the reflow's to drop.  Every named
line is taken before that fallback runs, so a numbered section line
keeps its own title from the display type further up the page.  A
title that finds no block at all stays for
`pdf-text--interleave-outline', which has only the page's start left
to put it at.  PROFILE says how a page sets a heading; VOCABULARY
joins a title split across lines."
  (when-let* ((pending (pdf-text--heading-alist headings)))
    (let ((texts (mapcar (lambda (block)
                           (and (memq (pdf-text-block-kind block) '(para item))
                                (pdf-text--join-block block vocabulary)))
                         blocks))
          assigned)
      (cl-loop for block in blocks
               for text in texts
               do (when-let* ((text)
                              (found (pdf-text--title-placement text pending)))
                    (push (list block (cdr found) t) assigned)
                    (setq pending (delq (car found) pending))))
      (cl-loop for block in blocks
               for text in texts
               while pending
               do (when (and text
                             (not (assq block assigned))
                             (pdf-text--heading-block-p block profile text))
                    (push (list block (cdr (pop pending))) assigned)))
      assigned)))

(defun pdf-text--assign-footnotes (blocks profile vocabulary page)
  "The footnotes among BLOCKS, as (DEFS REFS NOTES), or nil.
DEFS is an alist of (BLOCK LABEL . TEXT): the trailing blocks of the
page that define a footnote, with the label PAGE gives them and their
text cut past the marker.  REFS pairs each marker's regexps with its
label, for the body blocks that cite it.  NOTES is the rest of the
page's foot: trailing smaller-type blocks with no marker or no
citation - an author note, an imprint line - that render as the plain
text they are but carry the note face, when they sit past
`pdf-text-footnote-foot' on a page that has body text above them.  A
footnote takes both halves: a block at the page's foot, set under
`pdf-text-footnote-size' of PROFILE's body height - or under
`pdf-text-footnote-size-slack' when it opens with a marker, the
stronger signal buying the gate its slack - opening with a marker the
body above it cites, as a superscript or as the flat symbol the text
layer wrote.  A block missing either half renders as the ordinary
text it may well be.  VOCABULARY joins the texts.  Two footnotes
sharing one marker on one page would share a label, and org would
read them as one; books rotate their symbols, so the collision stays
theoretical."
  (when-let* ((body (plist-get profile :height)))
    (let* ((vec (vconcat blocks))
           (i (1- (length vec)))
           run)
      ;; the page's foot: the trailing run of smaller-type prose
      (while (and (<= 0 i)
                  (let ((block (aref vec i)))
                    (or (eq 'blank (pdf-text-block-kind block))
                        (and (memq (pdf-text-block-kind block) '(para item))
                             (when-let* ((height (pdf-text--block-height block)))
                               (or (< height (* pdf-text-footnote-size body))
                                   (and (< height (* pdf-text-footnote-size-slack
                                                     body))
                                        (pdf-text--footnote-open
                                         (pdf-text--join-block block
                                                               vocabulary)))))))))
        (unless (eq 'blank (pdf-text-block-kind (aref vec i)))
          (push (aref vec i) run))
        (cl-decf i))
      (when run
        (let ((texts (cl-loop for j from 0 to i
                              for block = (aref vec j)
                              when (memq (pdf-text-block-kind block) '(para item))
                              collect (pdf-text--join-block block vocabulary)))
              defs refs notes)
          (dolist (block run)
            (let* ((text (pdf-text--join-block block vocabulary))
                   (open (and text (pdf-text--footnote-open text)))
                   (res (and open
                             (delq nil
                                   (list (pdf-text--footnote-marker-re (car open))
                                         (pdf-text--footnote-flat-re (car open))))))
                   (cited (and res
                               (cl-some (lambda (re)
                                          (cl-some (lambda (above)
                                                     (string-match-p re above))
                                                   texts))
                                        res))))
              (cond
               (cited
                (let ((label (pdf-text--footnote-label page (car open))))
                  (push (cons block (cons label (substring text (cdr open))))
                        defs)
                  (dolist (re res) (push (cons re label) refs))))
               ((and texts
                     (when-let* ((top (pdf-text-line-top
                                       (car (pdf-text-block-lines block)))))
                       (<= pdf-text-footnote-foot top)))
                (push block notes)))))
          (and (or defs notes)
               (list (nreverse defs) (nreverse refs) (nreverse notes))))))))

(defun pdf-text--cite-footnotes (text notes)
  "TEXT with each marker NOTES names replaced by its reference.
NOTES is `pdf-text--assign-footnotes' output.  The generated ^{...}
form - or the flat symbol the text layer wrote - becomes [fn:LABEL],
attached where the page attached its marker."
  (dolist (ref (cadr notes) text)
    (setq text (replace-regexp-in-string
                (car ref) (concat "\\1[fn:" (cdr ref) "]") text t))))

(defun pdf-text--render-blocks (blocks profile vocabulary &optional headings page
                                       placed drops)
  "BLOCKS as the page's reflowed text, measured by PROFILE, joined by VOCABULARY.
HEADINGS are the org heading lines the outline puts on this page.
`pdf-text--assign-headings' says which block each one belongs at: a
section starts where the page starts it, not where the page it sits
on does.  PAGE is the number this page has in the book, which the
footnote labels carry.  PLACED overrides the outline assignment with
one already decided - the synthesized headings of an outline-less
book - and DROPS names blocks that render as nothing, because a
heading a merged pair makes carries both halves' text already."
  (let ((placed (or placed
                    (pdf-text--assign-headings blocks profile vocabulary headings)))
        (inset (pdf-text--inset-blocks blocks profile))
        (notes (pdf-text--assign-footnotes blocks profile vocabulary (or page 1)))
        out previous stack listing-left)
    (dolist (block (if drops
                       (cl-remove-if (lambda (b) (memq b drops)) blocks)
                     blocks))
      (let* ((kind (pdf-text-block-kind block))
             (placement (cdr (assq block placed)))
             (head (car placement))
             (note (cdr (assq block (car notes))))
             indent)
        (unless (eq 'item kind) (setq stack nil))
        (when (eq 'item kind)
          (let ((nesting (pdf-text--item-indent block stack profile)))
            (setq indent (car nesting) stack (cdr nesting))))
        (if (memq kind '(mono math))
            (unless (eq kind (and previous (pdf-text-block-kind previous)))
              (setq listing-left (pdf-text-block-left block)))
          (setq listing-left nil))
        (unless (eq 'blank kind)
          (when (and previous (pdf-text--blank-between-p block previous profile))
            (push "" out))
          (when (and head (not (cadr placement)))
            (push head out)
            (push "" out))
          (let ((text (or (and (cadr placement) head)
                          ;; a footnote definition renders at column zero -
                          ;; org reads [fn:LABEL] as a definition only there -
                          ;; with the page's own marker cut, the label now
                          ;; carrying what it carried
                          (and note (concat "[fn:" (car note) "] " (cdr note)))
                          (pcase kind
                            ((or 'mono 'math)
                             (pdf-text--render-mono block profile listing-left))
                            ((or 'table 'fixed)
                             (mapconcat (lambda (line)
                                          (string-trim-right
                                           (pdf-text-line-text line)))
                                        (pdf-text-block-lines block) "\n"))
                            ('item (pdf-text--cite-footnotes
                                    (pdf-text--render-item block vocabulary indent)
                                    notes))
                            (_ (let ((text (pdf-text--cite-footnotes
                                            (pdf-text--collapse-doubled
                                             (pdf-text--join-block block
                                                                   vocabulary))
                                            notes)))
                                 (if (memq block inset)
                                     (concat "  " text)
                                   text)))))))
            ;; the recognition rides a text property so the buffer's
            ;; font-lock rule can dim it: a face put here would not
            ;; survive org's refontification, the property does
            (push (if (and (not (and (cadr placement) head))
                           (or note (memq block (caddr notes))))
                      (propertize text 'pdf-text-note t)
                    text)
                  out))
          (setq previous block))))
    ;; a soft hyphen still standing marks a break that did happen - a
    ;; kept compound wrap, a paragraph ending mid-word at the page's
    ;; last line - so the page printed a hyphen there
    (replace-regexp-in-string "\u00AD" "-" (string-join (nreverse out) "\n"))))

;;; Org structure

(defvar pdf-text-org-escape-re
  (rx bos (or (seq (+ "*") " ")
              (seq (* (in " \t")) "#+")
              (seq (* (in " \t")) ":" (+ (in alnum "_@#%-")) ":" (* (in " \t")) eos)))
  "Extracted lines that org would parse as document structure.
Headlines, keyword/block lines, drawer and property lines.")

(defun pdf-text--escape-org-lines (text &optional headings)
  "TEXT with org-structural lines neutralized by a zero-width space.
The buffer derives from `org-mode' only so the interleaved outline
headings fold; a PDF bullet line starting `* ' must not become a real
headline and corrupt that folding.  The invisible prefix keeps the
line visually identical, and a plain-text search still matches it
whole.  HEADINGS are the heading lines the render placed itself: they
are the structure the folding is for, and stay as they are."
  (string-join
   (mapcar (lambda (line)
             (if (and (string-match-p pdf-text-org-escape-re line)
                      (not (member line headings)))
                 (concat "\u200B" line)
               line))
           (split-string text "\n"))
   "\n"))

(defun pdf-text-render-lines (pages &optional headings first synthesize)
  "PAGES of `pdf-text-line' records reflowed into readable text.
One string per page.  Body geometry, running heads and the
hyphenation vocabulary are all read across the whole of PAGES, so a
page never renders on its own: the same page reads differently
depending on what it arrives with.

HEADINGS carries one entry per page of PAGES - the org heading lines
the outline puts on it, from `pdf-text-page-headings'.  A page gets
its headings at the lines they name; only the caller knows which page
of the book each entry of PAGES is, which is why they arrive already
lined up.  FIRST is the book's number for the first of PAGES, 1 when
nil, for the same reason: the footnote labels carry the page number,
and a corpus window rendering pages 15-21 must label them as the book
does.

SYNTHESIZE, for a document with no outline, reads headings out of the
pages themselves: their glyph sizes and section numbers, weighed
document-wide by `pdf-text--synth-assignments'.  Pages with no glyph
geometry at all fall back to the text-only
`pdf-text--synthesize-headings'."
  (let* ((page-lines (pdf-text-reading-order pages))
         (profile (or pdf-text-extra-profile (pdf-text--profile page-lines)))
         (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                           page-lines))
         (page-lines (pdf-text-remove-marginal-lines page-lines profiles headings))
         (vocabulary (pdf-text--hyphenated-words page-lines))
         (pages-blocks (cl-loop for lines in page-lines
                                for page-profile in profiles
                                collect (pdf-text--blocks lines page-profile)))
         (geometry (cl-some (lambda (lines) (cl-some #'pdf-text-line-x0 lines))
                            page-lines))
         (assignments (and synthesize geometry
                           (pdf-text--synth-assignments pages-blocks profiles
                                                        vocabulary)))
         (rendered
          (cl-loop for blocks in pages-blocks
                   for page-profile in profiles
                   for number from (or first 1)
                   for heads = headings then (cdr heads)
                   for assigned = assignments then (cdr assigned)
                   ;; the placement is what the escape pass must know:
                   ;; a heading placed at a numbered section line reads
                   ;; as the page numbers it, which is neither the
                   ;; outline's line nor an extracted one to neutralize
                   for placed = (or (caar assigned)
                                    (pdf-text--assign-headings
                                     blocks page-profile vocabulary (car heads)))
                   collect (pdf-text--escape-org-lines
                            (pdf-text--render-blocks blocks page-profile
                                                     vocabulary (car heads)
                                                     number placed
                                                     (cdar assigned))
                            (append (mapcar #'cadr placed) (car heads))))))
    (if (and synthesize (not geometry))
        (pdf-text--synthesize-headings rendered)
      rendered)))

(defun pdf-text-render-pages (pages &optional layouts headings synthesize)
  "Raw PAGES reflowed into readable text, one string per page.
PAGES are `pdf-info-gettext' strings and LAYOUTS the matching
`pdf-info-charlayout' output.  The layout is what carries paragraph
structure - indents, line fullness, the air between baselines - so a
page without one falls back to character heuristics.  HEADINGS and
SYNTHESIZE are what `pdf-text-render-lines' takes."
  (pdf-text-render-lines
   (cl-loop for text in pages
            for rest = layouts then (cdr rest)
            collect (pdf-text--page-lines text (car rest)))
   headings nil synthesize))

(defun pdf-text--outline-heads (outline)
  "OUTLINE as org heading lines, keyed by the page each one names.
OUTLINE is `pdf-info-outline' output: alists with depth, title, and -
for goto-dest entries - page.  Entries without a usable page (URI
links, unresolved destinations reported as page 0) or without a title
are dropped.  A lone top-level entry is the book's own title, not a
chapter - as a headline it would fold the entire book into one line -
so it renders as a #+TITLE keyword and every deeper entry promotes to
close the gap."
  (let* ((usable (cl-remove-if-not
                  (lambda (entry)
                    (let-alist entry
                      (and (integerp .page) (<= 1 .page)
                           (stringp .title) (not (string-blank-p .title)))))
                  outline))
         (depth-of (lambda (entry) (max 1 (or (alist-get 'depth entry) 1))))
         (min-depth (and usable (apply #'min (mapcar depth-of usable))))
         (root (let ((top (cl-remove-if-not
                           (lambda (e) (eql min-depth (funcall depth-of e)))
                           usable)))
                 (and (eql 1 (length top)) (car top))))
         (heads (make-hash-table)))
    (dolist (entry usable)
      (let-alist entry
        (push (cond
               ((eq entry root) (concat "#+TITLE: " (string-trim .title)))
               (root (concat (make-string (max 1 (- (funcall depth-of entry)
                                                    min-depth))
                                          ?*)
                             " " (string-trim .title)))
               (t (concat (make-string (funcall depth-of entry) ?*)
                          " " (string-trim .title))))
              (gethash .page heads))))
    (dolist (page (hash-table-keys heads) heads)
      (puthash page (nreverse (gethash page heads)) heads))))

(defun pdf-text-page-headings (outline first count)
  "The heading lines OUTLINE puts on COUNT pages starting at page FIRST.
One entry per page, which is how `pdf-text-render-lines' takes them:
the reflow places a heading at the line naming it, and only the caller
knows which page of the book each page it renders is."
  (let ((heads (pdf-text--outline-heads outline)))
    (cl-loop for page from first below (+ first count)
             collect (gethash page heads))))

(defun pdf-text--head-key (line)
  "The title LINE carries as an org heading, normalised, or nil.
A rendered heading line may carry the section number the page set in
front of its title where the outline entry carries the title bare, so
both sides reduce to the same key.  An extracted line that only looks
like a headline carries a zero-width space before its stars and
answers nil, which is what the escape pass is for."
  (and (string-match-p "\\`\\(?:\\*+ \\|#\\+TITLE:\\)" line)
       (pdf-text--title-key (pdf-text--heading-title line))))

(defun pdf-text--interleave-page (page heads)
  "PAGE with every one of HEADS it does not already carry.
The reflow places a heading at the line the outline names, when the
page has that line; what is left over - a title no line of the page
reproduces - goes at the page's start, which is the only thing left
to say about where its section begins.

A leftover the outline opens after a heading the page does carry
cannot go there: the page's own text above that heading belongs to
it, and stacking a later section over it hands that text to the
wrong title, out of the order the reader folds.  Such a leftover goes
as late as its place in the outline allows - just above the next
heading the page carries, or at the page's end - so the section that
was found keeps its text and the one that was not opens after it."
  (if (null heads)
      page
    (let* ((lines (split-string page "\n"))
           (keys (mapcar #'pdf-text--head-key lines))
           (placed (mapcar (lambda (head)
                             (cl-position (pdf-text--head-key head) keys
                                          :test #'equal))
                           heads))
           leftovers)
      (cl-loop for head in heads
               for i from 0
               unless (nth i placed)
               do (push (cons (cond ((not (cl-some #'identity (seq-take placed i)))
                                     0)
                                    ((cl-some #'identity (seq-drop placed (1+ i))))
                                    (t (length lines)))
                              head)
                        leftovers))
      (if (null leftovers)
          page
        (setq leftovers (nreverse leftovers))
        (string-join
         (cl-loop for i from 0 to (length lines)
                  nconc (append (cl-loop for (at . head) in leftovers
                                         when (eql i at) collect head)
                                (and (< i (length lines)) (list (nth i lines)))))
         "\n")))))

(defun pdf-text--interleave-outline (pages outline)
  "PAGES with every OUTLINE heading its page does not already carry.
`pdf-text--interleave-page' places the leftovers of one page.  A nil
OUTLINE returns PAGES unchanged: PDFs without an outline degrade to
the flat view."
  (let ((heads (pdf-text--outline-heads outline))
        (n 0))
    (mapcar (lambda (page)
              (cl-incf n)
              (pdf-text--interleave-page page (gethash n heads)))
            pages)))

(defvar pdf-text-synth-support 5
  "Distinct pages a glyph size must head before it reads as a heading style.
A style the book uses - chapter titles, section heads - recurs across
it: thirteen chapters, twenty sections.  The display type of a cover
spread or one paper's private subsection style reaches three or four
pages, and a junk cluster costs more than it earns - every real
heading below it drops a level.")

(defvar pdf-text-synth-levels 4
  "Org levels the size clusters of an outline-less book may occupy.")

(defvar pdf-text-synth-number-min 0.98
  "Glyph height, in body heights, under which a numbered line is not a heading.
A real numbered section heading is never set smaller than the body it
heads; a cross-reference in a running head or a footnote is.")

(defun pdf-text--dotted-number-level (text)
  "Org level for TEXT opening with a dotted section number, nil otherwise.
\"2.2 Arguments\" carries one dot and heads a level-2 section; a
single number is not enough - exercises, footnotes and bibliography
entries open with one, and section headings that carry no dots at all
are the size rules' to find.  A trailing page number reads as a table
of contents entry, not a heading."
  (let ((case-fold-search nil))
    (and (string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)+\\)\\.? +[[:upper:]]" text)
         (not (string-match-p "[0-9]\\'" text))
         (1+ (cl-count ?. (match-string 1 text))))))

(defun pdf-text--synth-banded-p (block)
  "Whether BLOCK opens inside the top or bottom margin band.
A running head that slipped the marginal rules - detached a hair
under the threshold, its base a hair past the band - must not come
back as a heading."
  (when-let* ((line (car (pdf-text-block-lines block)))
              (top (pdf-text-line-top line)))
    (or (< top pdf-text-margin-band)
        (< (- 1.0 pdf-text-margin-band) top))))

(defun pdf-text--synth-dotted (text height x1 profile)
  "Level of TEXT as a numbered heading set at HEIGHT ending at X1, or nil.
The number gives the level; the geometry gates it: at least
`pdf-text-synth-number-min' of PROFILE's body - Benji's sections run
at 1.03, a running head's cross-reference at 0.73 - and ink stopping
short of the column's right edge, where a full line is prose."
  (when-let* ((level (pdf-text--dotted-number-level text))
              (body (plist-get profile :height))
              (right (plist-get profile :right)))
    (and height (<= (* pdf-text-synth-number-min body) height)
         x1 (< x1 (- right 0.02))
         level)))

(defun pdf-text--synth-tuple (blocks text height x1 profile)
  "The heading candidate BLOCKS make as TEXT, set at HEIGHT ending at X1.
A plist (:blocks :text :height :dotted), or nil.  TEXT must read as a
title: words rather than mathematics, no bracket-assembly glyphs, no
dot leaders, nothing closing the line, at most
`pdf-text-heading-max-words' words.  What passes is a heading when
its number says so (:dotted carries the level) or when it is set over
`pdf-text-heading-height' of PROFILE's body - the size rules decide
those against the whole book's clusters."
  (let ((body (plist-get profile :height))
        (trimmed (string-trim (or text "")))
        (case-fold-search nil))
    (when (and body height
               ;; a word of three letters or more, and an uppercase
               ;; letter or a digit opening the line: what every title
               ;; has and a scrambled legacy-font equation - "ftf", a
               ;; lone w before its paragraph - does not
               (string-match-p "[[:alpha:]]\\{3\\}" trimmed)
               (not (string-match-p "\\`[^[:alnum:]]*[[:lower:]]" trimmed))
               (not (string-match-p "[\u239B-\u23AD]" trimmed))
               (not (string-match-p "\\(?:\\. \\)\\{3\\}\\|\\.\\{4\\}" trimmed))
               (not (pdf-text-mathish-text-p trimmed))
               ;; operator glyphs no title carries: a Haskell type
               ;; signature or an equation set at display size defeats
               ;; the mathish vote when its operands are words; a comma
               ;; glued to a letter is an equation's typography too
               (not (string-match-p "[]={}|[`$←→↔⇒⇐∗∷¬≡≤≥≠∈∧∨±×÷−√]" trimmed))
               (not (string-match-p "[,;][[:alpha:]]" trimmed))
               (not (string-match-p "[.,;:]\\'" trimmed))
               (<= (length (split-string trimmed)) pdf-text-heading-max-words))
      (let ((dotted (pdf-text--synth-dotted trimmed height x1 profile)))
        (when (or dotted (< (* pdf-text-heading-height body) height))
          (list :blocks blocks :text trimmed :height height :dotted dotted))))))

(defun pdf-text--synth-pair (block next profile vocabulary)
  "BLOCK and NEXT as one heading candidate, when the page splits a title.
Two shapes.  A bare display-size number and the display block it
belongs to - poppler serves \"1.1\" and \"Functions\" as separate
lines when the gap between them is wide, and ANAYA hangs its unit
titles beside a giant unit digit.  And a worded eyebrow: a short
digit-carrying label like \"Chapter 2\" set over a title at least its
size.  Both halves must be set over PROFILE's body; VOCABULARY joins
each half's text."
  (when-let* ((body (plist-get profile :height))
              (leading (plist-get profile :leading))
              ((memq (pdf-text-block-kind block) '(para item)))
              ((memq (pdf-text-block-kind next) '(para item)))
              ((not (pdf-text--synth-banded-p block)))
              (height (pdf-text--block-height block))
              (next-height (pdf-text--block-height next))
              ((< (* pdf-text-heading-height body) height))
              ((< (* pdf-text-heading-height body) next-height))
              (last-line (car (last (pdf-text-block-lines block))))
              (base (pdf-text-line-base last-line))
              (top (pdf-text-line-top (car (pdf-text-block-lines next))))
              (text (pdf-text--join-block block vocabulary))
              (next-text (pdf-text--join-block next vocabulary)))
    (when (or (and (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)*\\.?\\'"
                                   (string-trim text))
                   (<= top (+ base leading)))
              (and (<= (length (split-string text)) 3)
                   (string-match-p "[[:alpha:]]" text)
                   ;; a digit, or the number spelled in caps: CHAPTER ONE
                   (or (string-match-p "[0-9]" text)
                       (let ((case-fold-search nil))
                         (not (string-match-p "[[:lower:]]" text))))
                   (not (pdf-text--dotted-number-level text))
                   (<= height next-height)
                   (<= top (+ base (* 4 leading)))))
      (pdf-text--synth-tuple
       (list block next)
       (concat (string-trim text) " " (string-trim next-text))
       (max height next-height)
       (apply #'max (delq nil (mapcar #'pdf-text-line-x1
                                      (append (pdf-text-block-lines block)
                                              (pdf-text-block-lines next)))))
       profile))))

(defun pdf-text--synth-single (block profile vocabulary)
  "BLOCK alone as a heading candidate against PROFILE, or nil.
A multi-line block can still be a sized heading - a long title wraps -
but never a numbered one: a numbered line that wraps is prose.
VOCABULARY joins the text."
  (when (and (memq (pdf-text-block-kind block) '(para item))
             (not (pdf-text--synth-banded-p block)))
    (let* ((lines (pdf-text-block-lines block))
           (height (pdf-text--block-height block))
           (body (plist-get profile :height)))
      ;; joining a block's text walks the vocabulary; a multi-line block
      ;; of body type - most paragraphs - can never be a heading, so it
      ;; never pays for the join
      (when (and height body
                 (or (and (null (cdr lines))
                          (<= (* pdf-text-synth-number-min body) height))
                     (< (* pdf-text-heading-height body) height)))
        (pdf-text--synth-tuple (list block)
                               (pdf-text--join-block block vocabulary)
                               height
                               (and (null (cdr lines))
                                    (pdf-text-line-x1 (car lines)))
                               profile)))))

(defun pdf-text--synth-page-tuples (blocks profile vocabulary)
  "Heading candidates among one page's BLOCKS, pairs merged.
PROFILE and VOCABULARY as the render reads them."
  (let* ((vec (vconcat (cl-remove-if (lambda (b)
                                       (eq 'blank (pdf-text-block-kind b)))
                                     blocks)))
         (i 0)
         tuples)
    (while (< i (length vec))
      (let* ((next (and (< (1+ i) (length vec)) (aref vec (1+ i))))
             (pair (and next (pdf-text--synth-pair (aref vec i) next
                                                   profile vocabulary)))
             (tuple (or pair (pdf-text--synth-single (aref vec i)
                                                     profile vocabulary))))
        (when tuple (push tuple tuples))
        (setq i (+ i (if pair 2 1)))))
    (nreverse tuples)))

(defun pdf-text--heading-clusters (pairs body)
  "PAIRS of (HEIGHT . PAGE) as supported height ranges, tallest first.
Candidate heights within a tenth of BODY of each other are one
style; a style must head `pdf-text-synth-support' distinct pages
before it earns an org level, which is what keeps a cover page's
display type and a one-off diagram out of the outline."
  (let ((sorted (sort (copy-sequence pairs) (lambda (a b) (< (car a) (car b)))))
        (gap (* 0.1 (or body 0.01)))
        groups current)
    (dolist (pair sorted)
      (if (and current (< (- (car pair) (caar current)) gap))
          (push pair current)
        (when current (push (nreverse current) groups))
        (setq current (list pair))))
    (when current (push (nreverse current) groups))
    (cl-loop for group in groups
             when (<= pdf-text-synth-support
                      (length (cl-remove-duplicates (mapcar #'cdr group))))
             collect (cons (caar group) (car (car (last group)))))))

(defun pdf-text--cluster-level (height clusters)
  "Org level of HEIGHT among CLUSTERS, nil when no cluster holds it.
CLUSTERS run tallest first, so the book's biggest recurring style is
level 1; levels cap at `pdf-text-synth-levels'."
  (when-let* ((pos (cl-position-if
                    (lambda (c) (and (<= (- (car c) 1e-4) height)
                                     (<= height (+ (cdr c) 1e-4))))
                    clusters)))
    (min (1+ pos) pdf-text-synth-levels)))

(defun pdf-text--synth-assignments (pages-blocks profiles vocabulary)
  "Where each synthesized heading goes, per page of PAGES-BLOCKS.
A list of (PLACED . DROPS): PLACED in `pdf-text--assign-headings'
shape - the heading replaces its block - and DROPS the blocks a
merged pair consumed.  A numbered candidate takes its dot depth; a
sized one the rank of its height among the whole document's supported
clusters, which only this document-wide pass can know.  PROFILES and
VOCABULARY as the render reads them."
  (let* ((body (plist-get (car profiles) :height))
         (tuples (cl-loop for blocks in pages-blocks
                          for profile in profiles
                          collect (pdf-text--synth-page-tuples blocks profile
                                                               vocabulary)))
         (clusters (or pdf-text-extra-heading-levels
                       (pdf-text--heading-clusters
                        (cl-loop for page-tuples in tuples
                                 for page from 1
                                 nconc (cl-loop for tuple in page-tuples
                                                unless (plist-get tuple :dotted)
                                                collect (cons (plist-get tuple :height)
                                                              page)))
                        body))))
    (cl-loop for page-tuples in tuples
             collect (let (placed drops)
                       (dolist (tuple page-tuples)
                         (when-let* ((level (or (plist-get tuple :dotted)
                                                (pdf-text--cluster-level
                                                 (plist-get tuple :height)
                                                 clusters))))
                           (push (list (car (plist-get tuple :blocks))
                                       (concat (make-string level ?*) " "
                                               (plist-get tuple :text))
                                       t)
                                 placed)
                           (setq drops (append drops
                                               (cdr (plist-get tuple :blocks))))))
                       (cons (nreverse placed) drops)))))

(defun pdf-text-document-facts (pages)
  "The document-wide readings a window render cannot derive from PAGES.
A plist (:profile :heading-levels), what `pdf-text-extra-profile' and
`pdf-text-extra-heading-levels' seed: the modal body geometry and the
heading-height clusters, both defined over the whole document.
The profile is measured over the cleaned pages before any repair,
exactly where `pdf-text-reading-order' measures its own: the repairs
move records - a lane unfold rewrites a facing column's coordinates -
and a profile taken after them could not reproduce them when seeded."
  (let* ((profile (pdf-text--profile (pdf-text-clean-pages pages)))
         (page-lines (let ((pdf-text-extra-profile profile))
                       (pdf-text-reading-order pages)))
         (profiles (mapcar (lambda (lines) (pdf-text--page-profile lines profile))
                           page-lines))
         (page-lines (pdf-text-remove-marginal-lines page-lines profiles))
         (vocabulary (pdf-text--hyphenated-words page-lines))
         (tuples (cl-loop for lines in page-lines
                          for page-profile in profiles
                          collect (pdf-text--synth-page-tuples
                                   (pdf-text--blocks lines page-profile)
                                   page-profile vocabulary))))
    (list :profile profile
          :heading-levels (pdf-text--heading-clusters
                           (cl-loop for page-tuples in tuples
                                    for page from 1
                                    nconc (cl-loop for tuple in page-tuples
                                                   unless (plist-get tuple :dotted)
                                                   collect (cons (plist-get tuple :height)
                                                                 page)))
                           (plist-get profile :height)))))

(defvar pdf-text-synth-heading-max-fraction 0.6
  "Widest fraction of the page's wrap column a synthesized heading fills.")

(defun pdf-text--synthesize-headings (pages)
  "PAGES with short numbered section lines promoted to org headings.
The fallback for documents carrying no outline metadata and no glyph
geometry either - with geometry, `pdf-text--synth-assignments' reads
the headings out of the page's own setting instead.  A line like
\"2.2 Arguments\" - a dotted section number, then a capitalized word,
well short of the page's wrap column, with no page number at the end
the way TOC entries have - reads as a section heading, its dot count
as the org level.  Prose and TOC pages pass through untouched."
  (let ((case-fold-search nil))
    (mapcar
     (lambda (page)
       (let* ((lines (split-string page "\n"))
              (limit (* pdf-text-synth-heading-max-fraction
                        (apply #'max 0 (mapcar (lambda (l) (length (string-trim l)))
                                               lines)))))
         (string-join
          (mapcar
           (lambda (line)
             (let ((trimmed (string-trim line)))
               (if (and (string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\.? +[[:upper:]]"
                                      trimmed)
                        (<= (length trimmed) limit)
                        (not (string-match-p "[0-9]\\'" trimmed)))
                   (concat (make-string (1+ (cl-count ?. (match-string 1 trimmed))) ?*)
                           " " trimmed)
                 line)))
           lines)
          "\n")))
     pages)))

(defvar pdf-text-min-text-fraction 0.05
  "Fraction of pages that must carry text before extraction proceeds.
Below it the document is a scan: a page or two of stray text in an
otherwise image-only book does not make a readable view.")

(defun pdf-text--scanned-p (pages)
  "Whether raw PAGES look like a scan: nearly no page carries text."
  (< (cl-count-if-not #'string-blank-p pages)
     (* pdf-text-min-text-fraction (length pages))))

;;; The companion buffer

(defvar-local pdf-text--page-starts nil
  "Vector of buffer positions; element N-1 is where page N starts.")

(defvar-local pdf-text--pdf-buffer nil
  "The `pdf-view-mode' buffer this text view mirrors.")

(defvar-local pdf-text--companion nil
  "The companion pdf-text buffer, on the `pdf-view-mode' side.")

(defvar-local pdf-text--source-stamp nil
  "The rendered PDF's `pdf-text--file-stamp'; freshness key for reuse.")

(defvar-local pdf-text--has-outline nil
  "Whether the rendered document carried an outline, so folds exist.")

(defun pdf-text--insert-pages (pages)
  "Insert PAGES (list of strings) at point, a form feed between pages.
Fills `pdf-text--page-starts' with each page's start position."
  (let (starts)
    (while pages
      (push (point) starts)
      (insert (string-trim (pop pages)) "\n")
      (when pages (insert "\f\n")))
    (setq pdf-text--page-starts (vconcat (nreverse starts)))))

(defun pdf-text--page-start (page)
  "Buffer position where PAGE starts, clamped to the known range."
  (let ((starts pdf-text--page-starts))
    (aref starts (min (max 0 (1- page)) (1- (length starts))))))

(defun pdf-text--page-end (page)
  "Buffer position where PAGE's text ends, clamped like its start.
For every page but the last that is the newline before the \"\\n\\f\\n\"
delimiter `pdf-text--insert-pages' writes; the last page runs to the
end of the buffer."
  (let* ((starts pdf-text--page-starts)
         (page (min (max 1 page) (length starts))))
    (if (< page (length starts))
        (- (aref starts page) 3)
      (point-max))))

(defun pdf-text--page-position (page fraction)
  "Buffer position FRACTION of the way into PAGE's text.
Character-based interpolation: reflow already re-shapes lines, so a
finer mapping from the image's geometry would be false precision."
  (let* ((start (pdf-text--page-start page))
         (end (pdf-text--page-end page)))
    (min (max start (+ start (round (* fraction (- end start))))) end)))

(defun pdf-text-page-at-point ()
  "Page number at point: one more than the form feeds above it."
  (save-excursion
    (let ((pos (point))
          (n 1))
      (goto-char (point-min))
      (while (search-forward "\f" pos t)
        (setq n (1+ n)))
      n)))

(defgroup pdf-text nil
  "Reflowed plain-text reading view for PDFs."
  :group 'convenience
  :prefix "pdf-text-")

(defface pdf-text-footnote-face
  '((t :inherit shadow))
  "Face dimming a footnote definition or an unmarked page-foot note.
Inherits `shadow' so the dimming tracks the theme either way; it is
appended behind whatever org paints, so `org-footnote' keeps the
label."
  :group 'pdf-text)

(defun pdf-text--match-note (limit)
  "Font-lock matcher: the next span the render marked as a note.
The render puts a `pdf-text-note' text property on footnote
definitions and unmarked page-foot notes; the property survives
org's refontification, so the rule keyed on it re-applies the face
each time font-lock runs.  Match data covers the span up to LIMIT."
  (let ((beg (point)))
    (unless (get-text-property beg 'pdf-text-note)
      (setq beg (next-single-property-change beg 'pdf-text-note nil limit)))
    (when (and beg (< beg limit))
      (let ((end (or (next-single-property-change beg 'pdf-text-note nil limit)
                     limit)))
        (goto-char end)
        (set-match-data (list beg end))
        t))))

(define-derived-mode pdf-text-mode org-mode "pdf-text"
  "Reflowed reading view of a PDF.
Derives from `org-mode' so the document outline, interleaved as
headings, gives folding, sparse trees, and heading-addressable
positions; the extracted text itself is escaped so none of it reads
as org structure.  Without an outline the buffer is the same flat
text it always was."
  (setq buffer-read-only t)
  (visual-line-mode 1)
  (goto-address-mode 1)
  (font-lock-add-keywords
   nil '((pdf-text--match-note 0 'pdf-text-footnote-face append)) t)
  ;; The reflow writes super- and subscripts as ^{...}/_{...}; org
  ;; renders only that braced form as real scripts, so a literal ^ or _
  ;; extracted from the page stays the plain glyph it was.  All three
  ;; variables are buffer-local: a user's org files owe nothing to how
  ;; a rendered book displays its exponents.
  (setq-local org-use-sub-superscripts '{})
  (setq-local org-pretty-entities t)
  (setq-local org-pretty-entities-include-sub-superscripts t)
  ;; Render each page-delimiting ^L as a rule instead of a glyph.
  (setq-local buffer-display-table (make-display-table))
  (aset buffer-display-table ?\f
        (vconcat (make-list 64 (make-glyph-code ?─ 'shadow)))))

(defconst pdf-text-render-version 15
  "Version of the rendering pipeline, part of the freshness stamp.
Bumping it stales every companion rendered by older code, so reuse
cannot serve output the current transforms would no longer produce.")

(defun pdf-text--file-stamp (file)
  "FILE's identity - path, mtime, render version - as the freshness key.
Nil (never fresh) without a file or with a vanished one."
  (when-let* ((attrs (and file (file-attributes file))))
    (list file (file-attribute-modification-time attrs)
          pdf-text-render-version)))

(defun pdf-text--view-fraction ()
  "How far down the page the `pdf-view' window's top edge sits, 0..1.
Pixel vscroll over the displayed image's height, the same quantities
`pdf-util-image-displayed-edges' derives its visible top from.  Zero
when the whole page fits the window; approximate under
`pdf-view-roll-minor-mode', where vscroll spans stacked pages."
  (condition-case nil
      (let ((height (cdr (pdf-view-image-size t))))
        (if (< 0 height)
            (min 1.0 (/ (window-vscroll nil t) (float height)))
          0.0))
    (error 0.0)))

(defun pdf-text--pdf-page ()
  "Page the current `pdf-view' buffer shows.
`pdf-view-current-page' is a macro, so compiled without pdf-tools on
the load path it becomes a function call no session can resolve; its
expansion - the page from image-mode's window properties - is called
directly instead."
  (image-mode-window-get 'page))

(defvar pdf-text-gc-cons-threshold (* 256 1024 1024)
  "Consing one render may do before garbage collection runs.
The reflow allocates on the order of 2000 glyph conses per page, and
in a live session every collection scans the user's whole working
heap, not just the render's share - measured at 60% of the render's
cost against the stock threshold.  256MB absorbs a 60-page window
without a single collection and caps the longest books at a handful;
more only enlarges the transient heap spike it licenses.")

(defmacro pdf-text--with-render-gc (&rest body)
  "Run BODY with garbage collection deferred.
`gc-cons-threshold' rises to `pdf-text-gc-cons-threshold', and
`gc-cons-percentage' to 0.6 - the collector fires on whichever rule
allows more consing, and on a multi-GB heap the percentage rule is
the one that fires.  Both bindings unwind when BODY exits, error
included, so the deferred collection runs against the session's own
thresholds afterwards."
  (declare (indent 0) (debug t))
  `(let ((gc-cons-threshold pdf-text-gc-cons-threshold)
         (gc-cons-percentage 0.6))
     ,@body))

(defvar pdf-text-sync-mode)

(defvar pdf-text-sync-default t
  "Non-nil starts `pdf-text-sync-mode' on a freshly rendered companion.
Reuse keeps whatever the reader last set, so switching the sync off
holds for that book until its next re-render.")

;;;###autoload
(defun pdf-view-as-text ()
  "Read the current PDF as reflowed text in a companion buffer.
Lands where the `pdf-view-mode' window is: same page, proportionally
as far into the page's text as the window top sits down the image.
The companion is reused as long as the PDF file on disk is unchanged;
a stale or missing one is re-extracted through epdfinfo.  The PDF
outline becomes org headings; without one, numbered section lines
found in the text stand in.  A fresh companion starts
`pdf-text-sync-mode' when `pdf-text-sync-default' is non-nil.  A
document whose pages carry almost no text - a scan - signals an
error instead of an empty buffer."
  (interactive)
  (unless (derived-mode-p 'pdf-view-mode)
    (user-error "Not in a pdf-view buffer"))
  (let* ((pdf-buf (current-buffer))
         (page (pdf-text--pdf-page))
         (fraction (pdf-text--view-fraction))
         (stamp (pdf-text--file-stamp buffer-file-name))
         (name (format "*pdf-text: %s*" (buffer-name)))
         (buf (get-buffer name))
         (fresh (not (and buf stamp
                          (equal stamp (buffer-local-value
                                        'pdf-text--source-stamp buf))))))
    (when fresh
      ;; the render blocks until it is done; say so up front
      (message "pdf-text: extracting text from %s..." (buffer-name))
      (pdf-text--with-render-gc
        ;; the layout carries the text as well as its geometry, so gettext
        ;; only runs for a page epdfinfo lays out no glyphs for
        (let* ((start (float-time))
               (layouts (pdf-text--charlayouts
                         buffer-file-name
                         (number-sequence 1 (pdf-info-number-of-pages))))
               (raw (cl-loop for p from 1
                             for layout in layouts
                             collect (if layout
                                         (pdf-text--layout-text layout)
                                       (pdf-info-gettext p '(0 0 1 1))))))
          (when (pdf-text--scanned-p raw)
            (user-error "%s has no text layer (%d of %d pages carry text)"
                        (buffer-name)
                        (cl-count-if-not #'string-blank-p raw) (length raw)))
          (let* ((outline (pdf-info-outline))
                 (rendered (pdf-text-render-pages
                            raw layouts
                            (pdf-text-page-headings outline 1 (length raw))
                            (null outline)))
                 (pages (if outline
                            (pdf-text--interleave-outline rendered outline)
                          rendered)))
            (setq buf (get-buffer-create name))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (pdf-text-mode)
                (pdf-text--insert-pages pages)
                (setq pdf-text--source-stamp stamp
                      pdf-text--has-outline
                      (and (save-excursion
                             (goto-char (point-min))
                             (re-search-forward "^\\*+ " nil t))
                           t))
                (when pdf-text--has-outline (org-cycle-overview)))))
          (message "pdf-text: extracting text from %s...done (%.1fs)"
                   (buffer-name) (- (float-time) start)))))
    (with-current-buffer buf
      (setq pdf-text--pdf-buffer pdf-buf)
      ;; re-enabling on reuse re-arms the pdf-side hook after the PDF
      ;; buffer was killed and reopened; an explicit off stays off
      (when (or (and fresh pdf-text-sync-default) pdf-text-sync-mode)
        (pdf-text-sync-mode 1)))
    (setq pdf-text--companion buf)
    (pop-to-buffer buf)
    (goto-char (pdf-text--page-position page fraction))
    (pdf-text--reveal-page page)
    (beginning-of-visual-line)
    (recenter 0)))

(defun pdf-text--reveal-page (page)
  "Open every section that shows on PAGE of the companion.
The lineage of point opens the section the page begins inside; a
section starting at a heading further down the page would stay folded,
and the reader following along the PDF side would have to open it by
hand.  Every heading line inside the page's span shows its lineage and
its body text; what runs past the page keeps its fold, so a chapter
whose title closes the page opens only as far as its opening text."
  (when pdf-text--has-outline
    (org-fold-show-set-visibility 'lineage)
    (save-excursion
      (let ((end (pdf-text--page-end page)))
        (goto-char (pdf-text--page-start page))
        (while (re-search-forward "^\\*+ " end t)
          ;; the reveals run regexps of their own and clobber the match
          (let ((next (match-end 0)))
            (goto-char (match-beginning 0))
            (org-fold-show-set-visibility 'lineage)
            (org-fold-show-entry)
            (goto-char next)))))))

(defun pdf-text-show-in-pdf ()
  "Jump the source PDF buffer to the page at point and focus it."
  (interactive)
  (let ((page (pdf-text-page-at-point))
        (buf pdf-text--pdf-buffer))
    (unless (buffer-live-p buf)
      (user-error "The source PDF buffer is gone"))
    (pop-to-buffer buf)
    (pdf-view-goto-page page)))

(defvar pdf-text-sync--inhibit nil
  "Non-nil while one side of the sync moves the other; breaks the loop.")

(defvar-local pdf-text-sync--last-page nil
  "Page the sync last settled on; motions within it stay silent.")

(defun pdf-text-sync--follow-text ()
  "Post-command in the companion: show the page at point in the PDF.
Only a visible PDF window follows; the sync is about reading side by
side, not about flipping pages in a buried buffer."
  (let ((page (pdf-text-page-at-point))
        (pdf pdf-text--pdf-buffer))
    (unless (or pdf-text-sync--inhibit (eql page pdf-text-sync--last-page))
      (setq pdf-text-sync--last-page page)
      (when-let* (((buffer-live-p pdf))
                  (win (get-buffer-window pdf t)))
        (let ((pdf-text-sync--inhibit t))
          (with-selected-window win
            (unless (eql page (pdf-text--pdf-page))
              (pdf-view-goto-page page))))))))

(defun pdf-text-sync--follow-pdf ()
  "Page-change hook in the PDF buffer: move the companion's point along.
Removes itself once the companion is gone or dropped the mode - the
hook must not outlive its buffer.  A companion already on the page
stays put, so the explicit RET jump keeps its exact position."
  (let ((companion pdf-text--companion)
        (page (pdf-text--pdf-page)))
    (cond
     ((not (and (buffer-live-p companion)
                (buffer-local-value 'pdf-text-sync-mode companion)))
      (remove-hook 'pdf-view-after-change-page-hook #'pdf-text-sync--follow-pdf t))
     ((not pdf-text-sync--inhibit)
      (let ((pdf-text-sync--inhibit t))
        (with-current-buffer companion
          (setq pdf-text-sync--last-page page)
          (let* ((moved (not (eql page (pdf-text-page-at-point))))
                 (pos (and moved (pdf-text--page-position page 0)))
                 (win (get-buffer-window companion t)))
            (if win
                (with-selected-window win
                  (when moved (goto-char pos))
                  (pdf-text--reveal-page page)
                  (when moved (recenter 0)))
              (when moved (goto-char pos))
              (pdf-text--reveal-page page)))))))))

(define-minor-mode pdf-text-sync-mode
  "Keep the companion and its PDF on the same page, both directions.
Point moving onto another page here scrolls the `pdf-view' window
there; flipping the PDF's page moves point here.  The explicit jump
on RET works without the mode."
  :lighter " pdf-sync"
  (unless (derived-mode-p 'pdf-text-mode)
    (setq pdf-text-sync-mode nil)
    (user-error "Not in a pdf-text buffer"))
  (let ((companion (current-buffer))
        (pdf pdf-text--pdf-buffer))
    (if pdf-text-sync-mode
        (progn
          (unless (buffer-live-p pdf)
            (setq pdf-text-sync-mode nil)
            (user-error "The source PDF buffer is gone"))
          (setq pdf-text-sync--last-page (pdf-text-page-at-point))
          (add-hook 'post-command-hook #'pdf-text-sync--follow-text nil t)
          (with-current-buffer pdf
            (setq pdf-text--companion companion)
            (add-hook 'pdf-view-after-change-page-hook
                      #'pdf-text-sync--follow-pdf nil t)))
      (remove-hook 'post-command-hook #'pdf-text-sync--follow-text t)
      (when (buffer-live-p pdf)
        (with-current-buffer pdf
          (remove-hook 'pdf-view-after-change-page-hook
                       #'pdf-text-sync--follow-pdf t))))))

(provide 'pdf-text)
;;; pdf-text.el ends here
