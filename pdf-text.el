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
;; shows and as far into it as the window sits down the image.  Text and its
;; geometry come from MuPDF's structured text (`mutool run' with a walker
;; script, one record per line with its font runs; `pdf-info-gettext' as the
;; fallback for a page it finds no text on), and the document outline
;; (`pdf-info-outline') becomes foldable org headings.
;; `pdf-text-show-in-pdf' jumps the PDF back to the page at point;
;; `pdf-text-sync-mode' keeps the two on the same page in both directions.
;;
;; Extractors hand out lines, never blocks: the engines compute paragraphs
;; and columns internally and then discard them while serving text, so the
;; reflow has to rebuild that structure from the line geometry - where a line
;; ends, how far the next one starts in, how much air sits between their
;; baselines.
;;
;; Everything below the entry commands is pure transformation, testable
;; without a PDF, pdf-tools or mutool on the load path.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)

;; pdf-tools is a runtime dependency only: every symbol below resolves when a
;; command runs, so the transforms load and test without it.  org arrives with
;; the derived mode.
(declare-function pdf-info-gettext "ext:pdf-info")
(declare-function pdf-info-number-of-pages "ext:pdf-info")
(declare-function pdf-info-outline "ext:pdf-info")
(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function image-mode-window-get "image-mode")
(declare-function pdf-view-image-size "ext:pdf-view")
(declare-function pdf-view-image-type "ext:pdf-view")
(declare-function pdf-view-display-image "ext:pdf-view")
(declare-function pdf-view-redisplay "ext:pdf-view")
(declare-function pdf-info-renderpage-text-regions "ext:pdf-info")
(declare-function pdf-util-face-colors "ext:pdf-util")
(declare-function pdf-util-frame-scale-factor "ext:pdf-util")
;; dynamic in pdf-info; declared so the let-binding around the async
;; render compiles as special without pdf-tools on the load path
(defvar pdf-info-asynchronous)
(declare-function org-cycle-overview "org-cycle")
(declare-function org-fold-show-set-visibility "org-fold")
(declare-function org-fold-show-entry "org-fold")

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
  font                                  ; dominant font name, or nil
  size                                  ; dominant font em size, page-relative
  bold italic                           ; dominant font's weight and slant
  synth                                 ; characters the extractor invented
  lead-font                             ; first inked run's font name, or nil
  lead-bold                             ; first inked run's weight
  claimed)                              ; owned by a lane region; zones keep out

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

(defvar pdf-text-script-symbol-offset 0.25
  "Baseline offset, in body heights, a run of pure symbols needs to be a script.
A footnote asterisk sits half a body height up; an operator font whose
boxes anchor a shade off the baseline - a midline ellipsis - sits
within 0.15 of it, and stays text.")

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

(defun pdf-text--page-lines (text)
  "Text-only line records for one page, from TEXT.
The fallback for a page the walker finds no text on: no geometry, so
every rule that reads some falls back to character heuristics."
  (mapcar (lambda (line)
            (pdf-text-line-create
             :text (pdf-text--strip-unprinted
                    (pdf-text--escape-literals line))))
          (split-string text "\n")))

;;; Line records off MuPDF's structured text

(defvar pdf-text-mupdf-program "mutool"
  "The MuPDF tool the text extraction runs.
Text comes from `mutool run' walking the structured text, because
poppler's extraction invents spaces in tracked type, loses small caps
and carries no font identity; MuPDF applies ActualText, expands
ligatures, flags the characters it synthesizes and names the font of
every glyph.")

(defconst pdf-text--walker-source "\
// Emitted by pdf-text.el; edit there.  One elisp form per stext line:
// (PAGE X0 TOP X1 BOT HEIGHT SPACE CV FIRST-WIDTH SYNTH RUNS \"TEXT\")
// with RUNS a list of (OFFSET LENGTH X0 X1 OY QH SIZE BOLD ITALIC \"FONT\"),
// one per stretch of chars agreeing on font, size and descent line.
// Coordinates are page fractions; offsets count characters, not UTF-16
// units, so they index elisp strings directly.
// mutool run WALKER FILE [FIRST [LAST]] < /dev/null
var doc = Document.openDocument(scriptArgs[0]);
var first = scriptArgs.length > 1 ? parseInt(scriptArgs[1]) : 1;
var last = scriptArgs.length > 2 ? parseInt(scriptArgs[2]) : doc.countPages();
if (last > doc.countPages()) last = doc.countPages();

// A dvips-era Type 3 bitmap font names each glyph by its character -
// /' /1 /#2F - and ships a ToUnicode CMap covering only the letters,
// so MuPDF reads the rest as U+FFFD while the font itself spells the
// answer.  Complete every Type 3 font's CMap from its glyph names
// before any page loads: existing entries are kept verbatim, a
// single printable-ASCII name maps to its character - a second
// dvips scheme writes the byte as literal '#XX' text, which reduces
// to the same byte - and the TeX ligature bytes 0B-0F map to ff fi
// fl ffi ffl when the font reads as text: it names a digit, or its
// control-byte names stay inside 0B-0F.  A math italic font fails
// both - no digits, Greek names past 0F - and its 0B-0F are Greek
// letters that must stay unmapped rather than turn into false
// ligatures.  In a text font of varied widths, the bytes TeX reads
// as dashes and double quotes take their cmr meaning - 7B/7C are
// en and em dash, 5C/22 the opening and closing double quote - but
// in typewriter type those bytes are the braces and bar they look
// like, and uniform widths are that font's own tell.
//
// A math font's byte names would decode to false ASCII - a cmmi
// period is named ':', a cmsy arrow '!' - so glyph identity comes
// from the widths instead.  Every Computer Modern face carries its
// canonical width at every position, the subset's Widths array
// copies them at some scale, and matching the two identifies the
// family: cmmi reads Greek and italic variables, cmsy operators,
// arrows and script capitals, cmex the big delimiters.  The same
// fit vetoes the lying names a text font carries (fpio names its
// Gamma ';') and overrides the shipped entries a subset CMap gets
// wrong (a script M mapped to V).  What no family fits keeps unit
// 1's reading untouched, and what no table maps stays U+FFFD: the
// honest placeholder over false prose.
function hx2(v) { return ('00' + v.toString(16).toUpperCase()).slice(-2); }
function hx4(v) { return ('0000' + v.toString(16).toUpperCase()).slice(-4); }
// Canonical Computer Modern tables, generated from the TeX
// distribution's AFMs: T3W holds each candidate family's glyph
// widths in 1/1000 em, four hex digits a slot, 128 slots; T3U
// holds the byte-to-Unicode readings as comma-separated UTF-16
// hex units, an empty slot meaning the byte stays unmapped on
// purpose - a hook or an accent piece has no honest standalone
// codepoint, and a wrong letter would read as prose.
var T3W = {
	r10: '02710341030902B6029A02EE02D2030902D2030902D20247022B022B034103410115013101F401F401F401F401F402EE01BC01F402D2030901F4038603F503090115011501F4034101F40341030901150184018401F403090115014D011501F401F401F401F401F401F401F401F401F401F401F4011501150115030901D801D8030902EE02C402D202FB02A8028C031002EE0169020103090271039402EE030902A8030902E0022B02D202EE02EE040302EE02EE0263011501F4011501F40115011501F4022B01BC022B01BC013101F4022B01150131020F01150341022B01F4022B020F0187018A0184022B020F02D2020F020F01BC01F403E801F401F401F4',
	ti10: '0273033102FE02B4029802E702CB02FE02CB02FE02CB02650232024B0371037E0132014C01FF01FF01FF01FF01FF033F01CC021802CB02CB01FF037203D902FE00FF0132020203310301033102FE01320198019801FF02FE01320165013201FF01FF01FF01FF01FF01FF01FF01FF01FF01FF01FF01320132013202FE01FF01FF02FE02E702BF02CB02F302A6028C030502E70181020D03000273038002E702FE02A602FE02D9023202CB02E702E703E602E702E7026501320202013201FF0132013201FF01CC01CC01FF01CC013201CC01FF0132013201CC00FF0331023201FF01FF01CC01A50198014C021801CC029801CF01E5019801FF03FE01FF01FF01FF',
	bx10: '02B303BE037E032502FE0384033E037E033E037E033E029E027E027E03BE03BE013F015F023F023F023F023F023F036501FF0255033E037E023F04110491037E013F015E025A03BE023F03BE037E013F01BF01BF023F037E013F017F013F023F023F023F023F023F023F023F023F023F023F023F013F013F015E037E021F021F037E03650332033E037102F302D30388038401B40252038502B304430384035F0312035F035E027E03200374036504A40365036502BE013F025A013F023F013F013F022F027E01FF027E020F015F023F027E013F015F025E013F03BE027E023F027E025E01D901C501BF027E025E033E025E025E01FF023F047E023F023F023F',
	sl10: '02710341030902B6029A02EE02D2030902D2030902D20247022B022B034103410115013101F401F401F401F401F4032801BC01F402D2030901F4038603F503090115011501F4034101F40341030901150184018401F403090115014D011501F401F401F401F401F401F401F401F401F401F401F4011501150115030901D801D8030902EE02C402D202FB02A8028C031002EE0169020103090271039402EE030902A8030902E0022B02D202EE02EE040302EE02EE0263011501F4011501F40115011501F4022B01BC022B01BC013101F4022B01150131020F01150341022B01F4022B020F0187018A0184022B020F02D2020F020F01BC01F403E801F401F401F4',
	mi10: '0267034102FA02B602E6033F030B0247029A02640304027F0235020501BC019501B501F001D5016102400247025A01ED01B5023A0205023B01B5021C02530271028B026E01D2024F033C0205016A028E03E803E803E803E80115011501F401F401F401F401F401F401F401F401F401F401F401F401150115030901F4030901F4021202EE02F602CA033B02E202830312033F01B7022A035102A803CA032302FA0282031602F70265024802AA024703B0033C024402AA01840184018403E803E801A0021001AD01B0020801D101E901DC02400158019B0208012A036E025801E401F701BE01C301D40169023C01E402CB023B01EA01D101420180027C01F40115',
	mi7: '02B503BA0364031D034C03A7037602A5030102CC037002E602870258020701DC0207024C022001A6029C02A502B6023C0207029C02500296020E027802AE02C902F302CF021B02B103B5025001B702EF04720472047204720153015302490249024902490249024902490249024902490249024901530153037C0249037C02490262035B035F033303A6034602D4037903A701FA027803BF030F04410388036402D70383035C02BD02A2030A02A2043203A8029F030A01CE01CE01CE0472047201DE026B01F601FE0252021E022D022D029C019401D8025F016903F502C20233024C020B0212021B01AF02A3023B033A028702430221018E01B902DA02490153',
	sy10: '03090115030901F4030901F4030903090309030903090309030903E801F401F403090309030903090309030903090309030903090309030903E803E80309030903E803E801F401F403E803E803E8030903E803E80263026303E803E803E80309011303E8029A029A0378037800000000022B022B029A01F402D202D2030903090263031E0290020E0303020F02CF0252034C022002A502F902B104B00334031C02B70330034F025D02200271026403DB02C9029C02D4029A029A029A029A029A0263026301BC01BC01BC01BC01F401F401840184011501F401F4026301F40115034102EE034101A0029A029A0309030901BC01BC01BC02630309030903090309',
	sy7: '037C0153037C0249037C0249037C037C037C037C037C037C037C047202490249037C037C037C037C037C037C037C037C037C037C037C037C04720472037C037C0472047202490249047204720472037C0472047202C402C4047204720472037C014904720301030103F703F7000000000286028603010249033F033F037C037C02C4039502F1026C03790268033202B003D20286030E03670317053E03A70389032903A703D502BE028702CD02CF046F033202FC03370301030103010301030102C402C4020B020B020B020B0249024901CE01CE01530249024902C40249015303AA035B03BA01ED03010301037C037C020B020B020B02C4037C037C037C037C',
	ex10: '01CA01CA01A001A001D801D801D801D80247024701D801D8014D022B024102410255025502E002E0020F020F024702470247024702EE02EE02EE02EE041404140317031702470247027E027E027E027E032503250325032504FD04FD032B032B036B036B029A029A029A029A029A029A0378037803780378037803780378029A036B036B036B036B026302630341045701D8022B045705E7045705E7045705E7041F03B001D80341034103410341034105A404FD022B0457045704570457045703B004FD022B03E805A4022B03E805A401D801D8020F020F020F020F029A029A03E803E803E803E8041F041F041F0309029A029A01C201C201C201C203090309',
};
var T3U = {
	mi: '0393,0394,0398,039B,039E,03A0,03A3,03A5,03A6,03A8,03A9,03B1,03B2,03B3,03B4,03F5,03B6,03B7,03B8,03B9,03BA,03BB,03BC,03BD,03BE,03C0,03C1,03C3,03C4,03C5,03C6,03C7,03C8,03C9,03B5,03D1,03D6,03F1,03C2,03D5,21BC,21BD,21C0,21C1,,,25B9,25C3,0030,0031,0032,0033,0034,0035,0036,0037,0038,0039,002E,002C,003C,002F,003E,22C6,2202,0041,0042,0043,0044,0045,0046,0047,0048,0049,004A,004B,004C,004D,004E,004F,0050,0051,0052,0053,0054,0055,0056,0057,0058,0059,005A,266D,266E,266F,2323,2322,2113,0061,0062,0063,0064,0065,0066,0067,0068,0069,006A,006B,006C,006D,006E,006F,0070,0071,0072,0073,0074,0075,0076,0077,0078,0079,007A,0131,0237,2118,,2040',
	sy: '2212,22C5,00D7,2217,00F7,22C4,00B1,2213,2295,2296,2297,2298,2299,25EF,2218,2219,224D,2261,2286,2287,2264,2265,2AAF,2AB0,223C,2248,2282,2283,226A,226B,227A,227B,2190,2192,2191,2193,2194,2197,2198,2243,21D0,21D2,21D1,21D3,21D4,2196,2199,221D,2032,221E,2208,220B,25B3,25BD,,21A6,2200,2203,00AC,2205,211C,2111,22A4,22A5,2135,D835DC9C,212C,D835DC9E,D835DC9F,2130,2131,D835DCA2,210B,2110,D835DCA5,D835DCA6,2112,2133,D835DCA9,D835DCAA,D835DCAB,D835DCAC,211B,D835DCAE,D835DCAF,D835DCB0,D835DCB1,D835DCB2,D835DCB3,D835DCB4,D835DCB5,222A,2229,228E,2227,2228,22A2,22A3,230A,230B,2308,2309,007B,007D,27E8,27E9,007C,2225,2195,21D5,005C,2240,221A,2A3F,2207,222B,2294,2293,2291,2292,00A7,2020,2021,00B6,2663,2662,2661,2660',
	ex: '0028,0029,005B,005D,230A,230B,2308,2309,007B,007D,27E8,27E9,23D0,2016,002F,005C,0028,0029,0028,0029,005B,005D,230A,230B,2308,2309,007B,007D,27E8,27E9,002F,005C,0028,0029,005B,005D,230A,230B,2308,2309,007B,007D,27E8,27E9,002F,005C,002F,005C,239B,239E,23A1,23A4,23A3,23A6,23A2,23A5,23A7,23AB,23A9,23AD,23A8,23AC,23AA,23D0,239D,23A0,239C,239F,27E8,27E9,2A06,2A06,222E,222E,2A00,2A00,2A01,2A01,2A02,2A02,2211,220F,222B,22C3,22C2,2A04,22C0,22C1,2211,220F,222B,22C3,22C2,2A04,22C0,22C1,2210,2210,02C6,02C6,02C6,02DC,02DC,02DC,005B,005D,230A,230B,2308,2309,007B,007D,221A,221A,221A,221A,,,,2016,,,,,,,,',
	text: '0393,0394,0398,039B,039E,03A0,03A3,03A5,03A6,03A8,03A9,00660066,00660069,0066006C,006600660069,00660066006C,0131,0237,0060,00B4,02C7,02D8,00AF,02DA,00B8,00DF,00E6,0153,00F8,00C6,0152,00D8,,,,,,,,,,,,,,,,,,,,,,,,,,,,,00A1,,00BF,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,2013,2014,02DD,007E,00A8',
};

var T3FAM = { r10: 'text', ti10: 'text', bx10: 'text', sl10: 'text',
	mi10: 'mi', mi7: 'mi', sy10: 'sy', sy7: 'sy', ex10: 'ex' };
var t3wCache = {}, t3uCache = {};
function t3w(fam) {
	if (!t3wCache[fam]) {
		var s = T3W[fam], out = [];
		for (var i = 0; i < 128; i++)
			out.push(parseInt(s.substring(i * 4, i * 4 + 4), 16) / 1000);
		t3wCache[fam] = out;
	}
	return t3wCache[fam];
}
function t3u(kind) {
	if (!t3uCache[kind]) t3uCache[kind] = T3U[kind].split(',');
	return t3uCache[kind];
}
function t3med(a) {
	var s = a.slice().sort(function (x, y) { return x - y; });
	var n = s.length;
	if (!n) return 0;
	return n % 2 ? s[(n - 1) / 2] : (s[n / 2 - 1] + s[n / 2]) / 2;
}
function t3tol(w) { return Math.max(0.03, 0.75 / w); }
// A glyph named ';' ',' or ';;' may really sit at byte zero - the
// producers write junk names for code zero's glyph (fpio's Gamma
// reads ';', CMU's minus reads ',') - so the width arbitrates
// between the name's own byte and byte zero.
function t3pos(g, F, em) {
	var p = g.nb;
	if (p < 0 || p > 127) return -1;
	if (!g.amb || g.w <= 0 || em <= 0) return p;
	var ds = F[p] > 0 ? Math.abs(g.w / (em * F[p]) - 1) : 9;
	var dz = F[0] > 0 ? Math.abs(g.w / (em * F[0]) - 1) : 9;
	var t = t3tol(g.w);
	if (ds <= dz) return ds <= t ? p : -1;
	return dz <= t ? 0 : -1;
}
// Score one candidate family: the widths are integers in the font's
// own units, so the em falls out as the median ratio against the
// family's canonical 1/1000-em widths, and the score is the fraction
// of glyphs whose ratio agrees with that em - tolerance floored for
// integer rounding on narrow glyphs.  Positions read from the glyph
// names or from the codes themselves (useCodes), whichever fits: the
// first is the dvips scheme, the second CMU's original-position one.
function t3fit(glyphs, F, useCodes) {
	var ratios = [], i, g, p;
	for (i = 0; i < glyphs.length; i++) {
		g = glyphs[i];
		p = useCodes ? g.code : g.nb;
		if (p < 0 || p > 127 || g.w <= 0) continue;
		if (!useCodes && g.amb) continue;
		if (F[p] > 0) ratios.push(g.w / F[p]);
	}
	if (ratios.length < 3) return null;
	var em = t3med(ratios);
	if (em <= 0) return null;
	var hits = 0, scor = 0, dw = {};
	for (i = 0; i < glyphs.length; i++) {
		g = glyphs[i];
		p = useCodes ? g.code : t3pos(g, F, em);
		if (p < 0 || p > 127 || g.w <= 0 || !(F[p] > 0)) continue;
		scor++;
		if (Math.abs(g.w / (em * F[p]) - 1) <= t3tol(g.w)) {
			hits++;
			dw[Math.round(F[p] * 1000)] = 1;
		}
	}
	var nd = 0, k;
	for (k in dw) nd++;
	return { em: em, hits: hits, scorable: scor, distinct: nd,
		score: scor ? hits / scor : 0 };
}
function completeType3(doc) {
	var n;
	try { n = doc.countObjects(); } catch (e) { return; }
	var lig = { 11: 'ff', 12: 'fi', 13: 'fl', 14: 'ffi', 15: 'ffl' };
	var texy = { 123: '\\u2013', 124: '\\u2014', 92: '\\u201C', 34: '\\u201D' };
	for (var i = 1; i < n; i++) {
		var r;
		try { r = doc.newIndirect(i, 0).resolve(); } catch (e) { continue; }
		if (!r || !r.isDictionary || !r.isDictionary()) continue;
		var st = r.get('Subtype');
		if (!st || st.toString() != '/Type3') continue;
		var enc = r.get('Encoding');
		if (!enc || enc.isNull()) continue;
		enc = enc.resolve();
		if (!enc.isDictionary()) continue;
		var diffs = enc.get('Differences');
		if (!diffs || diffs.isNull() || !diffs.isArray()) continue;
		// Differences: an int sets the next code, a name takes it
		var names = {}, code = 0, hasDigit = false, controlOutside = false;
		for (var k = 0; k < diffs.length; k++) {
			var el = diffs.get(k);
			if (el.isInteger()) { code = parseInt(el.toString()); continue; }
			if (!el.isName()) { code++; continue; }
			var nm = el.toString().substring(1);
			nm = nm.replace(/#([0-9A-Fa-f][0-9A-Fa-f])/g, function (m, h) {
				return String.fromCharCode(parseInt(h, 16));
			});
			if (nm.length == 3 && nm.charAt(0) == '#') {
				var hb = parseInt(nm.substring(1), 16);
				if (!isNaN(hb)) nm = String.fromCharCode(hb);
			}
			names[code] = nm;
			if (nm.length == 1) {
				var nb = nm.charCodeAt(0);
				if (nb >= 0x30 && nb <= 0x39) hasDigit = true;
				if (nb < 0x20 && (nb < 0x0B || nb > 0x0F)) controlOutside = true;
			}
			code++;
		}
		var textish = hasDigit || !controlOutside;
		// proportional type proven by its own Widths; no proof, no remap
		var fc = r.get('FirstChar');
		fc = (fc && !fc.isNull()) ? parseInt(fc.toString()) : 0;
		var varied = false, w = r.get('Widths'), seen = -1, wid = {};
		if (w && !w.isNull()) {
			w = w.resolve();
			if (w.isArray()) {
				for (var wi = 0; wi < w.length; wi++) {
					var wv = parseFloat(w.get(wi).toString());
					if (wv > 0) {
						wid[fc + wi] = wv;
						if (seen < 0) seen = wv;
						else if (Math.abs(wv - seen) > 0.01) varied = true;
					}
				}
			}
		}
		var covered = {}, keptChar = [], keptRange = [];
		var tu = r.get('ToUnicode');
		if (tu && !tu.isNull()) {
			var text = '';
			try { text = tu.readStream().asString(); } catch (e) { text = ''; }
			var sect = /(beginbfchar|beginbfrange)([^]*?)(endbfchar|endbfrange)/g;
			var m;
			while ((m = sect.exec(text)) !== null) {
				var isrange = m[1] == 'beginbfrange';
				var pair = /<([0-9A-Fa-f]+)>[ \\t\\r\\n]*<([0-9A-Fa-f]+)>[ \\t\\r\\n]*(<([0-9A-Fa-f]+)>)?/g;
				var e2;
				while ((e2 = pair.exec(m[2])) !== null) {
					var a = parseInt(e2[1], 16);
					if (isrange && e2[3]) {
						var b = parseInt(e2[2], 16);
						for (var c = a; c <= b; c++) covered[c] = true;
						keptRange.push([a, b, parseInt(e2[4], 16),
							'<' + e2[1] + '> <' + e2[2] + '> <' + e2[4] + '>']);
					} else if (!isrange) {
						covered[a] = true;
						keptChar.push([a, e2[2].toUpperCase(),
							'<' + e2[1] + '> <' + e2[2] + '>']);
					}
				}
			}
		}
		// the width fingerprint: which Computer Modern family is this?
		var glyphs = [];
		for (var cs in names) {
			var cc = parseInt(cs), nm1 = names[cs];
			var nb1 = nm1.length == 1 ? nm1.charCodeAt(0)
				: (nm1 == ';;' ? 0x3B : -1);
			glyphs.push({ code: cc, nb: nb1, w: wid[cc] || 0,
				dbl: nm1 == ';;', amb: nb1 == 0x3B || nb1 == 0x2C });
		}
		var best = null, byFam = {}, fam, fit, fitC;
		for (fam in T3W) {
			var F1 = t3w(fam);
			fit = t3fit(glyphs, F1, false);
			fitC = t3fit(glyphs, F1, true);
			// a tie keeps the names: dvips names glyphs honestly, and
			// where names equal codes both readings agree anyway
			if (fitC && (!fit || fitC.score > fit.score)) {
				fitC.codes = true;
				fit = fitC;
			}
			if (!fit) continue;
			fit.fam = fam;
			fit.kind = T3FAM[fam];
			fit.F = F1;
			if (!byFam[fit.kind] || fit.score > byFam[fit.kind].score)
				byFam[fit.kind] = fit;
			if (!best || fit.score > best.score) best = fit;
		}
		// small subsets must fit perfectly; every winner needs three
		// distinct canonical widths (a digits-only or typewriter
		// subset proves nothing) and a clear margin over the best of
		// every other family
		var win = null;
		if (best && best.scorable >= 3 && best.distinct >= 3 &&
			best.score >= (best.scorable < 5 ? 1.0 : 0.85)) {
			var rival = 0;
			for (var k2 in byFam)
				if (k2 != best.kind && byFam[k2].score > rival)
					rival = byFam[k2].score;
			if (best.score - rival >= 0.1) win = best;
		}
		var chars = [], ranges = [], c3, tgt, t;
		if (win && win.kind != 'text') {
			// a fingerprinted math font: the family table reads every
			// positioned glyph, the shipped entries included - subset
			// CMaps lie (fpio maps a script M to V, a C to E) - and a
			// byte the table leaves blank stays honestly unmapped
			var dtab = t3u(win.kind), dec = {}, changed = false;
			for (var gi = 0; gi < glyphs.length; gi++) {
				var g2 = glyphs[gi];
				var p2 = win.codes ? g2.code : t3pos(g2, win.F, win.em);
				if (p2 >= 0 && p2 <= 127 && dtab[p2]) dec[g2.code] = dtab[p2];
			}
			for (c3 in dec) {
				chars.push('<' + hx2(parseInt(c3)) + '> <' + dec[c3] + '>');
				if (!covered[c3]) changed = true;
			}
			for (var ki = 0; ki < keptChar.length; ki++) {
				if (dec[keptChar[ki][0]] === undefined)
					chars.push(keptChar[ki][2]);
				else if (dec[keptChar[ki][0]] != keptChar[ki][1])
					changed = true;
			}
			for (var ri = 0; ri < keptRange.length; ri++) {
				var kr = keptRange[ri], whole = true;
				for (var c4 = kr[0]; c4 <= kr[1]; c4++) {
					if (dec[c4] === undefined) whole = false;
					else if (dec[c4] != hx4(kr[2] + (c4 - kr[0]))) changed = true;
				}
				if (whole) continue;
				for (c4 = kr[0]; c4 <= kr[1]; c4++)
					if (dec[c4] === undefined)
						chars.push('<' + hx2(c4) + '> <' +
							hx4(kr[2] + (c4 - kr[0])) + '>');
			}
			if (!changed) continue;
		} else {
			// text or unfingerprinted: unit 1's name reading, plus the
			// text table where a text fingerprint licenses it - Greek
			// capitals, ligatures, accents, the OT1 dashes - and a
			// width veto where the metrics contradict a printable
			// name, so byte zero's junk name cannot print as prose
			var ttab = win ? t3u('text') : null;
			var added = 0;
			for (var gj = 0; gj < glyphs.length; gj++) {
				var g3 = glyphs[gj];
				c3 = g3.code;
				if (covered[c3]) continue;
				if (g3.dbl && !win) continue;
				var p3 = win ? t3pos(g3, win.F, win.em) : g3.nb;
				var u = null;
				tgt = null;
				if (p3 < 0 || p3 > 127) continue;
				if (win && ttab[p3]) {
					tgt = ttab[p3];
				} else if (p3 >= 0x20 && p3 < 0x7F) {
					if (win && g3.w > 0 && win.F[p3] > 0 &&
						Math.abs(g3.w / (win.em * win.F[p3]) - 1) > t3tol(g3.w))
						continue;
					u = (textish && varied && texy[p3]) ? texy[p3]
						: String.fromCharCode(p3);
				} else if (textish && lig[p3]) {
					u = lig[p3];
				}
				if (u !== null) {
					tgt = '';
					for (t = 0; t < u.length; t++) tgt += hx4(u.charCodeAt(t));
				}
				if (tgt === null) continue;
				chars.push('<' + hx2(c3) + '> <' + tgt + '>');
				added++;
			}
			for (var kj = 0; kj < keptChar.length; kj++)
				chars.push(keptChar[kj][2]);
			for (var rj = 0; rj < keptRange.length; rj++)
				ranges.push(keptRange[rj][3]);
			if (added == 0) continue;
		}
		var out = '/CIDInit /ProcSet findresource begin 12 dict begin begincmap\\n' +
			'/CMapName /T3UVX def /CMapType 2 def\\n' +
			'1 begincodespacerange <00> <ff> endcodespacerange\\n';
		if (chars.length > 0)
			out += chars.length + ' beginbfchar\\n' + chars.join('\\n') + '\\nendbfchar\\n';
		if (ranges.length > 0)
			out += ranges.length + ' beginbfrange\\n' + ranges.join('\\n') + '\\nendbfrange\\n';
		out += 'endcmap CMapName currentdict /CMap defineresource pop end end\\n';
		try { r.put('ToUnicode', doc.addStream(out, null)); } catch (e) {}
	}
}
completeType3(doc);

function esc(s) {
	if (s.indexOf('\\\\') < 0 && s.indexOf('\"') < 0) return s;
	var out = '';
	for (var i = 0; i < s.length; i++) {
		var c = s.charAt(i);
		if (c === '\\\\' || c === '\"') out += '\\\\';
		out += c;
	}
	return out;
}
function clen(s) {
	var n = 0;
	for (var i = 0; i < s.length; i++) {
		var u = s.charCodeAt(i);
		if (u < 0xDC00 || u > 0xDFFF) n++;
	}
	return n;
}
function asc(a) { return a.sort(function (x, y) { return x - y; }); }
function median(a) {
	return a.length === 0 ? null : asc(a)[Math.floor(a.length / 2)];
}
function quantile(a, f) {
	if (a.length === 0) return null;
	asc(a);
	var i = Math.floor(f * a.length);
	return a[i > a.length - 1 ? a.length - 1 : i];
}
function num(v) { return v === null ? 'nil' : v.toFixed(5); }
function sym(v) { return v ? 't' : 'nil'; }
// point-space values normalize at emit: positions against the page
// origin, extents by scale alone
function px(v) { return v === null ? 'nil' : ((v - PX) / PW).toFixed(5); }
function py(v) { return v === null ? 'nil' : ((v - PY) / PH).toFixed(5); }
function wx(v) { return v === null ? 'nil' : (v / PW).toFixed(5); }
function hy(v) { return v === null ? 'nil' : (v / PH).toFixed(5); }

var buf = [];
var lastFontObj = null, fontName = '', fontBold = false, fontItalic = false;
var line = null;
var pageno = 0, PX = 0, PY = 0, PW = 1, PH = 1;

var walker = {
	beginLine: function () {
		line = { text: '', off: 0, runs: [],
			 x0: null, x1: null, top: null, bot: null,
			 spaces: [], advances: [],
			 synth: 0, sawInk: false,
			 openX: null, fwX: null, lastInkX1: null, prevX: null };
	},
	onChar: function (c, origin, font, size, quad, color, flags) {
		if (font !== lastFontObj) {
			lastFontObj = font;
			fontName = font.getName();
			fontBold = font.isBold();
			fontItalic = font.isItalic();
		}
		var x0 = quad[0] < quad[2] ? quad[0] : quad[2];
		var x1 = quad[0] < quad[2] ? quad[2] : quad[0];
		var y0 = quad[1] < quad[5] ? quad[1] : quad[5];
		var y1 = quad[1] < quad[5] ? quad[5] : quad[1];
		if (quad[4] < x0) x0 = quad[4];
		if (quad[4] > x1) x1 = quad[4];
		if (quad[6] < x0) x0 = quad[6];
		if (quad[6] > x1) x1 = quad[6];
		if (quad[3] < y0) y0 = quad[3];
		if (quad[3] > y1) y1 = quad[3];
		if (quad[7] < y0) y0 = quad[7];
		if (quad[7] > y1) y1 = quad[7];
		var sp = c === ' ' || c === '\\t' || c === '\\u00A0';
		var L = line;
		var runs = L.runs;
		var run = runs.length === 0 ? null : runs[runs.length - 1];
		if (run === null
		    || (!sp
			&& (fontName !== run.f
			    || (size < run.szp ? run.szp - size : size - run.szp)
			    > 0.0005 * PH
			    || (y1 < run.oyp ? run.oyp - y1 : y1 - run.oyp)
			    > 0.0004 * PH))) {
			run = { off: L.off, len: 0, x0: null, x1: null,
				oy: null, qh: null, ink: 0, chars: 0,
				f: fontName, szp: size, oyp: y1,
				bold: fontBold, it: fontItalic };
			runs.push(run);
		}
		var n = c.length === 1 ? 1 : clen(c);
		run.len += n;
		L.off += n;
		L.text += c;
		if (flags & 4) L.synth += 1;
		if (sp) {
			// a synthetic space's quad is the engine's own gap
			// estimate: a DVI-born page has no other spaces at all
			if (x1 > x0) L.spaces.push(x1 - x0);
			if (L.sawInk && L.fwX === null) L.fwX = L.lastInkX1;
		} else {
			if (L.x0 === null || x0 < L.x0) L.x0 = x0;
			if (x1 > L.x1) L.x1 = x1;
			if (L.top === null || y0 < L.top) L.top = y0;
			if (y1 > L.bot) L.bot = y1;
			if (!L.sawInk) { L.sawInk = true; L.openX = x0; }
			L.lastInkX1 = x1;
			if (run.x0 === null || x0 < run.x0) run.x0 = x0;
			if (x1 > run.x1) run.x1 = x1;
			if (run.oy === null) { run.oy = y1; run.qh = y1 - y0; }
			run.ink += x1 - x0;
			run.chars += 1;
		}
		if (L.prevX !== null) {
			var step = x0 - L.prevX;
			if (step > 0) L.advances.push(step);
		}
		L.prevX = x0;
	},
	endLine: function () {
		var L = line;
		if (L === null || L.text.length === 0) { line = null; return; }
		var cv = null;
		var a = L.advances;
		if (a.length > 1) {
			var mean = 0, i;
			for (i = 0; i < a.length; i++) mean += a[i];
			mean /= a.length;
			if (mean > 0) {
				var vsum = 0;
				for (i = 0; i < a.length; i++)
					vsum += (a[i] - mean) * (a[i] - mean);
				cv = Math.sqrt(vsum / a.length) / mean;
			}
		}
		var fw = null;
		if (L.sawInk)
			fw = (L.fwX === null ? L.lastInkX1 : L.fwX) - L.openX;
		// the 0.8 quantile of glyph heights, run-weighted: chars of one
		// run share a font box, so the run carries their height once
		var levels = [], total = 0, r, q;
		for (r = 0; r < L.runs.length; r++) {
			q = L.runs[r];
			if (q.oy !== null) {
				levels.push([q.qh, q.chars]);
				total += q.chars;
			}
		}
		levels.sort(function (u, v) { return u[0] - v[0]; });
		var height = null, seen = 0, want = Math.floor(0.8 * total);
		for (r = 0; r < levels.length; r++) {
			seen += levels[r][1];
			if (seen > want) { height = levels[r][0]; break; }
		}
		var rs = '';
		for (r = 0; r < L.runs.length; r++) {
			q = L.runs[r];
			rs += (r === 0 ? '(' : ' ')
				+ '(' + q.off + ' ' + q.len
				+ ' ' + px(q.x0) + ' ' + px(q.x1)
				+ ' ' + py(q.oy) + ' ' + hy(q.qh)
				+ ' ' + hy(q.szp) + ' ' + sym(q.bold) + ' ' + sym(q.it)
				+ ' \"' + esc(q.f) + '\")';
		}
		rs += ')';
		buf.push('(' + pageno + ' ' + px(L.x0) + ' ' + py(L.top)
			 + ' ' + px(L.x1) + ' ' + py(L.bot)
			 + ' ' + hy(height)
			 + ' ' + wx(median(L.spaces)) + ' ' + num(cv)
			 + ' ' + wx(fw) + ' ' + L.synth + ' ' + rs
			 + ' \"' + esc(L.text) + '\")');
		line = null;
	}
};

for (var p = first; p <= last; p++) {
	var page = doc.loadPage(p - 1);
	var b = page.getBounds();
	pageno = p;
	PX = b[0]; PY = b[1]; PW = b[2] - b[0]; PH = b[3] - b[1];
	page.toStructuredText('preserve-whitespace').walk(walker);
	if (buf.length > 400) { print(buf.join('\\n')); buf = []; }
}
if (buf.length > 0) print(buf.join('\\n'));
"
  "The mutool walker, written to a file by `pdf-text--walker-file'.
Lives in the elisp so the reader and its walker can never drift apart,
whatever a package manager copies where.")

(defvar pdf-text--walker-cache nil
  "Path the walker source is written at, once per session.")

(defun pdf-text--walker-file ()
  "The walker source on disk, written on first use.
The cached file's contents are checked against the source, because a
live session that reloads a newer package keeps the old defvar and
the old file - and a stale walker under fresh elisp is exactly the
drift keeping the source in the elisp is meant to rule out."
  (if (and pdf-text--walker-cache
           (file-readable-p pdf-text--walker-cache)
           (with-temp-buffer
             (insert-file-contents pdf-text--walker-cache)
             (string= (buffer-string) pdf-text--walker-source)))
      pdf-text--walker-cache
    (setq pdf-text--walker-cache
          (make-temp-file "pdf-text-walker" nil ".js"
                          pdf-text--walker-source))))

(defvar pdf-text-mupdf-workers 4
  "Walker processes a whole-book extraction may fan out over.
Capped by `num-processors'.  The walker is compute-bound in mutool's
own interpreter, so separate processes are the only parallelism.")

(defvar pdf-text-mupdf-pool-min 64
  "Pages below which extraction stays on one walker process.
Opening the document costs each worker a fixed price a short window
never earns back.")

(defun pdf-text--mupdf-call (file first last)
  "One walker process over FILE's pages FIRST to LAST, as a string.
Signals with mutool's own words when the extraction fails, and names
the missing program when there is no mutool at all."
  (unless (executable-find pdf-text-mupdf-program)
    (error "No %s executable; the text extraction needs MuPDF (brew install mupdf-tools)"
           pdf-text-mupdf-program))
  (let ((stderr (make-temp-file "pdf-text-mupdf" nil ".err")))
    (unwind-protect
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8)
                (status (call-process pdf-text-mupdf-program nil
                                      (list (current-buffer) stderr) nil
                                      "run" (pdf-text--walker-file)
                                      (expand-file-name file)
                                      (number-to-string first)
                                      (number-to-string last))))
            (unless (eql status 0)
              (error "mutool: %s"
                     (with-temp-buffer
                       (insert-file-contents stderr)
                       (string-trim (buffer-string)))))
            (buffer-string)))
      (delete-file stderr))))

(defun pdf-text--mupdf-output (file first last)
  "The walker's output over FILE's pages FIRST to LAST, as a string.
Past `pdf-text-mupdf-pool-min' pages the range fans out over
`pdf-text-mupdf-workers' mutool processes, each walking its own
stretch of the document; a stretch whose worker fails retries on a
synchronous call, so a crashed worker costs speed, never a page.
Order does not matter to the parse - every form names its page."
  (let* ((total (1+ (- last first)))
         (workers (max 1 (min pdf-text-mupdf-workers (num-processors)))))
    (if (or (< total pdf-text-mupdf-pool-min) (= workers 1))
        (pdf-text--mupdf-call file first last)
      (let* ((chunk (ceiling total workers))
             (ranges (cl-loop for a from first to last by chunk
                              collect (cons a (min last (+ a chunk -1)))))
             (jobs (mapcar
                    (lambda (range)
                      (let* ((buf (generate-new-buffer " *pdf-text-mupdf*" t))
                             (proc
                              (condition-case nil
                                  ;; a worker's warnings are noise; the
                                  ;; synchronous retry captures them
                                  ;; when they matter
                                  (let ((process-connection-type nil)
                                        (coding-system-for-read 'utf-8))
                                    (start-process
                                     "pdf-text-mutool" buf
                                     shell-file-name shell-command-switch
                                     (concat
                                      (mapconcat
                                       #'shell-quote-argument
                                       (list pdf-text-mupdf-program "run"
                                             (pdf-text--walker-file)
                                             (expand-file-name file)
                                             (number-to-string (car range))
                                             (number-to-string (cdr range)))
                                       " ")
                                      " 2>/dev/null")))
                                (error nil))))
                        (when proc
                          (set-process-query-on-exit-flag proc nil))
                        (list proc buf range)))
                    ranges)))
        (unwind-protect
            (progn
              (while (cl-some (lambda (job)
                                (and (car job) (process-live-p (car job))))
                              jobs)
                (accept-process-output nil 0.05))
              (mapconcat
               (lambda (job)
                 (pcase-let ((`(,proc ,buf ,range) job))
                   (if (and proc (eql (process-exit-status proc) 0))
                       (with-current-buffer buf (buffer-string))
                     (pdf-text--mupdf-call file (car range) (cdr range)))))
               jobs))
          (dolist (job jobs) (kill-buffer (nth 1 job))))))))

(defun pdf-text--run-ink (run)
  "The ink width RUN covers, zero for a wordless one."
  (if (and (nth 2 run) (nth 3 run)) (- (nth 3 run) (nth 2 run)) 0.0))

(defun pdf-text--run-baseline (runs)
  "The typeset baseline of RUNS and its glyph height, as (BASE . HEIGHT).
The port of the glyph-bottom vote onto font runs: chars agreeing on
their descent line already arrive as one run, so the runs cluster
where the glyphs once did.  The widest cluster by ink names the
baseline, every cluster within a raise of it - measured by its own
glyph height - contests, and the highest wins, so a fragment set
mostly in subscript cedes to its few full-size glyphs.  Nil without
ink."
  (when-let* ((ink (seq-filter (lambda (r) (nth 4 r)) runs)))
    (let* ((sorted (sort (copy-sequence ink)
                         (lambda (a b) (< (nth 4 a) (nth 4 b)))))
           (gap (* 0.04 (pdf-text--quantile
                         (mapcar (lambda (r) (nth 5 r)) sorted) 0.5)))
           (levels nil)
           (current (list (car sorted))))
      (dolist (r (cdr sorted))
        (if (< (- (nth 4 r) (nth 4 (car current))) gap)
            (push r current)
          (push current levels)
          (setq current (list r))))
      (push current levels)
      (let* ((stats (mapcar
                     (lambda (level)
                       (let ((widest (cl-reduce
                                      (lambda (a b)
                                        (if (< (pdf-text--run-ink a)
                                               (pdf-text--run-ink b))
                                            b a))
                                      level)))
                         (list (apply #'max (mapcar (lambda (r) (nth 4 r))
                                                    level))
                               (nth 5 widest)
                               (apply #'+ (mapcar #'pdf-text--run-ink level)))))
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
        (cons (nth 0 ref) (nth 1 ref))))))

(defun pdf-text--run-dir (run base height)
  "Which script RUN reads as against baseline BASE and body HEIGHT.
`up', `down', or nil for a run on the baseline, too large to be a
script, or past `pdf-text-script-reach' - another typeset line the
record carries, not a script of this one."
  (when-let* ((oy (nth 4 run))
              (qh (nth 5 run))
              (offset (- oy base)))
    (when (and (< qh (* pdf-text-script-size height))
               (< (abs offset) (* pdf-text-script-reach height)))
      (cond ((< offset (* (- pdf-text-script-raise) height)) 'up)
            ((< (* pdf-text-script-drop height) offset) 'down)))))

(defvar pdf-text-run-space 0.2
  "Em fraction of gap at a font switch that reads as a word space.
TeX writes no space character between an operator glyph and its
operand - the gap is the math spacing itself, 0.29 em and up where
measured - while the kerns and italic corrections a font switch
leaves inside a word stay under 0.05 em.  A gap past this fraction
of the em size gets the space the page set.")

(defvar pdf-text--degenerate-ems nil
  "Whether the document's em sizes are garbage - the Type 3 class.
A dvips-era Type 3 bitmap-font document leaves its FontMatrix scale
in the sizes MuPDF reports: ems of 0.0001 against glyph heights a
hundredfold larger, zeroing every em-multiplied threshold.  Bound by
`pdf-text--mupdf-parse' over a document whose modal em sits orders
of magnitude under its modal height; the glyph height then stands in
for the em wherever a threshold multiplies it.")

(defun pdf-text--run-markup (text runs base height space)
  "TEXT with script runs wrapped as org ^{...}/_{...} markup.
RUNS are the walker's font runs over TEXT, BASE and HEIGHT the line's
typeset baseline and glyph height from `pdf-text--run-baseline', SPACE
its median word gap.  A run's edge spaces stay plain outside the wrap;
a lone space left between a run and its host at a fraction of a word
gap - a font switch, not a space the page set - goes, so the markup
attaches where the page attaches.  Two plain runs set a word gap
apart with no space character between them - `pdf-text-run-space' of
the em size - get one: the page spaced them, the extractor did not.
A run that fails its own test - symbols alone without a symbol's
offset, a brace among the characters - reads as the plain text it
was, and literal ^{ and _{ pairs from the page are broken so org
never parses them.  Under `pdf-text--degenerate-ems' the line's
glyph height stands in for the em the FontMatrix scale zeroed."
  (let ((out nil)
        (last-ink nil)
        (last-plain-size nil))
    (dolist (run runs)
      (let* ((seg (substring text (nth 0 run) (+ (nth 0 run) (nth 1 run))))
             (dir (pdf-text--run-dir run base height)))
        (if (null dir)
            (progn
              (when (and last-plain-size last-ink (nth 2 run)
                         (stringp (car out))
                         (not (string-suffix-p " " (car out)))
                         (not (string-prefix-p " " seg))
                         (not (string-empty-p seg))
                         ;; a letter run continuing into lowercase is a
                         ;; word the font change split - a swash
                         ;; capital's kern gap runs wide - and words
                         ;; stay whole; the operator gaps this rule is
                         ;; for always have a symbol on one side
                         (not (let ((case-fold-search nil))
                                (and (string-match-p "[[:alpha:]]\\'"
                                                     (car out))
                                     (string-match-p "\\`[[:lower:]]" seg))))
                         (<= (* pdf-text-run-space
                                (let ((em (max last-plain-size
                                               (or (nth 6 run) 0))))
                                  (if pdf-text--degenerate-ems
                                      (max em height)
                                    em)))
                             (- (nth 2 run) last-ink)))
                (push " " out))
              (push (pdf-text--escape-literals seg) out)
              (setq last-plain-size (nth 6 run)))
          (let* ((trimmed (string-trim seg))
                 (symbols (not (string-match-p "[[:alnum:]]" trimmed)))
                 (weak (and symbols
                            (< (abs (- (nth 4 run) base))
                               (* pdf-text-script-symbol-offset height)))))
            (setq last-plain-size nil)
            (if (or weak (string-match-p "[{}]" trimmed)
                    (string-empty-p trimmed))
                (push (pdf-text--escape-literals seg) out)
              ;; a lone space before the run at under half a word gap
              ;; is the font switch showing, not a space the page set
              (when (and (stringp (car out))
                         (string-suffix-p " " (car out))
                         (not (string-suffix-p "  " (car out)))
                         space last-ink (nth 2 run)
                         (< (- (nth 2 run) last-ink) (* 0.5 space)))
                (setcar out (substring (car out) 0 -1)))
              (when (string-match "\\`[ \t]+" seg)
                (push (match-string 0 seg) out))
              (push (concat (if (eq dir 'up) "^{" "_{") trimmed "}") out)
              (when (string-match "[ \t]+\\'" seg)
                (push (match-string 0 seg) out)))))
        (when (nth 3 run) (setq last-ink (nth 3 run)))))
    (apply #'concat (nreverse out))))

(defun pdf-text--mupdf-record (form)
  "The `pdf-text-line' one walker FORM describes.
The text carries org script markup for font runs set smaller than and
offset from the line's baseline; the dominant run by ink names the
line's font.  A line with no ink keeps only its text, like the
text-only fallback records."
  (pcase-let* ((`(,_page ,x0 ,top ,x1 ,bot ,height ,space ,cv ,fw ,synth
                  ,runs ,text)
                form)
               (ink (seq-filter (lambda (r) (nth 4 r)) runs)))
    (if (null ink)
        (pdf-text-line-create
         :text (pdf-text--strip-unprinted (pdf-text--escape-literals text)))
      (let* ((ref (pdf-text--run-baseline runs))
             (dominant (cl-reduce (lambda (a b)
                                    (if (< (pdf-text--run-ink a)
                                           (pdf-text--run-ink b))
                                        b a))
                                  ink)))
        (pdf-text-line-create
         ;; space runs collapse to the shape poppler served, the shape
         ;; every text rule was tuned on: MuPDF writes the page's own
         ;; runs - double-spaced sentences, wide-set section numbers,
         ;; the paragraph's first-line indent as leading spaces - and
         ;; the tabular-alignment and indent tests would read structure
         ;; into them.  The record's geometry carries the indent as x0.
         ;; The right-trim precedes the strip so a line-final soft
         ;; hyphen stays line-final for the wrap join to read.
         :text (pdf-text--strip-unprinted
                (string-trim-right
                 (replace-regexp-in-string
                  "\\([^ ]\\)  +" "\\1 "
                  (string-trim-left
                   (if (and ref (cdr runs))
                       (pdf-text--run-markup text runs (car ref) (cdr ref)
                                             space)
                     (pdf-text--escape-literals text))
                   " +"))
                 "[ \t]+"))
         :x0 x0 :x1 x1 :top top :bot bot
         :base (car ref)
         :height height :space space :cv cv :first-width fw
         :font (nth 9 dominant) :size (nth 6 dominant)
         :bold (nth 7 dominant) :italic (nth 8 dominant)
         :synth synth
         ;; the dominant run by ink can hand the record another face
         ;; than the one the line opens in - a sans identifier inside a
         ;; bold heading - and the opening face is what heading rules
         ;; key on
         :lead-font (nth 9 (car ink)) :lead-bold (nth 7 (car ink)))))))

(defun pdf-text--degenerate-ems-p (em height)
  "Whether EM against HEIGHT reads as the Type 3 class.
An order of magnitude of headroom on either side: a healthy font's
em runs level with its quad height, however tall a stray display
delimiter makes one line, while a Type 3 bitmap font's em is a
hundredth of it."
  (and em height (< (* 8 em) height)))

(defun pdf-text--forms-degenerate-ems-p (forms)
  "Whether the walker FORMS carry Type 3 em sizes, medians speaking."
  (let (heights ems)
    (dolist (form forms)
      (when-let* ((height (nth 5 form)))
        (push height heights))
      (dolist (run (nth 10 form))
        (when-let* ((em (nth 6 run)))
          (push em ems))))
    (pdf-text--degenerate-ems-p (pdf-text--quantile ems 0.5)
                                (pdf-text--quantile heights 0.5))))

(defun pdf-text--mupdf-parse (output first last)
  "OUTPUT of the walker as per-page record lists, pages FIRST to LAST.
A list in page order; nil where a page emitted no lines - the caller's
cue to fall back on plain text extraction.  The forms are read whole
before any record builds, so `pdf-text--degenerate-ems' can bind over
the build when the document's em sizes are the Type 3 class."
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (let ((pages (make-vector (1+ (- last first)) nil))
          forms form)
      (while (setq form (condition-case nil (read (current-buffer))
                          (end-of-file nil)))
        (when (and (consp form) (integerp (car form)))
          (push form forms)))
      (setq forms (nreverse forms))
      (let ((pdf-text--degenerate-ems (pdf-text--forms-degenerate-ems-p
                                       forms)))
        (dolist (form forms)
          (let ((i (- (car form) first)))
            (when (and (<= 0 i) (< i (length pages)))
              (aset pages i (cons (pdf-text--mupdf-record form)
                                  (aref pages i)))))))
      (mapcar #'nreverse (append pages nil)))))

(defun pdf-text--mupdf-pages (file first last)
  "Line records for FILE's pages FIRST to LAST, one list per page.
Nil for a page MuPDF finds no text on."
  (pdf-text--mupdf-parse (pdf-text--mupdf-output file first last)
                         first last))

(defun pdf-text--page-marker-p (text)
  "Whether TEXT is a bare page marker: a number, a numeral, or a rule."
  (let ((trimmed (string-trim text)))
    (and (not (string-blank-p trimmed))
         (string-match-p
          "\\`\\(?:[Pp]age[ \t]*\\)?\\(?:[0-9]+\\|[ivxlcdmIVXLCDM]+\\|[|·—–-]+\\)\\'"
          trimmed))))

(defun pdf-text--text-span (pages left right text-left space leading)
  "Modal top and bottom baselines of the body across PAGES, as a cons.
A page's body starts at its first column-anchored line - ink from the
leftmost text edge, at least half the column wide - and ends at its
last; LEFT and RIGHT give the column, TEXT-LEFT the edge where it
runs left of the modal column, SPACE the anchoring slack and LEADING
the baseline tolerance.  The anchor is the LEFTMOST edge on purpose:
a two-column paper's running head starts exactly where the right
column does, and against the leftmost edge it never anchors.  A line
sharing its baseline with a bare page marker never anchors either -
the folio names the furniture row, and a head set at the text edge
and running wide reads as a body line by every other measure.  The
mode over pages is the document's, so a chapter opener starting low
or a page with no anchored line at all does not move it."
  (let ((min-width (and left right (< left right) (* 0.5 (- right left))))
        (edge (or text-left left))
        (slack (* 2 (or space 0.005)))
        (tolerance (* 0.5 (or leading 0.02)))
        tops bottoms)
    (when (and min-width edge)
      (dolist (lines pages)
        (let ((markers (delq nil
                             (mapcar
                              (lambda (line)
                                (and (pdf-text-line-base line)
                                     (pdf-text--page-marker-p
                                      (pdf-text-line-text line))
                                     (pdf-text-line-base line)))
                              lines)))
              top bottom)
          (dolist (line lines)
            (when-let* ((x0 (pdf-text-line-x0 line))
                        (x1 (pdf-text-line-x1 line))
                        (base (pdf-text-line-base line))
                        ((<= min-width (- x1 x0)))
                        ((< (abs (- x0 edge)) slack))
                        ((not (cl-some (lambda (marker)
                                         (< (abs (- base marker)) tolerance))
                                       markers))))
              (unless top (setq top base))
              (setq bottom base)))
          (when top
            (push top tops)
            (push bottom bottoms)))))
    (cons (pdf-text--mode-value tops 0.005)
          (pdf-text--mode-value bottoms 0.005))))

(defun pdf-text--profile (pages)
  "Modal body geometry of PAGES, each a list of `pdf-text-line'.
A plist: :height glyph height, :leading baseline step, :left and
:right column edges, :space word gap, :em the em size the records
report - degenerate em against height marks the Type 3 class for the
repairs, the way `pdf-text--degenerate-ems' marks it for the parse.
Document-wide, because a page of listings or a page of table rows
has no representative body geometry of its own."
  (let (heights lefts rights widths spaces leadings ems)
    (dolist (lines pages)
      (let (prev)
        (dolist (line lines)
          (when (pdf-text-line-x0 line)
            (push (pdf-text-line-height line) heights)
            (when (pdf-text-line-size line)
              (push (pdf-text-line-size line) ems))
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
    (let* ((height (pdf-text--mode-value heights 0.001))
           (leading (pdf-text--mode-value leadings 0.002))
           (left (pdf-text--mode-value lefts 0.005 widths))
           (right (pdf-text--mode-value rights 0.005 widths))
           (text-left (car (pdf-text--strong-edges lefts 0.005 widths)))
           (space (let ((median (pdf-text--quantile spaces 0.5)))
                    (cond ((and median height) (max median (/ height 3.0)))
                          (median)
                          (height (/ height 3.0)))))
           (area (pdf-text--text-span pages left right text-left space
                                      leading)))
      (list :height height
            :leading leading
            :left left
            :right right
            ;; the modal column is ONE column of a two-column document;
            ;; the text area spans them all, which is what tells a
            ;; facing column's cells from a margin note's
            :text-left text-left
            :text-right (cdr (pdf-text--strong-edges rights 0.005 widths))
            ;; where the body starts and ends down the page, modally:
            ;; running heads and folios live outside this span, however
            ;; tight against it a paper sets them
            :text-top (car area)
            :text-bottom (cdr area)
            :space space
            :em (pdf-text--quantile ems 0.5)))))

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

(defun pdf-text--outside-text-area-p (base profile)
  "Whether baseline BASE sits above or below PROFILE's body span.
Nil when the profile carries no span.  Half a leading of slack keeps
the body's own first and last lines inside."
  (let ((top (plist-get profile :text-top))
        (bottom (plist-get profile :text-bottom))
        (slack (* 0.5 (or (plist-get profile :leading) 0.02))))
    (or (and top (< base (- top slack)))
        (and bottom (< (+ bottom slack) base)))))

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
     :first-width (pdf-text-line-first-width (car records))
     :font (pdf-text-line-font widest)
     :size (pdf-text-line-size widest)
     :bold (pdf-text-line-bold widest)
     :italic (pdf-text-line-italic widest)
     :synth (apply #'+ (mapcar (lambda (r) (or (pdf-text-line-synth r) 0))
                               records))
     ;; the merged line opens where its first record opens - a section
     ;; number joined to its title keeps the number's face as its lead
     :lead-font (cl-some #'pdf-text-line-lead-font records)
     :lead-bold (when-let* ((led (cl-find-if #'pdf-text-line-lead-font
                                             records)))
                  (pdf-text-line-lead-bold led)))))

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
(defvar pdf-text-heading-height)

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
table's.  A row above the profile's body start is page furniture - a
running head beside its folio aligns like a table row on every page,
and claiming it would bar both from the margin rules.  Below the span
rows stay eligible: pages end where their content runs out, and a
table under a short page's last paragraph is still a table.
MIN-WIDTH is the gutter the cells must leave open."
  (and (<= 2 (length (cdr row)))
       (not (when-let* ((top (plist-get profile :text-top)))
              (< (car row)
                 (- top (* 0.5 (or (plist-get profile :leading) 0.02))))))
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
`pdf-text--merge-script-fragments'; contents leader fills strip and
their entries pair with their folios, by `pdf-text--strip-leaders';
multicolumn regions come back as table rows or stay lane-ordered
flows, by `pdf-text--mark-lanes'; and the aligned arrays reading
order scattered come back row by row, by
`pdf-text--reassemble-zones'.  These are the lines the reflow reads,
and the lines the corpus measures survival against: the repairs
merge and reorder records, and the only records they drop are the
fills, which carry no letter the survival stream could miss."
  (let* ((page-lines (pdf-text-clean-pages pages))
         ;; the seeded document profile reaches the repairs too: the
         ;; merge and zone thresholds are leadings and spaces, and a
         ;; window measures both differently than its book does
         (profile (or pdf-text-extra-profile (pdf-text--profile page-lines))))
    (mapcar (lambda (lines)
              ;; the far edge reads the page before any repair moves
              ;; ink: the lane unfold rewrites a facing column's
              ;; coordinates, and the deferral downstream still needs
              ;; to know the column stood there
              (let ((far (pdf-text--page-far-edge lines profile)))
                (pdf-text--mark-entry-runs
                 (pdf-text--defer-margin-notes
                  (pdf-text--reassemble-zones
                   (pdf-text--mark-lanes
                    (pdf-text--strip-leaders
                     (pdf-text--merge-script-fragments
                      (pdf-text--join-split-lines
                       (pdf-text--float-drop-caps lines profile)
                       profile)
                      profile))
                    profile)
                   profile)
                  profile far))))
            page-lines)))

(defvar pdf-text-entry-run-min 3
  "Neighbouring folio-closed lines that make a contents run.
A lone prose line can wrap right after a bare number; three in a row
is a table of contents, an index column, a list of figures.")

(defun pdf-text--mark-entry-runs (lines)
  "Tag LINES' runs of contents-style entries to render as list items.
MuPDF serves a TOC entry with its folio inline - one line, full
measure - so the short-line rule reads every entry as a wrapped line
and the entries chain into paragraphs.  `pdf-text-entry-run-min'
neighbours each closing on a short digit group are a contents run,
not prose: each keeps its own line, set as an org list item."
  (let ((vec (vconcat lines))
        (entry-re "[^0-9 ] [0-9]\\{1,4\\}\\'")
        (run nil))
    (cl-flet ((flush ()
                (when (<= pdf-text-entry-run-min (length run))
                  (dolist (i run)
                    (setf (pdf-text-line-kind (aref vec i)) 'entry)))
                (setq run nil)))
      (dotimes (i (length vec))
        (let ((line (aref vec i)))
          (if (and (null (pdf-text-line-kind line))
                   (not (pdf-text-line-claimed line))
                   (string-match-p entry-re
                                   (string-trim (pdf-text-line-text line))))
              (push i run)
            (flush))))
      (flush))
    (append vec nil)))

(defvar pdf-text-leader-min 4
  "Repeats of one glyph that make a table-of-contents leader fill.
Dots, middots, dashes, the colons a dvips math font names its
periods by byte - whatever the glyph, its repetition between an
entry's words and the folio is the fill.  Four keeps a spaced
ellipsis and a tight table row out; real fills run far longer.")

(defconst pdf-text-folio-re
  "[0-9]\\{1,4\\}\\|[ivxlcdm]\\{1,8\\}\\|[IVXLCDM]\\{1,8\\}"
  "A folio as a contents entry writes it: a digit group or a numeral.
Matched with `case-fold-search' nil, so prose words stay out of the
numeral arm.")

(defun pdf-text--leader-run-re ()
  "Regex matching one glyph repeated into a leader fill.
The backreference is what makes detection glyph-agnostic: repetition
identifies a fill however the font names the glyph."
  (format "\\([^[:alnum:][:space:]]\\)\\(?: ?\\1\\)\\{%d,\\}"
          (1- pdf-text-leader-min)))

(defun pdf-text--leader-parts (text)
  "The pieces of TEXT around a trailing leader fill, nil without one.
A cons (ENTRY . FOLIO): the words standing before the fill - empty
for a record that is the fill - and the folio closing the line after
it, nil when the folio is served as its own record instead."
  (let ((case-fold-search nil))
    (when (string-match
           (format "\\`\\(.*?\\)\\([^[:alnum:][:space:]]\\)\\(?: ?\\2\\)\\{%d,\\}[ \t]*\\(%s\\)?[ \t]*\\'"
                   (1- pdf-text-leader-min) pdf-text-folio-re)
           text)
      (cons (string-trim (match-string 1 text))
            (match-string 3 text)))))

(defun pdf-text--leader-glyph (text)
  "The glyph TEXT repeats as a bare run, with its count, or nil.
A cons (GLYPH . COUNT) when the whole of TEXT is one non-alphanumeric
glyph once or more, single spaces allowed: the shape of a fill served
one record per glyph, which only its neighbours can add up to a fill."
  (let ((trimmed (string-trim text)))
    (when (string-match-p
           "\\`\\([^[:alnum:][:space:]]\\)\\(?: ?\\1\\)*\\'" trimmed)
      (let ((glyph (aref trimmed 0)))
        (cons glyph (cl-count glyph trimmed))))))

(defun pdf-text--bare-folio-p (text)
  "Whether TEXT is nothing but a folio."
  (let ((case-fold-search nil))
    (string-match-p (format "\\`\\(?:%s\\)\\'" pdf-text-folio-re)
                    (string-trim text))))

(defun pdf-text--leader-groups (lines)
  "LINES as consecutive same-baseline groups, each a list of lines.
A line with no baseline stands alone; the tolerance is the quarter
height `pdf-text--join-split-lines' reads baselines by."
  (let (groups current prev)
    (dolist (line lines)
      (let ((base (pdf-text-line-base line)))
        (if (and current prev base
                 (< (abs (- base prev))
                    (* 0.25 (or (pdf-text-line-height line) 0.01))))
            (push line current)
          (when current (push (nreverse current) groups))
          (setq current (list line)))
        (setq prev base)))
    (when current (push (nreverse current) groups))
    (nreverse groups)))

(defun pdf-text--leader-entry (survivors)
  "SURVIVORS of one baseline merged into a single contents entry line.
Sorted by ink, joined by single spaces, marked with the entry kind
so it renders as a list item and no heading rule reads it, and
claimed like a lane row: the fill proved the entry-folio pairing,
and the margin rules would read the folio it now closes on as a
running head's - a page whose contents run starts inside the band
loses its first entry to exactly that."
  (let* ((sorted (sort (copy-sequence survivors)
                       (lambda (a b)
                         (< (or (pdf-text-line-x0 a) 0)
                            (or (pdf-text-line-x0 b) 0)))))
         (merged (if (cdr sorted)
                     (pdf-text--merge-records
                      sorted
                      (mapconcat (lambda (l)
                                   (string-trim (pdf-text-line-text l)))
                                 sorted " "))
                   (car sorted))))
    (when (null (pdf-text-line-kind merged))
      (setf (pdf-text-line-kind merged) 'entry))
    (setf (pdf-text-line-claimed merged) t)
    merged))

(defun pdf-text--leader-fills (group parts)
  "Record into PARTS which of GROUP's lines are or carry a fill.
A record long enough on its own comes from `pdf-text--leader-parts';
records repeating one glyph below the minimum - a fill served one
glyph per record - add up along the baseline, and count as fills
only when the same glyph reaches `pdf-text-leader-min' jointly."
  (let ((count 0) run glyph)
    (cl-flet ((flush ()
                (when (and run (<= pdf-text-leader-min count))
                  (dolist (l run) (puthash l (cons "" nil) parts)))
                (setq run nil glyph nil count 0)))
      (dolist (line group)
        (let ((bare (pdf-text--leader-glyph (pdf-text-line-text line))))
          (cond
           ((and bare (or (null glyph) (eq glyph (car bare))))
            (setq glyph (car bare))
            (push line run)
            (setq count (+ count (cdr bare))))
           (bare (flush)
                 (setq glyph (car bare) run (list line) count (cdr bare)))
           (t (flush)))))
      (flush)))
  (dolist (line group)
    (unless (gethash line parts)
      (when-let* ((found (pdf-text--leader-parts (pdf-text-line-text line))))
        (puthash line found parts)))))

(defun pdf-text--strip-leaders (lines)
  "LINES with table-of-contents leader fills stripped, entries paired.
A fill is one glyph repeated `pdf-text-leader-min' times or more,
optionally spaced - the dots aligning a contents entry with its
folio, whatever glyph the font names them by.  Inline, fill and
folio ride the entry's own record and the strip alone repairs it.
Served apart, the fill record drops and the baseline's survivors -
entry words, a math fragment, the folio - merge into one entry line.
The folio is the warrant: a fill strips only against one inline, a
bare folio record closing its own baseline, or a page whose folioed
fills already prove a contents page - a workbook rules its answer
blanks with the same dots, prose ends on repeated punctuation, and
a scene-break run stands alone, none of them furniture to eat.  On
a contents page a fill-less baseline closing on a bare folio pairs
the same way - MuPDF serves front-matter entries and chapter rows
apart from their numbers - though never inside the margin band,
where the running foot shares that shape."
  (let* ((groups (pdf-text--leader-groups lines))
         (parts (make-hash-table :test 'eq))
         (definite (make-hash-table :test 'eq)))
    ;; first reading: which baselines carry a fill on a folio's
    ;; warrant - inline in the fill's own record, or a bare folio
    ;; record standing rightmost on the baseline
    (dolist (group groups)
      (pdf-text--leader-fills group parts)
      (when (cl-some (lambda (l) (gethash l parts)) group)
        (let ((rightmost (car (sort (copy-sequence group)
                                    (lambda (a b)
                                      (> (or (pdf-text-line-x1 a) 0)
                                         (or (pdf-text-line-x1 b) 0)))))))
          (when (or (cl-some (lambda (l) (cdr-safe (gethash l parts))) group)
                    (and (not (gethash rightmost parts))
                         (pdf-text--bare-folio-p
                          (pdf-text-line-text rightmost))))
            (puthash group t definite)))))
    (let ((contents (<= pdf-text-entry-run-min (hash-table-count definite))))
      (cl-loop
       for group in groups
       append
       (cond
        ;; a baseline with a warranted fill: strip it, merge what stands
        ((or (gethash group definite)
             (and contents
                  (cl-some (lambda (l) (gethash l parts)) group)))
         (let (survivors)
           (dolist (line group)
             (pcase (gethash line parts)
               (`(,entry . ,folio)
                (let ((text (string-trim
                             (concat entry (and folio " ") folio))))
                  (unless (string-empty-p text)
                    (setf (pdf-text-line-text line) text)
                    (push line survivors))))
               (_ (push line survivors))))
           (if survivors
               (list (pdf-text--leader-entry (nreverse survivors)))
             nil)))
        ;; a fill-less baseline pairing with its folio, on the word of
        ;; the page's own fills, clear of the margin band
        ((and contents (cdr group)
              (not (cl-some (lambda (l) (gethash l parts)) group))
              (when-let* ((base (pdf-text-line-base (car group))))
                (< pdf-text-margin-band base (- 1.0 pdf-text-margin-band)))
              (cl-every (lambda (l) (null (pdf-text-line-kind l))) group)
              (let ((sorted (sort (copy-sequence group)
                                  (lambda (a b)
                                    (< (or (pdf-text-line-x0 a) 0)
                                       (or (pdf-text-line-x0 b) 0))))))
                (and (pdf-text--bare-folio-p
                      (pdf-text-line-text (car (last sorted))))
                     (cl-some (lambda (l)
                                (string-match-p
                                 "[[:alpha:]]" (pdf-text-line-text l)))
                              sorted))))
         (list (pdf-text--leader-entry group)))
        (t group))))))

(defun pdf-text--page-far-edge (lines profile)
  "The far strong right edge LINES' own body ink establishes, or nil.
A second body column carries `pdf-text-column-strength' of the modal
column's ink and earns the edge; a margin-note rail never does on
any one page, however strong an edge a book of notes accumulates
document wide - which is why the page answers this and PROFILE's
document-wide text area cannot.  Only body-height lines vote, the
way `pdf-text--page-profile' filters them: the head matter above a
paper's columns must not vouch for its own ground."
  (let* ((height (plist-get profile :height))
         (body (cl-remove-if-not
                (lambda (line)
                  (and (pdf-text-line-x0 line) (pdf-text-line-x1 line)
                       (or (null height)
                           (null (pdf-text-line-height line))
                           (< (abs (- (pdf-text-line-height line) height))
                              (* 0.15 height)))))
                lines)))
    (and body
         (cdr (pdf-text--strong-edges
               (mapcar #'pdf-text-line-x1 body) 0.005
               (mapcar (lambda (l) (- (pdf-text-line-x1 l)
                                      (pdf-text-line-x0 l)))
                       body))))))

(defun pdf-text--defer-margin-notes (lines profile &optional far)
  "LINES with unclaimed records past the page's columns served last.
MuPDF serves an outer-margin term label at its vertical position -
mid-paragraph, severing the sentence around it - where poppler served
it after the page's flow.  A narrow record standing wholly right of
every column, and claimed by no lane, is such a label: it moves to
the page's end, reads after the text it annotates, and the paragraph
stays whole.  The boundary is the page's own far strong edge, not
the modal column and not the document's text area: a two-column
paper's modal column is one of the two, and its title page's
right-side author block stands past it while the right body column
vouches for the ground it stands on - where a margin-note rail never
carries `pdf-text-column-strength' of a column's ink on any one
page, however strong an edge a book of notes accumulates document
wide.  PROFILE hands the page profile its document frame; mirrored
margins put the column elsewhere on facing pages, so the page's own
edges are the measure.  FAR is the page's far edge measured before
the lane pass - the unfold rewrites a facing column's coordinates,
so the caller measures while the ink still stands where the page
set it - and is derived from LINES when the caller has none."
  (let* ((page (pdf-text--page-profile lines profile))
         (right (plist-get page :right))
         (space (or (plist-get profile :space) 0.005))
         (far (or far (pdf-text--page-far-edge lines profile)))
         (right (and right (max right (or far right)))))
    (if (null right)
        lines
      (let (body notes)
        (dolist (line lines)
          (if (and (not (pdf-text-line-claimed line))
                   (when-let* ((x0 (pdf-text-line-x0 line))
                               (x1 (pdf-text-line-x1 line)))
                     (and (<= (+ right space) x0)
                          (< (- x1 x0) 0.25))))
              (push line notes)
            (push line body)))
        (nconc (nreverse body) (nreverse notes))))))

(defvar pdf-text-split-line-gap 6
  "Modal spaces of gap under which a split typeset line may rejoin.
MuPDF opens a new line at a gap wider than its own space threshold -
a stretch of justified type, a wide kern - and serves one typeset
line as two records.  Table cells and paired lane items also share a
baseline, but their gaps run wider and their texts fail the clause
test, so the bound and the words together keep structure apart.")

(defvar pdf-text-kern-gap 0.15
  "Height fraction of gap under which same-font neighbours may be one word.
A Type 3 DVI document serves every kern chunk of a word as its own
record on the typeset line's baseline - gaps at or under zero, or a
hair over - where the page's own word gaps start at a third of the
glyph height.  The line's own height is the measure, not the modal
space, because gaps scale with the font and the shatter bites
hardest on display-size titles.  The bound reads on the positive
side; overlap is bounded at half a height, past which two records
are a shadow-painted double, not a kern.")

(defvar pdf-text-shatter-gap 1.0
  "Height fraction of gap a shattered document's word join reaches across.
Where the em sizes mark the Type 3 class, every word of a typeset
line is its own record, and the line must reassemble before the
block, alignment and heading rules read it - with no case demand,
since a title's words open on capitals.  Word gaps run a third of
the glyph height to a stretched justification's four fifths; a
table lane or a margin note stands further off and stays its own
record.")

(defun pdf-text--join-split-lines (lines profile)
  "LINES with a typeset line the extractor served as two rejoined.
Two neighbours on one baseline, set in the same face and size, closer
than `pdf-text-split-line-gap' of PROFILE's modal spaces, rejoin when
their words read as one clause - the first ends unpunctuated and the
second opens lowercase.  A folio opens on a digit, a table cell on a
capital, a list marker ends on its dot, and every one of them stays
its own record.  Same-font neighbours closer than `pdf-text-kern-gap'
are not two words but one, shattered at a kern, and rejoin with no
space - outright ink overlap alone confirms it, a positive hair of
gap needs the clause test's lowercase continuation.  Over a document
of degenerate ems - the Type 3 class, PROFILE's :em against its
:height - word-gap neighbours within `pdf-text-shatter-gap' rejoin
too, with the space the page set, whatever case they open on."
  (let ((space (or (plist-get profile :space) 0.005))
        (shattered (pdf-text--degenerate-ems-p (plist-get profile :em)
                                               (plist-get profile :height)))
        (case-fold-search nil)
        out)
    (dolist (line lines)
      (let* ((prev (car out))
             (joiner
              (when (and prev
                         (pdf-text-line-base prev) (pdf-text-line-base line)
                         (< (abs (- (pdf-text-line-base line)
                                    (pdf-text-line-base prev)))
                            (* 0.25 (or (pdf-text-line-height prev) 0.01)))
                         (pdf-text--similar-height-p prev line)
                         (pdf-text-line-x1 prev) (pdf-text-line-x0 line))
                (let ((gap (- (pdf-text-line-x0 line)
                              (pdf-text-line-x1 prev)))
                      (height (max (pdf-text-line-height prev)
                                   (pdf-text-line-height line)))
                      (same-font (and (pdf-text-line-font prev)
                                      (pdf-text-line-font line)
                                      (equal (pdf-text-line-font prev)
                                             (pdf-text-line-font line)))))
                  (cond
                   ;; kern chunks of one word, rejoined without a space
                   ((and same-font
                         (<= (* -0.5 height) gap)
                         (or (<= gap 0)
                             (and (<= gap (* pdf-text-kern-gap height))
                                  (string-match-p
                                   "[[:alnum:]]\\'"
                                   (string-trim (pdf-text-line-text prev)))
                                  (string-match-p
                                   "\\`[[:lower:]]"
                                   (string-trim (pdf-text-line-text line))))))
                    "")
                   ;; a shattered document's typeset line, reassembled
                   ;; word by word with the spaces the page set
                   ((and shattered same-font
                         (< 0 gap) (<= gap (* pdf-text-shatter-gap height))
                         (string-match-p "[^ \t]"
                                         (pdf-text-line-text prev))
                         (string-match-p "[^ \t]"
                                         (pdf-text-line-text line)))
                    " ")
                   ((and (< 0 gap)
                         (< gap (* pdf-text-split-line-gap space))
                         (or
                          ;; one clause split at a wide kern
                          (and same-font
                               (string-match-p "[[:alnum:]]\\'"
                                               (string-trim
                                                (pdf-text-line-text prev)))
                               (string-match-p "\\`[[:lower:]]"
                                               (string-trim
                                                (pdf-text-line-text line))))
                          ;; a dotted section number served apart from its
                          ;; title, often in another face; a bare enumerator
                          ;; carries no dot chain and stays a list marker
                          (and (string-match-p
                                "\\`[0-9]+\\(?:\\.[0-9]+\\)+\\.?\\'"
                                (string-trim (pdf-text-line-text prev)))
                               (string-match-p "\\`[[:upper:]]"
                                               (string-trim
                                                (pdf-text-line-text line))))
                          ;; a bare "N." beside its title, both in one face
                          ;; set clearly over the body: a paper's display
                          ;; section number.  A list enumerator at the body
                          ;; size in the body face stays a marker for the
                          ;; item machinery
                          (let ((body (plist-get profile :height)))
                            (and body
                                 (string-match-p "\\`[0-9]+\\.\\'"
                                                 (string-trim
                                                  (pdf-text-line-text prev)))
                                 (string-match-p "\\`[[:upper:]]"
                                                 (string-trim
                                                  (pdf-text-line-text line)))
                                 same-font
                                 (eq (pdf-text-line-bold prev)
                                     (pdf-text-line-bold line))
                                 (< (* pdf-text-heading-height body)
                                    (min (or (pdf-text-line-height prev) 0)
                                         (or (pdf-text-line-height line)
                                             0)))))))
                    " "))))))
        (if joiner
            (setcar out (pdf-text--merge-records
                         (list prev line)
                         (concat (string-trim-right (pdf-text-line-text prev))
                                 joiner
                                 (string-trim (pdf-text-line-text line)))))
          (push line out))))
    (nreverse out)))

(defun pdf-text--float-drop-caps (lines profile)
  "LINES with each drop cap floated down to the line it visually opens.
Poppler served a page's drop cap after the running head above it;
MuPDF serves the cap first, so the forced join after a drop cap would
swallow the head.  A cap whose following line sits above its own top
arrived early: it moves to just before the first line whose baseline
falls within the cap's span - PROFILE's leading extends the reach
below the cap - and every rule downstream sees the order the page
reads in.  A cap with no such line in reach stays put."
  (let ((leading (or (plist-get profile :leading) 0.02))
        out)
    (while lines
      (let* ((line (pop lines))
             (top (pdf-text-line-top line))
             (bot (pdf-text-line-bot line)))
        (if (and (pdf-text--drop-cap-p line profile) top bot lines
                 (when-let* ((next (pdf-text-line-base (car lines))))
                   (< next top)))
            (let ((span (cl-position-if
                         (lambda (l)
                           (when-let* ((base (pdf-text-line-base l)))
                             (and (<= top base) (<= base (+ bot leading)))))
                         lines :end (min 5 (length lines)))))
              (if (null span)
                  (push line out)
                (dotimes (_ span) (push (pop lines) out))
                (push line out)))
          (push line out))))
    (nreverse out)))

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

(defvar pdf-text-monospace-font-re
  "mono\\|courier\\|cmtt\\|lmtt\\|typewriter\\|menlo\\|consol"
  "Font names that carry code, matched case-blind.
The name is the extractor's own word for what the advance variation
infers; it also reads on a line too short for the variation to judge.
MuPDF's `isMono' flag is not the equal of the name - it reads false
on UbuntuMono - so the name is the signal.")

(defvar pdf-text-math-font-re
  "cmmi\\|cmsy\\|cmex\\|msam\\|msbm\\|math\\|symbol"
  "Font names that set mathematics, matched case-blind.
TeX's math italic, symbol and extension faces and their AMS kin; a
line served in one is mathematics whatever its codepoint density
says.")

(defun pdf-text--font-matches-p (font re)
  "Whether font name FONT matches RE, case-blind; nil for a nil FONT."
  (when font
    (let ((case-fold-search t))
      (string-match-p re font))))

(defun pdf-text--mark-monospace (lines)
  "Tag the monospaced LINES, which are listings and must not reflow.
A line served in a code font outright - `pdf-text-monospace-font-re'
on both the dominant and the opening run, so a line of prose that a
wide inline identifier dominates stays prose - is a listing's however
short it runs.  A line too short to judge on its own otherwise takes
the tag from a neighbour of the same size: a closing brace alone on
its line belongs to the listing above it."
  (dolist (line lines)
    (when (null (pdf-text-line-kind line))
      (if (and (pdf-text--font-matches-p (pdf-text-line-font line)
                                         pdf-text-monospace-font-re)
               (pdf-text--font-matches-p (or (pdf-text-line-lead-font line)
                                             (pdf-text-line-font line))
                                         pdf-text-monospace-font-re))
          (setf (pdf-text-line-kind line) 'mono)
        (when-let* ((cv (pdf-text-line-cv line)))
          (when (and (< cv pdf-text-monospace-variation)
                     (<= pdf-text-monospace-min-glyphs
                         (length (string-trim (pdf-text-line-text line)))))
            (setf (pdf-text-line-kind line) 'mono))))))
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
operators and single-letter variables rather than words, or a math
font by name where the letters outnumber the operators - and PROFILE
says the page sets it apart from the column.  A displayed line with no
words at all - a lone bracket, a bare variable under an operator run -
joins a neighbouring display, the way a short brace joins its
listing."
  (dolist (line lines)
    (when (and (null (pdf-text-line-kind line))
               (pdf-text--displayed-p line profile)
               (or (pdf-text-mathish-text-p (pdf-text-line-text line))
                   ;; the face knows what the codepoints cannot: a
                   ;; display whose letters outnumber its operators is
                   ;; still set in the math fonts
                   (pdf-text--font-matches-p (pdf-text-line-font line)
                                             pdf-text-math-font-re)
                   (pdf-text--font-matches-p (pdf-text-line-lead-font line)
                                             pdf-text-math-font-re)))
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
mid-text), the folio-merged ones only ever go from the band itself.

Outside the profile's body span the band and the detachment go
unmeasured - a paper sets its head tight over a text block that
starts far down a large page - but only the page marker itself and
the lines on its baseline qualify: the folio names the furniture row,
whatever width the head beside it runs.  Everything else out there is
the page's own - a chapter eyebrow, a paragraph ending high - and
none of it feeds the recurrence count, so NARROW-P is nil for the
marker path."
  (let ((leading (plist-get profile :leading))
        (left (plist-get profile :left))
        (right (plist-get profile :right)))
    (if (not (and leading left right (cl-some #'pdf-text-line-base lines)))
        (mapcar (lambda (line) (cons line t)) (pdf-text--edge-lines lines))
      (let ((vec (vconcat lines))
            (width (- right left))
            markers candidates)
        (dotimes (i (length vec))
          (let ((line (aref vec i)))
            (when-let* ((base (pdf-text-line-base line))
                        (x0 (pdf-text-line-x0 line))
                        (x1 (pdf-text-line-x1 line))
                        ((< (- x1 x0) (* 0.6 width)))
                        ((pdf-text--page-marker-p (pdf-text-line-text line)))
                        ((or (< base pdf-text-margin-band)
                             (< (- 1.0 pdf-text-margin-band) base)
                             (pdf-text--outside-text-area-p base profile))))
              (push base markers))))
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
                                 (* 0.6 width))))
                 (classic (and base
                               (or (< base pdf-text-margin-band)
                                   (< (- 1.0 pdf-text-margin-band) base))
                               (or narrow
                                   (pdf-text--folio-merged-p
                                    (pdf-text-line-text line)))
                               (or (null before) (< gap before))
                               (or (null after) (< gap after)))))
            (when (and base
                       ;; a lane region proved its rows aligned three
                       ;; deep; page furniture never does
                       (not (pdf-text-line-claimed line))
                       (or classic
                           (and (pdf-text--outside-text-area-p base profile)
                                (cl-some (lambda (marker)
                                           (< (abs (- base marker))
                                              (* 0.5 leading)))
                                         markers))))
              (push (cons line (and narrow classic)) candidates))))
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
  "Whether PIECE sits where a shadow echo of LINE would: on its ink.
The second paint lands a point off, never a leading down and never
beside, so a piece with geometry of its own must hold its baseline
near LINE's and its ink within LINE's span.  A doubled glyph a
shattered document serves as two records sits on the baseline but
beside its twin, and the span test keeps it.  Records without
geometry cannot argue and pass."
  (let ((base (pdf-text-line-base line))
        (other (pdf-text-line-base piece))
        (height (pdf-text-line-height line)))
    (and (or (null base) (null other)
             (< (abs (- other base)) (* 0.5 (or height 0.02))))
         (let ((x0 (pdf-text-line-x0 line))
               (x1 (pdf-text-line-x1 line))
               (px0 (pdf-text-line-x0 piece))
               (px1 (pdf-text-line-x1 piece))
               (slack (* 0.25 (or height 0.02))))
           (or (null x0) (null px0)
               (and (<= (- x0 slack) px0)
                    (<= px1 (+ x1 slack))))))))

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
               (pcase kind ('table 'row) ((or 'mono 'math 'entry) kind) (_ nil))))
      'face)
     ;; a wrapped table row stands two typeset lines tall, so the step
     ;; between row records exceeds any gap factor; consecutive rows
     ;; are one table regardless
     ((eq 'table kind) nil)
     ;; contents entries run as one list while the page stacks them;
     ;; the air before a chapter's group starts the next list
     ((eq 'entry kind)
      (and (pdf-text--gap-break-p line prev profile) 'gap))
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
                     ((eq 'entry (pdf-text-line-kind line)) 'entry)
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

(defun pdf-text--line-tagged (line)
  "LINE's text carrying its record as a `pdf-text-line' text property.
The follow highlight reads the record back from the rendered buffer,
mapping any span of text to the page rects that drew it.  Tagging the
one place a record's text enters the render is enough: every trim,
join and substring downstream preserves string properties, and
nothing outside the buffer sees them - `equal' ignores properties,
and a golden written to disk drops them."
  (propertize (pdf-text-line-text line) 'pdf-text-line line))

(defun pdf-text--join-block (block vocabulary)
  "BLOCK's lines joined into one paragraph line, de-hyphenated by VOCABULARY."
  (let (para)
    (dolist (line (pdf-text-block-lines block) (or para ""))
      (let ((text (string-trim (pdf-text--line-tagged line))))
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
                           (string-trim (pdf-text--line-tagged line)))))
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

(defun pdf-text--line-face (line)
  "The face LINE opens in, as (FONT . BOLD); (nil) without font data.
The opening run's face where the record carries one, the dominant
face otherwise: the dominant run can be another face than the one
the line opens in - a sans identifier inside a bold heading - and
what a heading style shares across its pages is its opening.  The
subset prefix goes: two embeddings of one font are one face."
  (let ((font (or (pdf-text-line-lead-font line) (pdf-text-line-font line)))
        (bold (if (pdf-text-line-lead-font line)
                  (pdf-text-line-lead-bold line)
                (pdf-text-line-bold line))))
    (cons (and font (replace-regexp-in-string "\\`[A-Z]\\{6\\}\\+" "" font))
          (and bold t))))

(defun pdf-text--block-face (block)
  "The face BLOCK opens in: `pdf-text--line-face' of its first line."
  (when-let* ((line (car (pdf-text-block-lines block))))
    (pdf-text--line-face line)))

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
             ;; a heading line's words are the outline's, not the page's,
             ;; so it carries its block's records whole: the follow
             ;; highlight lights the lines the heading stands for
             (head (when-let* ((title (car placement)))
                     (propertize title 'pdf-text-line
                                 (pdf-text-block-lines block))))
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
                                           (pdf-text--line-tagged line)))
                                        (pdf-text-block-lines block) "\n"))
                            ;; a table of contents is a list, and the
                            ;; reader gets one: entry per item, the
                            ;; folio riding at the end of its line
                            ('entry
                             (mapconcat (lambda (line)
                                          (concat "- " (string-trim
                                                        (pdf-text--line-tagged
                                                         line))))
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
         ;; the profile is measured over the cleaned pages, before the
         ;; repairs move records - a lane unfold rewrites a facing
         ;; column's coordinates - exactly where `pdf-text-reading-order'
         ;; and `pdf-text-document-facts' measure theirs, so a seeded
         ;; window render and its book read one geometry
         (profile (or pdf-text-extra-profile
                      (pdf-text--profile (pdf-text-clean-pages pages))))
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

(defun pdf-text-render-pages (pages &optional headings synthesize)
  "Raw text PAGES reflowed into readable text, one string per page.
PAGES are plain extraction strings with no geometry, so the reflow
runs on character heuristics alone - the shape of the gettext
fallback.  HEADINGS and SYNTHESIZE are what `pdf-text-render-lines'
takes."
  (pdf-text-render-lines (mapcar #'pdf-text--page-lines pages)
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

(defvar pdf-text-synth-bold-min 0.9
  "Glyph height, in body heights, a bold line needs to be a heading candidate.
A paper sets its section heads bold at the body size or a shade under
it - Applicative's run at 0.95 - where the size rules see nothing.
Bold text smaller than this is fine print, not a heading.")

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

(defun pdf-text--synth-dotted (text height x1 profile &optional bold)
  "Level of TEXT as a numbered heading set at HEIGHT ending at X1, or nil.
The number gives the level; the geometry gates it: at least
`pdf-text-synth-number-min' of PROFILE's body - Benji's sections run
at 1.03, a running head's cross-reference at 0.73 - and ink stopping
short of the column's right edge, where a full line is prose.  A
BOLD line clears the gate at `pdf-text-synth-bold-min' instead, the
same floor the sized rules give bold: LNCS sets its numbered
subsection heads bold a shade under the body, and their dot count is
their depth."
  (when-let* ((level (pdf-text--dotted-number-level text))
              (body (plist-get profile :height))
              (right (plist-get profile :right)))
    (and height
         (or (<= (* pdf-text-synth-number-min body) height)
             (and bold (<= (* pdf-text-synth-bold-min body) height)))
         x1 (< x1 (- right 0.02))
         level)))

(defun pdf-text--synth-tuple (blocks text height x1 profile &optional bold-ok)
  "The heading candidate BLOCKS make as TEXT, set at HEIGHT ending at X1.
A plist (:blocks :text :height :dotted :face), or nil.  TEXT must
read as a title: words rather than mathematics, no bracket-assembly
glyphs, no dot leaders, nothing closing the line, at most
`pdf-text-heading-max-words' words.  What passes is a heading when
its number says so (:dotted carries the level), when it is set over
`pdf-text-heading-height' of PROFILE's body, or - under BOLD-OK,
which carries the caller's word that the block stands like a
heading - when it opens bold at `pdf-text-synth-bold-min' of the
body.  The size rules decide the undotted against the whole book's
clusters, and :face is the key they cluster by."
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
      (let* ((face (pdf-text--block-face (car blocks)))
             (dotted (pdf-text--synth-dotted trimmed height x1 profile
                                             (and bold-ok (cdr face)))))
        (when (or dotted
                  (< (* pdf-text-heading-height body) height)
                  (and bold-ok (cdr face)
                       (<= (* pdf-text-synth-bold-min body) height)))
          (list :blocks blocks :text trimmed :height height :dotted dotted
                :face face))))))

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

(defun pdf-text--synth-single (block profile vocabulary &optional prev)
  "BLOCK alone as a heading candidate against PROFILE, or nil.
A multi-line block can still be a sized heading - a long title wraps -
but never a numbered one: a numbered line that wraps is prose.
VOCABULARY joins the text.  PREV, the block above, decides whether
BLOCK stands like a heading: the bold rules only admit a paragraph
block clear of the text above it by a paragraph gap - a bold
vocabulary label inside a workbook's tight flow, or a bold list
item, is the page's own text however its face reads."
  (when (and (memq (pdf-text-block-kind block) '(para item))
             (not (pdf-text--synth-banded-p block)))
    (let* ((lines (pdf-text-block-lines block))
           (height (pdf-text--block-height block))
           (body (plist-get profile :height))
           (leading (plist-get profile :leading))
           (bold-ok
            (and (eq (pdf-text-block-kind block) 'para)
                 (or (null prev)
                     (when-let* ((leading)
                                 (top (pdf-text-line-top (car lines)))
                                 (last (car (last (pdf-text-block-lines prev))))
                                 (base (pdf-text-line-base last)))
                       (<= (* pdf-text-gap-factor leading) (- top base)))))))
      ;; joining a block's text walks the vocabulary; a multi-line block
      ;; of body type - most paragraphs - can never be a heading, so it
      ;; never pays for the join
      (when (and height body
                 (or (and (null (cdr lines))
                          (or (<= (* pdf-text-synth-number-min body) height)
                              (and bold-ok
                                   (cdr (pdf-text--block-face block))
                                   (<= (* pdf-text-synth-bold-min body)
                                       height))))
                     (< (* pdf-text-heading-height body) height)))
        (pdf-text--synth-tuple (list block)
                               (pdf-text--join-block block vocabulary)
                               height
                               (and (null (cdr lines))
                                    (pdf-text-line-x1 (car lines)))
                               profile
                               bold-ok)))))

(defun pdf-text--contents-page-p (blocks)
  "Whether BLOCKS carry a contents run: entry lines paired to folios.
`pdf-text-entry-run-min' entry-kind lines closing on a folio - the
shape `pdf-text--strip-leaders' and `pdf-text--mark-entry-runs'
leave behind - name the page a table of contents.  There a dotted
number opens an entry, not a section: the entries whose own folio
went astray must not come back as headings."
  (let ((case-fold-search nil)
        (folio (format "[^0-9 ] +\\(?:%s\\)\\'" pdf-text-folio-re))
        (count 0))
    (dolist (block blocks)
      (when (eq 'entry (pdf-text-block-kind block))
        (dolist (line (pdf-text-block-lines block))
          (when (string-match-p folio
                                (string-trim (pdf-text-line-text line)))
            (setq count (1+ count))))))
    (<= pdf-text-entry-run-min count)))

(defun pdf-text--synth-page-tuples (blocks profile vocabulary)
  "Heading candidates among one page's BLOCKS, pairs merged.
PROFILE and VOCABULARY as the render reads them.  On a contents
page the dotted candidates stay out - they are the page's entries -
while the sized ones stand: the section a contents page opens with
is set over the body like any other."
  (let* ((vec (vconcat (cl-remove-if (lambda (b)
                                       (eq 'blank (pdf-text-block-kind b)))
                                     blocks)))
         (contents (pdf-text--contents-page-p blocks))
         (i 0)
         tuples)
    (while (< i (length vec))
      (let* ((next (and (< (1+ i) (length vec)) (aref vec (1+ i))))
             (pair (and next (pdf-text--synth-pair (aref vec i) next
                                                   profile vocabulary)))
             (tuple (or pair (pdf-text--synth-single
                              (aref vec i) profile vocabulary
                              (and (< 0 i) (aref vec (1- i)))))))
        (when (and tuple
                   (not (and contents (plist-get tuple :dotted))))
          (push tuple tuples))
        (setq i (+ i (if pair 2 1)))))
    (nreverse tuples)))

(defun pdf-text--heading-clusters (pairs body)
  "PAIRS of (HEIGHT FACE PAGE) as supported style clusters, tallest first.
Candidate heights within a tenth of BODY of each other, opening in
one FACE, are one style; a style must head `pdf-text-synth-support'
distinct pages before it earns an org level, which is what keeps a
cover page's display type and a one-off diagram out of the outline.
The face splits what height alone cannot: a paper's author names
share their height with its section heads, roman against bold, and
only the heads recur across enough pages to earn the level.  Each
cluster is (MIN MAX FONT BOLD)."
  (let ((gap (* 0.1 (or body 0.01)))
        groups)
    (dolist (face-group (seq-group-by #'cadr pairs))
      (let ((sorted (sort (mapcar (lambda (p) (cons (car p) (caddr p)))
                                  (cdr face-group))
                          (lambda (a b) (< (car a) (car b)))))
            current)
        (dolist (pair sorted)
          (if (and current (< (- (car pair) (caar current)) gap))
              (push pair current)
            (when current
              (push (cons (car face-group) (nreverse current)) groups))
            (setq current (list pair))))
        (when current
          (push (cons (car face-group) (nreverse current)) groups))))
    (sort (cl-loop for (face . group) in groups
                   when (<= pdf-text-synth-support
                            (length (cl-remove-duplicates
                                     (mapcar #'cdr group))))
                   collect (list (caar group) (car (car (last group)))
                                 (car face) (cdr face)))
          (lambda (a b) (< (car b) (car a))))))

(defun pdf-text--cluster-level (height face clusters)
  "Org level of HEIGHT opening in FACE among CLUSTERS, nil outside them.
CLUSTERS run tallest first, so the book's biggest recurring style is
level 1; levels cap at `pdf-text-synth-levels'.  A cluster stored as
a bare (MIN . MAX) range - the shape captures carried before faces
joined the key - matches any face."
  (when-let* ((pos (cl-position-if
                    (lambda (c)
                      (let* ((faced (proper-list-p c))
                             (max (if faced (cadr c) (cdr c))))
                        (and (<= (- (car c) 1e-4) height)
                             (<= height (+ max 1e-4))
                             (or (not faced)
                                 (equal (cons (caddr c) (cadddr c)) face)))))
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
                                                collect (list (plist-get tuple :height)
                                                              (plist-get tuple :face)
                                                              page)))
                        body))))
    (cl-loop for page-tuples in tuples
             collect (let (placed drops)
                       (dolist (tuple page-tuples)
                         (when-let* ((level (or (plist-get tuple :dotted)
                                                (pdf-text--cluster-level
                                                 (plist-get tuple :height)
                                                 (plist-get tuple :face)
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
                                                   collect (list (plist-get tuple :height)
                                                                 (plist-get tuple :face)
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
the way TOC entries have and no leader fill trailing it the way
their orphaned halves do - reads as a section heading, its dot count
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
                        (not (string-match-p "[0-9]\\'" trimmed))
                        ;; a leader fill trailing the number and title
                        ;; is a contents entry whose folio broke off,
                        ;; not a section head
                        (not (string-match-p (pdf-text--leader-run-re)
                                             trimmed)))
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

(defconst pdf-text-render-version 22
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

(defvar pdf-text-kill-together 'both
  "Which half of the pair a kill takes with it.
The companion and the PDF it mirrors are one document to the reader,
so by default closing either closes the other and neither half is
left orphaned.  The values:

  `both'      killing either buffer kills the other
  `from-pdf'  the PDF's kill takes the companion, not the reverse
  `from-text' the companion's kill takes the PDF, not the reverse
  nil         each buffer is killed on its own

Read at the moment of the kill, so a change reaches the pairs that
already exist.")

(defvar pdf-text--killing nil
  "Non-nil while one half's kill is taking the other; breaks the loop.")

(defun pdf-text--kill-partner ()
  "Kill the other half of the pair, as far as `pdf-text-kill-together' allows.
Runs from `kill-buffer-hook' on both sides, so the partner this kills
runs the same hook on the way out - which is what the guard stops."
  (unless pdf-text--killing
    (let* ((text (derived-mode-p 'pdf-text-mode))
           (partner (if text pdf-text--pdf-buffer pdf-text--companion)))
      (when (and (buffer-live-p partner)
                 (memq pdf-text-kill-together
                       (if text '(both from-text) '(both from-pdf))))
        (let ((pdf-text--killing t))
          (kill-buffer partner))))))

(defun pdf-text--pair (companion pdf)
  "Point COMPANION and PDF at each other, and arm the paired kill.
Both hooks go on whatever `pdf-text-kill-together' says, since the
option is read when one of them fires."
  (with-current-buffer companion
    (setq pdf-text--pdf-buffer pdf)
    (add-hook 'kill-buffer-hook #'pdf-text--kill-partner nil t))
  (with-current-buffer pdf
    (setq pdf-text--companion companion)
    (add-hook 'kill-buffer-hook #'pdf-text--kill-partner nil t)))

;;;###autoload
(defun pdf-view-as-text ()
  "Read the current PDF as reflowed text in a companion buffer.
Lands where the `pdf-view-mode' window is: same page, proportionally
as far into the page's text as the window top sits down the image.
The companion is reused as long as the PDF file on disk is unchanged;
a stale or missing one is re-extracted through epdfinfo.  The PDF
outline becomes org headings; without one, numbered section lines
found in the text stand in.  A fresh companion starts
`pdf-text-sync-mode' when `pdf-text-sync-default' is non-nil, and the
two buffers are killed together as `pdf-text-kill-together' says.  A
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
        ;; the walker carries the text as well as its geometry, so gettext
        ;; only runs for a page MuPDF finds no text on
        (let* ((start (float-time))
               (total (pdf-info-number-of-pages))
               (line-pages (pdf-text--mupdf-pages buffer-file-name 1 total))
               (raw (cl-loop for p from 1
                             for lines in line-pages
                             collect (if lines
                                         (mapconcat #'pdf-text-line-text
                                                    lines "\n")
                                       (pdf-info-gettext p '(0 0 1 1))))))
          (when (pdf-text--scanned-p raw)
            (user-error "%s has no text layer (%d of %d pages carry text)"
                        (buffer-name)
                        (cl-count-if-not #'string-blank-p raw) (length raw)))
          (let* ((outline (pdf-info-outline))
                 (rendered (pdf-text-render-lines
                            (cl-loop for text in raw
                                     for lines in line-pages
                                     collect (or lines
                                                 (pdf-text--page-lines text)))
                            (pdf-text-page-headings outline 1 (length raw))
                            nil (null outline)))
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
    (pdf-text--pair buf pdf-buf)
    (with-current-buffer buf
      ;; re-enabling on reuse re-arms the pdf-side hook after the PDF
      ;; buffer was killed and reopened; an explicit off stays off
      (when (or (and fresh pdf-text-sync-default) pdf-text-sync-mode)
        (pdf-text-sync-mode 1)))
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

;;; Follow highlight

(defvar pdf-text-follow-mode)

(defvar pdf-text-follow-default t
  "Whether enabling the sync also enables `pdf-text-follow-mode'.
Consulted when `pdf-text-sync-mode' turns on, not at a re-arm of a
sync already running, so a reader's explicit follow-off survives the
reuse path the way the sync's own state does.")

(defface pdf-text-follow-face
  '((t :inherit lazy-highlight))
  "Face of the sentence highlight on the PDF page.
Only its colors reach the page: epdfinfo renders the highlight into
the page image, so no overlay attributes apply."
  :group 'pdf-text)

(defvar pdf-text-follow--tick 0
  "Monotonic id of the newest highlight render; stale answers drop.")

(defvar-local pdf-text-follow--bounds nil
  "Sentence bounds the highlight last settled on.
Motions inside them repaint nothing.")

(defvar-local pdf-text-follow--painted nil
  "Page number the highlight last painted, nil while the page is clean.")

(defun pdf-text-follow--sentence ()
  "Bounds of the sentence at point, or nil off prose.
Reflowed text separates sentences with a single space, which the
default `sentence-end-double-space' reads past."
  (let ((sentence-end-double-space nil))
    (bounds-of-thing-at-point 'sentence)))

(defun pdf-text-follow--rects (bounds)
  "Source-line rects under BOUNDS, as (LEFT TOP RIGHT BOT) page fractions.
Reads the `pdf-text-line' properties the render left on the text and
reduces them to the distinct records' ink boxes.  A span without
geometry - a page rendered from bare text, characters the render
wrote itself - yields nil, and the highlight clears rather than
guess."
  (let ((pos (car bounds))
        (end (cdr bounds))
        records)
    (while (< pos end)
      (let ((value (get-text-property pos 'pdf-text-line)))
        ;; a heading carries its block's records as a list; prose
        ;; carries one record; untagged text carries nil, which is
        ;; also a list, and contributes nothing
        (dolist (record (if (listp value) value (list value)))
          (when (and record (not (memq record records)))
            (push record records))))
      (setq pos (or (next-single-property-change pos 'pdf-text-line nil end)
                    end)))
    (nreverse
     (delq nil
           (mapcar (lambda (record)
                     (let ((x0 (pdf-text-line-x0 record))
                           (top (pdf-text-line-top record))
                           (x1 (pdf-text-line-x1 record))
                           (bot (pdf-text-line-bot record)))
                       (and x0 top x1 bot (list x0 top x1 bot))))
                   records)))))

(defun pdf-text-follow--clear ()
  "Restore the PDF page image the highlight painted over, if any.
Bumps the tick so a render still in flight drops instead of painting
over the restored page."
  (setq pdf-text-follow--bounds nil)
  (when pdf-text-follow--painted
    (setq pdf-text-follow--painted nil)
    (cl-incf pdf-text-follow--tick)
    (when-let* ((pdf pdf-text--pdf-buffer)
                ((buffer-live-p pdf))
                (win (get-buffer-window pdf t)))
      (with-selected-window win
        (pdf-view-redisplay win)))))

(defun pdf-text-follow--paint (page rects)
  "Render PAGE with RECTS highlighted into the PDF window's image.
The render is asynchronous and its answer drops when a newer one was
issued, the window moved on, or the page turned under it - the
`pdf-isearch-hl-matches' shape.  Where `pdf-view-use-scaling' doubles
the page's own render width the highlight render doubles too, so the
highlighted page stays as crisp as the clean one."
  (when-let* ((pdf pdf-text--pdf-buffer)
              ((buffer-live-p pdf))
              (win (get-buffer-window pdf t)))
    (let* ((tick (cl-incf pdf-text-follow--tick))
           (width (with-selected-window win
                    (car (pdf-view-image-size nil win page))))
           (scale (if (buffer-local-value 'pdf-view-use-scaling pdf) 2 1))
           (colors (pdf-util-face-colors
                    'pdf-text-follow-face
                    (buffer-local-value 'pdf-view-dark-minor-mode pdf)))
           (pdf-info-asynchronous
            (lambda (status data)
              (when (and (null status)
                         (eq tick pdf-text-follow--tick)
                         (buffer-live-p pdf)
                         (window-live-p win)
                         (eq (window-buffer win) pdf))
                (with-selected-window win
                  (when (eq page (pdf-text--pdf-page))
                    (pdf-view-display-image
                     (apply #'create-image data (pdf-view-image-type) t
                            :width width
                            :relief (or (bound-and-true-p pdf-view-image-relief)
                                        0)
                            ;; the mac port serves 2x displays from
                            ;; :data-2x; other ports scale :width down
                            (when (and (eq (framep-on-display) 'mac)
                                       (= (pdf-util-frame-scale-factor) 2))
                              (list :data-2x data)))
                     page win)))))))
      (setq pdf-text-follow--painted page)
      (apply #'pdf-info-renderpage-text-regions
             page (* scale width) t nil pdf
             (list (append (list (car colors) (cdr colors)) rects))))))

(defvar pdf-text-follow-delay 0.15
  "Idle seconds before the highlight moves to the sentence at point.
The paint schedules instead of firing per command: a held-down motion
key must cost nothing, while an epdfinfo render per sentence queues
behind itself in the single-threaded server and lags every
synchronous query the reading flow makes - and each render's arrival
costs the main thread a full-page PNG decode.")

(defvar-local pdf-text-follow--timer nil
  "Pending idle render; the next command swaps it out.")

(defun pdf-text-follow--schedule ()
  "Move the highlight once the reader pauses.
The companion's post-command hook, and the call the pdf-side sync
makes after it moves point here - that path runs no post-command in
this buffer.  Per command this only swaps a timer, so motion issues
no renders at all."
  (when pdf-text-follow--timer
    (cancel-timer pdf-text-follow--timer))
  (setq pdf-text-follow--timer
        (run-with-idle-timer pdf-text-follow-delay nil
                             #'pdf-text-follow--idle-refresh
                             (current-buffer))))

(defun pdf-text-follow--idle-refresh (buffer)
  "Refresh BUFFER's highlight; the idle-timer half of the schedule."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq pdf-text-follow--timer nil)
      (when pdf-text-follow-mode
        (pdf-text-follow--refresh)))))

(defun pdf-text-follow--refresh ()
  "Move the highlight to the sentence at point when it changed."
  (let ((bounds (pdf-text-follow--sentence)))
    (unless (equal bounds pdf-text-follow--bounds)
      (let ((rects (and bounds (pdf-text-follow--rects bounds))))
        (if rects
            (progn
              (setq pdf-text-follow--bounds bounds)
              (pdf-text-follow--paint (pdf-text-page-at-point) rects))
          (pdf-text-follow--clear)
          ;; remember the rect-less bounds too, or every motion inside
          ;; a verbatim block would re-walk it just to clear again
          (setq pdf-text-follow--bounds bounds))))))

;;; Page sync

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
              (pdf-text--reveal-page page))
            ;; this path runs no post-command in the companion, so the
            ;; highlight is moved by hand
            (when pdf-text-follow-mode
              (pdf-text-follow--schedule)))))))))

(defvar-local pdf-text-sync--armed nil
  "Whether the sync's hooks are armed; tells a real enable from a re-arm.
The follow rides only a real enable, so the reuse path's re-arm
cannot resurrect a follow the reader toggled off.")

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
        (let ((rearm pdf-text-sync--armed))
          (unless (buffer-live-p pdf)
            (setq pdf-text-sync-mode nil)
            (user-error "The source PDF buffer is gone"))
          (setq pdf-text-sync--last-page (pdf-text-page-at-point))
          (add-hook 'post-command-hook #'pdf-text-sync--follow-text nil t)
          (with-current-buffer pdf
            (setq pdf-text--companion companion)
            (add-hook 'pdf-view-after-change-page-hook
                      #'pdf-text-sync--follow-pdf nil t))
          (setq pdf-text-sync--armed t)
          ;; the sync carries the follow: a real enable brings it up,
          ;; and the follow's own toggle alone turns it off for a
          ;; synced page without the highlight
          (unless (or rearm pdf-text-follow-mode
                      (not pdf-text-follow-default))
            (pdf-text-follow-mode 1)))
      (setq pdf-text-sync--armed nil)
      (when pdf-text-follow-mode
        (pdf-text-follow-mode -1))
      (remove-hook 'post-command-hook #'pdf-text-sync--follow-text t)
      (when (buffer-live-p pdf)
        (with-current-buffer pdf
          (remove-hook 'pdf-view-after-change-page-hook
                       #'pdf-text-sync--follow-pdf t))))))

(define-minor-mode pdf-text-follow-mode
  "Highlight the sentence at point on the PDF page.
Rides `pdf-text-sync-mode': enabling the follow pulls the sync up,
disabling the sync takes the follow down with it, and the follow
toggles off alone for a reader who wants the page synced but clean."
  :lighter " pdf-follow"
  (unless (derived-mode-p 'pdf-text-mode)
    (setq pdf-text-follow-mode nil)
    (user-error "Not in a pdf-text buffer"))
  (if pdf-text-follow-mode
      (progn
        (unless pdf-text-sync-mode
          (condition-case err
              (pdf-text-sync-mode 1)
            (error (setq pdf-text-follow-mode nil)
                   (signal (car err) (cdr err)))))
        ;; after the sync's own hook: the page must flip before the
        ;; paint reads the pdf window
        (add-hook 'post-command-hook #'pdf-text-follow--schedule 90 t)
        (pdf-text-follow--schedule))
    (remove-hook 'post-command-hook #'pdf-text-follow--schedule t)
    (when pdf-text-follow--timer
      (cancel-timer pdf-text-follow--timer)
      (setq pdf-text-follow--timer nil))
    (pdf-text-follow--clear)))

(provide 'pdf-text)
;;; pdf-text.el ends here
