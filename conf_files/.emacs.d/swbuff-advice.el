;;; @(#) swbuff-advice.el -- provides alternate regexp cycling for swbuff
;;; @(#) $Id: swbuff-advice.el,v 1.5 2001/07/16 03:21:38 jcasa Exp $

;; This file is not part of Emacs

;; Copyright (C) 2000-2001 by Joseph L. Casadonte Jr.
;; Author:          Joe Casadonte (emacs@northbound-train.com)
;; Maintainer:      Joe Casadonte (emacs@northbound-train.com)
;; Created:         August 24, 2000
;; Keywords:        swbuff alternate regexps filters
;; Latest Version:  http://www.northbound-train.com/emacs.html

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  The swbuff package provides a way to cycle thru the buffers,
;;  optionally filtering out certain buffers via a regexp.  The
;;  swbuff-advice package provides advice for several 'swbuff'
;;  functions to allow the use of more than one set of regexp filters.
;;
;;  It also provides a way to define inclusive (rather then exclusive)
;;  filters, and additive-exclusive filters.  Normally, a buffer is
;;  included in the list of buffers to switch to if it does not match
;;  at least one of the normal filters.  With inclusive filters, the
;;  buffer name must also match EVERY inclusive filter regexp.  with
;;  additive-exclusive filters, the buffer name must not match ANY of
;;  the additive filters.  See `joc-swbuff-allow-inclusive-regexps'
;;  and `joc-swbuff-allow-additive-regexps' for more details.
;;
;;  For use with swbuff versions 2.1 or later

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path and add the following
;;  to your ~/.emacs startup file
;;
;;     (load-library "swbuff-advice")

;;; Usage:
;;
;;  Use the swbuff functions as normal.  When you want to switch to
;;  the alternate regexp buffer filter, simply add a prefix argument
;;  to an swbuff function.  This will cause 'swbuff' to use the
;;  alternate regexps filter specified by
;;  `joc-swbuff-default-alt-regexps'.  To switch back to the main
;;  regexp buffer filter, add a prefix argument again to the swbuff
;;  function.  Also, as soon as you do any command other than
;;  `swbuff-switch-to-next-buffer' or
;;  `swbuff-switch-to-previous-buffer', the regexp filter will
;;  automatically revert back to the main filter.
;;
;;  For example, suppose `swbuff-switch-to-next-buffer' is bound to
;;  [F5], and `swbuff-switch-to-previous-buffer' is bound to F6.  To
;;  use the alternate regexp buffer filter, type C-u F5 or C-u F6,
;;  then just F5 & F6 to cycle thru the alternate buffers.  When you
;;  want to return to the main regexp filter, type C-u F5 or C-u F6
;;  again, followed by just F5 or F6 to cycle thru the normal buffers.
;;
;;  You can also have more than one set of alternate regexps by
;;  customizing `joc-swbuff-alt-regexps-alist'.  To use them, simply
;;  add a value to the prefix argument to indicate which alternate
;;  regexp list to use.  If there is no regexp list for the prefix
;;  given, the default alternate regexp list will be used (as
;;  specified in `joc-swbuff-default-alt-regexps').  Alternately, if
;;  you give a double prefix argument (i.e. C-u C-u), then ALL regexp
;;  filtering will be turned off and ALL buffers will appear in the
;;  swbuff list.
;;
;;  For the following example, alternate #1 filters out everything
;;  other then a MAN page using the inclusive regexp "!^\*Man\s-"
;;  (without the quotes) and alternate filter #2 filters out
;;  everything but Perl files using the inclusive regexp "!\.p[lm]$"
;;  (without the quotes).  The default alternate filter is a list:
;;
;;    ^[^ *] - filter everything other than 'internal' buffers
;;    &^ \*Custom - exclude all trivial Custom buffers
;;    &^ \*Minibuf - exclude all minibuf buffers
;;
;;  Assume F5 & F6 are bound as above, and the keys are hit in the
;;  following order (NOTE: F6 has no relevance other than it calls one
;;  of the swbuff-switch-to commands):
;;
;;    [A] - F6 ...
;;    [B] - C-u F6, F6 ...
;;    [C] - C-u 1 F6, F6 ...
;;    [D] - C-u 2 F6, F6 ...
;;    [E] - C-u C-u F6, F6 ...
;;    [F] - C-u F6, F6 ...
;;
;;  Given the following list of active buffers, here's what will
;;  appear each time:
;;
;;                                A    B    C    D    E    F
;;                               ---  ---  ---  ---  ---  ---
;;    *Scratch*                        X              X
;;    *Customize Group: Dired*         X              X
;;    foo.pl                      X              X    X    X
;;    .emacs                      X                   X    X
;;    *Man ls*                         X    X         X
;;    *Help*                           X              X
;;    *Completions*                    X              X
;;     *Customize-Work*                               X
;;     *info tag table*                X              X
;;    *Man vi*                         X    X         X
;;     *Minibuf-0*                                    X
;;     *Minibuf-1*                                    X
;;    Bar.pm                      X               X   X    X

;;; Customization:
;;
;;  M-x `joc-swbuff-advice-customize' to customize all package options.
;;
;;  The following variables can be customized:
;;
;;  o `joc-swbuff-main-regexps'
;;        Main list of regular expressions for excluded buffers.
;;
;;  o `joc-swbuff-default-alt-regexps'
;;        Default list of alternate regular expressions for excluded
;;        buffers.
;;
;;  o `joc-swbuff-alt-regexps-alist'
;;        List of named and numbered alternate regexp filters.
;;
;;  o `joc-swbuff-allow-inclusive-regexps'
;;        Indicates whether or not inclusive regexps are allowed.
;;
;;  o `joc-swbuff-allow-additive-regexps'
;;        Indicates whether or not additive-exclusive regexps are
;;        allowed.

;;; To Do:
;;
;;  o Nothing, for now.

;;; Credits:
;;
;;  The swbuff package was written by David Ponce and is available at:
;;  http://perso.wanadoo.fr/david.ponce/more-elisp.html

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Joe Casadonte (emacs@northbound-train.com).
;;
;;  This version of cua-lite was developed and tested with NTEmacs
;;  20.7.1 under Windows 2000 & NT 4.0 and Emacs 20.7.1 under Linux
;;  (RH7).  Please, let me know if it works with other OS and versions
;;  of Emacs.

;;; Change Log:
;;
;;  see http://www.northbound-train.com/emacs/swbuff-advice.log

;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; **************************************************************************
;;; Code:

(eval-when-compile
  ;; silence the old byte-compiler
  (defvar byte-compile-dynamic)
  (set (make-local-variable 'byte-compile-dynamic) t)

  ;; silence lint
  (defvar swbuff-buffer-list-holder)
  (defvar swbuff-exclude-buffer-regexps))

;;; **************************************************************************
;;; ***** customization routines
;;; **************************************************************************
(defgroup swbuff-advice nil
  "'swbuff-advice' package customization"
  :group 'tools)

;; ---------------------------------------------------------------------------
(defun joc-swbuff-advice-customize ()
  "Customization of the 'swbuff-advice' group."
  (interactive)
  (customize-group "joc-swbuff-advice"))

;; ---------------------------------------------------------------------------
(defcustom joc-swbuff-main-regexps '("^ " "^\*.*\*")
  "List of regular expressions for excluded buffers.

This is the default regexp (regular expression) list.  To exclude all
interal buffers (buffer with names like *scratch*, *Messages*, etc.)
use the regexp: (\"^ \" \"^\\*.*\\*\")."
  :group 'swbuff-advice
  :type '(repeat (regexp :format "%v")))

;; ---------------------------------------------------------------------------
(defcustom joc-swbuff-default-alt-regexps '("^[^ \*]")
  "Default list of regular expressions for excluded buffers.

This is the regexp list used whenever an alternate is asked for but
not specified (or the requested list does not exist -- see
`joc-swbuff-alt-regexps-alist').  To include the interal buffers
(buffer with names like *scratch*, *Messages*, etc.) and exclude
buffers with normal names, use the regexp: (\"^[^ \\*]\")."
  :group 'swbuff-advice
  :type '(repeat (regexp :format "%v")))

;; ---------------------------------------------------------------------------
(defcustom joc-swbuff-alt-regexps-alist
  '((1 "Man Pages" ("!^\\*Man\\s-"))
	(2 "System" ("!^\\*\\(scratch\\|Messages\\)\\*$")))
  "List of named and numbered alternate regular expression lists.

These are accessed by giving an swbuff function a prefix argument
whose number matches the numbered list.  If you ask for a regexp list
that does not exist, the lookup will silently fail and the default
regexps list will be used instead.  Similarly, if you simply hit the
prefix key without supplying an actual argument, the default alternate
regexps will be used.  See `joc-swbuff-default-alt-regexps' for more
details.

The name is used for display/documentation purposes.  If you ever need
to know which regexps are associated with which prefix number, you can
either customize this variable or pass a negative prefix value to the
swbuff, which will list the prefixes in a temporary buffer (see
`joc-swbuff-list-alt-regexps-alist')."
  :group 'swbuff-advice
  :type '(repeat :tag "regular expression set"
		  (list
		   (integer :tag "Prefix #")
		   (string :tag "Name/Description")
		   (repeat :tag "Regular Expressions"
				   (regexp :tag "Expansion")))))

;; ---------------------------------------------------------------------------
(defcustom joc-swbuff-allow-inclusive-regexps t
  "Indicates whether or not inclusive regexps are allowed.

Inclusive regexp filters are filters where the buffer name must match
EVERY of the inclusive filters in order to be included.  Having more
than one inclusive regexp in a regexp list will probably not be the
norm.  This is in addition to the other regexp filtering requirements.
To summarize: in order for a buffer to be included in the list of
buffers to switch to via swbuff it must:

  o not match ONE of the normal regexp filters
  o not match EVERY addtive-exclusive regexp filters (if allowed)
  o match EVERY inclusive regexp filter (if allowed)

Inclusive regexps are differentiated from exclusive ones by having an
exclamtion point ('!') as the first character in the regexp.  This is
then stripped off and the remainder is used as the regexp to which the
buffer in question must match.

Examples:

   o \"!^\\*Man\\s-\" - includes all man page buffers
   o \"!^\\*\\(scratch\\|Messages\\)\\*$\" - includes only the
     \"*scratch*\" and \"*Messages*\" buffers"
  :group 'swbuff-advice
  :type 'boolean)

;; ---------------------------------------------------------------------------
(defcustom joc-swbuff-allow-additive-regexps t
  "Indicates whether or not additive-exclusive regexps are allowed.

Additive-exclusive regexp filters are filters where the buffer name
must not match ANY of the additive-exclusive filters in order to be
included (as opposed to the normal filters, where the buffer name must
not match any ONE of the filters in order for it to be included).
This is in addition to the other regexp filtering requirements.  To
summarize: in order for a buffer to be included in the list of buffers
to switch to via swbuff it must:

  o not match ONE of the normal regexp filters
  o not match EVERY addtive-exclusive regexp filters (if allowed)
  o match EVERY inclusive regexp filter (if allowed)

Additive-exclusive regexps are differentiated from normal regexps by
having an ampersand ('&') as the first character in the regexp.  This
is then stripped off and the remainder is used as the regexp to which
the buffer in question must not match.

Examples:

   o \"&^ \*Custom\" - excludes all \"<space>*Custom\" buffers
     (but not the normal \"*Custom\" buffers)
   o \"&^ \*Minibuf\" - excludes <spce>*Minibuf buffers"
  :group 'swbuff-advice
  :type 'boolean)

;;; **************************************************************************
;;; ***** version related routines
;;; **************************************************************************
(defconst joc-swbuff-advice-version
  "$Revision: 1.5 $"
  "Return 'swbuff-advice' version number.")

;; ---------------------------------------------------------------------------
(defun joc-swbuff-advice-version-number ()
  "Return 'swbuff-advice' version number."
  (string-match "[0123456789.]+" joc-swbuff-advice-version)
  (match-string 0 joc-swbuff-advice-version))

;; ---------------------------------------------------------------------------
(defun joc-swbuff-advice-display-version ()
  "Display 'swbuff-advice' version."
  (interactive)
  (message "joc-swbuff-advice version <%s>." (joc-swbuff-advice-version-number)))

;;; **************************************************************************
;;; ***** "compile-time" functions
;;; **************************************************************************
(defadvice swbuff-include-p (around joc-swbuff-include-p act)
  "Check for inclusive or additive-exclusive regexps.

If inclusive regexps are allowed (see
`joc-swbuff-allow-inclusive-regexps') then NAME must match ALL
inclusive regexps (those with a leading '!').  If additive,
exclusionary regexps are allowed (see
`joc-swbuff-allow-additive-regexps') then NAME must not match ANY
additive regexps (those with a leading '&').  All other regexps are
then checked as usual (i.e. NAME must not match at least ONE of
them)."
  (let ((new-list '())
		(current (car swbuff-exclude-buffer-regexps))
		(rest (cdr swbuff-exclude-buffer-regexps))
		(allow t)
		(skip))
	(while (and current allow)
	  ;; reset skip flag
	  (setq skip nil)

	  ;; if inclusive and allowed
	  (if (and (string-match "^!" current)
			   joc-swbuff-allow-inclusive-regexps)
		  (if (not (string-match (substring current 1) name))
			  ;; doesn't match and it must -- we're done
			  (setq allow nil)
			;; test passed -- skip this regexp in new-list
			(setq skip t))
		;; if additive and allowed
		(if (and (string-match "^&" current)
				 joc-swbuff-allow-additive-regexps)
			(if (string-match (substring current 1) name)
				;; it matches and it must not -- we're done
				(setq allow nil)
			  ;; test passed -- skip this regexp in new-list
			  (setq skip t))))

	  ;; append curernt regexp onto new-list unless skipping
	  (unless skip (setq new-list (append (list current) new-list)))

	  ;; next loop
	  (setq current (car rest))
	  (setq rest (cdr rest)))

	(when allow
	  (let ((swbuff-exclude-buffer-regexps (nreverse new-list)))
		ad-do-it))))

;;; **************************************************************************
;;; ***** interactive functions & advice
;;; **************************************************************************
(defadvice swbuff-switch-to-next-buffer
  (around joc-swbuff-next-advice (raw) act)
  "Advice to manipulate the current regexp filter list."
  (interactive "P")
  (if (joc-swbuff-advice raw)
	  ad-do-it))

;; ---------------------------------------------------------------------------
(defadvice swbuff-switch-to-previous-buffer
  (around joc-swbuff-prev-advice (raw) act)
  "Advice to manipulate the current regexp filter list."
  (interactive "P")
  (if (joc-swbuff-advice raw)
	  ad-do-it))

;; ---------------------------------------------------------------------------
(defun joc-swbuff-list-alt-regexps-alist ()
  "Lists the current regexps alist."
  (interactive)
  (with-output-to-temp-buffer "*joc-swbuff-alist*"
	(princ "Prefix   Name/Description   Regexps\n")
	(princ "======   ================   =======\n")
	(let ((alt-alist (copy-sequence joc-swbuff-alt-regexps-alist))
		  (cell) (key) (name) (regexps))
	  (setq alt-alist (sort alt-alist 'joc-swbuff-sort-regexps-list))
	  (while alt-alist
		(setq cell (car alt-alist))
		(setq key (nth 0 cell))
		(setq name (nth 1 cell))
		(if (> (length name) 16)
			(setq name (concat (substring name 0 15) "+")))
		(setq regexps (nth 2 cell))
		(joc-swbuff-print-regexps-list key name regexps)
;		(princ (format "%6s    %-16s    %s\n" key name regexps))
		(setq alt-alist (cdr alt-alist)))

	  (joc-swbuff-print-regexps-list "" "<Main>" joc-swbuff-main-regexps)
	  (joc-swbuff-print-regexps-list "" "<default>" joc-swbuff-default-alt-regexps)
	  )))

;; ---------------------------------------------------------------------------
(defun joc-swbuff-print-regexps-list (prefix name regexps)
  "Print function for `joc-swbuff-list-alt-regexps-alist'.

PREFIX is the string to use for prefix value.
NAME is the string to use for name value.
REGEXPS is a list of regular expressions."
  (princ (format "%3s      %-16s   \"%s\"\n" prefix name (nth 0 regexps)))
  (let ((lcv 1)
		(len (length regexps)))
	(while (< lcv len)
	  (princ (format "%3s      %-16s   \"%s\"\n"
					 "" "" (regexp-quote (nth lcv regexps))))
	  (setq lcv (+ lcv 1)))
	))

;; ---------------------------------------------------------------------------
(defun joc-swbuff-sort-regexps-list (a b)
  "Sort function for `joc-swbuff-list-alt-regexps-alist'.

A - first argument (list) to be compared.
B - second argument (list) to be compared."
  (let ((A (nth 0 a))
		(B (nth 0 b)))
	(< A B)))

;;; **************************************************************************
;;; ***** non-interactive functions & advice
;;; **************************************************************************
(defvar joc-swbuff-use-alt nil
  "Global varial used to keep track current regexp (not be normally set by users).")

;; ---------------------------------------------------------------------------
(defun joc-swbuff-advice (raw)
  "Workhorse for swbuff-advice package.

RAW - raw prefix passed in from advised functions."

  ; if no prefix is given, raw will be nil
  (let ((rc t))
	(when raw
	   ;; if negative prefix - list regexps
	   (if (< (prefix-numeric-value raw) 1)
		   (progn
			  (setq rc nil)
			  (joc-swbuff-list-alt-regexps-alist))

		 ;; clear out current, to force a refresh
		 (setq swbuff-buffer-list-holder nil)

		 (if (and (listp raw) (> (prefix-numeric-value raw) 4))
			 (progn
			   (setq joc-swbuff-use-alt t)
			   (setq swbuff-exclude-buffer-regexps '()))

		   (if (nlistp raw)
			 (progn
			   (setq joc-swbuff-use-alt t)
			   (setq swbuff-exclude-buffer-regexps
					 (joc-swbuff-get-alt-regexps raw)))

			 ;; toggle
			 (setq joc-swbuff-use-alt (not joc-swbuff-use-alt))

			 ;; if on, get defaults
			 (if joc-swbuff-use-alt
				 (setq swbuff-exclude-buffer-regexps
					   joc-swbuff-default-alt-regexps)
			   ;; use main
			   (setq swbuff-exclude-buffer-regexps
					 joc-swbuff-main-regexps))
			 ))))

	;; simply return the return-code
	rc))

;; ---------------------------------------------------------------------------
(defun joc-swbuff-get-alt-regexps (raw)
  "Retrieves the indicated regexp list from the alist.

RAW - regexp number to retrieve."
  (let ((cell (assoc (prefix-numeric-value raw)
					 joc-swbuff-alt-regexps-alist))
		(alt-regexps))

	(if (consp cell)
;;		(setq alt-regexps (car (cdr (cdr cell))))
		(setq alt-regexps (nth 2 cell))
	  (setq alt-regexps joc-swbuff-default-alt-regexps))

  ;; return whatever was found
	alt-regexps))

;; ---------------------------------------------------------------------------
(defadvice swbuff-pre-command-hook
  (around joc-swbuff-pre-cmd-hook-advice act)
  "Advice for pre-command hook to filter out universal argument."
  (if (and (not (eq 'universal-argument this-command))
		   (not (eq 'universal-argument-other-key this-command)))
	  (progn
		ad-do-it
		(if (eq swbuff-buffer-list-holder nil)
			(progn
			  (setq swbuff-exclude-buffer-regexps joc-swbuff-main-regexps)
			  (setq joc-swbuff-use-alt nil))))))

;; ---------------------------------------------------------------------------
(setq swbuff-exclude-buffer-regexps joc-swbuff-main-regexps)

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(provide 'swbuff-advice)

;;; swbuff-advice.el ends here
;;; **************************************************************************
;;; *****  EOF  *****  EOF  *****  EOF  *****  EOF  *****  EOF  **************
