;; 5L.el --- Edit 5L source code using Emacs.
;; Copyright 2002 Trustees of Dartmouth College

;; Author: Brian Campbell <brian.p.campbell@dartmouth.edu>
;; Version: Unreleased

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published 
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Portions of this file have been stolen shamelessly from the scheme.el
;; that ships with Emacs 21 and from tamale.el, written for IML's newer
;; programming language, Tamale.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions
;;;
;;; To install 5L.el, place this file somewhere (your home directory is
;;; good), and add the following lines to your .emacs file:
;;;
;;;   (autoload 'tamale-mode "/path/to/5L.el"
;;;             "Mode for editing 5L source code" t)
;;;
;;; Next, set up your auto-mode-alist to map the right file extensions
;;; to tamale-mode.  You're set!

(require 'cl)
(require 'derived)
(require 'scheme)

(define-derived-mode 5L-mode fundamental-mode "5L"
  "Major mode for editing 5L source code.

\\(5L-mode-map)"

  ;;; Syntax table definition adapted from scheme.el
  (let ((i 0)) 
    (setq 5L-mode-syntax-table (make-syntax-table))
    (set-syntax-table 5L-mode-syntax-table)
    
    ;; Default is atom-constituent.
    (while (< i 256)
      (modify-syntax-entry i "_   ")
      (setq i (1+ i)))

    ;; Word components.
    (setq i ?0)
    (while (<= i ?9)
      (modify-syntax-entry i "w   ")
      (setq i (1+ i)))
    (setq i ?A)
    (while (<= i ?Z)
      (modify-syntax-entry i "w   ")
      (setq i (1+ i)))
    (setq i ?a)
    (while (<= i ?z)
      (modify-syntax-entry i "w   ")
      (setq i (1+ i)))
    
    ;; Whitespace
    (modify-syntax-entry ?\t "    ")
    (modify-syntax-entry ?\n ">   ")
    (modify-syntax-entry ?\f "    ")
    (modify-syntax-entry ?\r "    ")
    (modify-syntax-entry ?  "    ")
    
    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  ")
    (modify-syntax-entry ?\] ")[  ")
    (modify-syntax-entry ?{ "(}  ")
    (modify-syntax-entry ?} "){  ")
    (modify-syntax-entry ?\| "  23")
    
    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  ")
    (modify-syntax-entry ?\) ")(  ")
    (modify-syntax-entry ?\# "<   ")
    (modify-syntax-entry ?\\ "\\   "))

  (setq font-lock-defaults 
	'(5L-font-lock-keywords 
	  nil t (("+-*/.<>=!?%_&~^:" . "w")) beginning-of-defun
	  (font-lock-mark-block-function . mark-defun)))
	
  ;; Set up our imenu support (borrowed from scheme.el).
  (set (make-local-variable 'imenu-case-fold-search) t)
  (setq imenu-generic-expression 5L-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
	'(("+-*/.<>=?!%_&~^:" . "w")))

  ;; Install an "Index" menu.
  (imenu-add-menubar-index)

  ;; Setup tabs to be 4 spaces
  (setq tab-width 4)

  (set (make-local-variable 'comment-start) "#")

  )

;; Adapted from tamale.el, which in turn adapted it from scheme.el
(defconst 5L-imenu-generic-expression
  '((nil "^(card\\s-+\\(\\sw+\\)" 1)
    ("Stylesheets" "^(defstyle\\s-+\\(\\sw+\\)" 1)
    ("Macros" "^(macrodef\\s-+\\(\\sw+\\)" 1)
    ("Start cards" "^(card\\s-+\\(start\\sw+\\)" 1)
    ("Patient education" "^(card\\s-+\\(\\sw+q[[:digit:]]+\\)" 1) 
    ("Tiffany Ragan 2" "^(card\\s-+\\(\\TR2_\\w+\\)" 1) 
    ("Cards with video" "^(card\\s-+\\(\\sw+_v\\)" 1) 
    )
  "Imenu generic expression for 5L mode.  See `imenu-generic-expression'.")

;; Adapted from scheme.el
(defvar 5L-font-lock-keywords
  (list 
   (list (concat "(\\(\\(macrodef\\|card\\)"
		 "\\|\\(def\\(style\\|palette\\)\\)\\)\\>"
		 "\\s-*\\(\\sw+\\)?")
	 '(1 font-lock-keyword-face)
	 '(5 (cond ((match-beginning 2) font-lock-function-name-face)
		   ((match-beginning 3) font-lock-variable-name-face)
		   (t font-lock-warning-face))
	     nil t))
   (list 
    (concat
     "[^\\\\](" 
     (regexp-opt 
      '("add"
	"background"
	"beep"
	"blippo"
	"box"
	"buttpcx"
	"checkURL"
	"checkVol"
	"close"
	"ctouch"
	"cursor"
	"div"
	"ejectdisc"
	"exit"
	"fade"
	"header"
	"highlight"
	"if"
	"input"
	"jump"
	"keybind"
	"line"
	"loadpal"
	"loadpic"
	"lock"
	"lookup"
	"nap"
	"open"
	"origin"
	"oval"
	"pause"
	"playqtfile"
	"playqtrect"
	"preload"
	"read"
	"redoscript"
	"Resetorigin"
	"return"
	"rewrite"
	"screen"
	"get"
	"set"
	"sub"
	"text"
	"timeout"
	"touch"
	"unblippo"
	"unlock"
	"wait"
	"write"
	"textAA"
	"when"
	"unless"
	"while"
	"begin"
	"Strlen"
	"Substr"
	"Findsubstr"
	"Contains"
	"Haskey"
	"Getval"
	"Length"
	"Nth"
	"AND"
	"OR"
	"NOT"
	"+"
	"-"
	"/"
	"*"
	"%"
	"Truncate"
	"Float+"
	"Float-"
	"Float/"
	"Float*"
	"TouchCount"
	"TouchCoords"
	"TouchActivate"
	"<"
	">"
	"="
	"<>") t)
     "\\>")
    '(1 font-lock-builtin-face))
   (list 
    (concat
     "\\<" 
     (regexp-opt 
      '("_INCR_Y"
	"_INCR_X"
	"_QuickTimeVersion"
	"$date"
	"$longdate"
	"$time"
	"_EOF"
	"_seconds"
	"_system"
	"_bitDepth"
	"_resX"
	"_resY"
	"_curCard"
	"_prevCard"
	"_beforeCard"
	"_afterCard"
	"_error"
	"_graphPal"
	"_NoCheckDisc"
	"_lpstat"
	"_lpactive"
	"_BaseGraphicsLocation"
	"_BaseGraphicsExtension"
	"_PreloadedVideo"
	"_PreloadedAudio"
	"_FileNotFound"
	"_BaseVideoLocation"
	"_BaseAudioLocation"
	"_EngineBuildStr"
	"_EngineBuild"
	"_QuickTimeVideoPreload"
	"_QuickTimeVideoCycles"
	"_QuickTimeAudioPreload"
	"_QuickTimeAudioCycles"
	"_OriginX"
	"_OriginY"
	"_Graphic_X"
	"_Graphic_Y") t)
     "\\>")
    '(1 font-lock-type-face))
   (list 
    (concat
     "(" 
     (regexp-opt 
      '("playqtloop"
	"rnode"
	"rvar"
	"print") t)
     "\\>")
    '(1 font-lock-warning-face)))
     
  "Highlighting information for definitions")


