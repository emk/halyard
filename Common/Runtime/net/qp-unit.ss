;;;
;;; <qp-unit.ss> ---- Quoted Printable Implementation
;;;
;;; Copyright (C) 2002 by PLT. 
;;; Copyright (C) 2001 by Francisco Solsona. 
;;;
;;; This file is part of mime-plt.

;;; mime-plt is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; mime-plt is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with mime-plt; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;;
;; Commentary:

(module qp-unit mzscheme
  (require "qp-sig.ss"
           (lib "unitsig.ss")
           (lib "etc.ss"))

  (provide net:qp@)
  (define net:qp@
    (unit/sig net:qp^
      (import)
      
      ;; Exceptions:
      ;; String or input-port expected:
      (define-struct qp-error ())
      (define-struct (qp-wrong-input qp-error) ())
      (define-struct (qp-wrong-line-size qp-error) (size))
      
      ;; qp-encode : string -> string
      ;; returns the quoted printable representation of STR.
      (define qp-encode
        (lambda (str)
          (let ((out (open-output-string)))
            (qp-encode-stream (open-input-string str) out "\r\n")
            (get-output-string out))))
      
      ;; qp-decode : string -> string
      ;; returns STR unqp.
      (define qp-decode
        (lambda (str)
          (let ((out (open-output-string)))
            (qp-decode-stream (open-input-string str) out)
            (get-output-string out))))
      
      (define qp-decode-stream
        (lambda (in out)
          (let ((iport (cond ((input-port? in) in)
                             ((string? in) (open-input-string in))
                             (else
                              (raise (make-qp-wrong-input))))))
            (let loop ((ln (read-line iport 'return-linefeed)))
              (cond ((eof-object? ln) (void)) ;; done reading
                    (else
                     (when (> (string-length ln) 76)
                       (warning "quoted-printable line is too long: ~a" (string-length ln)))
                     (quoted-printable-decode-line ln out)
                     (loop (read-line iport 'return-linefeed))))))))
      
      (define quoted-printable-decode-line
        (lambda (line out)
          (let ((in (open-input-string line)))
            (let loop ((ch (read-char in)))
              (if (eof-object? ch)
		  (newline out) ;; preserve linefeed
		  (case ch
		    ((#\=);; quoted-printable stuff
		     (let ((next (read-char in)))
		       (cond ((eof-object? next);; end of qp-line
			      null)
			     ((hex-digit? next)
			      (let ((next-next (read-char in)))
				(cond ((eof-object? next-next)
				       (warning "Illegal qp sequence: `=~a'" next)
				       (display "=" out)
				       (display next out))
				      ((hex-digit? next-next)
				       ;; qp-encoded
				       (display (hex-octet->char
						 (format "~a~a" next next-next))
						out))
				      (else
				       (warning "Illegal qp sequence: `=~a~a'" next next-next)
				       (display "=" out)
				       (display next out)
				       (display next-next out)))))
			     (else
			      ;; Warning: invalid
			      (warning "Illegal qp sequence: `=~a'" next)
			      (display "=" out)
			      (display next out)))
		       (unless (eof-object? next) ;; eol is effectively consumed by =
			 (loop (read-char in)))))
		    (else
		     (display ch out)
		     (loop (read-char in)))))))))
      
      (define warning
        (lambda (msg . args)
	  (when #f
	    (fprintf (current-error-port)
		     (apply format msg args))
	    (newline (current-error-port)))))
	
      (define hex-digit?
        (lambda (char)
          (regexp-match (regexp "[0-9abcdefABCDEF]")
                        (string char))))
      
      (define hex-octet->char
        (lambda (str)
          (integer->char (string->number str 16))))
      
      (define qp-blank?
        (lambda (char)
          (or (char=? char #\space)
              (char=? char #\tab))))
      
      (define qp-newline
        (lambda (port)
          (display #\return port)
          (display #\linefeed port)))
      
      (define qp-uppercase
        (lambda (hex-octet)
          (list->string (map char-upcase (string->list hex-octet)))))
      
      (define char->hex-octet
        (lambda (char)
          (let* ((ans (qp-uppercase
                       (number->string (char->integer char) 16)))
                 (padding? (< (string-length ans) 2)))
            (if padding?
                (format "=0~a" ans)
                (format "=~a" ans)))))
      
      (define re:blanks (regexp "[ \t]+$"))
      
      (define display-qp-encoded
        (lambda (line out newline-string)
          (let* ((end-pos (string-length line))
                 (m (regexp-match-positions re:blanks line))
                 (force-encoding-pos (if m (caar m) end-pos)))
            (let loop ([p 0][col 0])
              (unless (= p end-pos)
                (if (= col 75)
                    (begin
                      (write-char #\= out)
                      (display newline-string out)
                      (loop p 0))
                    (let ([i (char->integer (string-ref line p))])
                      (cond
                        [(or (<= 33 i 60) (<= 62 i 126)
                             (and (or (= i 32) (= i 9)) (< p force-encoding-pos)))
                         ;; single-char mode:
                         (write-char (integer->char i) out)
                         (loop (add1 p) (add1 col))]
                        [(>= col 73)
                         ;; need a soft newline first
                         (write-char #\= out)
                         (display newline-string out)
                         ;; now the octect
                         (display (char->hex-octet (integer->char i)) out)
                         (loop (add1 p) 3)]
                        [else
                         ;; the octect
                         (display (char->hex-octet (integer->char i)) out)
                         (loop (add1 p) (+ col 3))]))))))))

      (define qp-encode-stream
        (opt-lambda (in out [newline-string "\n"])
          (let ((iport (cond ((input-port? in) in)
                             ((string? in) (open-input-string in))
                             (else
                              (raise (make-qp-wrong-input))))))
            (let loop ([first? #t])
              (let ([line (read-line iport 'linefeed)])
                (unless (eof-object? line)
                  (unless first?
                    (display newline-string out))
                  (display-qp-encoded line out newline-string)
                  (loop #f))))))))))

;;; qp-unit.ss ends here
