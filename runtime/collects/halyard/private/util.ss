;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2008 Trustees of Dartmouth College
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 2.1 of the
;; License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;; USA.
;;
;; @END_LICENSE

(module util (lib "swindle.ss" "swindle")

  (require (lib "begin-var.ss" "mizzen"))
  (require (lib "indent.ss" "halyard/private"))
  (require (lib "util.ss" "mizzen"))
  (require (lib "errortrace-lib.ss" "errortrace"))


  ;;=======================================================================
  ;;  Error Messages
  ;;=======================================================================

  ;; Import %call-prim from the engine.
  ;; DO NOT CALL ANYTHING BUT LOGGING FUNCTIONS USING '%CALL-PRIM'--USE
  ;; 'CALL-PRIM' INSTEAD.
  (require #%engine-primitives)

  (provide app-log debug-log caution debug-caution non-fatal-error
           fatal-error set-status-text!)

  ;;; Write a message to Halyard.log.  This log is always present on a user's
  ;;; system, and is never deleted, so use this function sparingly.
  (define (app-log msg)
    (%call-prim 'Log 'halyard msg 'log))
  
  ;;; Write a message to Debug.log, which is only present on developer
  ;;; systems (though the last hundred lines are always available in a
  ;;; crash report).  This is a very high-volume log, so feel free to be
  ;;; verbose.
  (define (debug-log msg)
    (%call-prim 'Log 'Debug msg 'log))
  
  ;;; Print a "Caution" message to Halyard.log.  This should be used for very
  ;;; serious warnings only--see the note about Halyard.log on APP-LOG.
  (define (caution msg)
    (%call-prim 'Log 'halyard msg 'caution))
  
  ;;; Print a "Caution" message to Debug.log.  High-volume output is OK.
  (define (debug-caution msg)
    (%call-prim 'Log 'Debug msg 'caution))
  
  ;;; Show a non-fatal error dialog in developer mode, or quit the engine
  ;;; and send a crash report in runtime mode.
  (define (non-fatal-error msg)
    (%call-prim 'Log 'halyard msg 'error))
  
  ;;; Show a fatal error and quit the engine, regardless of mode.  Sends
  ;;; a crash report.
  (define (fatal-error msg)
    (%call-prim 'Log 'halyard msg 'fatalerror))

  ;;; Show some text in the GUI's status bar.  Not visible in full screen
  ;;; mode!
  (define (set-status-text! msg)
    (%call-prim 'SetStatusText msg))

  
  ;;=======================================================================
  ;;  Stack trace error handler
  ;;=======================================================================

  (provide with-errors-blocked)

  ;;; Call THUNK, and if an error occurs, pass it to REPORT-FUNC.
  (define (call-with-errors-blocked report-func thunk)
    (let* ((result (with-handlers ([void (lambda (exn) (cons #f exn))])
                     (cons #t (thunk))))
           (good? (car result))
           (exn-or-value (cdr result)))
      (if good?
          exn-or-value
          (let [[backtrace
                 ;; Print the backtrace to a string, but don't throw
                 ;; an exception if there are any errors in the printing
                 ;; process.
                 (with-handlers [[void (lambda (exn) #f)]]
                   (define strport (open-output-string))
                   (print-error-trace strport exn-or-value)
                   (get-output-string strport))]]
            (if (and backtrace (> (string-length backtrace) 0))
              (report-func (cat (exn-message exn-or-value)
                                "\n\nBacktrace:\n\n" backtrace))
              (report-func (exn-message exn-or-value)))
            #f))))

  ;;; If an error occurs in BODY, pass it to REPORT-FUNC.
  (define-syntax with-errors-blocked
    (syntax-rules ()
      [(with-errors-blocked (report-func) body ...)
       (call-with-errors-blocked report-func
                                 (lambda () (begin/var body ...)))]))
  (define-syntax-indent with-errors-blocked 1)

  )
