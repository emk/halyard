;; @BEGIN_LICENSE
;;
;; Halyard - Multimedia authoring and playback system
;; Copyright 1993-2009 Trustees of Dartmouth College
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

  (provide logger trace debug info warn log-error fatal
           ;; TODO - These functions will be deprecated as soon as we
           ;; we fix with-errors-blocked.
           report-error fatal-error
           ;; TODO - These should probably be moved into the logging
           ;; framework as soon as we have better control over how things
           ;; get printed.
           set-status-text! command-line-error command-line-message)

  ;;; Log the string MSG.  Legal LEVEL values include the symbols: trace,
  ;;; debug, info, warn, error, fatal.  Legal categories are #f (for no
  ;;; category) or symbols of the form: mylib, mylib.q-and-a, and so on.
  ;;;
  ;;; Some notes on specific log levels:
  ;;;   info: This is stored semi-permanently in a central log file, so
  ;;;     please use it sparingly.
  ;;;   warn: This will be visible to Halyard scripters, but not to end-users
  ;;;     when Halyard is in runtime mode.
  ;;;   error: When in runtime mode, this is treated the same as fatal,
  ;;;     because end-users aren't expected to know how to recover from
  ;;;     script errors.
  ;;;
  ;;; The values in MORE-MSG will be formatted using CAT.
  ;;;
  ;;; Performance: Ideally, logging should be fairly fast.  But LOGGER is
  ;;; always at least as expensive as a primitive call, and it will also
  ;;; incur the overhead of calling CAT if MORE-MSG is not null.
  (define (logger level category msg . more-msg)
    (%call-prim 'Log level
                (if category (symbol->string category) "")
                (if (null? more-msg)
                  msg
                  ;; CAT is slow, so don't call it unless we have to.
                  (apply cat msg more-msg))))

  ;;; Call LOGGER with level 'trace.
  (define (trace category msg . more-msg)
    (apply logger 'trace category msg more-msg))

  ;;; Call LOGGER with level 'debug.
  (define (debug category msg . more-msg)
    (apply logger 'debug category msg more-msg))

  ;;; Call LOGGER with level 'info.
  (define (info category msg . more-msg)
    (apply logger 'info category msg more-msg))

  ;;; Call LOGGER with level 'warn.
  (define (warn category msg . more-msg)
    (apply logger 'warn category msg more-msg))

  ;;; Call LOGGER with level 'error.
  (define (log-error category msg . more-msg)
    (apply logger 'warn category msg more-msg))

  ;;; Call LOGGER with level 'fatal.
  (define (fatal category msg . more-msg)
    (apply logger 'warn category msg more-msg))

  ;;; Show a non-fatal error dialog in developer mode, or quit the engine
  ;;; and send a crash report in runtime mode.
  ;;;
  ;;; TODO - This will be deprecated soon.
  (define (report-error msg)
    (log-error #f msg))
  
  ;;; Show a fatal error and quit the engine, regardless of mode.  Sends
  ;;; a crash report.
  ;;;
  ;;; TODO - This will be deprecated soon.
  (define (fatal-error msg)
    (fatal #f msg))

  ;;; Show some text in the GUI's status bar.  Not visible in full screen
  ;;; mode!
  (define (set-status-text! msg)
    (%call-prim 'SetStatusText msg))

  ;;; If the engine is in COMMAND_LINE mode, display an error on the
  ;;; console (or the closest equivalent on a given platform).  This is
  ;;; mostly for use by the test driver.
  ;;;
  ;;; Note that report-error will also show an error on the command-line
  ;;; when running in COMMAND_LINE mode.  But there are two important
  ;;; differences: (1) this function doesn't ever show a dialog, and (2)
  ;;; this function simply prints the error and returns immediately, even
  ;;; in modes where report-error actually quits the program.
  (define (command-line-error msg)
    (%call-prim 'CommandLineError msg))

  ;;; This function prints out a message to the command line when running
  ;;; in COMMAND_LINE mode.
  (define (command-line-message msg)
    (command-line-error msg))


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
