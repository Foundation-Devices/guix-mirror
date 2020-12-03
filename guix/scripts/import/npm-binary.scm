;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix scripts import npm-binary)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix import npm-binary)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-41)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-npm-binary))


;;;
;;; Command-line options.
;;;

(define %default-options
  '())

(define (show-help)
  (display (G_ "Usage: guix import npm-binary PACKAGE-NAME [VERSION]
Import and convert the NPM package PACKAGE-NAME using the
`npm-build-system' (but without building the package from source)."))
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -r, --recursive        import packages recursively"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix import npm-binary")))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-npm-binary . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (G_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (alist-cons 'argument arg result))
                %default-options))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                             (('argument . value)
                              value)
                             (_ #f))
                           (reverse opts))))
    (let loop ((args args))
      (match args
        ((package-name version)
         (if (assoc-ref opts 'recursive)
             ;; Recursive import
             (map (match-lambda
                    ((and ('package ('name name) ('version version) . rest) pkg)
                     `(define-public ,(name+version->symbol name version)
                        ,pkg))
                    (_ #f))
                  (npm-binary-recursive-import package-name #:version version))
             ;; Single import
             (let ((sexp (npm-binary->guix-package package-name #:version version)))
               (unless sexp
                 (leave (G_ "failed to download meta-data for package '~a@~a'~%")
                        package-name version))
               sexp)))
        ((package-name)
         (loop (list package-name "*")))
        (()
         (leave (G_ "too few arguments~%")))
        ((many ...)
         (leave (G_ "too many arguments~%")))))))
