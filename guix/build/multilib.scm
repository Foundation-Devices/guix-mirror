;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Foundation Devices, Inc. <hello@foundationdevices.com>
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

(define-module (guix build multilib)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:export (parse-multi-lib
            multi-lib->directories))

;; Parser for GCC's output of -print-multi-lib.
(define-peg-string-patterns
  "multilib <- entry+
entry       <-- directory arguments NL
directory   <-- (!dir-delim .)+ dir-delim
arguments   <-- argument*
argument    <-- arg-delim (!(arg-delim / NL) .)+
dir-delim   < ';'
arg-delim   < '@'
NL          < '\n' / '\r\n' ")

(define* (parse-multi-lib #:optional (gcc "gcc"))
  "Parse the -print-multi-lib output of the invocation of the GCC program."
  (let* ((port (open-pipe* OPEN_READ gcc "-print-multi-lib"))
         (text (get-string-all port)))
    (close-pipe port)
    (keyword-flatten '(entry) (peg:tree (match-pattern multilib text)))))

(define (multi-lib->directories entries)
  (match entries
    ((('entry ('directory dir) _) ...) dir)))
