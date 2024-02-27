;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2017, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages avr)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix memoization)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages cross-base)
  #:export (make-avr-libc))

(define* (make-avr-libc/implementation #:key
                                       (xbinutils (cross-binutils "avr"))
                                       (xgcc (cross-gcc "avr"
                                                        #:xbinutils xbinutils)))
  (package
    (name "avr-libc")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah//avr-libc/avr-libc-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "15svr2fx8j6prql2il2fc0ppwlv50rpmyckaxx38d3gxxv97zpdj"))))
    (build-system gnu-build-system)
    (arguments
     '(#:target "avr"
       #:out-of-source? #t
       ;; Avoid including itself as this package is a target input and cannot
       ;; use the normal cross compilation inputs.
       #:implicit-cross-inputs? #f))
    (native-inputs `(("cross-binutils" ,xbinutils)
                     ("cross-gcc" ,xgcc)))
    (home-page "https://www.nongnu.org/avr-libc/")
    (synopsis "AVR C Library")
    (description
     "AVR Libc is a project whose goal is to provide a high quality C library
for use with GCC on Atmel AVR microcontrollers.")
    (license
     (license:non-copyleft "http://www.nongnu.org/avr-libc/LICENSE.txt"))))

(define make-avr-libc
  (memoize make-avr-libc/implementation))
