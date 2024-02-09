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

(define-module (gnu packages newlib)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:export (make-newlib
            make-newlib-union))

(define* (make-newlib/implementation target
                                     #:key
                                     (xbinutils (cross-binutils target))
                                     (xgcc (cross-gcc target
                                                      #:xbinutils xbinutils))
                                     (nano? #f))
  "Returns a package that contains the Newlib C standard library for TARGET
built using XBINUTILS and XGCC cross-compiler.

If NANO? is true and the then the Newlib-nano variant will be built."
  (let ((revision "0")
        (commit "5edd189d1ce70383f81214b9ea26e2637b3786fc"))
    (package
      ;; The prefix cross- is always appended since Newlib can't be compiled for
      ;; usage on any system supported by GNU Guix yet, unless native support for
      ;; Cygwin is added (and not only for cross-compilation).
      (name (string-append "cross-newlib-" (if nano? "nano-" "") target))
      (version (git-version "4.3.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://sourceware.org/git/newlib-cygwin.git")
                       (commit commit)))
                (file-name (git-file-name "newlib-cygwin" version))
                (sha256
                 (base32
                  "0z69bnm4190h9fz22wm6c7vdzh7f20zv9h910fjpwlvvv8g75fim"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:target target

        ;; Newlib documentation recommends it.
        #:out-of-source? #t

        ;; Avoid including itself as this package is a target input and cannot
        ;; use the normal cross compilation inputs.
        #:implicit-cross-inputs? #f

        #:configure-flags
        #~'("--disable-newlib-supplied-syscalls"
            "--enable-newlib-reent-check-verify"
            "--enable-newlib-retargetable-locking"
            #$@(if (not nano?)
                   '("--enable-newlib-io-long-long"
                     "--enable-newlib-io-c99-formats"
                     "--enable-newlib-register-fini"
                     "--enable-newlib-mb")
                   '("--disable-newlib-fseek-optimization"
                     "--disable-newlib-fvwrite-in-streamio"
                     "--disable-newlib-unbuf-stream-opt"
                     "--disable-newlib-wide-orient"
                     "--enable-lite-exit"
                     "--enable-newlib-global-atexit"
                     "--enable-newlib-nano-malloc"
                     "--enable-newlib-nano-formatted-io"
                     "--enable-newlib-reent-small")))

        #:phases
        #~(modify-phases %standard-phases
            ;; Adapted from gnu-build-system 'configure phase to change
            ;; --build, --host and --target as newlib uses the GCC
            ;; conventions for this.
            ;;
            ;; Normally --build is the system and --host is where the code
            ;; will run but Newlib interprets this as where the compiler
            ;; will run and target as the target of the compiler.
            ;;
            ;; So this means that build equals host and target is the
            ;; cross-compilation target here.
            (replace 'configure
              (lambda* (#:key native-inputs inputs build target
                        out-of-source? configure-flags
                        #:allow-other-keys)
                (let* ((bash (or (and=> (assoc-ref (or native-inputs inputs)
                                                   "bash")
                                        (lambda (path)
                                          (string-append path "/bin/bash")))
                                 "/bin/sh"))
                       (flags `(,@(if target
                                      '("CC_FOR_BUILD=gcc")
                                      '())
                                 ,(string-append "CONFIG_SHELL=" bash)
                                 ,(string-append "SHELL=" bash)
                                 ,(string-append "--prefix=" #$output)
                                 "--enable-fast-install"
                                 ,@(if build
                                       (list (string-append "--build=" build)
                                             (string-append "--host=" build))
                                       '())
                                 ,@(if target
                                       (list (string-append "--target=" target))
                                       '())
                                 ,@configure-flags))
                       (abs-srcdir (getcwd))
                       (srcdir     (if out-of-source?
                                       (string-append "../" (basename abs-srcdir))
                                       ".")))
                  (format #t "source directory: ~s (relative from build: ~s)~%"
                             abs-srcdir srcdir)

                  (when out-of-source?
                    (mkdir "../build")
                    (chdir "../build"))

                  (format #t "build directory: ~s~%" (getcwd))
                  (format #t "configure flags: ~s~%" flags)

                  (apply invoke bash
                         (string-append srcdir "/configure")
                         flags)))))))
      (native-inputs `(("cross-binutils" ,xbinutils)
                       ("cross-gcc" ,xgcc)
                       ("texinfo" ,texinfo)))
      (home-page "https://sourceware.org/newlib/")
      (synopsis (format #f "The Newlib C library (~a)" target))
      (description "The Newlib is a C library intended for use on embedded
systems.  It is a conglomeration of several library parts that make them
easily useable on embedded systems.")
      (license (license:non-copyleft
                 "https://www.sourceware.org/newlib/COPYING.NEWLIB")))))

(define make-newlib
  (memoize make-newlib/implementation))

(define* (make-newlib-union/implementation
           target
           #:key
           (xbinutils (cross-binutils target))
           (xgcc (cross-gcc target
                            #:xbinutils xbinutils))
           (newlib (make-newlib target
                                #:xgcc xgcc
                                #:xbinutils xbinutils))
           (newlib-nano (make-newlib target
                                     #:xgcc xgcc
                                     #:xbinutils xbinutils
                                     #:nano? #t))
           (libstdc++-nano (cross-libstdc++ target
                                            #:xbinutils xbinutils
                                            #:xgcc xgcc
                                            #:libc newlib-nano)))
  "Returns a package that is a mix of NEWLIB, NEWLIB-NANO, LIBSTDC++-NANO
packages for TARGET built using XBINUTILS and XGCC.

LIBSTDC++-NANO is a version of libstdc++ compiled targeting the NEWLIB-NANO C
standard library, this is necessary due to legacy reasons as commonly
arm-none-eabi provides both Newlib and Newlib-nano on the same toolchain."
  (package
    (inherit newlib)
    (name (string-append "cross-newlib-union-" target))
    (build-system trivial-build-system)
    (arguments
     (list
       #:modules '((guix build multilib)
                   (guix build union)
                   (guix build utils))
       #:builder
       #~(begin
           (use-modules (guix build multilib)
                        (guix build union)
                        (guix build utils))

           (define (nano-union package multi-lib-dir lib)
             (let ((src (string-append package "/" #$target "/lib/"
                                       multi-lib-dir "/" lib ".a"))
                   (dst (string-append #$output "/" #$target "/lib/"
                                       multi-lib-dir "/" lib "_nano.a")))
               (format #t "`~a' ~~> `~a'~%" src dst)
               (symlink src dst)))

           (setenv "PATH"
                   (string-append #$(this-package-native-input "cross-gcc")
                                  "/bin"))

           ;; Only add symbolic links to NEWLIB as is, create the
           ;; directories to add NEWLIB-NANO files.
           (union-build #$output (list #$(this-package-input "newlib"))
                        #:create-all-directories? #t)

           ;; Add NEWLIB-NANO on top of NEWLIB.
           ;;
           ;; Newlib already provides nano.specs with the information to
           ;; link to libc_nano.a for each multilib target, but doesn't
           ;; actually provide the libc_nano.a (and others) which need
           ;; to be built separately and installed where nano.specs can
           ;; find them.
           (let ((multi-lib (parse-multi-lib #$(cc-for-target target))))
             (for-each (lambda (multi-lib-dir)
                         (let ((newlib-nano #$(this-package-input "newlib-nano"))
                               (libstdc++-nano #$(this-package-input "libstdc++-nano")))
                           (nano-union newlib-nano multi-lib-dir "libc")
                           (nano-union newlib-nano multi-lib-dir "libg")
                           (nano-union newlib-nano multi-lib-dir "librdimon")
                           (nano-union libstdc++-nano multi-lib-dir "libstdc++")
                           (nano-union libstdc++-nano multi-lib-dir "libsupc++")))
                       (multi-lib->directories multi-lib))))))
    (native-inputs
     `(("cross-gcc" ,xgcc)))
    (inputs
     `(("cross-gcc" ,xgcc)
       ("libstdc++-nano" ,libstdc++-nano)
       ("newlib" ,newlib)
       ("newlib-nano" ,newlib-nano)))))

(define make-newlib-union
  (memoize make-newlib-union/implementation))
