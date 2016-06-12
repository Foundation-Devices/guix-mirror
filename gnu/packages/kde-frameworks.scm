;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
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

(define-module (gnu packages kde-frameworks)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg))

(define-public extra-cmake-modules
  (package
    (name "extra-cmake-modules")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "01m12ml529pwr2sal951r5z6yb1rwbpid1y4k14nlk3xqgmdakwa"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("qtbase" ,qtbase))) ; For tests (needs qmake)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; install and check phase are swapped to prevent install from failing
         ;; after testsuire has run
         (add-after 'install 'check-post-install
           (assoc-ref %standard-phases 'check))
         (delete 'check))))
    ;; optional dependencies - to save space, we do not add these inputs.
    ;; Sphinx > 1.2:
    ;;   Required to build Extra CMake Modules documentation in Qt Help format.
    ;; Qt5LinguistTools , Qt5 linguist tools. , <http://www.qt.io/>
    ;;   Required to run tests for the ECMPoQmTools module.
    ;; Qt5Core
    ;;   Required to run tests for the ECMQtDeclareLoggingCategory module,
    ;;   and for some tests of the KDEInstallDirs module.
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "CMake module files for common software used by KDE")
    (description "The Extra CMake Modules package, or ECM, adds to the
modules provided by CMake to find common software.  In addition, it provides
common build settings used in software produced by the KDE community.")
    (license license:bsd-3)))

;; Tier 1
;;
;; Tier 1 frameworks depend only on Qt (and possibly a small number of other
;; third-party libraries), so can easily be used by an Qt-based project.

(define-public attica
  (package
    (name "attica")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0d368gmds7m7k5pnn625wqsij38cvxk1gkm4zv24phnk9f67v7cw"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Open Collaboration Service client library")
    (description "Attica is a Qt library that implements the Open
Collaboration Services API version 1.6.

It grants easy access to the services such as querying information about
persons and contents.  The library is used in KNewStuff3 as content provider.
In order to integrate with KDE's Plasma Desktop, a platform plugin exists in
kdebase.

The REST API is defined here:
http://freedesktop.org/wiki/Specifications/open-collaboration-services/")
    (license (list license:lgpl2.1+ license:lgpl3+))))

(define-public bluez-qt
  (package
    (name "bluez-qt")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0gy0m7lcwwklf021l5i3v7j0cl7qz7cgvzrwpj87ix3kyw5xs80z"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("dbus" ,dbus)
       ("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:configure-flags
        '("-DINSTALL_UDEV_RULE:BOOL=OFF")
       #:phases
        (modify-phases %standard-phases
          (replace 'check
            (lambda* _
              (setenv "DBUS_FATAL_WARNINGS" "0")
              (zero? (system* "dbus-launch" "ctest" ".")))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "QML wrapper for BlueZ")
    (description "bluez-qt is a Qt-style library for accessing the bluez
Bluetooth stack.  It is used by the KDE Bluetooth stack, BlueDevil.")
    (license (list license:lgpl2.1+ license:lgpl3+))))

(define-public breeze-icons
  (package
    (name "breeze-icons")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "1dh7bijx99sdb3vn6394wmm5cq0fvvmz8h17sx4hakmbga849cx2"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Default KDE Plasma 5 icon theme")
    (description "Breeze provides a freedesktop.org compatible icon theme.
It is the default icon theme for the KDE Plasma 5 desktop.")
    ;; The license file mentions lgpl3+. The license files in the source
    ;; directories are lgpl3, while the top directory contains the lgpl2.1.
    ;; text.
    (license license:lgpl3+)))

(define-public kapidox
  (package
    (name "kapidox")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "19a7alvn71nxflsyi7y3hghx1iw04qqc77qy54mcxcpkiyvpsggf"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; has no test target
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (propagated-inputs
     ;; kapidox is a python programm
     ;; TODO: check if doxygen has to be installed, the readme does not
     ;; mention it. The openSuse .rpm lists doxygen, graphviz, graphviz-gd,
     ;; and python-xml.
     `(("python" ,python)
       ("python-jinja2" ,python-jinja2)
       ("python-pyyaml" ,python-pyyaml)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE Doxygen Tools")

    (description "This framework contains scripts and data for building API
documentation (dox) in a standard format and style for KDE.

For the actual documentation extraction and formatting the Doxygen tool is
used, but this framework provides a wrapper script to make generating the
documentation more convenient (including reading settings from the target
framework or other module) and a standard template for the generated
documentation.")
    ;; Most parts are bsd-2, but incuded jquery is expat
    ;; This list is taken from http://packaging.neon.kde.org/cgit/
    (license (list license:bsd-2 license:expat))))

(define-public kwindowsystem
  (package
    (name "kwindowsystem")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0w5ym8msl80v3q65253pdpj9f1fmb658rnndlbkrgpmm1rv1n6dz"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server))) ; for the tests
    (inputs
     `(("libxrender" ,libxrender)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("xcb-utils-keysyms" ,xcb-util-keysyms)))
    (arguments
     `(#:tests? #f)) ; FIXME: 8/10 tests fail.
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "KDE access to the windowing system")
    (description "KWindowSystem provides information about and allows
interaction with the windowing system.  It provides a high level API, which
is windowing system independent and has platform specific
implementations.  This API is inspired by X11 and thus not all functionality
is available on all windowing systems.

In addition to the high level API, this framework also provides several
lower level classes for interaction with the X Windowing System.")
    ;; Some source files mention lgpl2.0+, but the included license is
    ;; the lgpl2.1. Some source files are under non-copyleft licenses.
    (license license:lgpl2.1+)))

(define-public oxygen-icons
  (package
    (name "oxygen-icons")
    (version "5.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/frameworks/"
                            (version-major+minor version) "/"
                            name "5" "-" version ".tar.xz"))
        (sha256
         (base32
          "1c7spjbzk04725vv0ly7vmyvwa96mfa5ki2pm146ld4888a896wm"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Oxygen provides the standard icon theme for the KDE desktop")
    (description "Oxygen icon theme for the KDE desktop")
    (license license:lgpl3+)))
