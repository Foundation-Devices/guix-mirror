;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2019, 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Robert Smith <robertsmith@posteo.net>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Prafulla Giri <pratheblackdiamond@gmail.com>
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

(define-module (gnu packages education)
  #:use-module (ice-9 regex)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages chemistry)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages julia)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks) ; extra-cmake-modules
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public analitza
  (package
    (name "analitza")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/analitza-" version ".tar.xz"))
       (sha256
        (base32 "1qnqqbrjpzndbffjwqlyfqmxxxz04fi3i2g8dx6y8q79z927fzkd"))))
    (build-system qt-build-system)
    (native-inputs
     `(("eigen" ,eigen)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("qttools" ,qttools)))
    (inputs
     `(("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsvg" ,qtsvg)))
    (home-page "https://invent.kde.org/education/analitza")
    (synopsis "Library to add mathematical features to your program")
    (description "This library is used by KAlgebra and may be used by other
software to parse and work with mathematical expressions.

This package is part of the KDE education module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public artikulate
  (package
    (name "artikulate")
    (version "20.12.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/artikulate-" version ".tar.xz"))
      (sha256
       (base32 "1cvpya408r521p9398mk0xn8pb6awqm74qcjy2r0ylx0l6bkv3ca"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("karchive" ,karchive)
       ("kconfig" ,kconfig)
       ("kcrash" ,kcrash)
       ("ki18n" ,ki18n)
       ("kirigami" ,kirigami)
       ("knewstuff" ,knewstuff)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtmultimedia" ,qtmultimedia)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (home-page "https://kde.org/applications/education/org.kde.artikulate")
    (synopsis "Learning software to improve pronunciation skills")
    (description "Artikulate is a learning software that helps improving
pronunciation skills by listening to native speakers.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public blinken
  (package
    (name "blinken")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/blinken-" version ".tar.xz"))
       (sha256
        (base32 "05dbmh1lk1ag735yiv7vql6fx15lw9a3qihxflzhbfrgng7dsxks"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kauth" ,kauth)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (home-page "https://kde.org/applications/education/org.kde.blinken")
    (synopsis "Simon Says memory game")
    (description "Blinken is based on an electronic game released in 1978,
which challenges players to remember sequences of increasing length.  On the
face of the device, there are 4 different color buttons, each with its own
distinctive sound.  These buttons light up randomly, creating the sequence
that the player must then recall.  If the player is successful in remembering
the sequence of lights in the correct order, they advance to the next stage,
where an identical sequence with one extra step is presented.

This package is part of the KDE education module.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public cantor
  (package
    (name "cantor")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/cantor-" version ".tar.xz"))
      (sha256
       (base32 "1qj6lmcgmqr110qw2r906b0kp73f9gzvm75ry1gdb77bza5g67x2"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("karchive" ,karchive)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("knewstuff" ,knewstuff)
       ("kparts" ,kparts)
       ("kpty" ,kpty)
       ("ksyntaxhighlighting" ,ksyntaxhighlighting)
       ("ktexteditor" ,ktexteditor)
       ("ktextwidgets" ,ktextwidgets)
       ("kxmlgui" ,kxmlgui)
       ("libuv" ,libuv)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("poppler-qt5" ,poppler-qt5)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ;; optional:
       ("discount" ,discount)
       ("libspectre" ,libspectre)
       ;; back-ends:
       ("analitza" ,analitza)
       ("gmp" ,gmp)  ;; for libqalculate
       ("julia" ,julia)
       ("libqalculate" ,libqalculate)
       ("lua" ,luajit)
       ("mpfr" ,mpfr)  ;; for libqalculate
       ("python" ,python)))
       ;; TODO: ("r" ,r) ; also requires gfortran
    (home-page "https://kde.org/applications/education/org.kde.cantor")
    (synopsis "Front-end for doing Mathematics and Scientific Computing")
    (description "Cantor is a front-end to powerful mathematics and
statistics packages.  Cantor integrates them into the KDE Platform and provides
a nice, worksheet-based, graphical user interface.  It supports environments
for KAlgebra, Lua, Maxima, R, Sage, Octave, Python, Scilab, and Qalculate!

This package is part of the KDE education module.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public gcompris
  (package
    (name "gcompris")
    (version "17.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://gcompris.net/download/gtk/src/gcompris-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "18y483alb4r4vfmh80nnl0pah5gv0b8frcm6l1drb9njn5xlcpgc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ;; Use SDL mixer because otherwise GCompris would need an old version
       ;; of Gstreamer.
       (list "--enable-sdlmixer"
             "LDFLAGS=-lgmodule-2.0")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "sdl-mixer")
                                    "/include/SDL:"
                                    (or (getenv "CPATH") "")))
             #t)))))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)
       ("sdl-mixer" ,sdl-mixer)
       ("sqlite" ,sqlite)
       ("glib:bin" ,glib)
       ("python" ,python)))
    (native-inputs
     `(("intltool" ,intltool)
       ("texinfo" ,texinfo)
       ("texi2html" ,texi2html)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (home-page "https://gcompris.net")
    (synopsis "Educational software suite")
    (description "GCompris is an educational software suite comprising of
numerous activities for children aged 2 to 10.  Some of the activities are
game orientated, but nonetheless still educational.  Below you can find a list
of categories with some of the activities available in that category.

@enumerate
@item computer discovery: keyboard, mouse, different mouse gestures, ...
@item arithmetic: table memory, enumeration, double entry table, mirror image, ...
@item science: the canal lock, the water cycle, the submarine, electric simulation ...
@item geography: place the country on the map
@item games: chess, memory, connect 4, oware, sudoku ...
@item reading: reading practice
@item other: learn to tell time, puzzle of famous paintings, vector drawing, cartoon making, ...
@end enumerate
")
    (license license:gpl3+)))

(define-public gcompris-qt
  (package
    (name "gcompris-qt")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gcompris.net/download/qt/src/gcompris-qt-"
             version ".tar.xz"))
       (sha256
        (base32 "08dw1q0h4qz2q0ksa5pbmb9v60hr1zv9skx6z8dlq9b1i7harnds"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             (system (string-append (assoc-ref inputs "xorg-server")
                                    "/bin/Xvfb :1 &"))
             (setenv "DISPLAY" ":1")
             ;; The test suite wants to write to /homeless-shelter
             (setenv "HOME" (getcwd))
             #t))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/gcompris-qt")
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/plugins"))
                         '("qtbase" "qtdeclarative" "qtmultimedia" "qtsvg")))
                 `("QML2_IMPORT_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/qml"))
                         '("qtdeclarative" "qtgraphicaleffects"
                           "qtmultimedia" "qtquickcontrols"))))
               #t))))
       #:configure-flags (list "-DQML_BOX2D_MODULE=disabled"
                               "-DBUILD_TESTING=TRUE")))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("gettext" ,gettext-minimal)
       ("kdoctools" ,kdoctools)
       ("perl" ,perl)
       ("qttools" ,qttools)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("openssl" ,openssl)
       ("python" ,python-wrapper)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtmultimedia" ,qtmultimedia)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtsensors" ,qtsensors)
       ("qtsvg" ,qtsvg)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (home-page "https://gcompris.net/index-en.html")
    (synopsis "Educational games for small children")
    (description
     "Gcompris offers a large collection of educational games for small
children, designed to be a unified interface to integrate more educational
games.  Language-oriented games contain vocabulary, sounds, and voices for
many different languages.
Currently available boards include:
@enumerate
@item learning how to use a mouse and keyboard
@item learning simple arithmetic
@item learning how to read an analog clock
@item recognize letters after hearing their names
@item reading practice
@item small games (memory games, jigsaw puzzles, ...)
@end enumerate\n")
    (license (list license:silofl1.1    ; bundled fonts
                   license:gpl3+))))

(define-public tipp10
  (package
    (name "tipp10")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              ;; guix download is not able to handle the download links on the
              ;; home-page, which use '<meta http-equiv="refresh" …>'
              (uri (string-append "mirror://debian/pool/main/"
                                  "t/tipp10/tipp10_2.1.0.orig.tar.gz"))
              (sha256
               (base32
                "0d387b404j88gsv6kv0rb7wxr23v5g5vl6s5l7602x8pxf7slbbx"))
              (patches (search-patches "tipp10-fix-compiling.patch"
                                       "tipp10-remove-license-code.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; packages has no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-new-version-check
           (lambda _
             ;; Make new version check to default to false.
             ;; TODO: Remove the checkbox from the dialog and the check itself
             (substitute* '("widget/settingspages.cpp" "widget/mainwindow.cpp")
               (("settings.value(\"check_new_version\", true)")
                "settings.value(\"check_new_version\", false)"))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Make program honor $PREFIX
               (substitute* "tipp10.pro"
                 (("\\.path = /usr/") (string-append ".path = " out "/")))
               (substitute* "def/defines.h"
                 (("\"/usr/") (string-append "\"" out "/")))
               ;; Recreate Makefile
               (invoke "qmake")))))))
    (inputs
     `(("qt4" ,qt-4)
       ("sqlite" ,sqlite)))
    (home-page "https://www.tipp10.com/")
    (synopsis "Touch typing tutor")
    (description "Tipp10 is a touch typing tutor.  The ingenious thing about
the software is its intelligence feature: characters that are mistyped are
repeated more frequently.  Beginners will find their way around right away so
they can start practicing without a hitch.

Useful support functions and an extensive progress tracker, topical lessons
and the ability to create your own practice lessons make learning to type
easy.")
    (license license:gpl2)))

(define-public snap
  (package
    (name "snap")
    (version "6.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jmoenig/Snap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sqd4ddkfc7f7gx02wffvwbqgfbhpkcgyv7v5rh3gx60jca02p4w"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (out (assoc-ref %outputs "out"))
                (share (string-append out "/share/snap")))
           (copy-recursively source share)
           ;; Replace the sole minified file in the package.
           (with-directory-excursion (string-append share "/src")
             (delete-file "FileSaver.min.js")
             (symlink (string-append (assoc-ref %build-inputs "js-filesaver")
                                     "/share/javascript/FileSaver.min.js")
                      "FileSaver.min.js"))
           ;; Create a "snap" executable.
           (let* ((bin (string-append out "/bin"))
                  (script (string-append bin "/snap"))
                  (snap (string-append share "/snap.html"))
                  (bash (string-append (assoc-ref %build-inputs "bash")
                                       "/bin/sh"))
                  (xdg-open (string-append (assoc-ref %build-inputs "xdg-utils")
                                           "/bin/xdg-open")))
             (mkdir-p bin)
             (call-with-output-file script
               (lambda (port)
                 (format port "#!~a\n~a '~a'" bash xdg-open snap)))
             (chmod script #o555)))
         #t)))
    (inputs
     `(("bash" ,bash-minimal)
       ("js-filesaver" ,js-filesaver)
       ("xdg-utils" ,xdg-utils)))
    (home-page "https://snap.berkeley.edu")
    (synopsis "Visual, blocks based programming language")
    (description "Snap! (formerly BYOB) is a visual, drag-and-drop
programming language.  It is an extended reimplementation of Scratch (a
project of the Lifelong Kindergarten Group at the MIT Media Lab) that
allows you to Build Your Own Blocks.  It also features first class
lists, first class procedures, and continuations.  These added
capabilities make it suitable for a serious introduction to computer
science for high school or college students.

This package provides a @command{snap} executable calling @command{xdg-open}
to open the application in a web browser, for offline usage.")
    (license license:agpl3+)))

(define-public toutenclic
  (package
    (name "toutenclic")
    (version "7.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.bipede.fr/downloads/logiciels/"
                           "ToutEnClic-" version "-src.zip"))
       (sha256
        (base32 "0xg24p925rl5bfqsq3jb2lrkidb0f3kbmay5iyxxmjsn3ra0blyh"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/toutenclic"))
                    (pixmaps (string-append out "/share/pixmaps"))
                    (doc (string-append out "share/doc/" ,name "-" ,version))
                    (bin (string-append out "/bin"))
                    (executable "toutenclic"))
               ;; Install icon.
               (install-file "toutenclic.png" pixmaps)
               ;; Move files into "share/" directory.
               (for-each (lambda (f) (install-file f share))
                         (find-files "." "\\.py$"))
               ;; Install documentation.
               (install-file "ToutEnClic.pdf" doc)
               ;; Create executable in "bin/".
               (mkdir-p bin)
               (with-directory-excursion bin
                 (symlink (string-append share "/" executable ".py")
                          executable)))
             #t))
         (add-after 'install 'create-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications")))
               (mkdir-p applications)
               (call-with-output-file
                   (string-append applications "/toutenclic.desktop")
                 (lambda (file)
                   (format file
                           "[Desktop Entry]~@
                            Name=ToutEnClic~@
                            Comment=For schooling without difference~@
                            Exec=~a/bin/toutenclic~@
                            TryExec=~@*~a/bin/toutenclic~@
                            Terminal=false~@
                            Icon=toutenclic~@
                            Type=Application~%"
                           out)))
               #t))))))
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("python-pyqt" ,python-pyqt)))
    (synopsis "School tools for physically disabled children")
    (description "ToutEnClic is intended to facilitate the schooling
of physically disabled children in ordinary schools.  It is both
a multi-page virtual exercise book and a kit including pencil,
scissors, glue, ruler, compass, protractor and square.  A virtual
keyboard is also available if the child does not have any other
specialized device.")
    (home-page "https://bipede.fr/contrib/")
    (license license:gpl3)))

(define-public childsplay
  (package
    (name "childsplay")
    (version "3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/schoolsplay/"
                    "childsplay-" version ".tgz"))
              (sha256
               (base32
                "0z7yp2swjnbz51vn2zyfnjn40jq38l5mbh15yafmx1z3vn2z1m77"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-dejavu-font
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((dejavu-dir
                     (string-append (assoc-ref inputs "font-dejavu")
                                    "/share/fonts/truetype"))
                    (dejavu-font
                     (string-append dejavu-dir
                                    "/DejaVuSansCondensed-Bold.ttf")))
               (substitute* "SPConstants.py"
                 (("^(TTF(BOLD)? = ).*" _ prefix)
                  (string-append prefix "'" dejavu-font "'\n")))
               (for-each (lambda (f) (delete-file f))
                         (find-files "lib/SPData" "DejaVu"))
               #t)))
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pixmaps (string-append out "/share/pixmaps"))
                    (share (string-append out "/share/childsplay"))
                    (doc (string-append out "/share/doc/" ,name "-",version)))
               ;; Install icon.
               (install-file "lib/SPData/themes/childsplay/logo_cp.svg" pixmaps)
               ;; Install data.
               (mkdir-p share)
               (for-each (lambda (f)
                           (copy-recursively f (string-append share "/" f)))
                         '("alphabet-sounds" "lib" "locale" "SPWidgets"))
               (for-each (lambda (f) (install-file f share))
                         (find-files "." "\\.(py|dev|db)$"))
               ;; Install documentation.
               (mkdir-p doc)
               (copy-recursively "docs" doc)
               #t)))
         (add-after 'install 'create-executable
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((python (string-append (assoc-ref inputs "python")
                                           "/bin/python"))
                    (out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (executable (string-append bin "/childsplay")))
               (mkdir-p bin)
               (call-with-output-file executable
                 (lambda (file)
                   (format file
                           "~a ~a"
                           python
                           (string-append out "/share/childsplay/childsplay.py"))))
               (chmod executable #o555)
               #t)))
         (add-after 'install 'create-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications")))
               (mkdir-p applications)
               (call-with-output-file
                   (string-append applications "/childsplay.desktop")
                 (lambda (file)
                   (format file
                           "[Desktop Entry]~@
                            Name=Childsplay~@
                            Comment=Suite of educational games for young children~@
                            Comment[ca]=Conjunt de jocs educatius per a xiquets~@
                            Comment[es]=Conjunto de juegos educativos para niños~@
                            Comment[de]=Sammlung mit lehrreichen Spielen für kleine Kinder~@
                            Exec=~a/bin/childsplay~@
                            Terminal=false~@
                            Icon=logo_cp.svg~@
                            Type=Application~@
                            Categories=Application;Game;Education;KidsGame;~@
                            Keywords=suite;children;games;young;educational;~%"
                           out)))
               #t))))))
    (inputs
     `(("font-dejavu" ,font-dejavu)
       ("pygame" ,python2-pygame)
       ("sqlalchemy" ,python2-sqlalchemy)))
    (synopsis "Suite of educational games for young children")
    (description "Childsplay is a collection of educational activities
for young children.  Childsplay can be used at home, kindergartens and
pre-schools.  Childsplay is a fun and safe way to let young children
use the computer and at the same time teach them a little math,
letters of the alphabet, spelling, eye-hand coordination, etc.")
    (home-page "http://www.schoolsplay.org")
    (license license:gpl3+)))

(define-public omnitux
  (package
    (name "omnitux")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/omnitux/omnitux/"
                           "v" version "/omnitux-" version ".tar.bz2"))
       (sha256
        (base32 "1wmmmbzmxd0blhn00d4g91xwavnab143a31ca3i8hrqgzh6qz9w6"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove pre-compiled .pyc files from source.
           (for-each delete-file (find-files "bin" "\\.pyc$"))
           #t))))
    (build-system python-build-system)
    (inputs
     `(("python2-pygame" ,python2-pygame)
       ("python2-pygtk" ,python2-pygtk)))
    (arguments
     `(#:tests? #f                      ;no test
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (delete 'build)                ;no setup.py
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (data (string-append share "/omnitux")))
               ;; Install documentation.
               (let ((doc (string-append share "/doc/" ,name "-" ,version)))
                 (for-each (lambda (f) (install-file f doc))
                           '("LICENSE.txt" "README.txt")))
               ;; Install data.
               (install-file "omnitux.sh" data)
               (for-each (lambda (d)
                           (copy-recursively d (string-append data "/" d)))
                         '("bin" "data"))
               ;; Install the launcher.
               (let* ((bin (string-append out "/bin"))
                      (script (string-append bin "/omnitux"))
                      (bash (string-append (assoc-ref %build-inputs "bash")
                                           "/bin/bash"))
                      (python (string-append (assoc-ref %build-inputs "python")
                                             "/bin/python2")))
                 (mkdir-p bin)
                 (with-output-to-file script
                   (lambda ()
                     (format #t "#!~a~%" bash)
                     (format #t
                             "cd ~a; ~a menu.py~%"
                             (string-append data "/bin")
                             python)))
                 (chmod script #o755))
               ;; Install icon and desktop file.
               (let ((pixmaps (string-append share "/pixmaps")))
                 (install-file "data/default/icons/Omnitux_logo.svg" pixmaps))
               (let ((apps (string-append out "/share/applications")))
                 (mkdir-p apps)
                 (with-output-to-file (string-append apps "/omnitux.desktop")
                   (lambda _
                     (format #t
                             "[Desktop Entry]~@
                              Name=Omnitux~@
                              GenericName=Omnitux
                              Comment=An educational game based on multimedia elements.~@
                              Comment[fr]=Un jeu ludo-éducatif basé sur des éléments multimédias.~@
                              Exec=~a/bin/omnitux~@
                              Type=Application~@
                              Categories=Game;Education;~@
                              Terminal=false~@
                              Icon=Omnitux_logo.svg~@"
                             out))))
               #t))))))
    (home-page "http://omnitux.sourceforge.net/")
    (synopsis "Educational activities based on multimedia elements")
    (description "The project aims to provide various educational
activities around multimedia elements (images, sounds, texts).  Types
of activities include:
@itemize
@item associations,
@item items to place on a map or a schema,
@item counting activities,
@item puzzles,
@item card faces to remember,
@item find differences between two pictures,
@item ...
@end itemize

Activities are available in English, French, German, Polish,
Portuguese, Spanish and Italian.")
    ;; Project's license is GPL3+, but multimedia elements are
    ;; released under various licenses.
    (license (list license:gpl3+
                   license:gpl2+
                   license:cc-by-sa2.0
                   license:cc-by-sa3.0
                   license:public-domain))))

(define-public fet
  (package
    (name "fet")
    (version "5.48.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.lalescu.ro/liviu/fet/download/"
                           "fet-" version ".tar.bz2"))
       (sha256
        (base32 "0k728l6zi0lkhzyipsb0f2jw53s4xicm7arp33ikhrvc4jlwcp4v"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-hardcoded-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (list "fet.pro"
                                "src/src.pro"
                                "src/src-cl.pro"
                                "src/interface/fet.cpp")
               (("/usr") (assoc-ref outputs "out")))
             #t))
         (replace 'configure
           (lambda _ (invoke "qmake" "fet.pro"))))))
    (inputs
     `(("qtbase" ,qtbase)))
    (home-page "https://www.lalescu.ro/liviu/fet/")
    (synopsis "Timetabling software")
    (description
     "FET is a program for automatically scheduling the timetable of a school,
high-school or university.  It uses a fast and efficient timetabling
algorithm.

Usually, FET is able to solve a complicated timetable in maximum 5-20 minutes.
For extremely difficult timetables, it may take a longer time, a matter of
hours.")
    (license license:agpl3+)))

(define-public kalgebra
  (package
    (name "kalgebra")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kalgebra-" version ".tar.xz"))
      (sha256
       (base32 "10y9zygpik418y5781xmy5xysvf3xa97sbzdbch8lrvxwprbmkzm"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("analitza" ,analitza)
       ("kconfigwidgets" ,kconfigwidgets)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("mesa" ,mesa)
       ("ncurses" ,ncurses)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsvg" ,qtsvg)
       ("qtwebengine" ,qtwebengine)
       ("readline" ,readline)))
    (home-page "https://kde.org/applications/education/org.kde.kalgebra")
    (synopsis "Algebraic graphing calculator")
    (description "KAlgebra is an application that can replace your graphing
calculator.  It has numerical, logical, symbolic, and analysis features that
let you calculate mathematical expressions on the console and graphically plot
the results in 2D or 3D.  KAlgebra is rooted in the Mathematical Markup
Language (MathML); however, one does not need to know MathML to use KAlgebra.

This package is part of the KDE education module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kalzium
  (package
    (name "kalzium")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kalzium-" version ".tar.xz"))
       (sha256
        (base32 "1n1ar12zq2maa4dn5yq7m6l3m60n7c98c460mrd6rp7f73kadnsj"))))
    (build-system qt-build-system)
    (native-inputs
     `(("eigen" ,eigen)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(;; TODO: ("avogadrolibs" ,avogadrolibs)
       ;; TODO: facile
       ("karchive" ,karchive)
       ("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("khtml" ,khtml)
       ("ki18n" ,ki18n)
       ("kjs" ,kjs)
       ("kparts" ,kparts)
       ("kplotting" ,kplotting)
       ("kunitconversion" ,kunitconversion)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("ocaml" ,ocaml)
       ("openbabel" ,openbabel)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)
       ("qtscript" ,qtscript)
       ("qtsvg" ,qtsvg)
       ("solid" ,solid)))
    (home-page "https://kde.org/applications/education/org.kde.kalzium")
    (synopsis "Periodic table of chemistry elements")
    (description "Kalzium is a program that shows you the Periodic Table of
Elements.

You can use Kalzium to search for information about the elements or to learn
facts about the periodic table.  It provides an overview of the important
data (like melting points, electron affinity, electron negativity, electron
configuration, radii, mass, ionisation energy), an isotope table, and
different colored views of the periodic table (separation of the different
blocks, year simulator, temperature simulator).  It contains tools to
visualize the spectral lines of each element, a molecular weight calculator, a
3D molecule editor, and an equation solver for stoichiometric problems.

This package is part of the KDE education module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kanagram
  (package
    (name "kanagram")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kanagram-" version ".tar.xz"))
       (sha256
        (base32 "0bflybrm3kz1p1n6fksihvd8m0h0jj968b2wjz88663bs48jqf6q"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdeclarative" ,kdeclarative)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("knewstuff" ,knewstuff)
       ("libkeduvocdocument" ,libkeduvocdocument)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtspeech" ,qtspeech)
       ("sonnet" ,sonnet)))
    (home-page "https://kde.org/applications/education/org.kde.kanagram")
    (synopsis "Letter order game")
    (description "Kanagram is a game based on anagrams of words: the puzzle is
solved when the letters of the scrambled word are put back in the correct
order.  There is no limit on either time taken, or the amount of attempts to
solve the word.  It features several included word lists, a hints-and-cheats
help system, a word list editor, and allows for updating and distributing
wordlists via KNewStuff.  The interface is scalable and appropriate for
children.

This package is part of the KDE education module")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kbruch
  (package
    (name "kbruch")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kbruch-" version ".tar.xz"))
       (sha256
        (base32 "06sbr6wrn4nh69hq96i5rgzbr9g0rc6c54h9g2zpnpff339lnsqi"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kauth" ,kauth)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("ki18n" ,ki18n)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)))
    (home-page "https://kde.org/applications/education/org.kde.kbruch")
    (synopsis "Practice calculating with fractions")
    (description "KBruch is a small program to practice calculating with
fractions and percentages.  Different exercises are provided for this purpose
and you can use the learning mode to practice with fractions.  The program
checks the user's input and gives feedback.

This package is part of the KDE education module.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kgeography
  (package
    (name "kgeography")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kgeography-" version ".tar.xz"))
       (sha256
        (base32 "13jsrfv17lzlwi9rg7i8q2sfl8n19k15qdbv1y5lggykvf8prp8h"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kauth" ,kauth)
       ("kcodecs" ,kcodecs)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)))
    (home-page "hhttps://kde.org/applications/education/org.kde.kgeography")
    (synopsis "Geography Trainer")
    (description "KGeography is a geography learning tool, which allows you to
learn about the political divisions of some countries (divisions, capitals of
those divisions and their associated flags if there are some).

Features:
@itemize
@item Browse the maps clicking in a map division to see its name, capital and flag
@item The game tells you a map division name and you have to click on it
@item The game tells you a capital and you have to guess the division it belongs to
@item The game tells you a division and you have to guess its capital
@item The game shows you a map division flag and you have to guess its name
@item The game tells you a map division name and you have to guess its flag
@item The game shows an empty map and you have to place divisions on it one by one
@end itemize

This package is part of the KDE education module.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public khangman
  (package
    (name "khangman")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/khangman-" version ".tar.xz"))
       (sha256
        (base32 "0ljavjbh69qyp2323pqlkibzjkwgddmdjd35m0m5n4nwvnz3l5y7"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdeclarative" ,kdeclarative)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("kxmlgui" ,kxmlgui)
       ("libkeduvocdocument" ,libkeduvocdocument)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsvg" ,qtsvg)))
    (home-page "https://kde.org/applications/education/org.kde.khangman")
    (synopsis "Classical hangman game")
    (description "KHangMan is a game based on the well-known hangman game.  It
is aimed at children aged six and over.  The game has several categories of
words to play with, for example: Animals (animals words) and three difficulty
categories: Easy, Medium and Hard.  A word is picked at random, the letters
are hidden, and you must guess the word by trying one letter after another.
Each time you guess a wrong letter, part of a picture of a hangman is drawn.
You must guess the word before being hanged! You have 10 tries.

This package is part of the KDE education module.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kig
  (package
    (name "kig")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kig-" version ".tar.xz"))
       (sha256
        (base32 "0ns4rhk822p7jjqy9wnhkbrbais4ih1viw405rl5r5xlqn9bvsiz"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("boost" ,boost)
       ("karchive" ,karchive)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcrash" ,kcrash)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kparts" ,kparts)
       ("ktexteditor" ,ktexteditor)
       ("kxmlgui" ,kxmlgui)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (home-page "https://kde.org/applications/education/org.kde.kig")
    (synopsis "Interactive geometry tool")
    (description "Kig is an interactive mathematics software for learning and
teaching geometry.  It allows to explore mathematical figures and concepts
using the computer and also can serve as a drawing tool for mathematical
figures.  Constructions can be made with points, vectors, lines, and polygons
and all elements can be modified directly by using the mouse.  Kig helps
teachers and students to make conjectures and to understand how to prove
geometric theorems.

This package is part of the KDE education module.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kiten
  (package
    (name "kiten")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kiten-" version ".tar.xz"))
       (sha256
        (base32 "0dfz2wdscgn0f967lnhzpyb3iz1iw068x0l10542pm5dh32afs4m"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("karchive" ,karchive)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("khtml" ,khtml)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kjs" ,kjs)
       ("knotifications" ,knotifications)
       ("kparts" ,kparts)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ;; default icon set
       ("qtbase" ,qtbase)))
    (home-page "https://kde.org/applications/education/org.kde.kiten")
    (synopsis "Japanese reference and learning tool")
    (description "Kiten is a Japanese reference and study tool.  It features
an English to Japanese and Japanese to English dictionary, as well as a Kanji
dictionary.  For the Kanji dictionary there are multiple ways supported to
look up characters.

Kiten features:
@itemize
@item Search with english keyword, Japanese reading, or a Kanji string on a
      list of EDICT files.
@item Search with english keyword, Japanese reading, number of strokes, grade
      number, or a Kanji on a list of KANJIDIC files.
@item Limit searches to only common entries.
@item Nested searches of results possible.
@item Learning dialog. (One can even open up multiple ones and have them sync
      between each other.)
@item Browse Kanji by grade.
@item Add Kanji to a list for later learning.
@item Browse list, and get quizzed on them.
@end itemize
This package is part of the KDE education module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public klavaro
  (package
    (name "klavaro")
    (version "3.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/klavaro/klavaro-"
                            version ".tar.bz2"))
        (sha256
         (base32 "1rkxaqb62w4mv86fcnmr32lq6y0h4hh92wmsy5ddb9a8jnzx6r7w"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("curl" ,curl)
       ("gtk+" ,gtk+)
       ("pango" ,pango)))
    (home-page "https://klavaro.sourceforge.io/en/index.html")
    (synopsis "Touch typing tutor")
    (description
     "Klavaro is a simple tutor to teach correct typing, almost independently of
language and very flexible regarding to new or unknown keyboard layouts.")
    (license license:gpl3+)))

(define-public ktouch
  (package
    (name "ktouch")
    (version "20.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/release-service/"
                            version "/src/ktouch-" version ".tar.xz"))
        (sha256
         (base32
          "1s8pcwakx94aygfyjmyps5b43j4kv6dmfw7n12japcka2yfp9bi2"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-makefiles
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((qtdec (assoc-ref inputs "qtdeclarative")))
               (substitute* '("src/CMakeFiles/ktouch_autogen.dir/build.make"
                              "src/CMakeFiles/ktouch.dir/build.make")
                 (("/gnu/store/.*qmlcachegen")
                  (string-append qtdec "/bin/qmlcachegen"))))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kcmutils" ,kcmutils)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdeclarative" ,kdeclarative)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kitemviews" ,kitemviews)
       ("kqtquickcharts" ,kqtquickcharts)
       ("ktextwidgets" ,ktextwidgets)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libxcb" ,libxcb)
       ("libxkbfile" ,libxkbfile)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qtx11extras" ,qtx11extras)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (home-page "https://edu.kde.org/ktouch/")
    (synopsis "Touch typing tutor")
    (description
     "KTouch is an aid for learning how to type with speed and accuracy.  It
provides a sample text to type and indicates which fingers should be used for
each key.  A collection of lessons are included for a wide range of different
languages and keyboard layouts, and typing statistics are used to dynamically
adjust the level of difficulty.")
    (license license:gpl2)))

(define-public anki
  (package
    (name "anki")
    ;; Later versions have dependencies on npm packages not yet in Guix.
    (version "2.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://apps.ankiweb.net/downloads/archive/anki-"
                           version "-source.tgz"))
       (sha256
        (base32 "1gfr51rnllkyzli73p4r51h5ypzfa3m7lic3m3rzpywmqwrxs07k"))
       (patches (search-patches "anki-mpv-args.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #f                      ;no check target
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-update-check
           ;; Don't ‘phone home’ unasked to check for updates.
           (lambda _
             (substitute* "aqt/update.py"
               (("requests\\.post")
                "throw.an.exception.instead"))
             #t))
         (delete 'configure)            ;no configure script
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   ;; List of paths to the site-packages directories of Python
                   ;; library inputs.
                   (site-packages
                    (map (lambda (pyinput)
                           (string-append
                            (cdr pyinput)
                            "/lib/python"
                            ;; Calculate the python version to avoid breaking
                            ;; with future 3.X releases.
                            ,(version-major+minor
                              (package-version python-wrapper))
                            "/site-packages"))
                         (filter (match-lambda
                                   ((label . _)
                                    (string-prefix? "python-" label)))
                                 inputs)))
                   (qtwebengineprocess
                    (string-append (assoc-ref inputs "qtwebengine")
                                   "/lib/qt5/libexec/QtWebEngineProcess")))
               ;; The program fails to find the QtWebEngineProcess program, so
               ;; we set QTWEBENGINEPROCESS_PATH to help it.  PYTHONPATH is
               ;; wrapped to avoid declaring Python libraries as propagated
               ;; inputs.
               (for-each (lambda (program)
                           (wrap-program program
                             `("QTWEBENGINEPROCESS_PATH" =
                               (,qtwebengineprocess))
                             `("PATH" prefix (,(string-append
                                                (assoc-ref inputs "mpv")
                                                "/bin")))
                             `("PYTHONPATH" = ,site-packages)))
                         (find-files bin ".")))
             #t)))))
    (native-inputs
     `(("xdg-utils" ,xdg-utils)))
    (inputs
     `(("lame" ,lame)
       ("mpv" ,mpv)
       ("python" ,python-wrapper)
       ("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-decorator" ,python-decorator)
       ("python-distro" ,python-distro)
       ("python-jsonschema" ,python-jsonschema)
       ("python-markdown" ,python-markdown)
       ("python-pyaudio" ,python-pyaudio)
       ;; `python-pyqtwebengine' must precede `python-pyqt' in PYTHONPATH.
       ("python-pyqtwebengine" ,python-pyqtwebengine)
       ("python-pyqt" ,python-pyqt)
       ("python-requests" ,python-requests)
       ("python-send2trash" ,python-send2trash)
       ("python-sip" ,python-sip)
       ;; `qtwebengine' is included in `pyqtwebengine', included here for easy
       ;; wrapping.
       ("qtwebengine" ,qtwebengine)))
    (home-page "https://apps.ankiweb.net/")
    (synopsis "Powerful, intelligent flash cards")
    (description "Anki is a program which makes remembering things
easy.  Because it's a lot more efficient than traditional study
methods, you can either greatly decrease your time spent studying, or
greatly increase the amount you learn.

Anyone who needs to remember things in their daily life can benefit
from Anki.  Since it is content-agnostic and supports images, audio,
videos and scientific markup (via LaTeX), the possibilities are
endless.  For example:
@itemize
@item Learning a language
@item Studying for medical and law exams
@item Memorizing people's names and faces
@item Brushing up on geography
@item Mastering long poems
@item Even practicing guitar chords!
@end itemize")
    (license license:agpl3+)))

(define-public t4k-common
  (package
    (name "t4k-common")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tux4kids/t4kcommon")
             (commit (string-append "upstream/" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13q02xpmps9qg8zrzzy2gzv4a6afgi28lxk4z242j780v0gphchp"))
       (patches
        (search-patches "t4k-common-libpng16.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;FIXME: cannot find how to run tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH" (string-append (assoc-ref inputs "sdl")
                                            "/include/SDL:"
                                            (or (getenv "CPATH") "")))))
         (add-after 'unpack 'fix-andika-font-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/t4k_sdl.c"
               (("(/usr/share/.*?)/AndikaDesRevG\\.ttf")
                (string-append (assoc-ref inputs "font-andika")
                               "/share/fonts/truetype")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("font-andika" ,font-sil-andika)
       ("libpng" ,libpng)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)
       ("sdl" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-net sdl-pango)))))
    (home-page "https://github.com/tux4kids/t4kcommon")
    (synopsis "Library of code shared between TuxMath and TuxType")
    (description "Tux4Kids-Common is a library of code shared between
TuxMath and TuxType.")
    (license license:gpl3+)))

(define-public tuxmath
  (package
    (name "tuxmath")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tux4kids/tuxmath")
             (commit (string-append "upstream/" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f1pz83w6d3mbik2h6xavfxmk5apxlngxbkh80x0m55lhniwkdxv"))
       (modules '((guix build utils)))
       ;; Unbundle fonts.
       (snippet
        `(begin
           (for-each delete-file (find-files "data/fonts" "\\.ttf$"))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "sdl")
                                    "/include/SDL:"
                                    (or (getenv "CPATH") "")))
             #t))
         (add-after 'install 'install-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apps (string-append out "/share/applications"))
                    (pixmaps (string-append out "/share/pixmaps")))
               (install-file "tuxmath.desktop" apps)
               (for-each (lambda (f) (install-file f pixmaps))
                         (find-files "data/images/icons/"
                                     "tuxmath\\.(png|ico|svg)$"))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("librsvg" ,librsvg)
       ("libxml2" ,libxml2)
       ("sdl" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-net sdl-pango)))
       ("t4k-common" ,t4k-common)))
    (home-page "https://github.com/tux4kids/tuxmath")
    (synopsis "Educational math tutorial game")
    (description "@emph{Tux, of Math Command} is an educational math
tutorial game starring Tux, the Linux penguin, in which you play the
part of Commander Tux, as he defends his friends from an attack of
math equations.  Comets are crashing towards the friendly penguins in
their igloos, and you must destroy the comets by solving their
equations.

TuxMath also includes Factoroids, a game that gives practice in
factoring numbers and simplifying fractions, as well as zapping rocks
floating through space.")
    (license license:gpl3+)))

(define-public mdk
  (package
    (name "mdk")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/mdk/v" version "/mdk-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0bhk3c82kyp8167h71vdpbcr852h5blpnwggcswqqwvvykbms7lb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-gui=yes" "-with-readline=yes")))
    (native-inputs
     `(("flex" ,flex)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libglade" ,libglade)
       ("ncurses" ,ncurses)
       ("pango" ,pango)
       ("readline" ,readline)))
    (home-page "https://www.gnu.org/software/mdk/manual/")
    (synopsis "Virtual development environment for Knuth's MIX")
    (description
     "GNU MDK is the Mix Development Kit, an emulation of the pedagogical
computer MIX and its assembly language MIXAL.  MIX has a virtual CPU with
standard features such as registers, memory cells, an overflow toggle,
comparison flags, input-output devices, and a set of binary instructions.
The package includes a compiler, a virtual machine, a GUI for the virtual
machine, and more.")
    (license license:gpl3+)))

(define-public exercism
  (package
    (name "exercism")
    (version "3.0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/exercism/cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17gvz9a0sn4p36hf4l77bxhhfipf4x998iay31layqwbnzmb4xy7"))
       (patches (search-patches "exercism-disable-self-update.patch"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/exercism/cli/exercism"
       #:unpack-path "github.com/exercism/cli"
       #:install-source? #f))
    (inputs
     `(("github.com/blang/semver" ,go-github-com-blang-semver)
       ("github.com/spf13/cobra" ,go-github-com-spf13-cobra)
       ("github.com/spf13/pflag" ,go-github-com-spf13-pflag)
       ("github.com/spf13/viper" ,go-github-com-spf13-viper)
       ("golang.org/x/net" ,go-golang-org-x-net)
       ("golang.org/x/text" ,go-golang-org-x-text)))
    (home-page "https://exercism.io")
    (synopsis "Mentored learning for programming languages")
    (description "Commandline client for exercism.io, a free service providing
mentored learning for programming languages.")
    (license license:expat)))

(define-public libkeduvocdocument
  (package
    (name "libkeduvocdocument")
    (version "20.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/libkeduvocdocument-" version ".tar.xz"))
       (sha256
        (base32 "0kqnhaabfi91clqg7nlnjcybl5ca0p3ysn5zlwhxz1fcjxm83g4w"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("karchive" ,karchive)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'test-setup
           (lambda _
             (setenv "HOME" "/tmp")
             #t)))))
    (home-page "https://invent.kde.org/education/libkeduvocdocument")
    (synopsis "Library to parse, convert, and manipulate KVTML files")
    (description "Library to parse, convert, and manipulate KVTML files and
older formats including kvtml1, csv, etc.")
    (license ;; LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))
