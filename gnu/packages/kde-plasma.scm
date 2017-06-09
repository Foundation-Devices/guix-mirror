;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu packages kde-plasma)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public breeze
  (package
    (name "breeze")
    (version "5.19.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0dpk1w7zcafrzf46j060i1qb0fwqpsflkfzr6gcar81llmjnc4b1"))))
    (build-system qt-build-system)
    ;; TODO: Warning at /gnu/store/…-kpackage-5.34.0/…/KF5PackageMacros.cmake:
    ;;   warnings during generation of metainfo for org.kde.breezedark.desktop:
    ;;   Package type "Plasma/LookAndFeel" not found
    ;; TODO: Check whether is makes sence splitting into several outputs, like
    ;; Debian does:
    ;; - breeze-cursor-theme
    ;; - "out", "devel"
    ;; - kde-style-breeze - Widget style
    ;; - kde-style-breeze-qt4 - propably not useful
    ;; - kwin-style-breeze
    ;; - qml-module-qtquick-controls-styles-breeze - QtQuick style
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kcmutils" ,kcmutils) ; optional
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kde-frameworkintegration" ,kde-frameworkintegration) ; optional
       ("kdecoration" ,kdecoration)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes) ; for optional kde-frameworkintegration
       ("kpackage" ,kpackage)
       ("kwayland" ,kwayland) ; optional
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative) ; optional
       ("qtx11extras" ,qtx11extras)))
    (home-page "https://invent.kde.org/plasma/breeze")
    (synopsis "Default KDE Plasma theme")
    (description "Artwork, styles and assets for the Breeze visual style for
the Plasma Desktop.  Breeze is the default theme for the KDE Plasma desktop.")
    (license license:gpl2+)))

(define-public kdecoration
  (package
    (name "kdecoration")
    (version "5.19.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kdecoration-" version ".tar.xz"))
              (sha256
               (base32
                "0pn8n7zyb0adzjnn92vmbcf7pmpss60k9k1rk5llamj016xzfgnf"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/plasma/kdecoration")
    (synopsis "Plugin based library to create window decorations")
    (description "KDecoration is a library to create window decorations.
These window decorations can be used by for example an X11 based window
manager which re-parents a Client window to a window decoration frame.")
    (license license:lgpl3+)))

(define-public ksshaskpass
  (package
    (name "ksshaskpass")
    (version "5.19.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/ksshaskpass-" version ".tar.xz"))
              (sha256
               (base32
                "1k2va2v9051f71w78dn3gihk642iyy5yzrkcfnp97fag8g6dpisi"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/plasma/ksshaskpass")
    (synopsis "Front-end for ssh-add using kwallet")
    (description "Ksshaskpass is a front-end for @code{ssh-add} which stores the
password of the ssh key in KWallet.  Ksshaskpass is not meant to be executed
directly, you need to tell @code{ssh-add} about it.  @code{ssh-add} will then
call it if it is not associated to a terminal.")
    (license license:gpl2+)))

(define-public kscreenlocker
  (package
    (name "kscreenlocker")
    (version "5.19.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kscreenlocker-" version ".tar.xz"))
              (sha256
               (base32
                "1fd5sqaqx9kj3kr0bgxpllhcm5arf8bc9pkpd9yk9c8xjy0j0fxi"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f ;; TODO: make tests pass
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (system (string-append (assoc-ref inputs "xorg-server")
                                   "/bin/Xvfb :1 -screen 0 640x480x24 &"))
             (setenv "DISPLAY" ":1")
             #t))
         (delete 'check)
         ;; Tests use the installed library and require a DBus session.
         (add-after 'install 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (begin
                   (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
                   (invoke "dbus-launch" "ctest" ".")))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ;; For tests.
       ("dbus" ,dbus)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("kcmutils" ,kcmutils)
       ("kcrash" ,kcrash)
       ("kdeclarative" ,kdeclarative)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kidletime" ,kidletime)
       ("knotifications" ,knotifications)
       ("ktextwidgets" ,ktextwidgets)
       ("kwayland" ,kwayland)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libseccomp" ,libseccomp) ;for sandboxing the look'n'feel package
       ("libxcursor" ,libxcursor) ;missing in CMakeList.txt
       ("libxi" ,libxi)           ;XInput, required for grabbing XInput2 devices
       ("linux-pam" ,linux-pam)
       ("logind" ,elogind)        ;optional loginctl support
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtx11extras" ,qtx11extras)
       ("solid" ,solid)
       ("wayland" ,wayland)
       ("xcb-util-keysyms" ,xcb-util-keysyms)))
    (home-page "https://invent.kde.org/plasma/kscreenlocker")
    (synopsis "Screen locking library")
    (description
     "@code{kscreenlocker} is a library for creating secure lock screens.")
    (license license:gpl2+)))


(define-public ksysguard
  (package
    (name "ksysguard")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/ksysguard-" version ".tar.xz"))
      (sha256
       (base32 "0ivw4sgiwxd522lybs7h62cqagw90vnfklx5knhsv0h4fp4nv9p1"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f)) ;; 2/5 tests fail, probably due to dbus issues
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kinit" ,kinit)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("kwindowsystem" ,kwindowsystem)
       ("libksysguard", libksysguard)
       ("lm-sensors" ,lm-sensors "lib")
       ("qtbase" ,qtbase)))
    (home-page "https://www.kde.org/applications/system/ksysguard/")
    (synopsis "Plasma process and performance monitor")
    (description "KSysGuard is a program to monitor various elements of your
system, or any other remote system with the KSysGuard daemon (ksysgardd)
installed.

It features a client/server architecture that allows monitoring of local as
well as remote hosts.  The graphical front end uses so-called sensors to
retrieve the information it displays.  A sensor can return simple values or
more complex information like tables.  For each type of information, one or
more displays are provided.  Displays are organized in worksheets that can be
saved and loaded independently from each other.  So, KSysGuard is not only a
simple task manager but also a very powerful tool to control large server
farms.

Currently the daemon has been ported to Linux, FreeBSD, Irix,
NetBSD, OpenBSD, Solaris and Tru64 with varying degrees of completion.")
    (license (list license:gpl2 license:gpl2+ license:gpl3)))) ;; KDE e.V.

(define-public kwayland-integration
  (package
    (name "kwayland-integration")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/kwayland-integration-" version ".tar.xz"))
      (sha256
       (base32 "054a5hr601f1dzy2fmd4qbbrpbdbj7w4cy216r3n0mnj9irys6vd"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f  ;; FIXME: try to make this pass
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check-after-install
           (assoc-ref %standard-phases 'check))
         (add-before 'check-after-install 'check-setup
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "QT_PLUGIN_PATH"
                     (string-append (assoc-ref outputs "out")
                                    "/lib/qt5/plugins:"
                                    (getenv "QT_PLUGIN_PATH")))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("qtwayland" ,qtwayland))) ;; required for the tests
    (inputs
     `(("kguiaddons" ,kguiaddons)
       ("kidletime" ,kidletime)
       ("kwayland" ,kwayland)
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/plasma/kwayland-integration")
    (synopsis "KWayland runtime integration plugins")
    (description "Provides integration plugins for various KDE Frameworks for
Wayland")
    (license license:lgpl3+))) ; KDE e.V.

(define-public kwayland-server
  (package
    (name "kwayland-server")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/kwayland-server-" version ".tar.xz"))
      (sha256
       (base32 "06jlr24f3vhla8rjyygd7r51byksfv46apnh3bylslgxd2grrzah"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f  ;; TODO: seem to require a running wayland server
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'workaround-https://bugs.kde.org/428295
           (lambda _
             (mkdir-p (string-append %output "/include/KF5")))))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("qtwayland" ,qtwayland)
       ("wayland" ,wayland))) ;; required for the tests
    (inputs
     `(("kwayland" ,kwayland)
       ("kwindowsystem" ,kwindowsystem)
       ("plasma-wayland-protocols" ,plasma-wayland-protocols)
       ("qtbase" ,qtbase)
       ("wayland-protocols" ,wayland-protocols)))
    (home-page "https://invent.kde.org/plasma/kwayland-server")
    (synopsis "Wayland Server Components built on KDE Frameworks")
    (description "This package provides two libraries: KWayland::Client and
KWaylandServer.  As the names suggest they implement a Client respectively a
Server API for the Wayland protocol.  The API is Qt-styled removing the needs
to interact with a for a Qt developer uncomfortable low-level C-API. For
example the callback mechanism from the Wayland API is replaced by signals;
data types are adjusted to be what a Qt developer expects, e.g.  two arguments
of int are represented by a QPoint or a QSize.")
    (license license:lgpl2.1))) ; KDE e.V.

(define-public kwin
  (package
    (name "kwin")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/kwin-" version ".tar.xz"))
      (sha256
       (base32 "0fwh6khbn87i6sx2krq0mlakxhvcy2hjzqzlp2yc0c9xfxxs7brn"))))
    (build-system qt-build-system)
    (arguments
     '(#:tests? #f ;; TODO 14/147 tests fail – even with the phases below
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             ;; follow symlinks - taken from NIX
             (substitute* "plugins/kdecorations/aurorae/src/aurorae.cpp"
               (("^(\\s*QDirIterator it\\(path, QDirIterator::Subdirectories)(\\);)" _ a b)
                (string-append a " | QDirIterator::FollowSymlinks " b)))
             ;; hard-code path to xwayland binary - taken from NIX
             ;; FIXME: This will install xorg-server-xwayland on all systems
             ;; using kwin :-(
             (substitute* "main_wayland.cpp"
               (("^(\\s*m_xwaylandProcess->setProgram\\(QStringLiteral\\(\")Xwayland(\"\\)\\);)" _ a b)
                (string-append a (assoc-ref inputs "xorg-server-xwayland")
                               "/bin/Xwayland" b)))
             #t))
         (add-after 'install 'check
           ;; NOTE: "normal phase check disables by #:tests #f
           ;;(assoc-ref %standard-phases 'check))
           (lambda _
             ;;(invoke "ctest" ".")
             #t))
         (add-before 'check 'check-setup
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "HOME" (getcwd))
             (setenv "XDG_RUNTIME_DIR" "..")
             ;; auprobieren, ob das was bringt:
             (setenv "XDG_DATA_DIRS"
                     (string-append (assoc-ref outputs "out") "/share:"
                                    (getenv "XDG_DATA_DIRS")))
             (setenv "QT_PLUGIN_PATH"
                     (string-append (assoc-ref outputs "out") "/lib/qt5/plugins:"
                                    (getenv "QT_PLUGIN_PATH")))
             ;; The test suite requires a running X server.
             (system "Xvfb :98 -screen 0 640x480x24 &")
             (setenv "DISPLAY" ":98")
             #t))
         (add-after 'install 'add-symlinks
           ;; Some package(s) refer to these service types by the wrong name.
           ;; I would prefer to patch those packages, but I cannot find them!
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((kst5 (string-append (assoc-ref outputs "out")
                                        "/share/kservicetypes5/")))
               (symlink (string-append kst5 "kwineffect.desktop")
                        (string-append kst5 "kwin-effect.desktop"))
               (symlink (string-append kst5 "kwinscript.desktop")
                        (string-append kst5 "kwin-script.desktop")))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("kdoctools" ,kdoctools)
       ("qttools" ,qttools)
       ;; the remaining ones are required native for running the tests
       ("dbus" ,dbus)
       ("kwayland-integration" ,kwayland-integration)
       ("kwindowsystem" ,kwindowsystem)
       ("qtwayland" ,qtwayland)
       ("xorg-server" ,xorg-server)
       ("xorg-server-xwayland" ,xorg-server-xwayland)))
    (inputs
     `(("breeze" ,breeze)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("kactivities" ,kactivities)
       ("kcmutils" ,kcmutils)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdeclarative" ,kdeclarative)
       ("kdecoration" ,kdecoration)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kidletime" ,kidletime)
       ("kinit" ,kinit)
       ("kio" ,kio)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("kpackage" ,kpackage)
       ("kscreenlocker" ,kscreenlocker)
       ("kservice" ,kservice)
       ("ktextwidgets" ,ktextwidgets)
       ("kwayland" ,kwayland)
       ("kwayland-server" ,kwayland-server)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("libdrm" ,libdrm)
       ("libepoxy" ,libepoxy)
       ("libice" ,libice) ;; missing in CMakeList.txt
       ("libinput" ,libinput)
       ("libsm" ,libsm) ;; missing in CMakeList.txt
       ("libxi" ,libxi)
       ("libxkbcommon" ,libxkbcommon)
       ("plasma-framework" ,plasma-framework)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtmultimedia" ,qtmultimedia)
       ("qtscript" ,qtscript)
       ("qtsensors" ,qtsensors)
       ("qtx11extras" ,qtx11extras)
       ("wayland" ,wayland)
       ("xcb-util" ,xcb-util) ;; missing in CMakeList.txt
       ("xcb-util-cursor" ,xcb-util-cursor)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ;; TODO: optional feature: libhybris allows to run bionic-based HW
       ;; adaptations in glibc systems.
       ("xcb-util-wm" ,xcb-util-wm))) ; for icccm:, optional
    (home-page "http://community.kde.org/KWin")
    (synopsis "KDE Plasma 5 Window Manager")
    (description " KWin is the default window manager for the KDE Plasma
Desktop.  It gives you complete control over your windows, making sure they're
not in the way but aid you in your task.  It paints the window decoration, the
bar on top of every window with (configurable) buttons like close, maximize
and minimize.  It also handles placing of windows and switching between
them.")
    (license license:gpl2+)))

(define-public libkscreen
  (package
    (name "libkscreen")
    (version "5.19.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "0rf1pm0yyc069f4n5s9ipdx4glzfr9zvv5cbrmn4q9i4v6z1qd8i"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ;; For testing.
       ("dbus" ,dbus)))
    (inputs
     `(("kwayland" ,kwayland)
       ("libxrandr" ,libxrandr)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (arguments
     '(#:tests? #f)) ; FIXME: 55% tests passed, 5 tests failed out of 11
    (home-page "https://community.kde.org/Solid/Projects/ScreenManagement")
    (synopsis "KDE's screen management software")
    (description "KScreen is the new screen management software for KDE Plasma
Workspaces which tries to be as magic and automatic as possible for users with
basic needs and easy to configure for those who want special setups.")
    (license license:gpl2+)))

(define-public libksysguard
  (package
    (name "libksysguard")
    (version "5.19.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde//stable/plasma/" version
                           "/libksysguard-" version ".tar.xz"))
       (sha256
        (base32 "1kd0h3p8bf9k5pqp0frhr81pa0yyrpkckg9zznirk9p1v88v7bfq"))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kconfigwidgets" ,kconfigwidgets)
       ("kiconthemes" ,kiconthemes)
       ("kwindowsystem" ,kwindowsystem)
       ("ki18n" ,ki18n)
       ("kauth" ,kauth)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kdeclarative" ,kdeclarative)
       ("kglobalaccel" ,kglobalaccel)
       ("kio" ,kio)
       ("knewstuff" ,knewstuff)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kservice" ,kservice)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtscript" ,qtscript)
       ("qtwebkit" ,qtwebkit)
       ("qtx11extras" ,qtx11extras)
       ("plasma" ,plasma-framework)
       ("zlib" ,zlib)))
    (build-system qt-build-system)
    (arguments
     `(#:configure-flags
       `(,(string-append "-DKDE_INSTALL_DATADIR="
                         (assoc-ref %outputs "out") "/share"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-cmakelists
           (lambda _
             ;; TODO: Verify: This should no longer be necessary, since
             ;; KF5AuthConfig.cmake.in contains this already.
             (substitute* "processcore/CMakeLists.txt"
               (("KAUTH_HELPER_INSTALL_DIR") "KDE_INSTALL_LIBEXECDIR"))))
         (replace 'check
           (lambda _
             ;; TODO: Fix this failing test-case
             (invoke "ctest" "-E" "processtest"))))))
    (home-page "https://userbase.kde.org/KSysGuard")
    (synopsis "Network enabled task and system monitoring")
    (description "KSysGuard can obtain information on system load and
manage running processes.  It obtains this information by interacting
with a ksysguardd daemon, which may also run on a remote system.")
    (license license:gpl3+)))

(define-public plasma-wayland-protocols
  (package
    (name "plasma-wayland-protocols")
    (version "1.1.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://kde/stable/plasma-wayland-protocols/"
            version "/plasma-wayland-protocols-" version ".tar.xz"))
      (sha256
       (base32 "0f0xllv092aliyngyi25hhm66q38jsrj36pa6ls1q5qms61k6jf5"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (arguments
     `(#:tests? #f)) ;; package has no tests
    (home-page "https://invent.kde.org/libraries/plasma-wayland-protocols")
    (synopsis "Plasma-specific protocols for Wayland")
    (description "This project should be installing only the xml files of the
non-standard wayland protocols we use in Plasma..")
    (license license:gpl2+)))
