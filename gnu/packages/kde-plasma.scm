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
  #:use-module (ice-9 textual-ports)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public bluedevil
  (package
    (name "bluedevil")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/bluedevil-" version ".tar.xz"))
      (sha256
       (base32 "0bgybz7wppavkfdiibij1nb0wj4v6nl3avsh8jxhhv1x7b3y6mz8"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("bluez-qt" ,bluez-qt)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kded" ,kded)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("knotifications" ,knotifications)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("plasma-framework" ,plasma-framework)
       ("qtbase" ,qtbase)
       ;; FIXME: Could NOT find Qt5Qml, Qt5QuickTest
       ("qtdeclarative" ,qtdeclarative)
       ("shared-mime-info" ,shared-mime-info)
       ;; file kservicetypes5/kfileitemaction-plugin.desktop"
       ;; Required for property type definitions
       ("kcmutils" ,kcmutils) ;; defines only some of the properties
       ;; run-time dependencies
       ("bluez" ,bluez)
       ("pulseaudio" ,pulseaudio))) ;; more specific: pulseaudio-module-bluetooth
    (home-page "https://invent.kde.org/plasma/bluedevil")
    (synopsis "KDE Plasma Bluetooth stack")
    (description "BlueDevil is a set of components which integrate Bluetooth
in KDE.  It contains:
@itemize
@item A KDE Control Module (KCM) to configure all the Bluetooth-related
      options.
@item Integration with the KDE input/output system (KIO), which allows you to
      discover and explore Bluetooth devices from your favorite file browser.
@item A wizard to pair your devices and connect directly to services they
      offer, such as input (mouse, keyboard, Wiimote) and audio (headsets,
      phones).
@item A system tray application from where all BlueDevil actions can be done
      (disconnect devices, send files, configure, etc).
@item A daemon which listens to incoming requests, for example to receive
      files or to introduce a requested PIN.
@end itemize")
    (license license:gpl3))) ;; KDE e.V.

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

(define-public breeze-grub
  (package
    (name "breeze-grub")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/breeze-grub-" version ".tar.xz"))
      (sha256
       (base32 "0hqqz2f073jygmiw2rl295b21xg40d37ihvj1crphl77fp0yzcm0"))))
    (build-system trivial-build-system)
    (arguments
    `(#:modules ((guix build utils))
      #:builder
        (begin
          (use-modules (guix build utils))
          (let ((theme-dir (string-append %output "/grub/themes/breeze"))
                (tar      (string-append (assoc-ref %build-inputs "tar")
                                          "/bin/tar"))
                (PATH     (string-append (assoc-ref %build-inputs "xz")
                                          "/bin")))
            (setenv "PATH" PATH)
            (system* tar "xv" "--strip-components=1"
                     "-f" (assoc-ref %build-inputs "source"))
            (mkdir-p theme-dir)
            (system* "sh" "./mkfont.sh")
            (copy-recursively "breeze" theme-dir)))))
    (native-inputs
     `(("grub" ,grub)
       ("tar" ,tar)
       ("xz" ,xz)))
    (inputs
     `(("font-gnu-unifont" ,font-gnu-unifont)))
    (home-page "https://www.kde.org/plasma-desktop")
    (synopsis "'Breeze' theme for grub")
    (description "'Breeze' theme for grub")
    (license license:gpl3+)))

;; (define-public breeze-plymouth
;;   (package
;;     (name "breeze-plymouth")
;;     (version "5.9.3")
;;     (source
;;      (origin
;;       (method url-fetch)
;;       (uri (string-append "mirror://kde/stable/plasma/" version
;;                           "/breeze-plymouth-" version ".tar.xz"))
;;       (sha256
;;        (base32 "0vz0hgx3hl03h4rkfrcaydaddljam3jbg6gd7n600a068p74s2mm"))))
;;     ;; /gnu/store/n4nmkwvjxd1mjcl91h537m9y0h5gfv9x-breeze-plymouth-5.9.3.tar.xz
;;     (build-system trivial-build-system)
;;     (arguments
;;      `(#:builder #f))
;;     ;; TODO: GuixSD branding, see README in archive. Maybe replace the
;;     ;; os.log.{png,svgz}
;;     (native-inputs
;;      `(("extra-cmake-modules" ,extra-cmake-modules)))
;;     ;;    (inputs
;;     ;;     `("plymouth" ,#f))
;;     (home-page "https://www.kde.org/plasma-desktop")
;;     (synopsis "'Breeze' theme for Plymouth")
;;     (description "'Breeze' theme for Plymouth")
;;     (license license:gpl2+)))

(define-public breeze-gtk
  (package
    (name "breeze-gtk")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/breeze-gtk-" version ".tar.xz"))
      (sha256
       (base32 "1j2nq9yw1ragmgwrz9f6ca4ifpi86qv1bbprdgd2qm2yh7vb44sj"))))
    ;; TODO: move to gnome.scm?
    (properties `((upstream-name . "breeze-gtk")))
    (build-system qt-build-system)
    (arguments
     '(#:tests? #f)) ; No 'test' target
    (native-inputs
     `(("breeze" ,breeze)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("python" ,python)
       ("python-pycairo" ,python-pycairo) ; FIXME: needs to be a propagated input?
       ("sassc" ,sassc)))
    (inputs
     `(("qtbase" ,qtbase)))
    ;; TODO: run-time dependency: GTKEngine (required for GTK 2 theme)
    (home-page "https://www.kde.org/plasma-desktop")
    (synopsis "GTK+ theme matching KDE's Breeze theme")
    (description "A GTK+ theme created to match with the Plasma 5 Breeze
theme.

To set the theme in Plasma 5, install kde-gtk-config and use System Settings >
Application Style > GNOME Application Style.  Also make sure to disable “apply
colors to non-Qt applications“ in System Settings > Colors > Options.")
    (license license:lgpl2.1+)))

(define-public discover
  (package
    (name "discover")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/discover-" version ".tar.xz"))
      (sha256
       (base32 "10izi9i9q42sjf5fa29dafmscphc9ly6vyi4ciczmfqg8mh9163g"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f ;; FIXME
       #:validate-runpath? #f ;; FIXME
       #:configure-flags
       (let* ((out (assoc-ref %outputs "out"))
              (lib (assoc-ref %outputs "out"))
              (libdir (string-append lib "/lib")))
         (list ;(string-append "-DCMAKE_INSTALL_PREFIX=" out)
               ;(string-append "-DCMAKE_INSTALL_LIBDIR=" libdir)
               ;; We need both libdir and libdir/ceph in RUNPATH.
               (string-append "-DCMAKE_INSTALL_RPATH="
                              libdir ";" libdir "/plasma-discover")
;               (string-append "-DCMAKE_INSTALL_SYSCONFDIR=" out "/etc")
;               (string-append "-DCMAKE_INSTALL_MANDIR=" out "/share/man")
;               (string-append "-DCMAKE_INSTALL_DOCDIR=" out "/share/ceph/doc")
;               (string-append "-DCMAKE_INSTALL_LIBEXECDIR=" out "/libexec")
;               "-DALLOCATOR=jemalloc"
               ))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("attica" ,attica)
       ("karchive" ,karchive)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdeclarative" ,kdeclarative)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("kxmlgui" ,kxmlgui)
       ("plasma-framework" ,plasma-framework)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ;; run-time packages
       ("kirigami", kirigami)
       ;; optional packages
       ;;packagekitqt5 http://www.packagekit.org
       ;;AppStreamQt Library that lists Appstream resources
))
    (home-page "https://www.kde.org/plasma-desktop")
    (synopsis "Application Installer for applications delivered as AppStream packages") ; correct?
    (description "KDE and Plasma resources management GUI.

This most probably is not of much use in GuixSD for now.")
    (license (list license:lgpl2.0+ license:gpl3+ license:fdl1.2+))))

(define-public kactivitymanagerd
  (package
    (name "kactivitymanagerd")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/kactivitymanagerd-" version ".tar.xz"))
      (sha256
       (base32 "1c6394bz0n0qgynhiin9q99xazsj2pn2sfisx6c9pn0jd2cs1hgf"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("boost" ,boost)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kwindowsystem", kwindowsystem)
       ("kxmlgui", kxmlgui)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/plasma/kactivitymanagerd")
    (synopsis "System service to manage user's activities, track the usage patterns etc")
    (description "When a user is interacting with a computer, there are three
main areas of contextual information that may affect the behaviour of the
system: who the user is, where they are, and what they are doing.

*Activities* deal with the last one.  An activity might be \"developing a KDE
application\", \"studying 19th century art\", \"composing music\" or
\"watching funny videos\".  Each of these activites may involve multiple
applications, and a single application may be used in multiple activities (for
example, most activities are likely to involve using a web browser, but
different activities will probably involve different websites).

KActivities provides the infrastructure needed to manage a user's activites,
allowing them to switch between tasks, and for applications to update their
state to match the user's current activity.  This includes a daemon, a library
for interacting with that daemon, and plugins for integration with other
frameworks.")
    (license license:gpl3+)))

(define-public kde-cli-tools
  (package
    (name "kde-cli-tools")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/kde-cli-tools-" version ".tar.xz"))
      (sha256
       (base32 "1iw9afdyydng6wcxqy9jf6x9dayskzasmq10252j9d70mqr4hc87"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f)) ; TODO: 1/1 fails – propably due to search-path errors
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kactivities" ,kactivities)
       ("kcmutils" ,kcmutils)
       ("kconfig" ,kconfig)
       ("kdeclarative" ,kdeclarative)
       ("kdesu" ,kdesu)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kinit" ,kinit)
       ("kio" ,kio)
       ("kwindowsystem" ,kwindowsystem)
       ("plasma-workspace" ,plasma-workspace) ;; FIXME
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)))
    (home-page "https://invent.kde.org/plasma/kde-cli-tools")
    (synopsis "Tools to use KDE services from the command line")
    (description "These command line tools enable you to use KDE services such
as kioslaves, kdesu, QtSVG, kcontrol modules, KDE trader and start
applications from the command line.")
    (license (list license:gpl3+ license:gpl2)))) ;; KDE e.V.

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

(define-public kdeplasma-addons
  (package
    (name "kdeplasma-addons")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/kdeplasma-addons-" version ".tar.xz"))
      (sha256
       (base32 "08zzjc7kbqdg4qlrf9liybkzailhj4xc8xwg92yimny7afsi17k5"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f)) ;; 1/1 tests fails
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kactivities" ,kactivities)
       ("karchive" ,karchive)
       ("kcmutils" ,kcmutils)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdeclarative" ,kdeclarative)
       ("kholidays" ,kholidays)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("kparts" , kparts)
       ("kross" ,kross)
       ("krunner" ,krunner)
       ("kservice" ,kservice)
       ("kunitconversion" ,kunitconversion)
       ("plasma-framework" ,plasma-framework)
       ("plasma-workspace" ,plasma-workspace)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtscript" ,qtscript)
       ("qtx11extras" ,qtx11extras)
       ("purpose" ,purpose) ;; for the QuickShare applet
       ("qtwebengine" ,qtwebengine) ;; for the the web browser applet
       ("sonnet", sonnet)))
    (home-page "https://techbase.kde.org/Projects/Plasma/Plasmoids")
    (synopsis "Additionsl applets and engines for KDE Plasma")
    (description "Kdeplasma is a compilation of Plasma add-ons (runners,
applets, widgets, wallpappers, plasmoids, etc.)")
    (license (list license:gpl2+ license:lgpl2.0+))))

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
     `(("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("qtbase" ,qtbase)))
    (home-page "http://www.kde.org/")
    (synopsis "Monitor calibration panel for KDE")
    (description "This package contains a settings panel for adjusting the
brightness, contrast, and gamma-correction of a monitor.  Test patterns are
shown to help determine the settings that accurately display the full range of
colors.

Each of the red, green, and blue components can be adjusted individually, or
all three components can be adjusted together.")
    (license license:gpl2+)))

(define-public khotkeys
  (package
    (name "khotkeys")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/khotkeys-" version ".tar.xz"))
      (sha256
       (base32 "1nhvnflq7nzxkyfnzx1vdvjh4yrpi1qsf6mjjrnpg8w5ncgbd32n"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcmutils" ,kcmutils)
       ("kdbusaddons" ,kdbusaddons)
       ("kdelibs4support" ,kdelibs4support)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kxmlgui" ,kxmlgui)
       ("plasma-framwork" ,plasma-framework)
       ("plasma-workspace" ,plasma-workspace)
       ("qtx11extras" ,qtx11extras)))
    (home-page "https://invent.kde.org/plasma/khotkeys")
    (synopsis "Configure input actions for KDE Plasma")
    (description "KHotKeys is core-part of the KDE Plasma desktop.")
    (license license:gpl2+)))

(define-public kmenuedit
  (package
    (name "kmenuedit")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/kmenuedit-" version ".tar.xz"))
      (sha256
       (base32 "1xl9k7a3hcmz9yfx7yhy7ff8mcqg62bs4fll8r8nzm04qds1qw7l"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kdbusaddons" ,kdbusaddons)
       ("kded" ,kded) ; required for property type definitions
       ("kglobalaccel" ,kglobalaccel)
       ("khotkeys", khotkeys) ; optional
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kinit" ,kinit)
       ("kio" ,kio)
       ("kxmlgui" ,kxmlgui)
       ("plasma-framework" ,plasma-framework)
       ("qtbase" ,qtbase)
       ("sonnet" ,sonnet)))
    (home-page "https://invent.kde.org/plasma/kmenuedit")
    (synopsis "Plasma XDG Menu Editor")
    (description "This package provides a menu editor which may be used to
edit the KDE Plasma workspaces menu or any other XDG menu.")
    (license (list license:gpl2+ license:gpl2))))

(define-public kscreen
  (package
    (name "kscreen")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/kscreen-" version ".tar.xz"))
      (sha256
       (base32 "1s2ngg86128665mf6q7dnwv953jf2d4ad25klrm8wd85lj3wcwhd"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f ;; TODO: tests fail even when blacklisted
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             ;; The test suite requires a running X server.
             (system "Xvfb :98 -screen 0 640x480x24 &")
             (setenv "DISPLAY" ":98")
             ;; blacklist failing test-functions TODO: make them pass
             (with-output-to-file "bin/BLACKLIST"
               (lambda _
                 (display "[testNullConfig]\n*\n")))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("xorg-server" ,xorg-server))) ; required for the tests
    (inputs
     `(("kauth" ,kauth)
       ("kcmutils" ,kcmutils)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kdeclarative" ,kdeclarative)
       ("kded" ,kded) ;; missing in CMakeList.txt
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libkscreen" ,libkscreen)
       ("plasma-framework" ,plasma-framework)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsensors", qtsensors)))
    (home-page "https://projects.kde.org/projects/extragear/base/kscreen")
    (synopsis "KDE monitor hotplug and screen handling")
    (description "The KDE multiple monitor support is trying to be as smart as
possible adapting the behavior of it to each use case making the configuration
of monitors as simple as plugging them to your computer.

This package contains the modules and plugins for monitor hot-plugging and
automatic screen handling.")
    (license license:lgpl2.1+)))

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

(define-public kwallet-pam
  (package
    (name "kwallet-pam")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/kwallet-pam-" version ".tar.xz"))
      (sha256
       (base32 "0vwilwji3zvygahbp56h64v508gms2qg5nvh518v70jbcnbyv2kq"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f ;; no make target 'test'
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-socat-path
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Always install into /lib and not into /lib64.
             (substitute* "pam_kwallet_init"
               ((" socat ")
                (string-append " " (assoc-ref inputs "socat")
                               "/bin/socat ")))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kwallet" ,kwallet)
       ("libgcrypt" ,libgcrypt)
       ("pam" ,linux-pam)
       ("qtbase" ,qtbase)
       ("socat" ,socat)))
    (home-page "https://invent.kde.org/plasma/kwallet-pam")
    (synopsis "PAM module for KWallet")
    (description "KWallet (KDE Frameworks 5) integration with PAM")
    (license license:lgpl2.1+)))

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

(define-public kwrited
  (package
    (name "kwrited")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/kwrited-" version ".tar.xz"))
      (sha256
       (base32 "1nzl5964brxwkhgsadpa94bm985vq3qi75sml1fmgnw1ap4djd69"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("knotifications" ,knotifications)
       ;; FIXME: kwrited is currently build as an executable, since we don't
       ;; have utempter yet and thus kpty is build without utempter-support.
       ("kpty" ,kpty)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/plasma/kwrited")
    (synopsis "Read and write console output to X")
    (description "Kwrited captures console output (e.g. broadcast messages)
and prints it in a X window.")
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

(define-public milou
  (package
    (name "milou")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/milou-" version ".tar.xz"))
      (sha256
       (base32 "03h4m7wfl7ffsklngx5fwxd4mj082df9j0m8mlz6z3x98v3fbipd"))))
    (build-system qt-build-system)
    ;; TOOO: warnings during generation of metainfo for org.kde.milou: Package
    ;; type "Plasma/Applet" not found
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kdeclarative" ,kdeclarative)
       ("kwindowsystem" ,kwindowsystem)
       ("ki18n" ,ki18n)
       ("kitemmodels" ,kitemmodels)
       ("krunner" ,krunner)
       ("kservice" ,kservice)
       ("plasma-framework" ,plasma-framework)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtscript" ,qtscript)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/plasma/milou")
    (synopsis "Dedicated search plasmoid built on top of Baloo")
    (description "Milou can also be used as an alternative to KRunner, and
does provide application launching.  The main difference is that it
concentrates more on searching.")
    (license license:gpl3))) ;; KDE e.V.

(define-public oxygen
  (package
    (name "oxygen")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/oxygen-" version ".tar.xz"))
      (sha256
       (base32 "0d8cvg6h5zf0l4zs6sv7kplfa49n2brs7njwq8znw7c51zdd85s2"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kde-frameworkintegration" ,kde-frameworkintegration)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kiconthemes" ,kiconthemes)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kservice" ,kservice)
       ("kwayland" ,kwayland)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("kcmutils" ,kcmutils)
       ("kdecoration" ,kdecoration)
       ("qtx11extras" ,qtx11extras)))
    (home-page "https://invent.kde.org/plasma/oxygen")
    (synopsis "Widget and icon theme")
    (description "Plasma and Qt widget style and window decorations for Plasma
5 and KDE 4.")
    ;; Parts of the code is Expat licensed, other parts GPL-3+. The artwork is
    ;; under some different licenses.
    (license (list license:expat license:lgpl3+ ;; KDE e.V.
                   license:gpl2+ license:lgpl2.1))))

(define-public plasma-desktop
  (package
    (name "plasma-desktop")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/plasma-desktop-" version ".tar.xz"))
      (sha256
       (base32 "0w0snwgckz11gfwybjwf7wdb3cg366z3bqjrj5pnaf64xn0iqgn6"))))
    (build-system qt-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after `unpack `fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "kcms/dateandtime/helper.cpp"
               ;; define path to bin/hwclock
               ;; TODO: Rethink! NIX defines the path, but this leads to
               ;; util-linux being an additional requirement. We can just
               ;; leave this off and let KCM search $PATH
               (("(^\\s*QString hwclock = )QStandardPaths::findExecutable.*" l prefix)
                (string-append prefix "QLatin1String(\""
                               (assoc-ref inputs "util-linux")
                               "/bin/hwclock\");"))
               ;; define path to zoneinfo
               ;; TODO: nix also has a patch to honor $TZDIR
               (("\"/usr/share/zoneinfo/\"")
                (string-append "\"" (assoc-ref inputs "tzdata")
                               "/share/zoneinfo/\"")))
             (substitute* "kcms/keyboard/iso_codes.h"
               ;; define path to iso-codes
               (("\"/usr/share/xml/iso-codes\"")
                (string-append "\"" (assoc-ref inputs "iso-codes")
                               "/share/xml/iso-codes\"")))
               #t))
         (add-after 'unpack 'patch-qml-import-path
           (lambda _
             (substitute*
              '("applets/pager/package/contents/ui/main.qml"
                "containments/desktop/package/contents/ui/FolderView.qml"
                "containments/desktop/package/contents/ui/main.qml"
                "containments/panel/contents/ui/main.qml")
              (("^import \"(utils|FolderTools|LayoutManager).js\" as " line mod)
               (string-append "import \"../code/" mod ".js\" as ")))
             #t))
         (add-after 'unpack 'patch-includes
           ;; TODO: Is this correct? Why do other distributions not need this?
           (lambda _
             (substitute*
              '("kcms/touchpad/backends/x11/libinputtouchpad.cpp"
                "kcms/touchpad/backends/x11/xlibbackend.cpp"
                "kcms/touchpad/backends/x11/xlibtouchpad.cpp")
              (("^#include <xserver-properties.h>")
               "#include <xorg/xserver-properties.h>"))
             #t))
         ;; (delete 'check)
         ;; (add-after 'install 'check
         ;;   (assoc-ref %standard-phases 'check))
         (add-before 'check 'check-setup
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "HOME" (getcwd))
             ;; (setenv "QT_PLUGIN_PATH"
             ;;         (string-append (assoc-ref outputs "out") "/lib/qt5/plugins:"
             ;;                        (getenv "QT_PLUGIN_PATH")))
             ;; The test 'keyboard-geometry_parser' queries for device
             ;; information, for this it requires a running X server.
             ;;(system "Xvfb :98 -screen 0 640x480x24 &")
             ;;(setenv "DISPLAY" ":98")
             (with-output-to-file "bin/BLACKLIST"
               (lambda _
                 ;; test_kio_fonts - kcms/kfontinst/kio
                 ;; "URL cannot be listed: fonts://foo/System"
                 (display "[testDirLister]\n*\n")
                 ;; lookandfeel-kcmTest - kcms/lookandfeel
                 (display "[testWidgetStyle]\n*\n[testKCMSave]\n*\n")
                 ;; foldermodeltest - containments/desktop/plugins/folder
                 (display "[tst_rename]\n*\n")))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus) ; required for running the tests
       ("kdoctools" ,kdoctools)
       ("pkg-config" ,pkg-config)))
    (inputs
     ;; TODO: Still some unknown property types, e.g for key "X-Plasma-API",
     ;;  "X-KDE-ParentApp", "X-Plasma-RemoteLocation", "X-Plasma-MainScript".
     ;; TODO: Add more optional inputs: ibus-1.0, gio, gobject-2.0, scim, glib2
     ;; TODO: "Recommended" input AppStreamQt. appstreamcli
     `(("attica" ,attica)
       ("baloo" ,baloo) ; recommended
       ("boost" ,boost) ; optional
       ("breeze" ,breeze)
       ("eudev", eudev) ; optional
       ("fontconfig" ,fontconfig) ;; package fontutils
       ("iso-codes" ,iso-codes) ; for path-substitution (see phases), required for testing
       ("kactivities" ,kactivities)
       ("kactivities-stats" ,kactivities-stats)
       ("kauth" ,kauth)
       ("kcmutils" ,kcmutils)
       ("kconfig" ,kconfig)
       ("kdbusaddons" ,kdbusaddons)
       ("kdeclarative" ,kdeclarative)
       ("kded" ,kded) ; not checked in CmakeList
       ("kdelibs4support" ,kdelibs4support)
       ("kemoticons" ,kemoticons)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kirigami", kirigami) ;; ~~~runtime dependency~~
       ("kitemmodels" ,kitemmodels)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kpeople" ,kpeople)
       ("krunner" ,krunner)
       ("kscreenlocker" ,kscreenlocker)
       ("kwallet" ,kwallet)
       ("kwin" ,kwin)
       ("libcanberra_kde" , libcanberra) ; optional
       ("libksysguard" ,libksysguard)
       ("libxkbcommon" ,libxkbcommon)
       ("libxcursor", libxcursor) ; not checked in CMakelist
       ("libxi" ,libxi) ;; X11-Xinput, for kcms/input/
       ("libxft" ,libxft) ; feature
       ("libxkbfile" ,libxkbfile)
       ("libxtst", libxtst) ; not checked in CMakelist
       ("phonon" ,phonon)
       ("plasma-framework" ,plasma-framework)
       ("plasma-workspace" ,plasma-workspace)
       ("pulseaudio" ,pulseaudio) ; optional
       ("qqc2-desktop-style" ,qqc2-desktop-style)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("tzdata" ,tzdata) ; for path-substitution (see phases)
       ("util-linux" ,util-linux) ; for path-substitution (see phases)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xf86-input-evdev" ,xf86-input-evdev)
       ("xf86-input-libinput", xf86-input-libinput)
       ("xf86-input-synaptics" ,xf86-input-synaptics)
       ("xkeyboard-config" ,xkeyboard-config)
       ("xorg-server" ,xorg-server))) ;; xserver-properties.h, not checked in CmakeList
    (home-page "https://invent.kde.org/plasma/plasma-desktop")
    (synopsis "Plasma 5 application workspace components")
    (description "Tools and widgets for the desktop")
    (license (list license:lgpl2.0+ license:gpl2+ license:fdl1.2+))))

(define-public plasma-integration
  (package
    (name "plasma-integration")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/plasma-integration-" version ".tar.xz"))
      (sha256
       (base32 "12b3zgmvqg3nbgbqm785bbqjvm5ysw5ikvqn04qnldlfwxadizgp"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         ;; FIXME: re-enable checkes
         ;; (add-after 'install 'check
         ;;   (lambda _
         ;;     ;; Exclude a "Received signal" test TODO: Make this test pass
         ;;     (invoke "ctest" "."
         ;;             "-E" "frameworkintegration-kfiledialog_unittest")))
         (add-before 'check 'check-setup2
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "HOME" (getcwd))
             (setenv "XDG_RUNTIME_DIR" "..")
             (setenv "QT_PLUGIN_PATH"
                     (string-append (assoc-ref outputs "out") "/lib/qt5/plugins:"
                                    (getenv "QT_PLUGIN_PATH")))
             ;; The test suite requires a running X server.
             (system "Xvfb :98 -screen 0 640x480x24 &")
             (setenv "DISPLAY" ":98")
             ;; blacklist failing test-functions TODO: make them pass
             (with-output-to-file "autotests/BLACKLIST"
               (lambda _
                 ;; In frameworkintegration-kdeplatformtheme_unittest
                 (display "[testPlatformHints]\n*\n")
                 (display "[testPlatformPalette]\n*\n")
                 (display "[testPlatformHintChanges]\n*\n")
                 ;; In frameworkintegration-kfiledialogqml_unittest
                 (display "[testShowDialogParentless]\n*\n")
                 (display "[testShowDialogWithParent]\n*\n")))
             #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("dbus" ,dbus)
       ("xorg-server" ,xorg-server) ; required for running the tests
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("breeze" ,breeze)
       ;; Other distributions do not install these fonts as a dependency of
       ;; this package.
       ;;("font-google-noto" ,font-google-noto) ; runtime dependency
       ;;("font-hack" ,font-hack) ; runtime dependency
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("knotifications" ,knotifications)
       ("kwayland" ,kwayland)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("libxcursor" ,libxcursor)
       ("perl" ,perl) ; for the kconf_update scripts
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative) ; for generating auto-tests
       ("qtquickcontrols" ,qtquickcontrols) ; for running the tests
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("qtx11extras" ,qtx11extras)
       ("xcb-util" ,xcb-util)))
    (home-page "https://invent.kde.org/plasma/plasma-integration")
    (synopsis "Integration of Qt application with KDE workspaces")
    (description "Plasma Framework Integration is a set of plugins responsible
for better integration of Qt applications when running on a KDE Plasma
workspace.  Applications do not need to link to this directly.")
    (license license:lgpl3+))) ;; KDE e.V.

(define-public plasma-nm
  (package
    (name "plasma-nm")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/plasma-nm-" version ".tar.xz"))
      (sha256
       (base32 "1f9b9zb4lmg91a3hibf7yx4l8syzvv2n42addmkdxbmckcrf0gs3"))))
    (build-system qt-build-system)
    ;; TODO: Still some unknown property types, e.g for key
    ;; "X-KDE-ModuleType", "X-NetworkManager-Services"
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     ;; TODO: Think about enabling openconnect
     `(("kcompletion" ,kcompletion)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kdeclarative" ,kdeclarative)
       ("kded" ,kded) ;; required for property type definitions
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kinit" ,kinit)
       ("kio" ,kio)
       ("kitemviews" ,kitemviews)
       ("knotifications" ,knotifications)
       ("kservice" ,kservice)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("mobile-broadband-provider-info" ,mobile-broadband-provider-info)
       ("modemmanager-qt" ,modemmanager-qt)
       ("networkmanager-qt" ,networkmanager-qt)
       ("plasma-framework" ,plasma-framework)
       ("qca" ,qca)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("solid" ,solid)))
    (home-page "https://invent.kde.org/plasma/plasma-nm")
    (synopsis "Plasma5 NetworkManager applet")
    (description "This package contains the Network Management plasma widget
which aims to provide a fully featured GUI for managing networks.  Currently,
the only supported backend is NetworkManager.  Support for wired, wireless,
mobile and VPN networks are provided.  The widget is exclusively written for
KDE Plasma workspaces and it is not supposed to work in other environments.

In order to start using the widget, look for 'Network Management' in the 'Add
Widgets' dialog of the Plasma workspace you're using.")
    (license license:gpl3))) ;; KDE e.V.

(define-public plasma-pa
  (package
    (name "plasma-pa")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/plasma-pa-" version ".tar.xz"))
      (sha256
       (base32 "1n4imxjfz4zvhgzmsn10kdmxh7cdx8n54n4gwwx5zlvvgjybj3mh"))))
    ;; TODO: Still some unknown property types, e.g for key
    ;;  "X-KDE-ParentApp", "X-Plasma-API", "X-DocPath", "X-KDE-Keywords".
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gconf" ,gconf)
       ("kcmutils" ,kcmutils) ;; required for property type definitions
       ("kcoreaddons" ,kcoreaddons)
       ("kdeclarative" ,kdeclarative)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("knotifications" ,knotifications)
       ("kwindowsystem" ,kwindowsystem)
       ("libcanberra" ,libcanberra)
       ("perl" ,perl) ; for the kconf_update scripts
       ("plasma-framework" ,plasma-framework)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (home-page "https://invent.kde.org/plasma/plasma-pa")
    (synopsis "Plasma 5 Volume controller")
    (description "Plasma applet for audio volume management using
PulseAudio.")
    (license license:gpl3))) ;; KDE e.V.

(define-public plasma-sdk
  (package
    (name "plasma-sdk")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/plasma-sdk-" version ".tar.xz"))
      (sha256
       (base32 "086s60lr5wf5yab0la8p40v1czviw3bnj8hmh4jy9366nvdllhwc"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    ;; TODO: Still some unknown property types, e.g for key
    ;; "X-KDE-ParentApp", "X-DocPath", "X-KDE-Keywords".
    ;; TOOO: warnings during generation of metainfo for
    ;; org.kde.plasma.plasmoidviewershell: Package type "Plasma/Shell" not
    ;; found
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("qttools" ,qttools)))
    (inputs
     `(("grantlee" ,grantlee)
       ("karchive" ,karchive)
       ("kcmutils" ,kcmutils)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("kdeclarative" ,kdeclarative)
       ("kdevelop" ,kdevelop)
       ("kguiaddons" ,kguiaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kirigami", kirigami) ;; runtime dependency
       ("kitemmodels" ,kitemmodels)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kservice" ,kservice)
       ("ktexteditor" ,ktexteditor)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("plasma-framework" ,plasma-framework)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsvg" ,qtsvg)
       ("qtwebkit" ,qtwebkit)
       ("threadweaver" ,threadweaver)))
    (home-page "https://community.kde.org/Plasma/DeveloperGuide")
    (synopsis "Development tools for Plasma 5 components")
    (description "Plasma SDK contains the following tools for Plasma-related
development:
@enumerate
@item CuttleFish - icon theme browser
@item EngineExplorer - tool to browse and interact with data engines
@item PlasmoidViewer - an isolated Plasma environment for testing applets
@item ThemeExplorer - shows all components of a widget theme
@end enumerate")
    (license (list license:gpl2 license:gpl2+ license:gpl3+))))

(define plasma-tests
  (package
    (name "plasma-tests")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/plasma-tests-" version ".tar.xz"))
      (sha256
       (base32 "1ywh4dqzyj96l92v94z6lghkv49davf1d7drvrf1c0lw8j7zvpri"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    ;; TODO: Add input appstreamcli
    (home-page "https://invent.kde.org/plasma/plasma-tests")
    (synopsis "Integration-tests for the Plasma workspace")
    (description "Distributions should not package it, but might want to run
it as part of their Plasma builds.")
    ;; No license to be found in the archive. Since most parts of KDE are
    ;; GPL2+, I assume that here, too.
    (license license:gpl2+)))

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

(define-public plasma-workspace
  (package
    (name "plasma-workspace")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/plasma-workspace-" version ".tar.xz"))
      (sha256
       (base32 "09w1rrkppqnjcnw0hczmmhfzavmph2zk54bad7fnw0b3ivh2j0s9"))
      (patches (search-patches "plasma-workspace-startkde.patch"))))
    (build-system qt-build-system)
    (arguments
     `(;;#:configure-flags '("-Wno-dev") ;; too many dev-warnings, silence them
       #:modules ((ice-9 textual-ports)
                  ,@%qt-build-system-modules)
       #:phases
       (modify-phases (@ (guix build qt-build-system) %standard-phases)
         (add-after 'unpack 'patch-qml-import-path
           (lambda* (#:key outputs #:allow-other-keys)
             ;; NIX: absolute-wallpaper-install-dir.patch
             (substitute* "sddm-theme/theme.conf.cmake"
               (("\\$\\{CMAKE_INSTALL_PREFIX}/\\$\\{WALLPAPER_INSTALL_DIR}")
                (string-append (assoc-ref outputs "breeze")
                               "/share/wallpapers")))
             #t))
         (add-before 'configure 'add-definitions
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (dbus (assoc-ref inputs "dbus"))
                   (kinit (assoc-ref inputs "kinit"))
                   (xmessage (assoc-ref inputs "xmessage"))
                   (xprop (assoc-ref inputs "xprop"))
                   (xrdb (assoc-ref inputs "xrdb"))
                   (xsetroot (assoc-ref inputs "xsetroot")))
               (with-output-to-file "CMakeLists.txt.new"
                 (lambda _
                   (display
                    (string-append
                     "add_compile_definitions(\n"
                     "NIXPKGS_XMESSAGE=\"" xmessage "/bin/xmessage\"\n"
                     "NIXPKGS_XRDB=\"" xrdb "/bin/xrdb\"\n"
                     "NIXPKGS_XSETROOT=\"" xsetroot "/bin/xsetroot\"\n"
                     "NIXPKGS_XPROP=\"" xprop "/bin/xprop\"\n"
                     "NIXPKGS_DBUS_UPDATE_ACTIVATION_ENVIRONMENT=\""
                     dbus "/bin/dbus-update-activation-environment\"\n"
                     ;;"NIXPKGS_START_KDEINIT_WRAPPER=\""
                     ;;kinit "/lib/libexec/kf5/start_kdeinit_wrapper\"\n"   ;; FIXME: should be in /libexec
                     "NIXPKGS_KDEINIT5_SHUTDOWN=\""
                     kinit "/bin/kdeinit5_shutdown\"\n"
                     ")\n\n"))
                   (display
                    (call-with-input-file "CMakeLists.txt"
                      get-string-all))))
               (rename-file "CMakeLists.txt.new" "CMakeLists.txt"))
             #t))
         (replace 'check
           ;; TODO: Make this test pass. check-after-install, setting
           ;; QT_PLUGIN_PATH, starting a X11-server did not suffice to make
           ;; testdesktop pass.
           ;; launchertasksmodeltest fails since it relies on .desktop-files
           ;; from installed dolphin and konquerer, see
           ;; <https://bugs.kde.org/386458>
           (lambda _
             (invoke "ctest" "." "-E" "testdesktop|launchertasksmodeltest")))
         (add-before 'check 'check-setup
           (lambda _
             (setenv "HOME" (getcwd))
             #t))
         )))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("kdoctools" ,kdoctools)))
    (propagated-inputs
     `(("iso-codes" ,iso-codes))); run-time dependency
    ;; TODO: Warning at /gnu/store/…-kpackage-5.34.0/…/KF5PackageMacros.cmake:
    ;;   warnings during generation of metainfo for org.kde.breeze.desktop:
    ;;   Package type "Plasma/LookAndFeel" not found
    ;; TODO: Warning at /gnu/store/…-kpackage-5.37.0/…/KF5PackageMacros.cmake:
    ;;  warnings during generation of metainfo for org.kde.image:
    ;;  Package type "Plasma/Wallpaper" not found
    ;; TODO: Still some unknown property types, e.g for key "X-KDE-ParentApp",
    ;; "X-Plasma-RemoteLocation", "X-Plasma-EngineName",
    ;; "X-Plasma-MainScript".
    (inputs
     `(;; TODO: Optional: AppStreamQt, Qalculate, libgps
       ("baloo" ,baloo)
       ("breeze" ,breeze)
       ("dbus" ,dbus) ;; run-time dependency
       ("kactivities" ,kactivities)
       ("kactivities-stats" ,kactivities-stats)
       ("kcmutils" ,kcmutils) ; nicht in NIX
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdeclarative" ,kdeclarative)
       ("kded" ,kded) ;
       ("kdelibs4support" ,kdelibs4support)
       ("kdesu" ,kdesu)
       ("kglobalaccel" ,kglobalaccel)
       ("kholidays" ,kholidays) ;; optional, for Plasma Calendar plugin
       ("ki18n" ,ki18n)
       ("kidletime" ,kidletime)
       ("kinit" ,kinit) ;; required by startkde, not listed as a requirement
       ("kjs" ,kjs)
       ("kjsembed" ,kjsembed)
       ("knewstuff" ,knewstuff)
       ("knotifyconfig" ,knotifyconfig)
       ("kpackage" ,kpackage)
       ("kpeople" ,kpeople)
       ("krunner" ,krunner)
       ("kscreenlocker" ,kscreenlocker)
       ("kirigami" ,kirigami)  ;; run-time dependency  FIXME: needs patching?
       ("ksysguard" ,ksysguard)
       ("ktexteditor" ,ktexteditor)
       ("ktextwidgets" ,ktextwidgets)
       ("kuserfeedback" ,kuserfeedback) ; telemetry :-(
       ("kwallet" ,kwallet)
       ("kwayland" ,kwayland)
       ("kwin" ,kwin)
       ("kxmlrpcclient" ,kxmlrpcclient)
       ("libkscreen" ,libkscreen)
       ("libksysguard" ,libksysguard)
       ("libsm" ,libsm)
       ("libxrender" ,libxrender)
       ("libxtst" ,libxtst) ; not listed as a requirement
       ("networkmanager-qt" ,networkmanager-qt)
       ("phonon" ,phonon)
       ("plasma-framework" ,plasma-framework)
       ("prison" ,prison)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtscript" ,qtscript)
       ("qtx11extras" ,qtx11extras)
       ("solid" ,solid)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xrdb" ,xrdb) ;; run-time dependency
       ("xmessage" ,xmessage) ;; run-time dependency
       ("xprop" ,xprop) ;; run-time dependency
       ("xsetroot" ,xsetroot) ;; run-time dependency
       ("zlib" ,zlib)))
    (home-page "https://invent.kde.org/plasma/plasma-workspace")
    (synopsis "Plasma workspace components for KF5")
    (description "Workspaces provide support for KDE Plasma Widgets,
integrated search, hardware management and a high degree of customizability.")
    ;; Parts of the code is Expat licensed, other parts GPL-3+ and even other
    ;; parts are LGPL2.1+. The artwork is under some different licenses.
    (license (list license:expat license:lgpl3+ ;; KDE e.V.
                   license:gpl2 license:lgpl2.1 license:gpl2+))))

(define-public plasma-workspace-wallpapers
  (package
    (name "plasma-workspace-wallpapers")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/plasma-workspace-wallpapers-" version ".tar.xz"))
      (sha256
       (base32 "1ybl2flm9a8g1ycvsyn2wimwm7vxpiyrsvlnq1qwlv1m6mhc7vja"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (home-page "http://vdesign.kde.org/")
    (synopsis "Wallpapers for Plasma workspace")
    (description "This package contains the default wallpapers for the Plasma
workspace.")
    ;; The archive includes the text of both gpl2 and lgpl3, but the files are
    ;; actually all lgpl3 (according to the metadata.desktop files).
    (license license:lgpl3)))

(define-public polkit-kde-agent-1
  (package
    (name "polkit-kde-agent-1")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/polkit-kde-agent-1-" version ".tar.xz"))
      (sha256
       (base32 "0c6v19sp7fcqfbjz8lnb6dr3znwl00q1iv6pakrq09x8a3s2jq87"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("polkit-qt5" ,polkit-qt)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/plasma/polkit-kde-agent-1")
    (synopsis "PolicyKit authentication agent dialogs for KDE")
    (description "PolicyKit is an application-level toolkit for defining and
handling the policy that allows unprivileged processes to speak to privileged
processes.  It is a framework for centralizing the decision making process
with respect to granting access to privileged operations (like calling the HAL
Mount() method) for unprivileged (desktop) applications.

PolicyKit-Kde provides a D-Bus session bus service that is used to bring up
authentication dialogs used for obtaining privileges.")
    (license license:gpl2+)))

(define-public powerdevil
  (package
    (name "powerdevil")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/powerdevil-" version ".tar.xz"))
      (sha256
       (base32 "06mcm361b07i175lnn5prdyrjfm9q9pa9b0rnzv9g04ja0y02pyi"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bluez-qt" ,bluez-qt)
       ("eudev", eudev)
       ("kactivities" ,kactivities)
       ("kauth" ,kauth)
       ("kconfig" ,kconfig)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kidletime" ,kidletime)
       ("kio" ,kio)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kwayland" ,kwayland)
       ("libkscreen" ,libkscreen)
       ("networkmanager-qt" ,networkmanager-qt)
       ("plasma-workspace" ,plasma-workspace)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtx11extras" ,qtx11extras)
       ("solid" ,solid)
       ("xcb-util-keysyms" ,xcb-util-keysyms)))
    (home-page "https://invent.kde.org/plasma/powerdevil")
    (synopsis "Manages the power consumption in Plasma")
    (description "Power Devil sets and manages the power consumption according
to your settings.")
    (license license:gpl2+)))

(define-public sddm-kcm
  (package
    (name "sddm-kcm")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/sddm-kcm-" version ".tar.xz"))
      (sha256
       (base32 "0f77pmg7m8s3vjswl3ksam2rrpawy25p88w1rfigqiy83y1cd8iv"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("karchive" ,karchive)
       ("kauth" ,kauth)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("knewstuff" ,knewstuff)
       ("kxmlgui" ,kxmlgui)
       ("libxcursor" ,libxcursor) ;; Missing as dependency
       ("qtbase" ,qtbase)
       ("xcb-util-image" ,xcb-util-image)
       ("qtdeclarative" ,qtdeclarative)
       ("qtx11extras" ,qtx11extras)))
    (home-page "https://invent.kde.org/plasma/sddm-kcm")
    (synopsis "SDDM configuration module for KDE")
    (description "This is a System Settings configuration module for
configuring the SDDM Display Manager.")
    (license license:gpl2+)))

(define-public systemsettings
  (package
    (name "systemsettings")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/systemsettings-" version ".tar.xz"))
      (sha256
       (base32 "1y9gkqjn985v51pbi0gg13p2w31bp76wbxnpp9qp47l6ihmxr3xs"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kactivities" ,kactivities)
       ("kactivities-stats" ,kactivities-stats)
       ("kcmutils" ,kcmutils)
       ("kconfig" ,kconfig)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdeclarative" ,kdeclarative)
       ("kguiaddons" ,kguiaddons)
       ("khtml" ,khtml)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kitemmodels" ,kitemmodels)
       ("kitemviews" ,kitemviews)
       ("kjs" ,kjs)
       ("kpackage" ,kpackage)
       ("kparts" ,kparts)
       ("kservice" ,kservice)
       ("kwindowsystem" ,kwindowsystem)
       ("kirigami" ,kirigami)
       ("kxmlgui" ,kxmlgui)
       ("plasma-workspace" ,plasma-workspace)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (home-page "https://invent.kde.org/plasma/systemsettings")
    (synopsis "Plasma System Settings")
    (description "Plasma System Settings is an improved user interface for
configuring the desktop and other aspects of the system.")
    (license license:gpl2+)))

(define-public user-manager
  (package
    (name "user-manager")
    (version "5.19.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/plasma/" version
                          "/user-manager-" version ".tar.xz"))
      (sha256
       (base32 "1k31lkxd5l37avwr63c4lcmwm0l6l6xifi4qhs07qa7nfljhn9ir"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("kauth" ,kauth)
       ("kcmutils" ,kcmutils)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("libpwquality" ,libpwquality)
       ("qtbase" ,qtbase)))
    (home-page "https://invent.kde.org/plasma/user-manager")
    (synopsis "User management tool for the Plasma workspace")
    (description "A simple system settings module to manage the users on your
system.")
    (license license:gpl2+)))
