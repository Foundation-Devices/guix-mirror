;; rm -rf /tmp/guix-build-kwin-5.10.4.drv-*
;; ./pre-inst-env guix build -K kwin
;; tar c -C /tmp/guix-build-kwin-5.10.4.drv-0 -f /tmp/guix-build-kwin-prebuild.tar .
;; ./pre-inst-env guix build -K -e '(@ (gnu packages kde-plasma) kwin-test)'
;; ./pre-inst-env guix build -K -f test-kwin.scm

(use-modules (guix)
             (guix build-system gnu)
             (gnu packages kde-plasma)
             (gnu packages kde-frameworks)
             (gnu packages qt)
             (gnu packages xorg) ;; for X11 server and Xwayland
             (gnu packages xdisorg)
             (gnu packages glib) ;; for dbus
             (gnu packages linux) ;; for strace

             (gnu packages base)
             (gnu packages boost)
             (gnu packages bootloaders)
             (gnu packages compression)
             (gnu packages fonts)
             (gnu packages fontutils)
             (gnu packages freedesktop)
             (gnu packages gl)
             (gnu packages glib)
             (gnu packages gnome)
             (gnu packages gnupg)
             (gnu packages iso-codes)
             (gnu packages networking)
             (gnu packages pkg-config)
             (gnu packages polkit)
             (gnu packages pulseaudio)
             
;             (srfi srfi-1)
             )

(define OUT "/gnu/store/p96xaxq0nyvihsvj1rqy3f5ahvibpkvx-kwin-5.10.4")

;;;http://svnweb.mageia.org/packages/cauldron/kf5-macros/releases/5.38.0/1.mga7/SOURCES/kf5.macros?revision=1161289&view=co


(package (inherit kwin)
    (source (local-file "/tmp/guix-build-kwin-prebuild.tar"))
    (native-inputs
     `(("dbus" ,dbus)
       ("xorg-server" ,xorg-server) ; required for running the tests
       ("xorg-server-xwayland" ,xorg-server-xwayland) ; required for running the tests
       ("kwayland-integration" ,kwayland-integration) ; required for running the tests
       ("kwindowsystem" ,kwindowsystem) ; required for running the tests
       ("plasma-framework" ,plasma-framework) ;; maybe
       ("breeze" ,breeze) ;; maybe?
       ("qtwayland" ,qtwayland) ;; meybe propagate with kwayland?
       ("libxkbcommon" ,libxkbcommon) ;; maybe?
       ("strace" ,strace)


       ("breeze" ,breeze)
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
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libdrm" ,libdrm)
       ("libepoxy" ,libepoxy)
       ("libice" ,libice) ;; missing in CMakeList.txt
       ("libinput" ,libinput)
       ("libsm" ,libsm) ;; missing in CMakeList.txt
       ("libxkbcommon" ,libxkbcommon)
       ("plasma-framework" ,plasma-framework)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtmultimedia" ,qtmultimedia)
       ("qtscript" ,qtscript)
       ("qtx11extras" ,qtx11extras)
       ("wayland" ,wayland)
       ("xcb-util-cursor" ,xcb-util-cursor)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)

       
    ;;    ,@(alist-delete
    ;;       "python"
       ,@(package-native-inputs kwin)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "../build")))
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (delete 'check-setup)
         (add-after 'install 'check
         ;;   (assoc-ref %standard-phases 'check))
         ;;(replace 'check
           (lambda _
             (zero? (system*
                     ;;"dbus-launch"
                     "strace" "-f" "-e" "trace=all" "-o" "../trace.txt"
                     ;;"autotests/integration/testStart"
                     "ctest" "." ;;"--verbose"
                     "-E" "kwin-testXClipboardSync"
                     ;; ;;"-R" "kwin-testDontCrashGlxgears$"
                     ;;"-R" "kwin-testStart"
                     ))))
         ;;(delete 'install)
         (add-after 'check 'kill-daemons
           (lambda _
             (system "ps as")
             (system "echo ----------------------")
             (system "killall --user")
             (system "ps as")
             (system "echo ----------------------")
             #t))
         (add-before 'check 'check-setup
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; make Qt render "offscreen", required for tests
             ;(setenv "QT_QPA_PLATFORM" "offscreen")
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (setenv "XDG_RUNTIME_DIR" "..")
             (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
             (setenv "HOME" (getcwd))
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (setenv "QT_PLUGIN_PATH"
                     (string-append
                      ,OUT "/lib/qt5/plugins:"
                      ;(assoc-ref inputs "kwindowsystem") "/lib/qt5/plugins:"
                      (getenv "QT_PLUGIN_PATH")))
             ;; The test suite requires a running X server, setting
             ;; QT_QPA_PLATFORM=offscreen does not suffice and even make
             ;; some tests fail.
             (system (string-append (assoc-ref inputs "xorg-server")
                                   "/bin/Xvfb :1 -screen 0 640x480x24 &"))
             (setenv "DISPLAY" ":1")
             (system* "ls" "-l"
                      ,OUT
                      ,(string-append OUT "/lib/qt5/plugins"))
             #t))
    ))))
