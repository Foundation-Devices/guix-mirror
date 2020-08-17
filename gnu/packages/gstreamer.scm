;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Leo Prikler <leo.prikler@student.tugraz.at>
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

(define-module (gnu packages gstreamer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages xml))

(define-public openni2
  (package
    (name "openni2")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/occipital/OpenNI2.git")
         (commit (string-append "v" version "-debian"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mfnyzpq53wnzgjfx91xcbx0nrl0lp1vrk1rk20a3gb3kshsr675"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("graphviz" ,graphviz)
       ("doxygen" ,doxygen)
       ("jdk" ,icedtea "jdk")
       ("jre" ,icedtea)
       ("python" ,python-wrapper)))
    (inputs
     `(("freeglut3" ,freeglut)
       ("libudev" ,eudev)
       ("libusb" ,libusb)))
    (synopsis "Framework for sensor-based 'Natural Interaction")
    (description "OpenNI is a framework for getting data to support
'Natural Interaction', i.e. skeleton tracking, gesture tracking, and similar
ways of getting data from humans.  It provides the interface for physical devices
and for middleware components.")
    (home-page "https://structure.io/openni")
    (license license:asl2.0)))

(define-public libdc1394
  (package
    (name "libdc1394")
    (version "2.2.6")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://sourceforge.net/projects/" name "/files/"
                              name "-2" "/" version "/" name "-" version ".tar.gz"))
              (sha256
               (base32 "1v8gq54n1pg8izn7s15yylwjf8r1l1dmzbm2yvf6pv2fmb4mz41b"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glu" ,glu)
       ("libraw1394" ,libraw1394)
       ("libusb" ,libusb)
       ("libxv" ,libxv)
       ("linux-headers" ,linux-libre-headers)
       ("mesa" ,mesa)
       ("sdl" ,sdl)
       ("v4l" ,v4l-utils)))
    (synopsis "1394-Based Digital Camera Control Library")
    (description "LibDC1394 is a library that provides functionality to control
any camera that conforms to the 1394-Based Digital Camera Specification written
by the 1394 Trade Association.  It utilizes the lowlevel functionality provided
by libraw1394 to communicate with the camera.  It also uses the video1394 kernel
module for the DMA capture of the video flow.")
    (home-page "https://damien.douxchamps.net/ieee1394/libdc1394/")
    (license license:lgpl2.0+)))

(define-public ccextractor
  (package
    (name "ccextractor")
    (version "0.88")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/CCExtractor/ccextractor.git")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sya45hvv4d46bk7541yimmafgvgyhkpsvwfz9kv6pm4yi1lz6nb"))))
    (build-system cmake-build-system)
    (arguments
     `(#:python? #t ; To wrap binaries
       #:tests? #f                      ; No target
       #:configure-flags
       (list
        "-DWITH_FFMPEG=ON"
        "-DWITH_OCR=ON"
        "-DWITH_SHARING=ON"
        "-DWITH_HARDSUBX=ON")
       #:phases
       (modify-phases %standard-phases
         ;; The package is in a sub-dir of this repo.
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "src")
             #t))
         (add-after 'chdir 'fix-build-errors
           (lambda _
             (substitute* "CMakeLists.txt"
               (("libnanomsg")
                "nanomsg"))
             #t)))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("ffmeg" ,ffmpeg-3.4)
       ("nanomsg" ,nanomsg)
       ("ocr" ,tesseract-ocr)
       ("zlib" ,zlib)))
    (synopsis "Closed Caption Extractor")
    (description "CCExtractor is a tool that analyzes video files and produces
independent subtitle files from the closed captions data.  It is portable, small,
and very fast.")
    (home-page "https://www.ccextractor.org/")
    (license license:gpl2+)))

(define-public libvisual
  (package
    (name "libvisual")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/Libvisual/libvisual.git")
         (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02xwakwkqjsznc03pjlb6hcv1li1gw3r8xvyswqsm4msix5xq18a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The package is in a sub-dir of this repo.
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "libvisual")
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("libintl" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("sdl" ,sdl)))
    (native-search-paths
     (list
      (search-path-specification
       (variable "LIBVISUAL_PLUGINS_BASE_DIR")
       (files '("lib/libvisual-0.4")))))
    ;; To load libvisual-plugins.
    (search-paths native-search-paths)
    (synopsis "Audio visualisation library")
    (description "Libvisual is a library that acts as a middle layer between
applications that want audio visualisation and audio visualisation plugins.")
    (home-page "http://libvisual.org/")
    (license
     (list
      ;; Libraries.
      license:lgpl2.1+
      ;; Examples and Tests.
      license:gpl2+))))

(define-public libvisual-plugins
  (package
    (name "libvisual-plugins")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/Libvisual/libvisual.git")
         (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02xwakwkqjsznc03pjlb6hcv1li1gw3r8xvyswqsm4msix5xq18a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-gstreamer-plugin"
        "--disable-corona"
        "--disable-gforce"
        (string-append "--with-plugins-base-dir=" (assoc-ref %outputs "out")
                       "/lib/libvisual-0.4"))
       #:phases
       (modify-phases %standard-phases
         ;; The package is in a sub-dir of this repo.
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "libvisual-plugins")
             #t)))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("libintl" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("esound" ,esound)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("gtk+" ,gtk+-2)
       ("jack" ,jack-2)
       ("libx11" ,libx11)
       ("libxext" ,libxext)))
    (propagated-inputs
     `(("libvisual" ,libvisual)))
    (synopsis "Audio visualisation library")
    (description "Libvisual is a library that acts as a middle layer between
applications that want audio visualisation and audio visualisation plugins.")
    (home-page "http://libvisual.org/")
    (license license:gpl2+)))

(define-public esound
  (package
    (name "esound")
    (version "0.2.41")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.gnome.org/Archive/esound.git")
         (commit "ESOUND_0_2_41")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "141jg70fim276i8k2kyypm84gy89i1k9mm4yf68mfwnybvjw1d6n"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("gnome-common" ,gnome-common)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("tcsh" ,tcsh)                      ; for the tests
       ("which" ,which)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("pcaudiolib" ,pcaudiolib)
       ("tcp-wrappers" ,tcp-wrappers)))
    (propagated-inputs
     `(("audiofile" ,audiofile)))
    (synopsis "Enlightened Sound Daemon")
    (description "The Enlightened Sound Daemon mixes several audio streams for
playback by a single audio device.  You can also pre-load samples, and play them
back without having to send all the data for the sound.  Network transparency is
also built in, so you can play sounds on one machine, and listen to them on
another.")
    (home-page "https://web.archive.org/web/20160528230227/
http://www.tux.org/~ricdude/overview.html")
    (license
     (list
      ;; Libraries.
      license:lgpl2.0+
      ;; Others.
      license:gpl2+))))

(define-public orc
  (package
    (name "orc")
    (version "0.4.31")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gstreamer.freedesktop.org/data/src/"
                                  "orc/orc-" version ".tar.xz"))
              (sha256
               (base32
                "0xb0c7q3xv1ldmz5ipybazb01gy3cijj8622dcx7rbm9lq85zax0"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-faulty-test
           (lambda _
             ;; XXX Disable the 'test-limits' and 'exec_opcodes_sys'
             ;; tests, which fail on some machines.  See:
             ;; https://bugzilla.gnome.org/show_bug.cgi?id=735273
             (substitute* '("testsuite/test-limits.c"
                            "testsuite/exec_opcodes_sys.c")
               (("if \\(error\\) return 1;")
                "if (error) return 77;"))
             #t)))))
    (native-inputs
     `(("gtk-doc" ,gtk-doc)))
    (home-page "https://gstreamer.freedesktop.org/modules/orc.html")
    (synopsis "Oil runtime compiler")
    (description
     "Orc is a just-in-time compiler implemented as a library and set of
associated tools for compiling and executing simple programs that operate on
arrays of data.")
    ;; The source code implementing the Marsenne Twister algorithm is licensed
    ;; under the 3-clause BSD license, the rest is under 2-clause BSD license.
    (license (list license:bsd-2 license:bsd-3))))

;; Increase the test timeouts to accommodate slow or busy machines.
(define %common-gstreamer-phases
  '((add-after 'unpack 'increase-test-timeout
      (lambda _
        (substitute* "tests/check/meson.build"
          (("'CK_DEFAULT_TIMEOUT', '20'")
           "'CK_DEFAULT_TIMEOUT', '60'")
          (("timeout ?: 3 \\* 60")
           "timeout: 9 * 60"))
        #t))))

(define-public gstreamer
  (package
    (name "gstreamer")
     (version "1.16.2")
     (source
      (origin
       (method url-fetch)
       (uri
        (string-append
         "https://gstreamer.freedesktop.org/src/gstreamer/gstreamer-"
         version ".tar.xz"))
       (sha256
        (base32 "0kp93622y29pck8asvil1fmzf55s2gx76wv475a6izc3cwj49w73"))))
     (build-system meson-build-system)
     (outputs '("out" "doc"))
     (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
        (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* '("gst/building.xml" "gst/running.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml-4.3")
                                 "/xml/dtd/docbook/")))
               (substitute* '("libs/gstreamer-libs-docs.sgml"
                              "plugins/gstreamer-plugins-docs.sgml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml-4.1.2")
                                 "/xml/dtd/docbook/"))))
             #t))
          (add-after 'install 'move-docs
            (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
                (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
                #t))))))
     (native-inputs
     `(("bash-completion" ,bash-completion)
       ("bison" ,bison)
       ("docbook-xml-4.1.2" ,docbook-xml-4.1.2)
       ("docbook-xml-4.3" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
        ("flex" ,flex)
       ("gettext" ,gettext-minimal)
        ("glib" ,glib "bin")
        ("gobject-introspection" ,gobject-introspection)
        ("gtk-doc" ,gtk-doc)
       ("libxml2" ,libxml2)
        ("perl" ,perl)
        ("pkg-config" ,pkg-config)
        ("python-wrapper" ,python-wrapper)))
    (inputs
     `(("gmp" ,gmp)
       ("gsl" ,gsl)
       ("gtk+" ,gtk+)
       ("setcap" ,libcap)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("libdw" ,elfutils)
       ("libunwind" ,libunwind)))
     (native-search-paths
     (list
      (search-path-specification
       (variable "GST_PLUGIN_SYSTEM_PATH")
       (files '("lib/gstreamer-1.0")))))
     ;; To load gst-plugins.
     (search-paths native-search-paths)
    (synopsis "Multimedia framework core library")
    (description "GStreamer is a library for constructing graphs of
media-handling components.  The applications it supports range from simple
Ogg/Vorbis playback, audio/video streaming to complex audio and video
processing.")
     (home-page "https://gstreamer.freedesktop.org/")
     (license license:lgpl2.0+)))

(define-public gst-plugins-base
  (package
    (name "gst-plugins-base")
    (version "1.16.2")
     (source
      (origin
        (method url-fetch)
       (uri
        (string-append "https://gstreamer.freedesktop.org/src/" name "/"
                       name "-" version ".tar.xz"))
        (sha256
        (base32 "0sl1hxlyq46r02k7z70v09vx1gi4rcypqmzra9jid93lzvi76gmi"))))
     (build-system meson-build-system)
    (outputs '("out" "doc"))
     (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:python? #t ; To wrap binaries
        #:phases
        (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* '("libs/compiling.sgml"
                              "libs/gst-plugins-base-libs-docs.sgml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'unpack 'patch-tests
            (lambda _
              (substitute* "tests/check/libs/pbutils.c"
                (("/bin/sh") (which "sh")))
             #t))
         (add-before
             'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             ;; Tests look for $XDG_RUNTIME_DIR.
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t))))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cdparanoia" ,cdparanoia)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glu" ,glu)
       ("graphene" ,graphene)
       ("gtk+" ,gtk+)
       ("iso-codes" ,iso-codes)
       ("libgudev" ,libgudev)
       ("libjpeg" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ("libtheora" ,libtheora)
       ("libvisual" ,libvisual)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxshm" ,libxshmfence)
       ("libxv" ,libxv)
       ("mesa" ,mesa)
       ("opus" ,opus)
       ("pango" ,pango)
       ("sdl" ,sdl)
       ("wayland" ,wayland)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gstreamer" ,gstreamer)
       ("orc" ,orc)))
    (synopsis "GStreamer plugins and helper libraries")
    (description "Gst-Plugins-Base is a well-groomed and well-maintained
collection of GStreamer plug-ins and elements, spanning the range of possible
types of elements one would want to write for GStreamer.")
     (home-page "https://gstreamer.freedesktop.org/")
     (license license:lgpl2.0+)))

(define-public gst-plugins-good
  (package
    (name "gst-plugins-good")
    (version "1.16.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://gstreamer.freedesktop.org/src/" name "/"
         name "-" version ".tar.xz"))
       (sha256
        (base32 "068k3cbv1yf3gbllfdzqsg263kzwh21y8dpwr0wvgh15vapkpfs0"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:python? #t ; To wrap binaries
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* "plugins/gst-plugins-good-plugins-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before
             'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             ;; Tests look for $XDG_RUNTIME_DIR.
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("aalib" ,aalib)
       ("bzip2" ,bzip2)
       ("cairo" ,cairo)
       ("flac" ,flac)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("glu" ,glu)
       ("gtk+" ,gtk+)
       ("jack" ,jack-2)
       ("lame" ,lame)
       ("libavc1394" ,libavc1394)
       ("libcaca" ,libcaca)
       ("libdv" ,libdv)
       ("libgudev" ,libgudev)
       ("libiec61883" ,libiec61883)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libshout" ,libshout)
       ("libsoup" ,libsoup)
       ("libvpx" ,libvpx)
       ("libx11" ,libx11)
       ("libxdamage" ,libxdamage)
       ("libxfixes" ,libxfixes)
       ("libxext" ,libxext)
       ("libxshm" ,libxshmfence)
       ("mesa" ,mesa)
       ("mpg123" ,mpg123)
       ("orc" ,orc)
       ("pulseaudio" ,pulseaudio)
       ("speex" ,speex)
       ("taglib" ,taglib)
       ("twolame" ,twolame)
       ("v4l-utils" ,v4l-utils)
       ("wavpack" ,wavpack)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (synopsis "GStreamer plugins and helper libraries")
    (description "GStreamer-Plugins-Good is a collection of plug-ins you'd want
to have right next to you on the battlefield.  Shooting sharp and making no
mistakes, these plug-ins have it all: good looks, good code, and good
licensing.  Documented and dressed up in tests.  If you're looking for a role
model to base your own plug-in on, here it is.")
    (home-page "https://gstreamer.freedesktop.org/")
    (license license:lgpl2.0+)))

(define-public gst-plugins-bad
  (package
    (name "gst-plugins-bad")
    (version "1.16.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://gstreamer.freedesktop.org/src/"
                       name "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0x0y0hm0ga3zqi5q4090hw5sjh59y1ry9ak16qsaascm72i7mjzi"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:python? #t ; To wrap binaries
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build-errors
           (lambda _
             (substitute* "ext/vulkan/vkerror.c"
               (("VK_RESULT_BEGIN_RANGE")
                "VK_RESULT_MAX_ENUM"))
             #t))
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* '("libs/compiling.sgml"
                              "libs/gst-plugins-bad-libs-docs.sgml"
                              "plugins/gst-plugins-bad-plugins-docs.sgml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "tests/check/meson.build"
               (("\\[\\['elements/msdkh264enc.c'\\], not have_msdk, \\[msdk_dep\\]\\],")
                "")
               (("\\[\\['elements/dash_mpd.c'\\], not xml2_dep.found\\(\\), \\[xml2_dep\\]\\],")
                "")
               (("\\[\\['elements/shm.c'\\], not shm_enabled, shm_deps\\],")
                "")
               (("\\[\\['elements/webrtcbin.c'\\], not libnice_dep.found\\(\\), \\[gstwebrtc_dep\\]\\],")
                ""))
             #t))
         (add-before
             'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             ;; Tests look for $XDG_RUNTIME_DIR.
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("bluez" ,bluez)
       ("bzip2" ,bzip2)
       ("cairo" ,cairo)
       ("ccextractor" ,ccextractor)
       ("chromaprint" ,chromaprint)
       ("curl" ,curl)
       ("directfb" ,directfb)
       ;;("dssim" ,dssim)
       ("faac" ,faac)
       ("faad2" ,faad2)
       ("flite" ,flite)
       ("fluidsynth" ,fluidsynth)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("glu" ,glu)
       ("gsm" ,gsm)
       ("gtk+" ,gtk+)
       ("iqa" ,iqa)
       ("ladspa" ,ladspa)
       ("lcms" ,lcms)
       ("libaom" ,libaom)
       ("libass" ,libass)
       ("libbs2b" ,libbs2b)
       ("libdc1394" ,libdc1394)
       ("libdca" ,libdca)
       ("libde265" ,libde265)
       ("libdrm" ,libdrm)
       ("libdvdnav" ,libdvdnav)
       ("libdvdread" ,libdvdread)
       ("libexif" ,libexif)
       ("libfdk" ,libfdk)
       ("libgcrypt" ,libgcrypt)
       ("libgme" ,libgme)
       ("libgudev" ,libgudev)
       ("libkate" ,libkate)
       ("libmfx" ,mediasdk)
       ("libmms" ,libmms)
       ("libmodplug" ,libmodplug)
       ("libmpcdec" ,libmpcdec)
       ("libnice" ,libnice)
       ("libofa" ,libofa)
       ("libopenmpt" ,libopenmpt)
       ("librsvg" ,librsvg)
       ("libsndfile" ,libsndfile)
       ("libsrtp" ,libsrtp)
       ("libssh2" ,libssh2)
       ("libtiff" ,libtiff)
       ("libusb" ,libusb)
       ("libva" ,libva)
       ("libvdpau" ,libvdpau)
       ("libwebp" ,libwebp)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxext" ,libxext)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("libxshm" ,libxshmfence)
       ("lilv" ,lilv)
       ("lrdf" ,lrdf)
       ("lv2" ,lv2)
       ("mesa" ,mesa)
       ("mjpegtools" ,mjpegtools)
       ("neon" ,neon)
       ("nettle" ,nettle)
       ("openal" ,openal)
       ("opencv" ,opencv)
       ("openexr" ,openexr)
       ("openh264" ,openh264)
       ("openjpeg" ,openjpeg)
       ("openni2" ,openni2)
       ("opensles" ,opensles)
       ("openssl" ,openssl)
       ("opus" ,opus)
       ("orc" ,orc)
       ("pango" ,pango)
       ("rtmp" ,rtmpdump)
       ("sbc" ,sbc)
       ("sctp" ,lksctp-tools)
       ("soundtouch" ,soundtouch)
       ("spandsp" ,spandsp)
       ("srt" ,srt)
       ("svthevcenc" ,svt-hevc)
       ("tinyalsa" ,tinyalsa)
       ("transcode" ,transcode)
       ("v4l" ,v4l-utils)
       ("voaacenc", vo-aacenc)
       ("voamrwbenc" ,vo-amrwbenc)
       ("vulkan-headers" ,vulkan-headers)
       ("vulkan-loader" ,vulkan-loader)
       ("x265" ,x265)
       ("wayland" ,wayland)
       ("webrtcdsp" ,webrtc-audio-processing)
       ("wildmidi" ,wildmidi)
       ("wpebackend-fdo" ,wpebackend-fdo)
       ("wpewebkit" ,wpewebkit)
       ("zbar" ,zbar)
       ("zxing" ,zxing-cpp)))
    (propagated-inputs
     `(("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (synopsis "GStreamer plugins and helper libraries")
    (description "Gst-Plugins-Bad are Bad with a capital B.  They look fine on
the outside, and might even appear to get the job done, but at the end of the
day they're a black sheep.  Without a golden-haired angel to watch over them,
they'll probably land in an unmarked grave at the final showdown.")
    (home-page "https://gstreamer.freedesktop.org/")
    (license license:lgpl2.0+)))

(define-public gst-plugins-ugly
  (package
    (name "gst-plugins-ugly")
    (version "1.16.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://gstreamer.freedesktop.org/src/"
                       name "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1jpvc32x6q01zjkfgh6gmq6aaikiyfwwnhj7bmvn52syhrdl202m"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* "plugins/gst-plugins-ugly-plugins-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before
             'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             ;; Tests look for $XDG_RUNTIME_DIR.
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("liba52" ,liba52)
       ("libcdio" ,libcdio)
       ("libdvdread" ,libdvdread)
       ("libmpeg2" ,libmpeg2)
       ("libx264" ,libx264)
       ("opencore-amr" ,opencore-amr)
       ("orc" ,orc)))
    (propagated-inputs
     `(("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (synopsis "GStreamer plugins and helper libraries")
    (description "Gst-Plugins-Ugly are the ones that might have a patent noose
around their neck, or a lock-up license, or any other problem that makes you
think twice about shipping them.")
    (home-page "https://gstreamer.freedesktop.org/")
    (license license:lgpl2.0+)))

(define-public gst-libav
  (package
    (name "gst-libav")
    (version "1.16.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://gstreamer.freedesktop.org/src/" name "/"
         name "-" version ".tar.xz"))
       (sha256
        (base32 "1wpfilc98bad9nsv3y1qapxp35dvn2mvwvrmqwrsj58cf09gc967"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Drop bundled ffmpeg.
           (delete-file-recursively "gst-libs/ext/libav")
           #t))))
    (build-system meson-build-system)
    (arguments
     `(#:python? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* "plugins/gst-libav-plugins-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("ruby" ,ruby)))
    (inputs
     `(("ffmpeg" ,ffmpeg)))
    (propagated-inputs
     `(("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (synopsis "GStreamer plugins and helper libraries")
    (description "Gst-Libav contains a GStreamer plugin for using the encoders,
decoders, muxers, and demuxers provided by FFmpeg.")
    (home-page "https://gstreamer.freedesktop.org/")
    (license license:lgpl2.0+)))

(define-public gst-editing-services
  (package
    (name "gst-editing-services")
    (version "1.16.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://gstreamer.freedesktop.org/src/" name "/"
         "gstreamer-editing-services-" version ".tar.xz"))
       (sha256
        (base32 "05hcf3prna8ajjnqd53221gj9syarrrjbgvjcbhicv0c38csc1hf"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:tests? #f           ; FIXME
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/libs"
               (substitute* '("architecture.xml" "ges-docs.sgml")
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml-4.3")
                                 "/xml/dtd/docbook/"))
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml-4.1.2")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t))))))
    (native-inputs
     `(("docbook-xml-4.1.2" ,docbook-xml-4.1.2)
       ("docbook-xml-4.3" ,docbook-xml-4.3)
       ("flex" ,flex)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-bad" ,gst-plugins-bad)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)
       ("libxml2" ,libxml2)))
    (synopsis "GStreamer Editing Services Library")
    (description "Gst-Editing-services is a high-level library for facilitating
the creation of audio/video non-linear editors.")
    (home-page "https://gstreamer.freedesktop.org/")
    (license license:lgpl2.0+)))

(define-public python-gst
  (package
    (name "python-gst")
    (version "1.16.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gstreamer.freedesktop.org/src/gst-python/"
                    "gst-python-" version ".tar.xz"))
              (sha256
               (base32
                "1a48ca66izmm8hnp608jv5isg3jxb0vlfmhns0bg9nbkilag7390"))
              (patches
               (search-patches "python-gst-fix-build-with-python-3.8.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:modules ((guix build meson-build-system)
                  (guix build utils)
                  ((guix build python-build-system) #:prefix python:))
       #:imported-modules (,@%meson-build-system-modules
                           (guix build python-build-system))
       #:configure-flags
       (list (string-append
              "-Dpygi-overrides-dir="
              (python:site-packages %build-inputs %outputs) "gi/overrides"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)))
    (propagated-inputs
     `(("gst-plugins-base" ,gst-plugins-base)
       ("python-pygobject" ,python-pygobject)))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "GStreamer GObject Introspection overrides for Python")
    (description
     "This package contains GObject Introspection overrides for Python that can
be used by Python applications using GStreamer.")
    (license license:lgpl2.1+)
    (properties `((python2-variant . ,(delay python2-gst))))))

(define-public python2-gst
  (package (inherit python-gst)
    (name "python2-gst")
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (propagated-inputs
     `(("gst-plugins-base" ,gst-plugins-base)
       ("python-pygobject" ,python2-pygobject)))))

(define-public gst123
  (package
    (name "gst123")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://space.twc.de/~stefan/gst123/gst123-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0zaa117n4wkya9p903vkj8hj58lmdb66pxsdx5wwcv7nffbp5d67"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk+" ,gtk+-2)
       ("ncurses" ,ncurses)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://space.twc.de/~stefan/gst123.php")
    (synopsis "Flexible command line media player based on gstreamer")
    (description "The program gst123 is designed to be a more flexible command
line player in the spirit of ogg123 and mpg123, based on the gstreamer media
framework.  It plays all file formats gstreamer supports, so if you have a
music collection which contains different file formats, like flac, ogg and
mp3, you can use gst123 to play all your music files.")
    (license license:lgpl2.0+)))
