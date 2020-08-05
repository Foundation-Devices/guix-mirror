;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2014, 2016, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2014, 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2015, 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2016 Jochem Raat <jchmrt@riseup.net>
;;; Copyright © 2016, 2017, 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017, 2018 nee <nee-git@hidamari.blue>
;;; Copyright © 2017 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Mohammed Sadiq <sadiq@sadiqpk.org>
;;; Copyright © 2017, 2020 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2018 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018, 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Jonathan Frederickson <jonathan@terracrypt.net>
;;; Copyright © 2019, 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;; Copyright © 2019, 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2019, 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2019, 2020 Leo Prikler <leo.prikler@student.tugraz.at>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Naga Malleswari <nagamalli@riseup.net>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
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

(define-module (gnu packages gnome)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages php)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages search)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu artwork)
  #:use-module ((guix build utils) #:select (modify-phases))
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public brasero
  (package
    (name "brasero")
    (version "3.12.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/brasero/"
                                 (version-major+minor version) "/"
                                 "brasero-" version ".tar.xz"))
             (sha256
              (base32
               "0h90y674j26rvjahb8cc0w79zx477rb6zaqcj26wzvq8kmpic8k8"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list
                          (string-append "--with-girdir="
                                         (assoc-ref %outputs "out")
                                         "/share/gir-1.0")
                          (string-append "--with-typelibdir="
                                         (assoc-ref %outputs "out")
                                         "/lib/girepository-1.0"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'embed-growisofs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "plugins/growisofs/burn-growisofs.c"
               (("\"growisofs") (string-append "\"" (which "growisofs"))))
             #t)))))
    (propagated-inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib" ,glib "bin")                       ; glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dvd+rw-tools" ,dvd+rw-tools)
       ("glib" ,glib)
       ("gnome-doc-utils" ,gnome-doc-utils)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libice" ,libice)
       ("libnotify" ,libnotify)
       ("libsm" ,libsm)
       ("libxml2" ,libxml2)
       ("totem-pl-parser" ,totem-pl-parser)))
    (home-page "https://wiki.gnome.org/Apps/Brasero")
    (synopsis "CD/DVD burning tool for Gnome")
    (description "Brasero is an application to burn CD/DVD for the Gnome
Desktop.  It is designed to be as simple as possible and has some unique
features to enable users to create their discs easily and quickly.")
    (license license:gpl2+)))

(define-public libcloudproviders
  (package
    (name "libcloudproviders")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0aars24myf6n8b8hm1n12hsgcm54097kpbpm4ba31zp1l4y22qs7"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Denable-gtk-doc=true")
       #:phases
       (modify-phases %standard-phases
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
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)))
    (synopsis "Cloudproviders Integration API")
    (description "Libcloudproviders is a DBus API that allows cloud storage sync
clients to expose their services.  Clients such as file managers and desktop
environments can then provide integrated access to the cloud providers
services.")
    (home-page "https://csorianognome.wordpress.com/2015/07/07/cloud-providers/")
    (license license:lgpl3+)))

(define-public libgrss
  (package
    (name "libgrss")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1nalslgyglvhpva3px06fj6lv5zgfg0qmj0sbxyyl5d963vc02b7"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/reference"
               (substitute* "libgrss-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)))
    (synopsis "Glib library for feeds")
    (description "LibGRSS is a Glib abstaction to handle feeds in RSS, Atom and
other formats.")
    (home-page "https://wiki.gnome.org/Projects/Libgrss")
    (license license:lgpl3+)))

(define-public gnome-js-common
  (package
    (name "gnome-js-common")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32 "1zv5b9bcbclzj64xd9kgql4ndmbwvvi6cl937ykw8fp21xgh8z7y"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static")))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (synopsis "Common JS Modules")
    (description "GNOME-JS-Common provides common modules for GNOME JavaScript
bindings.")
    (home-page "https://wiki.gnome.org/Projects/Seed")
    (license license:gpl3+)))

(define-public seed
  (package
    (name "seed")
    (version "3.8.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0cmcxaggcdcy13j27gy8id2qsf2p2sl4bz2mwb9zhv3gzavlvjw0"))
       (patches
        (search-patches "seed-webkit.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-xorg-module"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html")
        "--with-webkit=4.0")
       #:phases
       (modify-phases %standard-phases
         ;; The seed-webkit.patch patches configure.ac.
         ;; So the source files need to be re-bootstrapped.
         (add-after 'unpack 'trigger-bootstrap
           (lambda _
             (for-each delete-file
                       (list
                        "configure"
                        "Makefile.in"))
             #t))
         (add-after 'unpack 'patch-tests
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (find-files "." "\\.js$")
              (("#!/usr/bin/env seed")
               (string-append "#!" (getcwd) "/src/seed")))
             #t))
         (add-before 'build 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* '("reference/seed-docs.sgml" "modules/book.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("gnome-js-common" ,gnome-js-common)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("libffi" ,libffi)
       ("libxml2" ,libxml2)
       ("mpfr" ,mpfr)
       ("readline" ,readline)
       ("sqlite" ,sqlite)
       ("xscrnsaver" ,libxscrnsaver)))
    (propagated-inputs
     `(("glib" ,glib)
       ("webkit" ,webkitgtk)))
    (synopsis "GObject JavaScriptCore bridge")
    (description "Seed is a library and interpreter, dynamically bridging
(through GObjectIntrospection) the WebKit JavaScriptCore engine, with the
GNOME platform.  It serves as something which enables you to write standalone
applications in JavaScript, or easily enable your application to be extensible
in JavaScript.")
    (home-page "https://wiki.gnome.org/Projects/Seed")
    (license license:lgpl2.0+)))

(define-public libdmapsharing
  (package
    (name "libdmapsharing")
    (version "3.9.10")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.flyn.org/projects/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "152hnddwxv590cn802awv3mn27ixc3s6ac691a7z02d1c5fl45p2"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f                      ; Tests require networking.
       #:configure-flags
       (list
        "--disable-static"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "libdmapsharing-4.0-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("check" ,check)
       ("docbook-xml" ,docbook-xml-4.3)
       ("gobject-introspection" ,gobject-introspection)
       ("pedansee" ,pedansee)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("avahi" ,avahi)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("gee" ,libgee)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gstreamer" ,gstreamer)
       ("libsoup" ,libsoup)))
    (synopsis "Media management library")
    (description "Libdmapsharing is a library which allows programs to access,
share and control the playback of media content using DMAP (DAAP, DPAP & DACP).
It is written in C using GObject and libsoup.")
    (home-page "https://flyn.org/projects/libdmapsharing/")
    (license license:lgpl2.1+)))

(define-public gtx
  (package
    (name "gtx")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://launchpad.net/gtx/trunk/"
                       version "/+download/gtx-" version ".tar.gz"))
       (sha256
        (base32 "0i4zvn5v4rf0cw3fxylk6j2pyy5lkrswdiw8jdxkys0ph0nan33n"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "GLib Testing Framework")
    (description "GTX is a small collection of convenience functions intended to
enhance the GLib testing framework.  With specific emphasis on easing the pain
of writing test cases for asynchronous interactions.")
    (home-page "https://launchpad.net/gtx")
    (license license:lgpl2.1+)))

(define-public dee
  (package
    (name "dee")
    (version "1.2.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://launchpad.net/dee/1.0/"
                       version "/+download/dee-" version ".tar.gz"))
       (sha256
        (base32 "12mzffk0lyd566y46x57jlvb9af152b4dqpasr40zal4wrn37w0v"))
       (patches
        (search-patches "dee-vapi.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-maintainer-flags"
        (string-append "--with-pygi-overrides-dir="
                       (assoc-ref %outputs "out")
                       "/lib/python"
                       ,(version-major+minor
                         (package-version python))
                       "/site-packages/gi/overrides")
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/reference/dee-1.0"
               (substitute* "dee-1.0-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'patch-docbook-xml 'disable-failing-tests
           (lambda _
             (substitute* "tests/test-icu.c"
               (("g_test_add \\(DOMAIN\"/Default/AsciiFolder\", Fixture, 0,")
                "")
               (("setup, test_ascii_folder, teardown\\);")
                ""))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running dbus-daemon.
             (system "dbus-daemon &")
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("dbus" ,dbus)
       ("dbus-test-runner" ,dbus-test-runner)
       ("docbook-xml" ,docbook-xml-4.3)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ;; Would only be required by configure flag "--enable-extended-tests".
       ;("gtx" ,gtx)
       ("pkg-config" ,pkg-config)
       ("pygobject" ,python-pygobject)
       ("python" ,python-wrapper)
       ("vala" ,vala)))
    (inputs
     `(("icu" ,icu4c)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "Model to synchronize multiple instances over DBus")
    (description "Dee is a library that uses DBus to provide objects allowing
you to create Model-View-Controller type programs across DBus.  It also consists
of utility objects which extend DBus allowing for peer-to-peer discoverability
of known objects without needing a central registrar.")
    (home-page "https://launchpad.net/dee")
    (license
     ;; Dual-licensed
     (list
      license:lgpl3+
      license:gpl3+))))

(define-public zeitgeist
  (package
    (name "zeitgeist")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.freedesktop.org/zeitgeist/zeitgeist.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "0ig3d3j1n0ghaxsgfww6g2hhcdwx8cljwwfmp9jk1nrvkxd6rnmv"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--enable-explain-queries"
        "--enable-fts"
        "--enable-docs")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/libzeitgeist"
               (substitute* "zeitgeist-gtkdoc-index.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'patch-docbook-xml 'disable-failing-tests
           (lambda _
             (substitute* "test/direct/Makefile.am"
               (("	log-test ")
                ""))
             (substitute* "test/c/Makefile.am"
               (("	test-log ")
                ""))
             #t))
         (add-before 'bootstrap 'remove-autogen-script
           (lambda _
             ;; To honor `autoreconf -vif` by build-system.
             (delete-file "autogen.sh")
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xorg-server-for-tests" ,xorg-server-for-tests)))
    (inputs
     `(("dee-icu" ,dee)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("sqlite" ,sqlite)
       ("telepathy-glib" ,telepathy-glib)
       ("python" ,python-wrapper)
       ("python-rdflib" ,python-rdflib)
       ("xapian-config" ,xapian)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "Desktop Activity Logging")
    (description "Zeitgeist is a service which logs the users’s activities and
events, anywhere from files opened to websites visited and conversations.  It
makes this information readily available for other applications to use.  It is
able to establish relationships between items based on similarity and usage
patterns.")
    (home-page "https://zeitgeist.freedesktop.org/")
    (license
     ;; Dual-licensed
     (list
      license:lgpl2.1+
      license:gpl2+))))

(define-public gnome-photos
  (package
    (name "gnome-photos")
    (version "3.34.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "06ml5sf8xhpan410msqz085hmfc7082d368pb82yq646y9pcfn9w"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list "-Ddogtail=false"     ; Not available
             ;; Required for RUNPATH validation.
             (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib/gnome-photos"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-gnome-photos
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let*
                 ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/gnome-photos")
                 `("GRL_PLUGIN_PATH" = (,(getenv "GRL_PLUGIN_PATH")))))
             #t)))))
    (native-inputs
     `(("dbus" ,dbus)
       ("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("git" ,git-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+:bin" ,gtk+ "bin")
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("babl" ,babl)
       ("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("gegl" ,gegl)
       ("geocode-glib" ,geocode-glib)
       ("gexiv2" ,gexiv2)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("gnome-online-miners" ,gnome-online-miners)
       ("grilo" ,grilo)
       ("grilo-plugins" ,grilo-plugins)
       ("gtk+" ,gtk+)
       ("libdazzle" ,libdazzle)
       ("libgdata" ,libgdata)
       ("libgfbgraph" ,gfbgraph)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("librest" ,rest)
       ("pygobject" ,python-pygobject)
       ("tracker" ,tracker)
       ("tracker-miners" ,tracker-miners)))
    (synopsis "Access, organize and share your photos on GNOME desktop")
    (description "GNOME Photos is a simple and elegant replacement for using a
file manager to deal with photos.  Enhance, crop and edit in a snap.  Seamless
cloud integration is offered through GNOME Online Accounts.")
    (home-page "https://wiki.gnome.org/Apps/Photos")
    (license license:gpl3+)))

(define-public gnome-music
  (package
    (name "gnome-music")
    (version "3.34.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "1r5sfw5cbd6qqh27lzhblazir0bfi3k7nqppw66qw990isqm5psy"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-gnome-music
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let*
                 ((out (assoc-ref outputs "out"))
                  (pylib (string-append out "/lib/python"
                                        ,(version-major+minor
                                          (package-version python))
                                        "/site-packages")))
               (wrap-program (string-append out "/bin/gnome-music")
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))
                 `("GST_PLUGIN_SYSTEM_PATH" = (,(getenv "GST_PLUGIN_SYSTEM_PATH")))
                 `("GRL_PLUGIN_PATH" = (,(getenv "GRL_PLUGIN_PATH")))
                 `("PYTHONPATH" = (,(getenv "PYTHONPATH") ,pylib))))
             #t)))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("itstools" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("grilo" ,grilo)
       ("grilo-plugins" ,grilo-plugins)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gstreamer" ,gstreamer)
       ("gvfs" ,gvfs)
       ("libdazzle" ,libdazzle)
       ("libmediaart" ,libmediaart)
       ("libsoup" ,libsoup)
       ("pycairo" ,python-pycairo)
       ("pygobject" ,python-pygobject)
       ("tracker" ,tracker)
       ("tracker-miners" ,tracker-miners)))
    (synopsis "Simple music player for GNOME desktop")
    (description "GNOME Music is the new GNOME music playing application that
aims to combine an elegant and immersive browsing experience with simple
and straightforward controls.")
    (home-page "https://wiki.gnome.org/Apps/Music")
    (license license:gpl2+)))

(define-public portablexdr
  (package
    (name "portablexdr")
    (version "4.9.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://people.redhat.com/~rjones/" name "/files/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32
         "0b77ipvvi520nv7rr6jb1c3xryhc3m2mywhby7m48kfgag8vvx2w"))))
    (build-system gnu-build-system)
    (synopsis "External Data Representation Library")
    (description "PortableXDR is an implementation of External Data
Representation (XDR) Library.  It is a standard data serialization format, for
uses such as computer network protocols.  It allows data to be transferred
between different kinds of computer systems.")
    (home-page "https://people.redhat.com/~rjones/portablexdr/")
    (license
     (list
      license:gpl2+
      license:lgpl2.1+))))

(define-public tepl
  (package
    (name "tepl")
    (version "4.4.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0mm2z849hnni7597an05mrv0dckrxjngpf2xfa0g5s17i8x6gxp6"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* '( "api-breaks.xml" "intro.xml.in"
                               "porting-guides.xml" "tepl-docs.xml.in")
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'disable-failing-tests
           (lambda _
             (substitute* "testsuite/Makefile.in"
               (("test-file-loader\\$\\(EXEEXT\\) test-file-metadata\\$\\(EXEEXT\\)")
                "test-file-loader$(EXEEXT)"))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server-for-tests)))
    (propagated-inputs
     `(("amtk" ,amtk)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("libxml2" ,libxml2)
       ("uchardet" ,uchardet)))
    (synopsis "Text editor product line")
    (description "Tepl is a library that eases the development of
GtkSourceView-based text editors and IDEs.")
    (home-page "https://wiki.gnome.org/Projects/Tepl")
    (license license:lgpl2.1+)))

(define-public krb5-auth-dialog
  (package
    (name "krb5-auth-dialog")
    (version "3.26.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "1w91gsvmifqhiam3xqf88i5rk2w6qadjalmbvvamjdc37j0vdc6x"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libnotify" ,libnotify)
       ("mit-krb5" ,mit-krb5)
       ("network-manager" ,network-manager)))
    (synopsis "Popup dialogs for Kerberos 5")
    (description "krb5-auth-dialog is a simple dialog that monitors Kerberos
tickets, and pops up a dialog when they are about to expire.")
    (home-page "https://gitlab.gnome.org/GNOME/krb5-auth-dialog")
    (license license:gpl2+)))

(define-public notification-daemon
  (package
    (name "notification-daemon")
    (version "3.20.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "1rgchqi4j2ll7d6a7lgy7id0w9rrkwkgic1096fbm2zx6n7pc4yx"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("x11" ,libx11)))
    (synopsis "Notification Daemon for GNOME Desktop")
    (description "Notification-Daemon is the server implementation of the
freedesktop.org desktop notification specification.")
    (home-page "https://wiki.gnome.org/Projects/NotificationDaemon")
    (license license:gpl2+)))

(define-public mm-common
  (package
    (name "mm-common")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/mm-common/"
                       (version-major+minor version) "/"
                       "mm-common-" version ".tar.xz"))
       (sha256
        (base32 "1jasx9a9g7nqf7jcv3mrg4qh5cp9sq724jxjaz4wa1dzmxsxg8i8"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "util/mm-common-prepare.in"
               (("ln")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/ln"))
               (("cp")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/cp"))
               (("sed")
                (string-append (assoc-ref inputs "sed")
                               "/bin/sed"))
               (("cat")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/cat")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("automake" ,automake)
       ("coreutils" ,coreutils)
       ("sed" ,sed)
       ("python" ,python)))
    (synopsis "Module of GNOME C++ bindings")
    (description "The mm-common module provides the build infrastructure and
utilities shared among the GNOME C++ binding libraries.  Release archives of
mm-common include the Doxygen tag file for the GNU C++ Library reference
documentation.")
    (home-page "https://gitlab.gnome.org/GNOME/mm-common")
    (license license:gpl2+)))

(define-public phodav
  (package
    (name "phodav")
    (version "2.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1hxq8c5qfah3w7mxcyy3yhzdgswplll31a69p5mqdl04bsvw5pbx"))))
    (build-system meson-build-system)
    (outputs '("out" "tools" "spice" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dsystemd=disabled")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/reference"
               (substitute* '("phodav-2.0-docs.sgml" "phodav-docs.sgml.in")
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'patch-udevdir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "meson.build"
               (("udev\\.get_pkgconfig_variable\\('udevdir'\\)")
                (string-append "'"
                               (assoc-ref outputs "out")
                               "/lib/udev" "'")))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'move-tools
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (tools (assoc-ref outputs "tools")))
               (mkdir-p (string-append tools "/bin"))
               (rename-file
                (string-append out "/bin")
                (string-append tools "/bin"))
               #t)))
         (add-after 'move-tools 'move-spice
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (spice (assoc-ref outputs "spice")))
               (mkdir-p (string-append spice "/sbin"))
               (rename-file
                (string-append out "/sbin")
                (string-append spice "/sbin"))
               #t))))))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("docbook-xml" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("xmlto" ,xmlto)))
    (inputs
     `(("avahi" ,avahi)
       ("udev" ,eudev)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)))
    (synopsis "WebDAV Support for GNOME")
    (description "PhoDav is a WebDav server implementation using libsoup
(RFC 4918).")
    (home-page "https://wiki.gnome.org/phodav")
    (license license:lgpl2.1+)))

(define-public gnome-color-manager
  (package
    (name "gnome-color-manager")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0fxdng74d8hwhfx1nwl1i4jx9h9f6c2hkyc12f01kqbjcimrxnwx"))))
    (build-system meson-build-system)
    (outputs '("out" "help"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson_post_install.sh"
               (("gtk-update-icon-cache")
                "true"))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             #t))
         (add-after 'install 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/help")
                (string-append help "/share/help"))
               #t))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ;; ("docbook2man" ,docbook-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("colord" ,colord)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("lcms" ,lcms)))
    (synopsis "Color profile manager for the GNOME desktop")
    (description "GNOME Color Manager is a session framework that makes it easy
to manage, install and generate color profiles in the GNOME desktop.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-color-manager")
    (license license:gpl2+)))

(define-public gnome-online-miners
  (package
    (name "gnome-online-miners")
    (version "3.34.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1n2jz9i8a42zwxx5h8j2gdy6q1vyydh4vl00r0al7w8jzdh24p44"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static")))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("grilo" ,grilo)
       ("libgdata" ,libgdata)
       ("libgfbgraph" ,gfbgraph)
       ("tracker" ,tracker)
       ("zapojit" ,libzapojit)))
    (synopsis "Web Crawlers for GNOME")
    (description "GNOME Online Miners provides a set of crawlers that go through
your online content and index them locally in Tracker.  It has miners for
Facebook, Flickr, Google, ownCloud and SkyDrive.")
    (home-page "https://wiki.gnome.org/Projects/GnomeOnlineMiners")
    (license license:gpl2+)))

(define-public gssdp
  (package
    (name "gssdp")
    (version "1.2.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version) "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "1s57i8a8wnnxnsfl27cq4503dkdlzbrhry5zpg23sfqfffvdqqx2"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "gssdp-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
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
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("gtk+" ,gtk+)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("libsoup" ,libsoup)))
    (synopsis "GObject-based API over @acronym{SSDP, Simple Service Discovery
Protocol} for GNOME")
    (description "This package provides a library to handle resource discovery
and announcement over @acronym{SSDP, Simple Service Discovery Protocol} and
a debugging tool, @command{gssdp-device-sniffer}.")
    (home-page "https://gitlab.gnome.org/GNOME/gssdp")
    (license license:lgpl2.0+)))

(define-public gupnp
  (package
    (name "gupnp")
    (version "1.2.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version) "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "1ld7mrpdv9cszmfzh7i19qx4li25j3fr7x1jp38l8phzlmz3187p"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* '("client-tutorial.xml" "glossary.xml"
                              "overview.xml" "server-tutorial.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.4/")
                  (string-append (assoc-ref inputs "docbook-xml-4.4")
                                 "/xml/dtd/docbook/")))
               (substitute* '("gupnp-binding-tool.xml" "gupnp-docs.xml")
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
       ("docbook-xml-4.4" ,docbook-xml-4.4)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("gjs" ,gjs)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("gtk+" ,gtk+)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gssdp" ,gssdp)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)))
    (synopsis "PnP API for GNOME")
    (description "This package provides GUPnP, an object-oriented framework
for creating UPnP devices and control points, written in C using
@code{GObject} and @code{libsoup}.")
    (home-page "https://gitlab.gnome.org/GNOME/gupnp")
    (license license:lgpl2.0+)))

(define-public gupnp-dlna
  (package
    (name "gupnp-dlna")
    (version "0.10.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0spzd2saax7w776p5laixdam6d7smyynr9qszhbmq7f14y13cghj"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute*
                   '("gupnp-dlna-gst/gupnp-dlna-gst-docs.sgml"
                     "gupnp-dlna-metadata/gupnp-dlna-metadata-docs.sgml"
                     "gupnp-dlna/gupnp-dlna-docs.sgml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("gupnp" ,gupnp)
       ("libxml2" ,libxml2)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (synopsis "GUPnP DLNA for GNOME")
    (description "GUPnP-DLNA is a small utility library that aims to ease the
DLNA-related tasks such as media profile guessing, transcoding to a given
profile, etc.")
    (home-page "https://gitlab.gnome.org/GNOME/gupnp-dlna")
    (license license:lgpl2.0+)))

(define-public gupnp-av
  (package
    (name "gupnp-av")
    (version "0.12.11")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1p3grslwqm9bc8rmpn4l48d7v9s84nina4r9xbd932dbj8acz7b8"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "gupnp-av-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("gtk+" ,gtk+)
       ("gupnp" ,gupnp)
       ("libxml2" ,libxml2)))
    (synopsis "GUPnP A/V for GNOME")
    (description "GUPnP-AV is a small utility library that aims to ease the
handling and implementation of UPnP A/V profiles.")
    (home-page "https://gitlab.gnome.org/GNOME/gupnp-av")
    (license license:lgpl2.0+)))

(define-public libmediaart
  (package
    (name "libmediaart")
    (version "1.9.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0gc10imyabk57ar54m0qzms0x9dnmkymhkzyk8w1aj3y4lby0yx5"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference/libmediaart"
               (substitute* "libmediaart-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
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
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (propagated-inputs
     `(("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)))
    (synopsis "Media-Art Library")
    (description "LibMediaArt provides library tasked with managing, extracting
and handling media art caches.")
    (home-page "https://gitlab.gnome.org/GNOME/libmediaart")
    (license license:lgpl2.1+)))

(define-public gnome-initial-setup
  (package
    (name "gnome-initial-setup")
    (version "3.36.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/gnome-initial-setup/"
                       (version-major+minor version)
                       "/gnome-initial-setup-" version ".tar.xz"))
       (sha256
        (base32 "17szzz2a5wpi7kwjnhimiwf8vg0bfliyk3k0adgv1pw2mcfpxp5s"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dsystemd=false")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-gkbd-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Allow the "Preview" button in the keyboard layout
             ;; selection dialog to display the layout.
             (let* ((libgnomekbd (assoc-ref inputs "libgnomekbd")))
               (substitute* "gnome-initial-setup/pages/keyboard/cc-input-chooser.c"
                 (("\"gkbd-keyboard-display")
                  (string-append "\"" libgnomekbd
                                 "/bin/gkbd-keyboard-display")))
               #t))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("accountsservice" ,accountsservice)
       ("cheese" ,cheese)
       ("gdm" ,gdm)
       ("geocode-glib" ,geocode-glib)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("gtk+" ,gtk+)
       ("gweather" ,libgweather)
       ("ibus" ,ibus)
       ("json-glib" ,json-glib)
       ("krb5" ,mit-krb5)
       ("libgeoclue" ,geoclue)
       ("libgnomekbd" ,libgnomekbd)
       ("libnm" ,network-manager)
       ("libnma" ,libnma)
       ("libsecret" ,libsecret)
       ("malcontent" ,malcontent)
       ("pango" ,pango)
       ("polkit" ,polkit)
       ("pwquality" ,libpwquality)
       ("rest" ,rest)
       ("webkitgtk" ,webkitgtk)))
    (synopsis "Initial setup wizard for GNOME desktop")
    (description "GNOME-Initial-Setup aims to provide a simple, easy, and safe
way to prepare a new system.")
    (home-page "https://wiki.gnome.org/Design/OS/InitialSetup")
    (license license:gpl2+)))

(define-public gnome-user-share
  (package
   (name "gnome-user-share")
   (version "3.33.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0lf790pyamdyj7180ils8vizjl8brxcg7jsm1iavfp9ay4wa8mz7"))))
   (build-system meson-build-system)
   (arguments
    `(#:glib-or-gtk? #t
      #:configure-flags
       `("-Dsystemd=false"
         ;; Enable nautilus extension for file sharing.
         "-Dnautilus_extension=true")))
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("gtk+:bin" ,gtk+ "bin")
      ("pkg-config" ,pkg-config)
      ("yelp-tools" ,yelp-tools)))
   (inputs
    `(("glib" ,glib)
      ("gnome-bluetooth" ,gnome-bluetooth)
      ("gtk+" ,gtk+)
      ("libcanberra" ,libcanberra)
      ("libnotify" ,libnotify)
      ("nautilus" ,nautilus)))      ; For nautilus extension.
   (synopsis "File sharing for GNOME desktop")
   (description "GNOME User Share is a small package that binds together
various free software projects to bring easy to use user-level file
sharing to the masses.")
   (home-page "https://gitlab.gnome.org/GNOME/gnome-user-share")
   (license license:gpl2+)))

(define-public sushi
  (package
    (name "sushi")
    (version "3.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "02idvqjk76lii9xyg3b1yz4rw721709bdm5j8ikjym6amcghl0aj"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'glib-or-gtk-wrap 'wrap-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/sushi")))
               ;; Put existing typelibs before sushi's deps, so as to correctly
               ;; infer gdk-pixbuf
               (wrap-program prog
                 `("GI_TYPELIB_PATH" suffix (,(getenv "GI_TYPELIB_PATH"))))
               #t))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("clutter" ,clutter)
       ("clutter-gst" ,clutter-gst)
       ("clutter-gtk" ,clutter-gtk)
       ("evince" ,evince)                         ; For file previewing.
       ("freetype" ,freetype)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gjs" ,gjs)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gstreamer" ,gstreamer)
       ("gtksourceview" ,gtksourceview-3)
       ("harfbuzz" ,harfbuzz)
       ("libepoxy" ,libepoxy)
       ("libmusicbrainz" ,libmusicbrainz)
       ("libxml2" ,libxml2)
       ("neon" ,neon)
       ("webkitgtk" ,webkitgtk)))
    (synopsis "File previewer for the GNOME desktop")
    (description "Sushi is a DBus-activated service that allows applications to
preview files on the GNOME desktop.")
    (home-page "https://gitlab.gnome.org/GNOME/sushi")
    (license license:gpl2+)))

(define-public rygel
  (package
    (name "rygel")
    (version "0.38.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "003xficqb08r1dgid20i7cn889lbfwrglpx78rjd5nkvgxbimhh8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("gssdp" ,gssdp)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)
       ("gupnp" ,gupnp)
       ("gupnp-av" ,gupnp-av)
       ("gupnp-dlna" ,gupnp-dlna)
       ("libgee" ,libgee)
       ("libmediaart" ,libmediaart)
       ("libsoup" ,libsoup)
       ("libxslt" ,libxslt)
       ("libunistring" ,libunistring)
       ("tracker" ,tracker)))
    (synopsis "Share audio, video, and pictures with other devices")
    (description
     "Rygel is a home media solution (@dfn{UPnP AV MediaServer and
MediaRenderer}) for GNOME that allows you to easily share audio, video, and
pictures, and to control a media player on your home network.

Rygel achieves interoperability with other devices by trying to conform to the
strict requirements of DLNA and by converting media on-the-fly to formats that
client devices can handle.")
    (home-page "https://wiki.gnome.org/Projects/Rygel")
    (license (list
              ;; For logo (data/icons/*).
              license:cc-by-sa3.0
              ;; For all others.
              license:lgpl2.1+))))

(define-public libnma
  (package
    (name "libnma")
    (version "1.8.30")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1d5gzn7ss5vi0bhc8s4i5gsrck1ajslajam5jxfqazg094mffcys"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libnma-docs.xml"
               (("http://www.oasis-open.org/docbook/xml/4.3/")
                (string-append (assoc-ref inputs "docbook-xml")
                               "/xml/dtd/docbook/")))
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk-doc" ,gtk-doc)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("gcr" ,gcr)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("iso-codes" ,iso-codes)
       ("mobile-broadband-provider-info" ,mobile-broadband-provider-info)))
    (propagated-inputs
     `(("libnm" ,network-manager)))
    (synopsis "Network-Manager Applet Library")
    (description "Libnma is an applet library for Network Manager.  It was
initially part of network-manager-applet and has now become a separate
project.")
    (home-page "https://gitlab.gnome.org/GNOME/libnma")
    (license
     (list
      ;; Library
      license:gpl2+
      ;; Others
      license:lgpl2.1+))))

(define-public gnome-menus
  (package
    (name "gnome-menus")
    (version "3.32.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-menus/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0x2blzqrapmbsbfzxjcdcpa3vkw9hq5k96h9kvjmy9kl415wcl68"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("pkg-config" ,pkg-config)))
    (synopsis "Menu support for GNOME desktop")
    (description "GNOME Menus contains the libgnome-menu library, the layout
configuration files for the GNOME menu, as well as a simple menu editor.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-menus")
    (license license:lgpl2.0+)))

(define-public deja-dup
  (package
    (name "deja-dup")
    (version "40.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.gnome.org/World/deja-dup/-/archive/"
                                  version "/deja-dup-" version ".tar.bz2"))
              (sha256
               (base32
                "0lwazh6crby5wpy9fg6zvwy4plqbhs2f98bm5lbizjdlbh88n5q0"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list
        ;; Otherwise, the RUNPATH will lack the final path component.
        (string-append "-Dc_link_args=-Wl,-rpath="
                       (assoc-ref %outputs "out") "/lib/deja-dup"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((python (assoc-ref inputs "python")))
               (substitute* '("libdeja/tools/duplicity/DuplicityInstance.vala"
                              "libdeja/tests/scripts/instance-error.test")
                 (("/bin/rm")
                  (which "rm")))
               (substitute* "libdeja/tests/runner.vala"
                 (("/bin/sh")
                  (which "sh")))
               (substitute* "libdeja/tests/scripts/instance-error.test"
                 (("`which python3`")
                  (string-append python "/bin/python3"))))))
         (add-after 'unpack 'patch-libgpg-error
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libgpg-error (assoc-ref inputs "libgpg-error")))
               (substitute* "meson.build"
                 (("(gpgerror_libs = ).*" _ var)
                  (format #f "~a '-L~a/lib -lgpg-error'\n" var libgpg-error))))
             #t))
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "data/post-install.sh"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (inputs
     `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("duplicity" ,duplicity)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libnotify" ,libnotify)
       ("libgpg-error" ,libgpg-error)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("packagekit" ,packagekit)))
    (native-inputs
     `(("appstream-glib" ,appstream-glib)
       ("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")             ; for glib-compile-schemas.
       ("gobject-introspection" ,gobject-introspection)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://wiki.gnome.org/Apps/DejaDup")
    (synopsis "Simple backup tool, for regular encrypted backups")
    (description
     "Déjà Dup is a simple backup tool, for regular encrypted backups.  It
uses duplicity as the backend, which supports incremental backups and storage
either on a local, or remote machine via a number of methods.")
    (license license:gpl3+)))

(define-public gnome-user-docs
  (package
   (name "gnome-user-docs")
   (version "3.32.3")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/gnome-user-docs/"
                          (version-major+minor version)
                          "/gnome-user-docs-" version ".tar.xz"))
      (sha256
       (base32 "0dvsl0ldg8rf7yq0r4dv1pn41s7gjgcqp7agkbflkbmhrl6vbhig"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("itstool" ,itstool)
      ("pkg-config" ,pkg-config)
      ("xmllint" ,libxml2)))
   (synopsis "User documentation for the GNOME desktop")
   (description
    "The GNOME User Documentation explains how to use the GNOME desktop and its
components.  It covers usage and setup of the core GNOME programs by end-users
and system administrators.")
   (home-page "https://live.gnome.org/DocumentationProject")
   (license license:cc-by3.0)))

(define-public gnome-getting-started-docs
  (package
    (name "gnome-getting-started-docs")
    (version "3.36.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/gnome-getting-started-docs/"
                       (version-major+minor version)
                       "/gnome-getting-started-docs-" version ".tar.xz"))
       (sha256
        (base32 "1ihxa9g687rbb4s2gxd2pf726adx98ahq4kfad868swl7a8vi504"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (synopsis "Help a new user get started in GNOME")
    (description "GNOME-Getting-Started-Docs provides the Getting Started guide
for GNOME Desktop.")
    (home-page "https://live.gnome.org/DocumentationProject")
    (license license:cc-by-sa3.0)))

(define-public dia
  ;; This version from GNOME's repository includes fixes for compiling with
  ;; recent versions of the build tools.  The latest activity on the
  ;; pre-GNOME version has been in 2014, while GNOME has continued applying
  ;; fixes since.
  (let ((commit "3cf7ec4c2e5bca139a7f3e17f9fc9009c237fcc5")
        (revision "2"))
    (package
      (name "dia")
      (version (git-version "0.97.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.gnome.org/GNOME/dia.git/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "04r8dspa6nmicrifhi3sh46hqvyy88hzq37xx99q3q1mwsrpmwy8"))))
      (build-system meson-build-system)
      (inputs
       `(("graphene" ,graphene)
         ("gtk+" ,gtk+-2)
         ("libxml2" ,libxml2)
         ("libxslt" ,libxslt)
         ("poppler" ,poppler)
         ;; Without Python 2, build fails: plug-ins/python/meson.build:4:0:
         ;; ERROR: Unknown method "dependency" in object.
         ("python-2" ,python-2)))
      (native-inputs
       `(("appstream-glib" ,appstream-glib)
         ("docbook-xsl" ,docbook-xsl)
         ("glib" ,glib "bin")
         ("intltool" ,intltool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://wiki.gnome.org/Apps/Dia")
      (synopsis "Diagram creation for GNOME")
      (description "Dia can be used to draw different types of diagrams, and
includes support for UML static structure diagrams (class diagrams), entity
relationship modeling, and network diagrams.  The program supports various file
formats like PNG, SVG, PDF and EPS.")
      (license license:gpl2+))))

(define-public libgdata
  (package
    (name "libgdata")
    (version "0.17.12")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0613nihsvwvdnmlbjnwi8zqxgmpwyxdapzznq4cy1fp84246zzd0"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     '(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Doauth1=enabled"
        "-Dman=true"
        "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* '("gdata-docs.xml"
                              "gdata-overview.xml"
                              "gdata-running.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk-doc" ,gtk-doc)
       ("gtk+:bin" ,gtk+ "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("uhttpmock" ,uhttpmock)
       ("vala" ,vala)))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("gtk+" ,gtk+)))
    (propagated-inputs
     `(("gcr" ,gcr)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("json-glib" ,json-glib)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("oauth" ,liboauth)))
    (synopsis "Google Data APIs")
    (description "LibGData is a GLib-based library for accessing online service
APIs using the GData protocol.  It provides APIs to access the common Google
services, and has full asynchronous support.")
    (home-page "https://wiki.gnome.org/Projects/libgdata")
    (license license:lgpl2.1+)))

(define-public libgxps
  (package
    (name "libgxps")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "157s4c9gjjss6yd7qp7n4q6s72gz1k4ilsx4xjvp357azk49z4qs"))))
    (build-system meson-build-system)
    (outputs '("out" "bin" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Denable-gtk-doc=true"
        "-Denable-man=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* "reference/libgxps-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/")))
               (substitute* "tools/meson.build"
                 (("http://docbook.sourceforge.net/release/xsl/current")
                  (string-append (assoc-ref inputs "docbook-xsl")
                                 "/xml/xsl/docbook-xsl-"
                                 ,(package-version docbook-xsl)))))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'move-bin
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (assoc-ref outputs "bin")))
               (mkdir-p (string-append bin "/bin"))
               (rename-file
                (string-append out "/bin")
                (string-append bin "/bin"))
               #t))))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("docbook-xsl" ,docbook-xsl)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("freetype" ,freetype)
       ("gtk+" ,gtk+)
       ("lcms" ,lcms)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)))
    (propagated-inputs
     `(("cairo" ,cairo)
       ("glib" ,glib)
       ("libarchive" ,libarchive)))
    (synopsis "XPS management library")
    (description "LibGxps is a GObject-based library for handling and rendering XPS
documents.")
    (home-page "https://wiki.gnome.org/Projects/libgxps")
    (license license:lgpl2.1+)))

(define-public gnome-characters
  (package
    (name "gnome-characters")
    (version "3.34.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/"
                       "gnome-characters/" (version-major+minor version)
                       "/gnome-characters-" version ".tar.xz"))
       (sha256
        (base32 "0mqaxsa7hcmvid3zbzvxpfkp7s01ghiq6kaibmd3169axrr8ahql"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache")
                "true"))
             #t))
         (add-after 'install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             ;; GNOME Characters needs Typelib files from GTK and
             ;; gnome-desktop.
             (wrap-program
                 (string-append (assoc-ref outputs "out")
                                "/bin/gnome-characters")
               `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH"))))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("ruby" ,ruby)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gjs" ,gjs)
       ("glib" ,glib)
       ("gnome-desktop" ,gnome-desktop)
       ("gtk+" ,gtk+)
       ("libunistring" ,libunistring)
       ("pango" ,pango)))
    (synopsis "Browse and search for non-standard characters")
    (description "GNOME-Characters is a simple utility application to find and
insert unusual characters.  It allows you to quickly find the character you are
looking for by searching for keywords.")
    (home-page "https://wiki.gnome.org/Apps/Characters")
    (license
     (list
      ;; GTK-JS-App
      license:bsd-3
      ;; Others
      license:gpl2+))))

(define-public gnome-common
  (package
    (name "gnome-common")
    (version "3.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1kzqi8qvh5p1zncj8msazlmvcwsczjz2hqxp4x2y0mg718vrwmi2"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnome.org/")
    (synopsis "Bootstrap GNOME modules built from Git")
    (description "gnome-common contains various files needed to bootstrap
GNOME modules built from Git.  It contains a common \"autogen.sh\" script that
can be used to configure a source directory checked out from Git and some
commonly used macros.")
    (license license:gpl2+)))

(define-public gnome-contacts
  (package
    (name "gnome-contacts")
    (version "3.36.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/gnome-contacts/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "048l07biy8xrfidfyzrjd5lrnfzqhb767ih2gl7w6c4mmhj4g2dy"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        ;; To enabled after upstream fixes the bug,
        ;; https://gitlab.gnome.org/GNOME/gnome-contacts/-/issues/176
        ;; "-Dtelepathy=true"
        "-Ddocs=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "man"
               (substitute* '("gnome-contacts.xml" "meson.build")
                 (("http://docbook.sourceforge.net/release/xsl/current")
                  (string-append (assoc-ref inputs "docbook-xsl")
                                 "/xml/xsl/docbook-xsl-"
                                 ,(package-version docbook-xsl)))
                 (("http://www.oasis-open.org/docbook/xml/4.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache")
                "true"))
             #t)))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("appstream-util" ,appstream-glib)
       ("cheese" ,cheese)
       ("evolution-data-server" ,evolution-data-server)
       ("folks" ,folks)
       ("gee" ,libgee)
       ("glib" ,glib)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("gtk+" ,gtk+)
       ("libhandy" ,libhandy)
       ("telepathy-glib" ,telepathy-glib)))
    (synopsis "Contact manager for GNOME")
    (description "GNOME-Contacts organizes your contact information from online
and offline sources, providing a centralized place for managing your contacts.")
    (home-page "https://wiki.gnome.org/Apps/Contacts")
    (license license:gpl2+)))

(define-public gnome-desktop
  (package
    (name "gnome-desktop")
    (version "3.36.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1ilv49qkppbbag5728iswg1jhhqx9hbj7j8k8wccnbyaq54bsyq0"))))
    (build-system meson-build-system)
    (outputs '("out" "help" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dgnome_distributor=Guix"
        "-Dudev=enabled"
        "-Dsystemd=disabled"
        "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference/gnome-desktop3"
               (substitute* "gnome-desktop3-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((libc (assoc-ref inputs "libc")))
               (substitute* "libgnome-desktop/gnome-languages.c"
                 (("\"locale\"")
                  (string-append "\"" libc "/bin/locale\"")))
               #t)))
         (add-before 'configure 'patch-bubblewrap
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libgnome-desktop/gnome-desktop-thumbnail-script.c"
               (("\"bwrap\",")
                (string-append "\"" (which "bwrap") "\","))
               (("\"--ro-bind\", \"/usr\", \"/usr\",")
                (string-append "\"--ro-bind\", \""
                               (%store-directory)
                               "\", \""
                               (%store-directory)
                               "\","))
               (("\"--ro-bind\", \"/etc/ld.so.cache\", \"/etc/ld.so.cache\",")
                ""))
             #t))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require a running X server and locales.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             (setenv "GUIX_LOCPATH"
                     (string-append (assoc-ref inputs "glibc-locales")
                                    "/lib/locale"))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/help")
                (string-append help "/share/help"))
               #t))))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("glib:bin" ,glib "bin")
       ("glibc-locales" ,glibc-locales)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("bubblewrap" ,bubblewrap)))
    (propagated-inputs
     `(("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("iso-codes" ,iso-codes)
       ("libseccomp" ,libseccomp)
       ("libudev" ,eudev)
       ("xkeyboard-config" ,xkeyboard-config)
       ("x11" ,libx11)))
    (synopsis "Library for sharing code between GNOME desktop components")
    (description "GNOME-Desktop contains the libgnome-desktop library as well as
a data file that exports the GNOME version to the Settings Details panel.
The libgnome-desktop library provides API shared by several applications on the
desktop.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-desktop")
    (license
     (list
      ;; Documentation
      license:fdl1.1+
      ;; Library
      license:lgpl2.0+
      ;; Others
      license:gpl2+))))

(define-public gnome-doc-utils
  (package
    (name "gnome-doc-utils")
    (version "0.20.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (version-major+minor version)  "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "19n4x25ndzngaciiyd8dd6s2mf9gv6nv3wv27ggns2smm7zkj1nb"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("docbook-xml" ,docbook-xml-4.4)
       ("python2-libxml2" ,python2-libxml2)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (home-page "https://wiki.gnome.org/GnomeDocUtils")
    (synopsis
     "Documentation utilities for the Gnome project")
    (description
     "Gnome-doc-utils is a collection of documentation utilities for the
Gnome project.  It includes xml2po tool which makes it easier to translate
and keep up to date translations of documentation.")
    (license license:gpl2+))) ; xslt under lgpl

(define-public gnome-disk-utility
  (package
    (name "gnome-disk-utility")
    (version "3.36.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0yhnjmjzkixj29vcw6rzaijpg4mlwm2k1kqp4g3hn1xb6qzks0yx"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dlogind=libelogind")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xsl
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/man"
               (substitute* "meson.build"
                 (("http://docbook.sourceforge.net/release/xsl/current")
                  (string-append (assoc-ref inputs "docbook-xsl")
                                 "/xml/xsl/docbook-xsl-"
                                 ,(package-version docbook-xsl)))))
             #t))
         (add-before 'configure 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache")
                "true"))
             #t)))))
    (native-inputs
     `(("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("dvdread" ,libdvdread)
       ("elogind" ,elogind)
       ("glib" ,glib)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("liblzma" ,xz)
       ("libnotify" ,libnotify)
       ("libsecret" ,libsecret)
       ("pwquality" ,libpwquality)
       ("udisks" ,udisks)))
    (synopsis "View, modify and configure disks and media")
    (description "GNOME-Disk-Utility provides libraries and applications for
dealing with storage devices.")
    (home-page "https://wiki.gnome.org/Apps/Disks")
    (license license:gpl2+)))

(define-public gnome-font-viewer
  (package
    (name "gnome-font-viewer")
    (version "3.34.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/gnome-font-viewer/"
                       (version-major+minor version)
                       "/gnome-font-viewer-" version ".tar.xz"))
       (sha256
        (base32 "12xrsqwmvid7hksiw4zhj4jd1qwxn8w0czskbq4yqfprwn1havxa"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))   ; To wrap binaries and/or compile schemas
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("gnome-desktop" ,gnome-desktop)
       ("gtk+" ,gtk+)
       ("harfbuzz" ,harfbuzz)))
    (synopsis "View fonts on your system")
    (description "GNOME-Font-Viewer is an application to show you the fonts
installed on your computer for your use as thumbnails.  Selecting any thumbnails
shows the full view of how the font would look under various sizes.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-font-viewer")
    (license license:gpl2+)))

(define-public gcr
  (package
    (name "gcr")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "00b6bzpr8rj8mvj66r2273r417wg2y21m6n88mhkq9m22z8bxyda"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     '(#:glib-or-gtk? #t    ; To wrap binaries and/or compile schemas.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* "gcr/gcr-visual-index.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml-4.3")
                                 "/xml/dtd/docbook/")))
               (substitute* '("gcr/gcr-docs.sgml" "gck/gck-docs.sgml"
                              "gck/gck-pkcs11-links.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml-4.1.2")
                                 "/xml/dtd/docbook/"))))
             #t))
         ;; Some tests fail due to missing /etc/machine-id.
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "gcr/meson.build"
               (("'system-prompt',")
                ""))
             #t))
         ;; Some tests expect to write to $HOME.
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" "/tmp")
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
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("gtk+bin" ,gtk+ "bin")
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("dbus" ,dbus)
       ("gnupg" ,gnupg)
       ("gnupg2" ,gnupg-2.0)
       ("libgcrypt" ,libgcrypt)))
    (propagated-inputs
     `(("p11-kit" ,p11-kit)
       ("glib" ,glib)
       ("gtk+" ,gtk+)))
    (synopsis "GCR and GCK libraries for GNOME")
    (description "GCR is a library for displaying certificates, and crypto UI,
accessing key stores.  It also provides the viewer for crypto files on the GNOME
desktop.  GCK is a library for accessing PKCS#11 modules like smart cards, in a
GObject oriented way.")
    (home-page "https://www.gnome.org") ; No dedicated home-page
    (license license:lgpl2.1+)))

(define-public gdl
  (package
    (name "gdl")
    (version "3.34.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/gdl.git")
                    (commit (string-append "GDL_" (string-map (match-lambda
                                                                (#\. #\_)
                                                                (c c))
                                                              version)))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "154qcr0x6f68f4q526y87imv0rscmp34n47nk1pp82rsq52h2zna"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("gnome-common" ,gnome-common)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("libtool" ,libtool)
       ("which" ,which)))
    (inputs
     `(("libxml2" ,libxml2)))
    (propagated-inputs
     ;; The gdl-3.0.pc file 'Requires' GTK+.
     `(("gtk+" ,gtk+)))
    (home-page "https://gitlab.gnome.org/GNOME/gdl/")
    (synopsis "GNOME docking library")
    (description "This library provides docking features for gtk+.")
    (license license:lgpl2.1+)))

;;; A minimal variant used to break a cycle with Inkscape.
(define-public gdl-minimal
  (package
    (inherit gdl)
    (name "gdl-minimal")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-doc-generation
           ;; XXX: There is no easy way to disable generating the
           ;; documentation.
           (lambda _
             (substitute* "configure.in"
               (("GTK_DOC_CHECK.*") "")
               (("docs/.*") ""))
             (substitute* "Makefile.am"
               (("gdl docs po") "gdl po"))
             #t)))))
    (native-inputs (alist-delete "gtk-doc" (package-native-inputs gdl)))))

(define-public libgnome-keyring
  (package
    (name "libgnome-keyring")
    (version "3.12.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "10vpjhgbjm7z2djy04qakd02qlzpd02xnbfjhk2aqwjzn3xpihf4"))))
    (build-system gnu-build-system)
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("dbus" ,dbus)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     `(("glib" ,glib)))
    (home-page "https://www.gnome.org")
    (synopsis "Accessing passwords from the GNOME keyring")
    (description
     "Client library to access passwords from the GNOME keyring.")

    ;; Though a couple of files are LGPLv2.1+.
    (license license:lgpl2.0+)))

(define-public gnome-keyring
  (package
    (name "gnome-keyring")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "11sgffrrpss5cmv3b717pqlbhgq17l1xd33fsvqgsw8simxbar52"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f                  ; Tests require missing machine-id
       #:configure-flags
       (list
        "--enable-doc"
        (string-append "--with-pkcs11-config="
                       (assoc-ref %outputs "out")
                       "/share/p11-kit/modules")
        (string-append "--with-pkcs11-modules="
                       (assoc-ref %outputs "out")
                       "/lib/pkcs11"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'trigger-bootstrap
           (lambda _
             (for-each delete-file
                       (list
                        "configure"
                        "Makefile.in"))
             #t))
         (add-before 'configure 'patch-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* '("gnome-keyring-daemon.xml"
                              "gnome-keyring.xml" "Makefile.am")
                 (("http://docbook.sourceforge.net/release/xsl/current")
                  (string-append (assoc-ref inputs "docbook-xsl")
                                 "/xml/xsl/docbook-xsl-"
                                 ,(package-version docbook-xsl)))
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("docbook-xml" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("gcr" ,gcr)
       ("glib" ,glib)
       ("libcap-ng" ,libcap-ng)
       ("libgcrypt" ,libgcrypt)
       ("linux-pam" ,linux-pam)
       ("openssh" ,openssh)))
    (synopsis "GNOME Key Manager")
    (description "GNOME-Keyring is a collection of components in GNOME that
store secrets, passwords, keys, certificates and make them available to
applications.  It is integrated with the user's login, so that their secret
storage can be unlocked when the user logins into their session.  It is based
around a standard called PKCS#11, which is a standard way for applications to
manage certificates and keys on smart cards or secure storage.")
    (home-page "https://wiki.gnome.org/Projects/GnomeKeyring")
    (license
     (list
      ;; Library
      license:lgpl2.1+
      ;; Others
      license:gpl2+))))

(define-public evince
  (package
    (name "evince")
    (version "3.36.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/evince/"
                       (version-major+minor version) "/"
                       "evince-" version ".tar.xz"))
       (sha256
        (base32 "0clg9fhgjyj23mmcmw7dp512wzgv5m18fppn05qf1frz7r11mmk5"))))
    (build-system meson-build-system)
    (outputs '("out" "help" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dbrowser_plugin=true"
        "-Dps=auto"
        "-Dsystemduserunitdir=no")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "help/reference"
               (substitute* '("libdocument/libevdocument-docs.xml"
                              "libview/libevview-docs.xml" "shell/evince-docs.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache")
                "true"))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/help")
                (string-append help "/share/help"))
               #t))))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("dogtail" ,python-dogtail)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("adwaita-icon-theme" ,adwaita-icon-theme)
       ("atk" ,atk)
       ("cairo" ,cairo)
       ("dbus" ,dbus)
       ("djvulibre" ,djvulibre)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("ghostscript" ,ghostscript)
       ("gnome-desktop" ,gnome-desktop)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gspell" ,gspell)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libarchive" ,libarchive)
       ("libcanberra" ,libcanberra)
       ("libgnome-keyring" ,libgnome-keyring)
       ("libgxps" ,libgxps)
       ("libice" ,libice)
       ("libnautilus-extension" ,nautilus)
       ("libsecret" ,libsecret)
       ("libsm" ,libsm)
       ("libspectre" ,libspectre)
       ("libtiff" ,libtiff)
       ("libxml2" ,libxml2)
       ("pango" ,pango)
       ("poppler" ,poppler)
       ("shared-mime-info" ,shared-mime-info)
       ("t1lib" ,t1lib)
       ("texlive-bin" ,texlive-bin)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)))
    (synopsis "Document viewer")
    (description "Evince is a document viewer for multiple document formats.
The goal of evince is to replace the multiple document viewers that exist on
the GNOME Desktop with a single simple application.")
    (home-page "https://wiki.gnome.org/Apps/Evince")
    (license license:gpl2+)))

(define-public gsettings-desktop-schemas
  (package
    (name "gsettings-desktop-schemas")
    (version "3.37.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0fvnchxvvx7d00ffn7cx007ahgfpwa0aw5jhspl22kd9prrazi8p"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t))   ; To wrap binaries and/or compile schemas
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)))
    (synopsis "Shared GSettings schemas for the GNOME desktop")
    (description "Gsettings-desktop-schemas contains a collection of GSettings
schemas for settings shared by various components of the GNOME desktop.")
    (home-page "https://launchpad.net/gsettings-desktop-schemas")
    (license license:lgpl2.1+)))

(define-public icon-naming-utils
  (package
    (name "icon-naming-utils")
    (version "0.8.90")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://tango.freedesktop.org/releases/icon-naming-utils-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1mc3v28fdfqanx3lqx233vcr4glb4c2376k0kx2v91a4vxwqcdxi"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl)
       ("perl-xml-simple" ,perl-xml-simple)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'set-load-paths
           ;; Tell 'icon-name-mapping' where XML::Simple is.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (prog (string-append out "/libexec/icon-name-mapping")))
               (wrap-program
                   prog
                 `("PERL5LIB" = ,(list (getenv "PERL5LIB")))))
             #t)))))
    (home-page "http://tango.freedesktop.org/Standard_Icon_Naming_Specification")
    (synopsis
     "Utility to implement the Freedesktop Icon Naming Specification")
    (description
     "To help with the transition to the Freedesktop Icon Naming
Specification, the icon naming utility maps the icon names used by the
GNOME and KDE desktops to the icon names proposed in the specification.")
    (license license:lgpl2.1+)))

(define-public gnome-icon-theme
  (package
    (name "gnome-icon-theme")
    (version "3.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (version-major+minor version)  "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "0fjh9qmmgj34zlgxb09231ld7khys562qxbpsjlaplq2j85p57im"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; Don't create 'icon-theme.cache'.
       (let* ((coreutils (assoc-ref %build-inputs "coreutils"))
              (true      (string-append coreutils "/bin/true")))
         (list (string-append "GTK_UPDATE_ICON_CACHE=" true)))))
    (native-inputs
     `(("icon-naming-utils" ,icon-naming-utils)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://wiki.gnome.org/Personalization")
    (synopsis "GNOME icon theme")
    (description "Icons for the GNOME desktop.")
    (license license:lgpl3))) ; or Creative Commons BY-SA 3.0

(define-public adwaita-icon-theme
  (package
    (name "adwaita-icon-theme")
    (version "3.36.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "0kgiq712lfidd81yzx0yk7qwlj3q8cymp6npnxyzsk844y353674"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags
       ;; Don't create 'icon-theme.cache'.
       (let*
           ((coreutils (assoc-ref %build-inputs "coreutils"))
            (true (string-append coreutils "/bin/true")))
         (list
          (string-append "GTK_UPDATE_ICON_CACHE=" true)))))
    (native-inputs
     `(("gtk+:bin" ,gtk+ "bin")
       ("icon-naming-utils" ,icon-naming-utils)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)))
    (synopsis "GNOME icon theme")
    (description "The standard icon theme for GNOME desktop environment.")
    (home-page "https://wiki.gnome.org/Personalization")
    (license
     ;; Any one of the following licenses can be chosen.
     (list
      license:cc-by-sa3.0
      license:lgpl3+))))

(define-public tango-icon-theme
  (package
    (name "tango-icon-theme")
    (version "0.8.90")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://tango.freedesktop.org/releases/"
                                  "tango-icon-theme-" version ".tar.bz2"))
              (sha256
               (base32
                "034r9s944b4yikyfgn602yv7s54wdzlq0qfvqh52b9x6kbx08h79"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("icon-naming-utils" ,icon-naming-utils)
       ("intltool" ,intltool)
       ("imagemagick" ,imagemagick)
       ("pkg-config" ,pkg-config)))
    (home-page "http://tango-project.org/")
    (synopsis "Tango icon theme")
    (description "This is an icon theme that follows the Tango visual
guidelines.")
    (license license:public-domain)))

(define-public shared-mime-info
  (package
    (name "shared-mime-info")
    (version "1.15")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://gitlab.freedesktop.org/xdg/shared-mime-info/uploads/"
                   "b27eb88e4155d8fccb8bb3cd12025d5b/shared-mime-info-" version
                   ".tar.xz"))
             (sha256
              (base32
               "146vynj78wcwdq0ms52jzm1r4m6dzi1rhyh3h4xyb6bw8ckv10pl"))))
    (build-system gnu-build-system)
    (arguments
     ;; The build system appears not to be parallel-safe.
     '(#:parallel-build? #f))
    (inputs
     `(("glib" ,glib)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.freedesktop.org/wiki/Software/shared-mime-info")
    (synopsis "Database of common MIME types")
    (description
     "The shared-mime-info package contains the core database of common types
and the update-mime-database command used to extend it.  It requires glib2 to
be installed for building the update command.  Additionally, it uses intltool
for translations, though this is only a dependency for the maintainers.  This
database is translated at Transifex.")
    (license license:gpl2+)))

(define-public system-config-printer
  (package
    (name "system-config-printer")
    (version "1.5.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/OpenPrinting/system-config-printer/releases/"
             "download/" version
             "/system-config-printer-" version ".tar.xz"))
       (sha256
        (base32 "050yrx1vfh9f001qsn06y1jcidxq0ymxr64kxykasr0zzryp25kb"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%glib-or-gtk-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Makefile.am
           (lambda _
             ;; The Makefile generates some scripts, so set a valid shebang
             (substitute* "Makefile.am"
               (("/bin/bash") (which "bash")))
             (delete-file "configure")
             #t))
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Modify the man XML otherwise xmlto tries to access the network
             (substitute* "man/system-config-printer.xml"
               (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                (string-append (assoc-ref inputs "docbook-xml")
                               "/xml/dtd/docbook/")))
             #t))
         (add-after 'install 'wrap-for-python
           (@@ (guix build python-build-system) wrap))
         (add-after 'install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (for-each
                (lambda (program)
                  (wrap-program program
                    `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                (map (lambda (name)
                       (string-append out "/bin/" name))
                     '("system-config-printer"
                       "system-config-printer-applet"
                       "install-printerdriver"
                       "scp-dbus-service"))))
             #t)))))
    (inputs
     `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gobject-introspection" ,gobject-introspection)
       ("python" ,python)
       ("cups" ,cups)
       ("gtk+" ,gtk+)
       ("python-dbus" ,python-dbus)
       ("python-pygobject" ,python-pygobject)
       ("python-pycups" ,python-pycups)
       ("python-requests" ,python-requests)
       ("python-pycairo" ,python-pycairo)
       ("libnotify" ,libnotify)
       ("packagekit" ,packagekit)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("glib" ,glib)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("intltool" ,intltool)
       ("xmlto" ,xmlto)
       ("docbook-xml" ,docbook-xml-4.1.2)
       ("docbook-xsl" ,docbook-xsl)
       ("libxml2" ,libxml2)))
    (home-page "https://github.com/zdohnal/system-config-printer")
    (synopsis "CUPS administration tool")
    (description
     "system-config-printer is a CUPS administration tool.  It's written in
Python using GTK+, and uses the @acronym{IPP, Internet Printing Protocol} when
configuring CUPS.")
    (license license:gpl2+)))

(define-public hicolor-icon-theme
  (package
    (name "hicolor-icon-theme")
    (version "0.17")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://icon-theme.freedesktop.org/releases/"
                          "hicolor-icon-theme-" version ".tar.xz"))
      (sha256
       (base32
        "1n59i3al3zx6p90ff0l43gzpzmlqnzm6hf5cryxqrlbi48sq8x1i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no check target
    (home-page "https://icon-theme.freedesktop.org/releases/")
    (synopsis
     "Freedesktop icon theme")
    (description
     "Freedesktop icon theme.")
    (license license:gpl2)))

(define-public libnotify
  (package
    (name "libnotify")
    (version "0.7.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0qa7cx6ra5hwqnxw95b9svgjg5q6ynm8y843iqjszxvds5z53h36"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dman=false")                  ; XXX: docbook-xsl-ns not available
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute*
                   (append
                    (list
                     "notification-spec.xml"
                     "reference/libnotify-docs.sgml")
                    (find-files "releases" "\\.xml$"))
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))))
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
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("gtk+" ,gtk+)))
    (propagated-inputs
     `(("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)))
    (synopsis "GNOME desktop notification library")
    (description "Libnotify is a library that sends desktop notifications to a
notification daemon, as defined in the Desktop Notifications spec.  These
notifications can be used to inform the user about an event or display some
form of information without getting in the user's way.")
    (home-page "https://developer.gnome.org/libnotify/")
    (license license:lgpl2.1+)))

(define-public libpeas
  (package
    (name "libpeas")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0xkk9zhkw8f2fm7g9nb4ry4xxig5n27s7rjmx6l7jr2941zdfxm9"))))
    (build-system meson-build-system)
    (outputs '("out" "demo" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dpython2=true"
        "-Dvapi=true"
        "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* "libpeas-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
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
               #t)))
         (add-after 'move-doc 'move-demo
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (demo (assoc-ref outputs "demo")))
               (mkdir-p (string-append demo "/bin"))
               (mkdir-p (string-append demo "/lib"))
               (rename-file
                (string-append out "/bin")
                (string-append demo "/bin"))
               (rename-file
                (string-append out "/lib/peas-demo")
                (string-append demo "/lib/peas-demo"))
               #t))))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("luajit" ,luajit)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("glade" ,glade)
       ("lua" ,lua)
       ("python2" ,python-2)
       ("python3" ,python)
       ("python2-pygobject" ,python2-pygobject)
       ("python3-pygobject" ,python-pygobject)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)))
    (synopsis "GObject Plugin System")
    (description
     "Libpeas is a gobject-based plugin engine, targeted at giving every
application the chance to assume its own extensibility.  It also has a set of
features including, but not limited to: multiple extension points; on-demand
(lazy) programming language support for C, Python and JS; simplicity of the
API.")
    (home-page "https://wiki.gnome.org/Projects/Libpeas")
    (license license:lgpl2.1+)))

(define-public gtkglext
  (package
    (name "gtkglext")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gtkglext/gtkglext/"
                                  version "/gtkglext-" version ".tar.gz"))
              (sha256
               (base32 "1ya4d2j2aacr9ii5zj4ac95fjpdvlm2rg79mgnk7yvl1dcy3y1z5"))
              (patches (search-patches
                        "gtkglext-disable-disable-deprecated.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Fix a collision between a local variable and a definition from
         ;; glibc's string.h.
         (add-after 'unpack 'fix-collision
           (lambda _
             (substitute* "gdk/gdkglshapes.c"
               ((" index") " triangle_index"))
             #t)))))
    (inputs `(("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("glu" ,glu)
              ("libx11" ,libx11)
              ("libxt" ,libxt)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("glib" ,glib "bin")))
    (propagated-inputs `(("pangox-compat" ,pangox-compat)))
    (home-page "https://projects.gnome.org/gtkglext")
    (synopsis "OpenGL extension to GTK+")
    (description "GtkGLExt is an OpenGL extension to GTK+.  It provides
additional GDK objects which support OpenGL rendering in GTK+ and GtkWidget
API add-ons to make GTK+ widgets OpenGL-capable.")
    (license license:lgpl2.1+)))

(define-public glade
  (package
    (name "glade")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "023gx8rj51njn8fsb6ma5kz1irjpxi4js0n8rwy22inc4ysldd8r"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc" "help"))
    (arguments
     `(#:configure-flags
       (list
        "--enable-gtk-doc"
        "--enable-man-pages"
        "--enable-gladeui"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html")
        (string-append "--with-help-dir="
                       (assoc-ref %outputs "help")
                       "/share/help"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "gladeui-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.5/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("pygobject" ,python-pygobject)
       ("python" ,python-wrapper)
       ("xorg-server" ,xorg-server-for-tests)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("glib" ,glib)
       ("webkitgtk" ,webkitgtk)))
    (propagated-inputs
     `(("gtk+" ,gtk+)
       ("libxml2" ,libxml2)))
    (synopsis "User Interface designer for Gtk+ and GNOME")
    (description "Glade is a RAD tool to enable quick and easy development of
user interfaces for the GTK+ toolkit and the GNOME desktop environment.")
    (home-page "https://glade.gnome.org")
    (license
     (list
      ;; Most of the code base.
      license:lgpl2.0+
      ;; Some of the code base.
      license:gpl2+))))

(define-public libcroco
  (package
    (name "libcroco")
    (version "0.6.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1m110rbj5d2raxcdp4iz0qp172284945awrsbdlq99ksmqsc4zkn"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("libxml2" ,libxml2)
       ("zlib" ,zlib)))
    (home-page "https://github.com/GNOME/libcroco")
    (synopsis "CSS2 parsing and manipulation library")
    (description
     "Libcroco is a standalone CSS2 parsing and manipulation library.
The parser provides a low level event driven SAC-like API and a CSS object
model like API.  Libcroco provides a CSS2 selection engine and an experimental
XML/CSS rendering engine.")

    ;; LGPLv2.1-only.
    (license license:lgpl2.1)))

(define-public libgsf
  (package
    (name "libgsf")
    (version "1.14.47")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0kbpp9ksl7977xiga37sk1gdw1r039v6zviqznl7alvvg39yp26i"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "bin" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-introspection"
        (string-append "--with-gir-dir="
                       (assoc-ref %outputs "out")
                       "/share/gir-"
                       ,(version-major
                         (package-version gobject-introspection))
                       ".0")
        (string-append "--with-typelib-dir="
                       (assoc-ref %outputs "out")
                       "/lib/girepository-"
                       ,(version-major
                         (package-version gobject-introspection))
                       ".0")
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html")
        "--with-zlib"
        "--with-bz2")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "gsf-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.5/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("perl" ,perl)
       ("perl-xml-parser" ,perl-xml-parser)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("bzip2" ,bzip2)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("glib" ,glib)
       ("libxml2" ,libxml2)))
    (synopsis "G Structured File Library")
    (description "Libgsf aims to provide an efficient extensible I/O abstraction
for dealing with different structured file formats.")
    (home-page "https://gitlab.gnome.org/GNOME/libgsf")
    (license
     (list
      ;; Library
      license:lgpl2.1+
      ;; Others
      license:lgpl2.0+))))

(define-public librsvg
  (package
    (name "librsvg")
    (version "2.40.21")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1fljkag2gr7c4k5mn798lgf9903xslz8h51bgvl89nnay42qjqpp"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-vala"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "rsvg-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'pre-configure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gdk-pixbuf-loader/Makefile.in"
               ;; By default the gdk-pixbuf loader is installed under
               ;; gdk-pixbuf's prefix.  Work around that.
               (("gdk_pixbuf_moduledir = .*$")
                (string-append "gdk_pixbuf_moduledir = "
                               "$(prefix)/lib/gdk-pixbuf-2.0/2.10.0/"
                               "loaders\n"))
               ;; Drop the 'loaders.cache' file, it's in gdk-pixbuf+svg.
               (("gdk_pixbuf_cache_file = .*$")
                "gdk_pixbuf_cache_file = $(TMPDIR)/loaders.cache\n"))
             #t))
         (add-before 'check 'remove-failing-tests
           (lambda _
             (with-directory-excursion "tests/fixtures/reftests"
               (for-each delete-file
                         '("bugs/340047.svg"
                           "bugs/587721-text-transform.svg"
                           "bugs/749415.svg"
                           "bugs/777834-empty-text-children.svg")))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("vala" ,vala)))
    (inputs
     `(("bzip2" ,bzip2)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("harfbuzz" ,harfbuzz)
       ("libcroco" ,libcroco)
       ("libgsf" ,libgsf)
       ("libxml2" ,libxml2)
       ("pango" ,pango)))
    (propagated-inputs
     `(("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)))
    (synopsis "SVG rendering library")
    (description "Librsvg is a library to render SVG images to Cairo surfaces.
GNOME uses this to render SVG icons.  Outside of GNOME, other desktop
environments use it for similar purposes.  Wikimedia uses it for Wikipedia's SVG
diagrams.")
    (home-page "https://wiki.gnome.org/LibRsvg")
    (license license:lgpl2.1+)))

(define-public librsvg-next
  (package
    (name "librsvg")
    (version "2.48.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/librsvg/"
                                  (version-major+minor version)  "/"
                                  "librsvg-" version ".tar.xz"))
              (sha256
               (base32
                "14i6xzghcidv64cyd3g0wdjbl82rph737yxn9s3x29nzpcjs707l"))
              (modules '((guix build utils)))
              (snippet
               '(begin (delete-file-recursively "vendor")
                       #t))))
    (build-system cargo-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:modules
       ((guix build cargo-build-system)
        (guix build utils)
        ((guix build gnu-build-system) #:prefix gnu:))
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.8)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-cast" ,rust-cast-0.2)
        ("rust-cssparser" ,rust-cssparser-0.27)
        ("rust-data-url" ,rust-data-url-0.1)
        ("rust-downcast-rs" ,rust-downcast-rs-1.1)
        ("rust-encoding" ,rust-encoding-0.2)
        ("rust-float-cmp" ,rust-float-cmp-0.6)
        ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.8)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.9)
        ("rust-gio" ,rust-gio-0.8)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-itertools" ,rust-itertools-0.8)
        ("rust-language-tags" ,rust-language-tags-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-locale-config" ,rust-locale-config-0.3)
        ("rust-markup5ever" ,rust-markup5ever-0.10)
        ("rust-nalgebra" ,rust-nalgebra-0.19)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-once-cell" ,rust-once-cell-1.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-pango" ,rust-pango-0.8)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-pangocairo" ,rust-pangocairo-0.9)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rctree" ,rust-rctree-0.3)
        ("rust-rgb" ,rust-rgb-0.8)
        ("rust-regex" ,rust-regex-1)
        ("rust-selectors" ,rust-selectors-0.22)
        ("rust-url" ,rust-url-2.1)
        ("rust-xml5ever" ,rust-xml5ever-0.16))
       #:cargo-development-inputs
       (("rust-cairo-rs" ,rust-cairo-rs-0.8)
        ("rust-criterion" ,rust-criterion-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "rsvg-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'unpack 'prepare-for-build
           (lambda _
             ;; In lieu of #:make-flags
             (setenv "CC" ,(cc-for-target))
             ;; Something about the build environment resists building
             ;; successfully with the '--locked' flag.
             (substitute* '("Makefile.am" "Makefile.in")
               (("--locked") ""))
             #t))
         (add-before 'configure 'pre-configure
           (lambda _
             (substitute* "gdk-pixbuf-loader/Makefile.in"
               ;; By default the gdk-pixbuf loader is installed under
               ;; gdk-pixbuf's prefix.  Work around that.
               (("gdk_pixbuf_moduledir = .*$")
                (string-append "gdk_pixbuf_moduledir = "
                               "$(prefix)/lib/gdk-pixbuf-2.0/2.10.0/"
                               "loaders\n"))
               ;; Drop the 'loaders.cache' file, it's in gdk-pixbuf+svg.
               (("gdk_pixbuf_cache_file = .*$")
                "gdk_pixbuf_cache_file = $(TMPDIR)/loaders.cache\n"))
             #t))
         (add-after 'configure 'gnu-configure
           (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
             ((assoc-ref gnu:%standard-phases 'configure)
              #:native-inputs native-inputs
              #:inputs inputs
              #:outputs outputs
              #:configure-flags
              (list "--disable-static"
                    "--enable-vala"
               (string-append "--with-html-dir="
                              (assoc-ref %outputs "doc")
                              "/share/gtk-doc/html")))))
         (add-after 'configure 'dont-vendor-self
           (lambda* (#:key vendor-dir #:allow-other-keys)
             ;; Don't keep the whole tarball in the vendor directory
             (delete-file-recursively
              (string-append vendor-dir "/" ,name "-" ,version ".tar.xz"))
             #t))
         (replace 'build
           (assoc-ref gnu:%standard-phases 'build))
         (replace 'check
           (lambda* args
             ((assoc-ref gnu:%standard-phases 'check)
              #:test-target "check")))
         (replace 'install
           (assoc-ref gnu:%standard-phases 'install)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("ruby" ,ruby)
       ("vala" ,vala)))
    (inputs
     `(("bzip2" ,bzip2)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("harfbuzz" ,harfbuzz)
       ("libcroco" ,libcroco)
       ("libgsf" ,libgsf)
       ("libxml2" ,libxml2)
       ("pango" ,pango)))
    (propagated-inputs
     `(("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)))
    (synopsis "SVG rendering library")
    (description "Librsvg is a library to render SVG images to Cairo surfaces.
GNOME uses this to render SVG icons.  Outside of GNOME, other desktop
environments use it for similar purposes.  Wikimedia uses it for Wikipedia's SVG
diagrams.")
    (home-page "https://wiki.gnome.org/LibRsvg")
    (license license:lgpl2.1+)))

(define-public libidl
  (package
    (name "libidl")
    (version "0.8.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libIDL/"
                                  (version-major+minor version) "/"
                                  "libIDL-" version ".tar.bz2"))
              (sha256
               (base32
                "08129my8s9fbrk0vqvnmx6ph4nid744g5vbwphzkaik51664vln5"))))
    (build-system gnu-build-system)
    (inputs `(("glib" ,glib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("flex" ,flex)
       ("bison" ,bison)))
    (home-page "http://freecode.com/projects/libidl")
    (synopsis "Create trees of CORBA Interface Definition Language files")
    (description  "Libidl is a library for creating trees of CORBA Interface
Definition Language (idl) files, which is a specification for defining
portable interfaces. libidl was initially written for orbit (the orb from the
GNOME project, and the primary means of libidl distribution).  However, the
functionality was designed to be as reusable and portable as possible.")
    (properties `((upstream-name . "libIDL")))
    (license license:lgpl2.0+)))


(define-public orbit2
  (package
    (name "orbit2")
    (version "2.14.19")
    (source (origin
              (method url-fetch)
              (uri (let ((upstream-name "ORBit2"))
                     (string-append "mirror://gnome/sources/" upstream-name "/"
                                    (version-major+minor version) "/"
                                    upstream-name "-" version ".tar.bz2")))
              (sha256
               (base32
                "0l3mhpyym9m5iz09fz0rgiqxl2ym6kpkwpsp1xrr4aa80nlh1jam"))))
    (build-system gnu-build-system)
    (arguments
     `(;; The "timeout-server" test hangs when run in parallel.
       #:parallel-tests? #f
       #:configure-flags
       '(;; We don't need static libraries, plus they don't build reproducibly
         ;; (non-deterministic ordering of .o files in the archive.)
         "--disable-static"

         ;; The programmer kindly gives us a hook to turn off deprecation
         ;; warnings ...
         "DISABLE_DEPRECATED_CFLAGS=-DGLIB_DISABLE_DEPRECATION_WARNINGS")
       ;; ... which they then completly ignore !!
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'ignore-deprecations
           (lambda _
             (substitute* "linc2/src/Makefile.in"
               (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))
             #t)))))
    (inputs `(("glib" ,glib)
              ("libidl" ,libidl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/orbit2/")
    (synopsis "CORBA 2.4-compliant Object Request Broker")
    (description  "ORBit2 is a CORBA 2.4-compliant Object Request Broker (orb)
featuring mature C, C++ and Python bindings.")
    ;; Licence notice is unclear.  The Web page simply say "GPL" without giving
    ;; a version.  SOME of the code files have licence notices for GPLv2+.
    ;; The tarball contains files of the text of GPLv2 and LGPLv2.
    (license license:gpl2+)
    (properties `((upstream-name . "ORBit2")))))


(define-public libbonobo
  (package
    (name "libbonobo")
    (version "2.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)
                                  "/" name "-" version ".tar.bz2"))
              (sha256
               (base32 "0swp4kk6x7hy1rvd1f9jba31lvfc6qvafkvbpg9h0r34fzrd8q4i"))
              (patches (search-patches
                        "libbonobo-activation-test-race.patch"))))
    (build-system gnu-build-system)
    (arguments
     ;; The programmer kindly gives us a hook to turn off deprecation warnings ...
     `(#:configure-flags
       '("DISABLE_DEPRECATED_CFLAGS=-DGLIB_DISABLE_DEPRECATION_WARNINGS")
       ;; ... which they then completly ignore !!
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'ignore-deprecations
           (lambda _
             (substitute* "activation-server/Makefile.in"
               (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))
             #t)))

       ;; There's apparently a race condition between the server stub
       ;; generation and linking of the example under 'samples/echo' that can
       ;; lead do undefined references when building in parallel, as reported
       ;; at <https://forums.gentoo.org/viewtopic-t-223376-start-550.html>.
       ;; Thus, disable parallel builds.
       #:parallel-build? #f))
    (inputs `(("popt" ,popt)
              ("libxml2" ,libxml2)))
    ;; The following are Required by the .pc file
    (propagated-inputs
     `(("glib" ,glib)
       ("orbit2" ,orbit2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("flex" ,flex)
       ("bison" ,bison)))
    (home-page "https://developer.gnome.org/libbonobo/")
    (synopsis "Framework for creating reusable components for use in GNOME applications")
    (description "Bonobo is a framework for creating reusable components for
use in GNOME applications, built on top of CORBA.")
    ;; Licence not explicitly stated.  Source files contain no licence notices.
    ;; Tarball contains text of both GPLv2 and LGPLv2
    ;; GPLv2 covers both conditions
    (license license:gpl2+)))


(define-public gconf
  (package
    (name "gconf")
    (version "3.2.6")
    (source (origin
              (method url-fetch)
              (uri
               (let ((upstream-name "GConf"))
                 (string-append "mirror://gnome/sources/" upstream-name "/"
                                (version-major+minor version) "/"
                                upstream-name "-" version ".tar.xz")))
              (sha256
               (base32 "0k3q9nh53yhc9qxf1zaicz4sk8p3kzq4ndjdsgpaa2db0ccbj4hr"))))
    (build-system gnu-build-system)
    (inputs `(("dbus-glib" ,dbus-glib)
              ("libxml2" ,libxml2)))
    (propagated-inputs `(("glib" ,glib) ; referred to in the .pc file
                         ("orbit2" ,orbit2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/gconf/")
    (synopsis "Store application preferences")
    (description "Gconf is a system for storing application preferences.  It
is intended for user preferences; not arbitrary data storage.")
    (license license:lgpl2.0+)
    (properties '((upstream-name . "GConf")))))


(define-public gnome-mime-data
  (package
    (name "gnome-mime-data")
    (version "2.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1mvg8glb2a40yilmyabmb7fkbzlqd3i3d31kbkabqnq86xdnn69p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("intltool" ,intltool)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'configure 'use-our-intltool
                    (lambda _
                      ;; Do not use the bundled intltool commands, which lack
                      ;; the "dotless @INC" fixes of our 'intltool' package.
                      (substitute* (find-files "." "^Makefile$")
                        (("^INTLTOOL_(EXTRACT|UPDATE|MERGE) = .*$" _ tool)
                         (string-append "INTLTOOL_" tool " = intltool-"
                                        (string-downcase tool) "\n")))
                      #t)))))
    (home-page "https://www.gnome.org")
    (synopsis "Base MIME and Application database for GNOME")
    (description  "GNOME Mime Data is a module which contains the base MIME
and Application database for GNOME.  The data stored by this module is
designed to be accessed through the MIME functions in GnomeVFS.")
    (license license:gpl2+)))


(define-public gnome-vfs
  (package
    (name "gnome-vfs")
    (version "2.24.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1ajg8jb8k3snxc7rrgczlh8daxkjidmcv3zr9w809sq4p2sn9pk2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'ignore-deprecations
           (lambda _
             (substitute* '("libgnomevfs/Makefile.in"
                            "daemon/Makefile.in")
               (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))
             #t))
         (add-before 'configure 'patch-test-async-cancel-to-never-fail
           (lambda _
             (substitute* "test/test-async-cancel.c"
               (("EXIT_FAILURE") "77"))
             #t)))))
    (inputs `(("libxml2" ,libxml2)
              ("dbus-glib" ,dbus-glib)
              ("gconf" ,gconf)
              ("gnome-mime-data" ,gnome-mime-data)
              ("zlib" ,zlib)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-mkenums, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/gnome-vfs/")
    (synopsis "Access files and folders in GNOME applications")
    (description
     "GnomeVFS is the core library used to access files and folders in GNOME
applications.  It provides a file system abstraction which allows applications
to access local and remote files with a single consistent API.")
    (license license:lgpl2.0+)))



(define-public libgnome
  (package
    (name "libgnome")
    (version "2.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "197pnq8y0knqjhm2fg4j6hbqqm3qfzfnd0irhwxpk1b4hqb3kimj"))
              (patches (search-patches "libgnome-encoding.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enable-deprecated
           (lambda _
             (substitute* "libgnome/Makefile.in"
               (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))
             #t)))))
    (inputs `(("libxml2" ,libxml2)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-mkenums, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    ;; The following are listed as Required in the .pc file
    ;; (except for libcanberra -- which seems to be oversight on the part
    ;; of the upstream developers -- anything that links against libgnome,
    ;; must also link against libcanberra
    (propagated-inputs
     `(("libcanberra" ,libcanberra)
       ("libbonobo" ,libbonobo)
       ("gconf" ,gconf)
       ("gnome-vfs" ,gnome-vfs)
       ("popt" ,popt)))                       ;gnome-program.h includes popt.h
    (home-page "https://developer.gnome.org/libgnome/")
    (synopsis "Useful routines for building applications")
    (description  "The libgnome library provides a number of useful routines
for building modern applications, including session management, activation of
files and URIs, and displaying help.")
    (license license:lgpl2.0+)))


(define-public libart-lgpl
  (package
    (name "libart-lgpl")
    (version "2.3.21")
    (source (origin
              (method url-fetch)
              (uri (let ((upstream-name "libart_lgpl"))
                     (string-append "mirror://gnome/sources/" upstream-name "/"
                                    (version-major+minor version) "/"
                                    upstream-name "-" version ".tar.bz2")))
              (sha256
               (base32
                "1yknfkyzgz9s616is0l9gp5aray0f2ry4dw533jgzj8gq5s1xhgx"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://people.gnome.org/~mathieu/libart")
    (synopsis "2D drawing library")
    (description  "Libart is a 2D drawing library intended as a
high-quality vector-based 2D library with antialiasing and alpha composition.")
    (license license:lgpl2.0+)))



(define-public libgnomecanvas
  (package
    (name "libgnomecanvas")
    (version "2.30.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nhnq4lfkk8ljkdafscwaggx0h95mq0rxnd7zgqyq0xb6kkqbjm8"))))
    (build-system gnu-build-system)
    ;; Mentioned as Required in the .pc file
    (propagated-inputs `(("libart-lgpl" ,libart-lgpl)
                         ("gtk+" ,gtk+-2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libgnomecanvas/")
    (synopsis "Flexible widget for creating interactive structured graphics")
    (description  "The GnomeCanvas widget provides a flexible widget for
creating interactive structured graphics.")
    (license license:lgpl2.0+)))

(define-public libgnomecanvasmm
  (package
    (name "libgnomecanvasmm")
    (version "2.26.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0679hcnpam2gkag2i63sm0wdm35gwvzafnz1354mg6j5gzwpfrcr"))))
    (build-system gnu-build-system)
    (propagated-inputs `(("libgnomecanvas" ,libgnomecanvas)))
    (native-inputs
     `(("gtkmm-2" ,gtkmm-2)
       ("pkg-config" ,pkg-config)))
    (home-page "https://gtkmm.org")
    (synopsis "C++ bindings to the GNOME Canvas library")
    (description "C++ bindings to the GNOME Canvas library.")
    (license license:lgpl2.0+)))

(define-public libgnomeui
  (package
    (name "libgnomeui")
    (version "2.24.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (patches (search-patches "libgnomeui-utf8.patch"))
              (sha256
               (base32
                "03rwbli76crkjl6gp422wrc9lqpl174k56cp9i96b7l8jlj2yddf"))))
    (build-system gnu-build-system)
    ;; Mentioned as Required in the .pc file
    (propagated-inputs `(("libbonoboui" ,libbonoboui)
                         ("libgnome" ,libgnome)
                         ("libgnomecanvas" ,libgnomecanvas)
                         ("libgnome-keyring" ,libgnome-keyring)))
    (inputs `(("libjpeg" ,libjpeg-turbo)
              ("popt" ,popt)
              ("libbonobo" ,libbonobo)
              ("libxml2" ,libxml2)
              ("libglade" ,libglade)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-mkenums, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libgnomeui/")
    (synopsis "Additional widgets for applications")
    (description "The libgnomeui library provides additional widgets for
applications.  Many of the widgets from libgnomeui have already been
ported to GTK+.")
    (license license:lgpl2.0+)))

(define-public libglade
  (package
    (name "libglade")
    (version "2.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1v2x2s04jry4gpabws92i0wq2ghd47yr5n9nhgnkd7c38xv1wdk4"))))
    (build-system gnu-build-system)
    (inputs
     `(("python" ,python))) ;; needed for the optional libglade-convert program
    (propagated-inputs
     `(("gtk+-2" ,gtk+-2)
       ("libxml2" ,libxml2))) ; required by libglade-2.0.pc
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libglade")
    (synopsis "Load glade interfaces and access the glade built widgets")
    (description "Libglade is a library that provides interfaces for loading
graphical interfaces described in glade files and for accessing the
widgets built in the loading process.")
    (license license:gpl2+))) ; This is correct.  GPL not LGPL

(define-public libgnomeprint
  ;; This library has been deprecated since 2006; see
  ;; <https://mail.gnome.org/archives/devel-announce-list/2006-August/msg00005.html>.
  (package
    (name "libgnomeprint")
    (version "2.18.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "14cnimvlc7ky22g2snyf4362412k3jk1syjf8b9887q5a63fqd0h"))))
    (build-system gnu-build-system)
    (inputs
     `(("popt" ,popt)
       ("libart-lgpl" ,libart-lgpl)
       ("gtk+" ,gtk+-2)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/gnome-print/home/faq.html")
    (synopsis "Printing framework for GNOME")
    (description
     "GNOME-print was a printing framework for GNOME.  It has been deprecated
since ca. 2006, when GTK+ itself incorporated printing support.")
    (license license:lgpl2.0+)))


(define-public libgnomeprintui
  ;; Deprecated; see libgnomeprint.
  (package
    (name "libgnomeprintui")
    (version "2.18.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0spl8vinb5n6n1krnfnr61dwaxidg67h8j94z9p59k2xdsvfashm"))))
    (build-system gnu-build-system)
    ;; Mentioned as Required in the .pc file
    (propagated-inputs `(("libgnomeprint" ,libgnomeprint)))
    (inputs `(("gtk+" ,gtk+-2)
              ("glib" ,glib)
              ("gnome-icon-theme" ,gnome-icon-theme)
              ("libgnomecanvas" ,libgnomecanvas)
              ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/gnome-print/home/faq.html")
    (synopsis "Printing framework for GNOME")
    (description (package-description libgnomeprint))
    (license license:lgpl2.0+)))

(define-public libbonoboui
  (package
    (name "libbonoboui")
    (version "2.24.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1kbgqh7bw0fdx4f1a1aqwpff7gp5mwhbaz60c6c98bc4djng5dgs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xserver
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server"))
                   (disp ":1"))

               (setenv "HOME" (getcwd))
               (setenv "DISPLAY" disp)
               ;; There must be a running X server and make check doesn't start one.
               ;; Therefore we must do it.
               (zero? (system (format #f "~a/bin/Xvfb ~a &" xorg-server disp)))))))))
    ;; Mentioned as Required by the .pc file
    (propagated-inputs `(("libxml2" ,libxml2)))
    (inputs
     `(("popt" ,popt)
       ("pangox-compat" ,pangox-compat)
       ("libgnome" ,libgnome)
       ("libgnomecanvas" ,libgnomecanvas)
       ("libglade" ,libglade)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("intltool" ,intltool)
       ("xorg-server" ,xorg-server-for-tests) ; For running the tests
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libbonoboui/")
    (synopsis "Some user interface controls using Bonobo")
    (description  "The Bonobo UI library provides a number of user interface
controls using the Bonobo component framework.")
    (license license:lgpl2.0+)))

(define-public libwnck
  (package
    (name "libwnck")
    (version "3.32.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1jp3p1lnwnwi6fxl2rz3166cmwzwy9vqz896anpwc3wdy9f875cm"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin") ; for glib-mkenums
       ("gobject-introspection" ,gobject-introspection) ; for g-ir-scanner
       ("intltool" ,intltool)))
    (propagated-inputs
     `(("gtk+" ,gtk+)
       ("libxres" ,libxres)
       ("startup-notification" ,startup-notification)))
    (home-page "https://developer.gnome.org/libwnck/")
    (synopsis "Window Navigator Construction Kit")
    (description
     "Libwnck is the Window Navigator Construction Kit, a library for use in
writing pagers, tasklists, and more generally applications that are dealing
with window management.  It tries hard to respect the Extended Window Manager
Hints specification (EWMH).")
    (license license:lgpl2.0+)))

;; stable version for gtk2, required by xfwm4.
(define-public libwnck-2
  (package (inherit libwnck)
    (name "libwnck")
    (version "2.30.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "15713yl0f8f3p99jzqqfmbicrdswd3vwpx7r3bkf1bgh6d9lvs4b"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (propagated-inputs
     `(("gtk+" ,gtk+-2)
       ("libxres" ,libxres)
       ("startup-notification" ,startup-notification)))))

(define-public goffice
  (package
    (name "goffice")
    (version "0.10.47")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/goffice/"
                                  (version-major+minor version)  "/"
                                  "goffice-" version ".tar.xz"))
              (sha256
               (base32 "0xmigfdzvmlpa0fw79mf3xwchmxc8rlidryn5syv8bz7msmrb215"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                  ; 4.0 MiB of gtk-doc
    (arguments
     '(#:configure-flags (list (string-append "--with-html-dir="
                                              (assoc-ref %outputs "doc")
                                              "/share/gtk-doc/html"))))
    (inputs
     `(("gtk+" ,gtk+)
       ("libgsf" ,libgsf)
       ("librsvg" ,librsvg)
       ("libxslt" ,libxslt)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/goffice/")
    (synopsis "Document-centric objects and utilities")
    (description "A GLib/GTK+ set of document-centric objects and utilities.")
    (license
     ;; Dual licensed under GPLv2 or GPLv3 (both without "or later")
     ;; Note: NOT LGPL
     (list license:gpl2 license:gpl3))))

(define-public goffice-0.8
  (package (inherit goffice)
    (version "0.8.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" (package-name goffice) "/"
                                  (version-major+minor version)  "/"
                                  (package-name goffice) "-" version ".tar.xz"))
              (sha256
               (base32 "05fvzbs5bin05bbsr4dp79aiva3lnq0a3a40zq55i13vnsz70l0n"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-pcre-check
           (lambda _
             ;; Only glib.h can be included directly.  See
             ;; https://bugzilla.gnome.org/show_bug.cgi?id=670316
             (substitute* "configure"
               (("glib/gregex\\.h") "glib.h")) #t)))

       ,@(package-arguments goffice)))
    (propagated-inputs
     ;; libgoffice-0.8.pc mentions libgsf-1
     `(("libgsf" ,libgsf)))
    (inputs
     `(("gtk" ,gtk+-2)
       ,@(alist-delete "gtk" (package-inputs goffice))))))

(define-public gnumeric
  (package
    (name "gnumeric")
    (version "1.12.46")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnumeric/"
                                  (version-major+minor version)  "/"
                                  "gnumeric-" version ".tar.xz"))
              (sha256
               (base32
                "1qdmw2dp7rmq8fmjapgwaks7ajh270wm6kyvlxlzwbgmg8vngp4z"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(;; The gnumeric developers don't worry much about failing tests.
       ;; See https://bugzilla.gnome.org/show_bug.cgi?id=732387
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'pre-conf
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make install tries to write into the directory of goffice
             ;; I am informed that this only affects the possibility to embed a
             ;; spreadsheet inside an Abiword document.   So presumably when we
             ;; package Abiword we'll have to refer it to this directory.
             (substitute* "configure"
               (("^GOFFICE_PLUGINS_DIR=.*")
                (string-append "GOFFICE_PLUGINS_DIR="
                               (assoc-ref outputs "out")
                               "/goffice/plugins"))))))))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("goffice" ,goffice)
       ("libgsf" ,libgsf)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("python" ,python-2)
       ("python2-pygobject" ,python2-pygobject)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("docbook-xml" ,docbook-xml)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.gnumeric.org")
    (synopsis "Spreadsheet application")
    (description
     "GNUmeric is a GNU spreadsheet application, running under GNOME.  It is
interoperable with other spreadsheet applications.  It has a vast array of
features beyond typical spreadsheet functionality, such as support for linear
and non-linear solvers, statistical analysis, and telecommunication
engineering.")
    (license
    ;; Dual licensed under GPLv2 or GPLv3 (both without "or later")
     (list license:gpl2 license:gpl3))))

(define-public gnome-themes-standard
  (package
    (name "gnome-themes-standard")
    (version "3.22.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/" name "-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0smmiamrgcgf5sa88bsn8hwmvsyx4gczzs359nwxbkv14b2qgp31"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; Don't create 'icon-theme.cache'.
       (let* ((coreutils (assoc-ref %build-inputs "coreutils"))
              (true      (string-append coreutils "/bin/true")))
         (list (string-append "GTK_UPDATE_ICON_CACHE=" true)))))
    (inputs
     `(("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)
       ("glib" ,glib)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (home-page "https://launchpad.net/gnome-themes-standard")
    (synopsis "Default GNOME 3 themes")
    (description
     "The default GNOME 3 themes (Adwaita and some accessibility themes).")
    (license license:lgpl2.1+)))

(define-public seahorse
  (package
    (name "seahorse")
    (version "3.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/" name "-"
                           version ".tar.xz"))
       (sha256
        (base32
         "16sfnqrdlr5xx6kixx2ln1mva7nngjlw1k3f5n454vyaigffjh2v"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (inputs
     `(("gtk+" ,gtk+)
       ("gcr" ,gcr)
       ("gnupg" ,gnupg)
       ("gpgme" ,gpgme)
       ("openldap" ,openldap)
       ("openssh" ,openssh)
       ("avahi" ,avahi)
       ("libpwquality" ,libpwquality)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (home-page "https://wiki.gnome.org/Apps/Seahorse")
    (synopsis "Manage encryption keys and passwords in the GNOME keyring")
    (description
     "Seahorse is a GNOME application for managing encryption keys and
passwords in the GNOME keyring.")
    (license license:gpl2+)))

(define-public vala
  (package
    (name "vala")
    (version "0.48.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0lswkb7gj0chas9n3l3dbrm9l71hs77adhvm2v600id2ipi37pi8"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--enable-coverage")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/manual"
               (substitute* '("manual.xml" "version.xml.in")
                 (("http://www.oasis-open.org/docbook/xml/4.4/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             (setenv "CC" "gcc")
             (substitute* "valadoc/tests/testrunner.sh"
               (("export PKG_CONFIG_PATH=" m)
                (string-append m "$PKG_CONFIG_PATH:")))
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("bison" ,bison)
       ("dbus" ,dbus)
       ("docbook-xml" ,docbook-xml-4.4)
       ("docbook-xsl" ,docbook-xsl)
       ("flex" ,flex)
       ("gobject-introspection" ,gobject-introspection)
       ("help2man" ,help2man)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     `(("glib" ,glib)
       ("libgvc" ,graphviz)))
    (synopsis "Compiler using the GObject type system")
    (description "Vala is a programming language using modern high level
abstractions without imposing additional runtime requirements and without using
a different ABI compared to applications and libraries written in C.  Vala uses
the GObject type system and has additional code generation routines that make
targeting the GNOME stack simple.")
    (home-page "https://wiki.gnome.org/Projects/Vala/")
    (license license:lgpl2.1+)))

(define-public vala-0.48
  (package
    (inherit vala)
    (version "0.48.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/vala/"
                                  (version-major+minor version) "/"
                                  "vala-" version ".tar.xz"))
              (sha256
               (base32
                "0lswkb7gj0chas9n3l3dbrm9l71hs77adhvm2v600id2ipi37pi8"))))))

(define-public vte
  (package
    (name "vte")
    (version "0.60.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/vte/"
                                  (version-major+minor version) "/"
                                  "vte-" version ".tar.xz"))
              (sha256
               (base32
                "0al2v6fn061v4j1wwvppim1q283y2a6s0iyl29hxhmx3h48nxdzy"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Ddocs=true"
        "-D_systemd=false")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/reference"
               (substitute* "vte-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
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
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (inputs
     `(("fribidi" ,fribidi)
       ("gnutls" ,gnutls)
       ("gperf" ,gperf)
       ("icu-uc" ,icu4c)
       ("libpcre2" ,pcre2)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("glib" ,glib)
       ("pango" ,pango)
       ("gtk+" ,gtk+)))
    (synopsis "Virtual Terminal library")
    (description "VTE provides a virtual terminal widget for GTK applications.")
    (home-page "https://wiki.gnome.org/Apps/Terminal/VTE")
    (license
     (list
      ;; Documentation
      license:cc-by-sa4.0
      ;; Library
      license:lgpl3+
      ;; Others
      license:gpl3+))))

(define-public vte-ng
  (package
    (inherit vte)
    (name "vte-ng")
    (version "0.58.2.a")
    (home-page "https://github.com/thestinger/vte-ng")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rnm5c6m3abbm81jsfdas0y80z299ny54gr4syn4bfrms3s4g19l"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gtk-doc" ,gtk-doc)
       ,@(package-native-inputs vte)))
    (arguments
     `(#:configure-flags '("-Ddocs=true")))
  (synopsis "Enhanced VTE terminal widget")
  (description
   "VTE is a library (libvte) implementing a terminal emulator widget for
GTK+, this fork provides additional functions exposed for keyboard text
selection and URL hints.")))

;; Stable version for gtk2, required by gnurobots and lxterminal as of 2020-07.
(define-public vte/gtk+-2
  (package (inherit vte)
    (name "vte")
    (version "0.28.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1bmhahkf8wdsra9whd3k5l5z4rv7r58ksr8mshzajgq2ma0hpkw6"))
              (patches (search-patches
                         "vte-CVE-2012-2738-pt1.patch"
                         "vte-CVE-2012-2738-pt2.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-python")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib" ,glib "bin")))   ; for glib-genmarshal, etc.
    (propagated-inputs
     `(("gtk+" ,gtk+-2)         ; required by libvte.pc
       ("ncurses" ,ncurses))))) ; required by libvte.la

(define-public vinagre
  (package
    (name "vinagre")
    (version "3.22.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "vinagre-newer-freerdp.patch"
                                       "vinagre-newer-rdp-parameters.patch"))
              (sha256
               (base32
                "10jya3jyrm18nbw3v410gbkc7677bqamax44pzgd3j15randn76d"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     ;; Disable -Werror and such, to avoid build failures on compilation
     ;; warnings.
     '(#:configure-flags '("--enable-compile-warnings=minimum")
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'skip-gtk-update-icon-cache
           (lambda _
             ;; Don't create 'icon-theme.cache'
             (substitute* (find-files "." "^Makefile$")
               (("gtk-update-icon-cache") (which "true")))
             #t))
         (add-after 'unpack 'patch-configure
           (lambda _
             (substitute* "configure"
               (("freerdp") "freerdp2"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib-bin" ,glib "bin")))                 ;for glib-compile-schemas
    (inputs
     `(("libxml2" ,libxml2)
       ("gtk-vnc" ,gtk-vnc)
       ("gnome-keyring" ,gnome-keyring)
       ("libsecret" ,libsecret)
       ("freerdp" ,freerdp)
       ("spice" ,spice)
       ("spice-gtk" ,spice-gtk)
       ("telepathy-glib" ,telepathy-glib)
       ("vte" ,vte)))
    (home-page "https://wiki.gnome.org/Apps/Vinagre")
    (synopsis "Remote desktop viewer for GNOME")
    (description "Vinagre is a remote display client supporting the VNC, SPICE
and RDP protocols.")
    (license license:gpl3+)))

(define-public dconf
  (package
    (name "dconf")
    (version "0.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://gnome/sources/" name "/"
         (version-major+minor version) "/"
         name "-" version ".tar.xz"))
       (sha256
        (base32
         "0bfs069pjv6lhp7xrzmrhz3876ay2ryqxzc6mlva1hhz34ibprlz"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list
        "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-timeout-tests
           (lambda _
             (substitute* "tests/meson.build"
               (("^  \\['engine', .*$")
                ""))
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
     `(("bash-completion" ,bash-completion)
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin")
       ("gtk-doc" ,gtk-doc)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)
       ("vala" ,vala)))
    (inputs
     `(("dbus" ,dbus)
       ("gtk+" ,gtk+)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "Low-level GNOME configuration system")
    (description "Dconf is a low-level configuration system.  Its main purpose
is to provide a backend to GSettings on platforms that don't already have
configuration storage systems.")
    (home-page "https://wiki.gnome.org/Projects/dconf")
    (license license:lgpl2.1+)))

(define-public json-glib
  (package
    (name "json-glib")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0ixwyis47v5bkx6h8a1iqlw3638cxcv57ivxv4gw2gaig51my33j"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Ddocs=true"
        "-Dman=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "json-glib-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/")))
               (substitute* "meson.build"
                 (("http://docbook.sourceforge.net/release/xsl/current/")
                  (string-append (assoc-ref inputs "docbook-xsl")
                                 "/xml/xsl/docbook-xsl-1.79.1/"))))
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "Glib and GObject implementation of JSON")
    (description "JSON-GLib is a library providing serialization and
deserialization support for the JavaScript Object Notation (JSON) format
described by RFC 4627.  It implements a full JSON parser and generator using
GLib and GObject, and integrates JSON with GLib data types.")
    (home-page "https://wiki.gnome.org/Projects/JsonGlib")
    (license license:lgpl2.1+)))

(define-public libxklavier
  (package
    (name "libxklavier")
    (version "5.4")
    (source (origin
              ;; Note: There's no tarball at ftp.gnome.org for this version.
              (method git-fetch)
              (uri (git-reference
                    (url "https://anongit.freedesktop.org/git/libxklavier")
                    (commit (string-append "libxklavier-" version))))
              (sha256
               (base32
                "1w1x5mrgly2ldiw3q2r6y620zgd89gk7n90ja46775lhaswxzv7a"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-xkb-base="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb")
             "--disable-xmodmap-support")))
    (native-inputs
     `(("glib:bin"              ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config"            ,pkg-config)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("which" ,which)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (propagated-inputs
     ;; Required by libxklavier.pc.
     `(("glib"    ,glib)
       ("libxml2" ,libxml2)))
    (inputs
     `(("iso-codes"        ,iso-codes)
       ("libxi"            ,libxi)
       ("libxkbfile"       ,libxkbfile)
       ("xkbcomp"          ,xkbcomp)
       ("xkeyboard-config" ,xkeyboard-config)))
    (home-page "https://www.freedesktop.org/wiki/Software/LibXklavier/")
    (synopsis "High-level API for X Keyboard Extension")
    (description
     "LibXklavier is a library providing high-level API for X Keyboard
Extension known as XKB.  This library is intended to support XFree86 and other
commercial X servers.  It is useful for creating XKB-related software (layout
indicators etc).")
    (license license:lgpl2.0+)))

(define-public python2-rsvg
  ;; XXX: This is actually a subset of gnome-python-desktop.
  (package
    (name "python2-rsvg")
    (version "2.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://gnome/sources/gnome-python-desktop/2.32/gnome-python-desktop-"
             version ".tar.bz2"))
       (sha256
        (base32
         "1s8f9rns9v7qlwjv9qh9lr8crp88dpzfm45hj47zc3ivpy0dbnq9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python-2)
       ("python2-pygtk" ,python2-pygtk)
       ("librsvg" ,librsvg)))
    (home-page "https://www.gnome.org")
    (synopsis "Python bindings to librsvg")
    (description
     "This package provides Python bindings to librsvg, the SVG rendering
library.")

    ;; This is the license of the rsvg bindings.  The license of each module
    ;; of gnome-python-desktop is given in 'COPYING'.
    (license license:lgpl2.1+)))

(define-public glib-networking
  (package
    (name "glib-networking")
    (version "2.64.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/glib-networking/"
                       (version-major+minor version) "/"
                       "glib-networking-" version ".tar.xz"))
       (sha256
        (base32 "0s518l4bwvdvcp51lbjqcw8g0vq18bznpf5hq2zi6a054jqhcylk"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dopenssl=auto")))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gnutls" ,gnutls)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libproxy" ,libproxy)
       ("openssl" ,openssl)))
    (synopsis "Network extensions for GLib")
    (description "Glib-networking contains the implementations of certain GLib
networking features that cannot be implemented directly in GLib itself because
of their dependencies.  Currently it contains GnuTLS and OpenSSL-based
implementations of GTlsBackend, a libproxy-based implementation of
GProxyResolver, GLibproxyResolver, and a GNOME GProxyResolver that uses the
proxy information from the GSettings schemas in gsettings-desktop-schemas.")
    (home-page "https://wiki.gnome.org/Projects/GLib")
    (license license:lgpl2.1+)))

(define-public rest
  (package
    (name "rest")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/rest/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1j81bgqmd55s5lxyaxcplym9n6xywcs1cm9wmvafsg2xiv9sl4q5"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html")
        "--with-ca-certificates=/etc/ssl/certs/ca-certificates.crt")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference/rest"
               (substitute* "rest-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'disable-failing-tests
           (lambda _
             (substitute* "tests/Makefile.in"
               (("oauth\\$\\(EXEEXT\\) oauth-async\\$\\(EXEEXT\\) oauth2\\$\\(EXEEXT\\)")
                "")
               (("flickr\\$\\(EXEEXT\\) lastfm\\$\\(EXEEXT\\) xml\\$\\(EXEEXT\\)")
                "")
               (("XFAIL_TESTS = xml\\$\\(EXEEXT\\)")
                "XFAIL_TESTS ="))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)))
    (synopsis "Library for Representational State Transfer")
    (description "LibREST was designed to make it easier to access web services
that claim to be RESTful.  It includes convenience wrappers for libsoup and
libxml to ease remote use of the RESTful API.")
    (home-page "https://gitlab.gnome.org/GNOME/librest")
    (license license:lgpl2.1+)))

(define-public libsoup
  (package
    (name "libsoup")
    (version "2.70.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/libsoup/"
                       (version-major+minor version) "/"
                       "libsoup-" version ".tar.xz"))
       (sha256
        (base32 "0hjk9lgppc5435my0lyywbpmj7ib5vvcylwfin8ki97g9bvj1c2l"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* '("libsoup-2.4-docs.sgml"
                              "build-howto.xml" "client-howto.xml"
                              "request-howto.xml" "server-howto.xml"
                              "session-porting.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'unpack 'disable-failing-tests
           ;; These tests require network services.
           (lambda _
             (substitute* "tests/meson.build"
               (("\\['socket', true, \\[\\]\\],")
                ""))
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
     `(("curl" ,curl)
       ("docbook-xml" ,docbook-xml-4.1.2)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("httpd" ,httpd)
       ("intltool" ,intltool)
       ("php" ,php)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("vala" ,vala)))
    (inputs
     `(("krb5_config" ,mit-krb5)
       ("ntlm_auth" ,samba)))
    (propagated-inputs
     `(("brotli" ,google-brotli)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("libpsl" ,libpsl)
       ("libxml2" ,libxml2)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (synopsis "GLib-based HTTP Library")
    (description "LibSoup is an HTTP client/server library for GNOME.  It uses
GObjects and the GLib main loop, to integrate well with GNOME applications.")
    (home-page "https://wiki.gnome.org/Projects/libsoup")
    (license license:lgpl2.0+)))

;;; A minimal version of libsoup used to prevent a cycle with Inkscape.
(define-public libsoup-minimal
  (package
    (inherit libsoup)
    (name "libsoup-minimal")
    (outputs (delete "doc" (package-outputs libsoup)))
    (arguments
     (substitute-keyword-arguments (package-arguments libsoup)
       ((#:configure-flags configure-flags)
        `(delete "-Dgtk_doc=true" ,configure-flags))
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'move-doc)))))
    (native-inputs (alist-delete "gtk-doc" (package-native-inputs libsoup)))))

(define-public libsecret
  (package
    (name "libsecret")
    (version "0.20.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1r4habxdzmn02id324m0m4mg5isf22q1z436bg3vjjmcz1b3rjsg"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* "man/secret-tool.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.2/")
                  (string-append (assoc-ref inputs "docbook-xml-4.2")
                                 "/xml/dtd/docbook/")))
               (substitute* '("reference/libsecret/libsecret-docs.sgml"
                              "reference/libsecret/libsecret-examples.sgml"
                              "reference/libsecret/libsecret-using.sgml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml-4.1.2")
                                 "/xml/dtd/docbook/"))))))
         (replace 'check
           (lambda _
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "meson" "test")
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
     `(("dbus" ,dbus)
       ("docbook-xml-4.1.2" ,docbook-xml-4.1.2)
       ("docbook-xml-4.2" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("python-dbus" ,python-dbus)
       ("python-pygobject" ,python-pygobject)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("libxml2" ,libxml2)))
    (propagated-inputs
     `(("glib" ,glib)
       ("libgcrypt" ,libgcrypt)))
    (synopsis "GObject bindings for Secret Service API")
    (description "Libsecret is a library for storing and retrieving passwords
and other secrets.  It communicates with the Secret Service, using D-Bus.
Gnome-Keyring and Ksecretservice are both implementations of a Secret Service.")
    (home-page "https://wiki.gnome.org/Projects/Libsecret/")
    (license
     (list
      ;; Tests
      license:asl2.0
      license:gpl2+
      ;; Others
      license:lgpl2.1+))))

(define-public five-or-more
  (package
    (name "five-or-more")
    (version "3.32.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/five-or-more/"
                           (version-major+minor version) "/"
                           "five-or-more-" version ".tar.xz"))
       (sha256
        (base32 "19pf8wzbf3ciqf2k4bj9sddvyhckfd62x86pnqr6s8h4vn9jc6ii"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("appstream-glib" ,appstream-glib)
       ("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libgnome-games-support" ,libgnome-games-support)
       ("librsvg" ,librsvg)))
    (home-page "https://wiki.gnome.org/Apps/Five%20or%20more")
    (synopsis "Logic puzzle game")
    (description "Five or More is a game where you try to align
 five or more objects of the same color and shape causing them to disappear.
 On every turn more objects will appear, until the board is full.
 Try to last as long as possible.")
    (license license:gpl2+)))

(define-public gnome-mines
  (package
    (name "gnome-mines")
    (version "3.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1spxa6qr1y8s5rrsvpciywpvhk812ngn95s1apaxaigwy2g1iw54"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")       ; for glib-compile-resources
       ("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("vala" ,vala)
       ("yelp" ,yelp)
       ("appstream-glib" ,appstream-glib)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libgnome-games-support" ,libgnome-games-support)
       ("librsvg" ,librsvg)))
    (home-page "https://wiki.gnome.org/Apps/Mines")
    (synopsis "Minesweeper game")
    (description
     "Mines (previously gnomine) is a puzzle game where you locate mines
floating in an ocean using only your brain and a little bit of luck.")
    (license license:gpl2+)))

(define-public gnome-multi-writer
  (package
    (name "gnome-multi-writer")
    (version "3.35.90")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/gnome-multi-writer/"
                           (version-major+minor version) "/"
                           "gnome-multi-writer-" version ".tar.xz"))
       (sha256
        (base32
         "07vgzjjdrxcp7h73z13h9agafxb4vmqx5i81bcfyw0ilw9kkdzmp"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-post-install
           (lambda _
             (substitute* "meson.build"
               (("meson.add_install_script" &) (string-append "# " &)))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("glib" ,glib)                   ; for gio
       ("gusb" ,gusb)
       ("udisks" ,udisks)
       ("libgudev" ,libgudev)
       ("libcanberra" ,libcanberra)
       ("polkit" ,polkit)))
    (home-page "https://wiki.gnome.org/Apps/MultiWriter")
    (synopsis "Write to multiple USB devices at once")
    (description
     "MultiWriter can be used to write an ISO file to multiple USB devices at
once.")
    (license license:gpl2+)))

(define-public gnome-sudoku
  (package
    (name "gnome-sudoku")
    (version "3.34.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "025y85r4qqardivvwiwhbmgarziykdy224m8zlrq8b79zv82793b"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libgee" ,libgee)
       ("librsvg" ,librsvg)
       ("qqwing" ,qqwing)))
    (home-page "https://wiki.gnome.org/Apps/Sudoku")
    (synopsis "Japanese logic game")
    (description
     "Sudoku is a Japanese logic game that exploded in popularity in 2005.
GNOME Sudoku is meant to have an interface as simple and unobstrusive as
possible while still providing features that make playing difficult Sudoku
more fun.")
    (license license:gpl2+)))

(define-public gnome-terminal
  (package
    (name "gnome-terminal")
    (version "3.34.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0gc004f9b5k94gkdanmqjz3wqgnpny0l3nqm8zd19h4f0ps27mrv"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags
       (list "--disable-migration" "--disable-search-provider"
             "--without-nautilus-extension")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-/bin/true
                     (lambda _
                       (substitute* "configure"
                         (("/bin/true") (which "true"))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (inputs
     `(("gtk+" ,gtk+)
       ("vte" ,vte)
       ("gnutls" ,gnutls)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("util-linux" ,util-linux "lib")
       ("vala" ,vala)))
    (home-page "https://wiki.gnome.org/Apps/Terminal")
    (synopsis "Terminal emulator")
    (description
     "GNOME Terminal is a terminal emulator application for accessing a
UNIX shell environment which can be used to run programs available on
your system.

It supports several profiles, multiple tabs and implements several
keyboard shortcuts.")
    (license license:gpl3+)))

(define-public colord
  (package
    (name "colord")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/colord/releases/"
                           "colord-" version ".tar.xz"))
       (sha256
        (base32 "19f0938fr7nvvm3jr263dlknaq7md40zrac2npfyz25zc00yh3ws"))))
    (build-system meson-build-system)
    (arguments
     '(;; FIXME: One test fails:
       ;; /colord/icc-store (in lib/colord/colord-self-test-private):
       ;; Incorrect content type for /tmp/colord-vkve/already-exists.icc, got
       ;; application/x-zerosize
       #:tests? #f
       #:glib-or-gtk? #t
       #:configure-flags (list "-Dlocalstatedir=/var"
                               ;; No dep on systemd.
                               "-Dsystemd=false"
                               ;; Wants to install to global completion dir;
                               ;; punt.
                               "-Dbash_completion=false"
                               "-Ddaemon_user=colord"
                               "-Dsane=true"
                               "-Dvapi=true"
                               ;; Requires spotread.
                               "-Dargyllcms_sensor=false"
                               ;; TODO: Requires docbook2x.
                               "-Dman=false")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-build-system
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "rules/meson.build"
               (("udev.get_pkgconfig_variable\\('udevdir'\\)")
                (string-append "'" (assoc-ref outputs "out") "/lib/udev'")))
             #t))
         (add-before 'configure 'set-sqlite3-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             ;; "colormgr dump" works by invoking the "sqlite3" command.
             ;; Record its absolute file name.
             (let ((sqlite (assoc-ref inputs "sqlite")))
               (substitute* "client/cd-util.c"
                 (("\"sqlite3\"")
                  (string-append "\"" sqlite "/bin/sqlite3\"")))
               #t))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")         ; for glib-compile-resources, etc.
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (propagated-inputs
     ;; colord.pc refers to all these.
     `(("glib" ,glib)
       ("lcms" ,lcms)
       ("udev" ,eudev)))
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("gusb" ,gusb)
       ("libgudev" ,libgudev)
       ("libusb" ,libusb)
       ("polkit" ,polkit)
       ("python" ,python-wrapper)
       ("sqlite" ,sqlite)
       ("sane-backends" ,sane-backends)))
    (home-page "https://www.freedesktop.org/software/colord/")
    (synopsis "Color management service")
    (description "Colord is a system service that makes it easy to manage,
install and generate color profiles to accurately color manage input and
output devices.")
    (license license:gpl2+)))

(define-public geoclue
  (package
    (name "geoclue")
    (version "2.5.6")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.freedesktop.org/geoclue/geoclue.git")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13fk6n4j74lvcsrg3kwbw1mkxgcr3iy9dnysmy0pclfsym8z5m5m"))
       (patches (search-patches "geoclue-config.patch"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Ddbus-srv-user=geoclue")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* '("geoclue-docs.xml" "lib/libgeoclue-docs.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
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
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc", gtk-doc)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("avahi" ,avahi)
       ("glib-networking" ,glib-networking)
       ("json-glib" ,json-glib)
       ("libnotify" ,libnotify)
       ("libsoup" ,libsoup)
       ("modem-manager" ,modem-manager)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "Geoinformation Service")
    (description "Geoclue is a D-Bus geoinformation service.  The goal of the
Geoclue project is to make creating location-aware applications as simple as
possible.")
    (home-page "https://gitlab.freedesktop.org/geoclue/geoclue/-/wikis/home")
    (license license:gpl2+)))

(define-public geocode-glib
  (package
    (name "geocode-glib")
    (version "3.26.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/geocode-glib/"
                       (version-major+minor version) "/"
                       "geocode-glib-" version ".tar.xz"))
       (sha256
        (base32 "1l8g0f13xgkrk335afr9w8k46mziwb2jnyhl07jccl5yl37q9zh1"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Denable-installed-tests=false")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* "geocode-glib-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         ;; The tests require a bunch of locales.
         (add-before 'check 'set-locales
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "GUIX_LOCPATH"
                     (string-append (assoc-ref inputs "glibc-locales")
                                    "/lib/locale"))
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("glibc-locales" ,glibc-locales)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("json-glib" ,json-glib)
       ("libsoup" ,libsoup)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)))
    (synopsis "Geocoding and reverse-geocoding library")
    (description "Geocode-Glib is a convenience library for geocoding (finding
longitude, and latitude from an address) and reverse geocoding (finding an
address from coordinates) using the Nominatim service.  It also caches requests
for faster results and to avoid unnecessary server load.")
    (home-page "https://developer.gnome.org/geocode-glib/")
    (license license:lgpl2.0+)))

(define-public upower
  (package
    (name "upower")
    (version "0.99.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://upower.freedesktop.org/releases/"
                                  "upower-" version ".tar.xz"))
              (sha256
               (base32
                "1vxxvmz2cxb1qy6ibszaz5bskqdy9nd9fxspj9fv3gfmrjzzzdb4"))
              (patches (search-patches "upower-builddir.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Upstream commit
                  ;; <https://cgit.freedesktop.org/upower/commit/?id=18457c99b68786cd729b315723d680e6860d9cfa>
                  ;; moved 'dbus-1/system.d' from etc/ to share/.  However,
                  ;; 'dbus-configuration-directory' in (gnu services dbus)
                  ;; expects it in etc/.  Thus, move it back to its previous
                  ;; location.
                  (substitute* "src/Makefile.in"
                    (("^dbusconfdir =.*$")
                     "dbusconfdir = $(sysconfdir)/dbus-1/system.d\n"))
                  #t))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((umockdev (string-append (assoc-ref inputs "umockdev")
                                            "/lib")))
               (setenv "LD_LIBRARY_PATH" umockdev))
             #t)))
       #:configure-flags (list "--localstatedir=/var"
                               (string-append "--with-udevrulesdir="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("python" ,python)

       ;; For tests.
       ("python-dbus" ,python-dbus)
       ("python-dbusmock" ,python-dbusmock)
       ("python-pygobject" ,python-pygobject)
       ("umockdev" ,umockdev)

       ;; For man pages.
       ("libxslt" ,libxslt)                       ;for 'xsltproc'
       ("libxml2" ,libxml2)                       ;for 'XML_CATALOG_FILES'
       ("docbook-xsl" ,docbook-xsl)))
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("libgudev" ,libgudev)
       ("libusb" ,libusb)))
    (propagated-inputs
     ;; In Requires of upower-glib.pc.
     `(("glib" ,glib)))
    (home-page "https://upower.freedesktop.org/")
    (synopsis "System daemon for managing power devices")
    (description
     "UPower is an abstraction for enumerating power devices,
listening to device events and querying history and statistics.  Any
application or service on the system can access the org.freedesktop.UPower
service via the system message bus.")
    (license license:gpl2+)))

(define-public libgweather
  (package
    (name "libgweather")
    (version "3.36.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0l74hc02rvzm4p530y539a67jwb080fqdaazdl8j0fr3xvq0j9yy"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:tests? #f           ; Tests require networking
       #:configure-flags
       (list
        (string-append "-Dzoneinfo_dir="
                       (assoc-ref %build-inputs "tzdata")
                       "/share/zoneinfo")
        ;; NOTE: This is the API-Key for OpenWeatherMaps.
        ;; It has been generated from my OWM account.
        ;; Currently, the account subscription is on "Free Plan".
        ;; It provides 3 hour forecast for 5 days.
        ;; It allows 60 calls/minute and 1,000,000 calls/month.
        ;; Feel free to use it.
        ;; Raghav (RG) Gururajan <raghavgururajan@disroot.org>
        "-Dowm_apikey=9c052a3406aa129d5261cfb999104cb7"
        "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "libgweather-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("glade" ,glade)
       ("tzdata" ,tzdata)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("geocode-glib" ,geocode-glib)
       ("gtk+" ,gtk+)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)))
    (synopsis "Weather information library and database")
    (description "LibGWeather is a library to access weather information from
online services for numerous locations.")
    (home-page "https://wiki.gnome.org/Projects/LibGWeather")
    (license license:gpl2+)))

(define-public gnome-settings-daemon
  (package
    (name "gnome-settings-daemon")
    (version "3.34.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "07y1gbicz0pbxmdgwrdzyc4byy30wfwpbqgvnx27gnpqmc5s50cr"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list (string-append "-Dudev_dir="
                            (assoc-ref %outputs "out")
                            "/lib/udev/rules.d/")
             "-Dsystemd=false"
             ;; Otherwise, the RUNPATH will lack the final path component.
             (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/gnome-settings-daemon-3.0"))
       ;; Color management test can't reach the colord system service.
       #:tests? #f))
    (native-inputs
     `(("glib:bin" ,glib "bin")     ; for glib-mkenums
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("xsltproc" ,libxslt)
       ("libxml2" ,libxml2)                       ;for XML_CATALOG_FILES
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("colord" ,colord)
       ("libgudev" ,libgudev)
       ("upower" ,upower)
       ("polkit" ,polkit)
       ("pulseaudio" ,pulseaudio)
       ("libcanberra" ,libcanberra)
       ("libx11" ,libx11)
       ("libxtst" ,libxtst)
       ("lcms" ,lcms)
       ("libnotify" ,libnotify)
       ("geoclue" ,geoclue)
       ("geocode-glib" ,geocode-glib)
       ("libgweather" ,libgweather)
       ("gnome-desktop" ,gnome-desktop)
       ("nss" ,nss)
       ("cups" ,cups)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libwacom" ,libwacom)
       ("librsvg" ,librsvg)
       ("xf86-input-wacom" ,xf86-input-wacom)
       ("wayland" ,wayland)
       ("network-manager" ,network-manager)
       ("gcr" ,gcr)
       ("modem-manager" ,modem-manager)))
    (home-page "https://www.gnome.org")
    (synopsis "GNOME settings daemon")
    (description
     "This package contains the daemon responsible for setting the various
parameters of a GNOME session and the applications that run under it.  It
handles settings such keyboard layout, shortcuts, and accessibility, clipboard
settings, themes, mouse settings, and startup of other daemons.")
    (license license:gpl2+)))

(define-public totem-pl-parser
  (package
    (name "totem-pl-parser")
    (version "3.26.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/totem-pl-parser/"
                       (version-major+minor version) "/"
                       "totem-pl-parser-" version ".tar.xz"))
       (sha256
        (base32 "132jihnf51zs98yjkc6jxyqib4f3dawpjm17g4bj4j78y93dww2k"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:tests? #f           ; Tests require networking
       #:configure-flags
       (list
        "-Denable-gtk-doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* "totem-pl-parser-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("intltool" ,intltool)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("libquvi" ,libquvi)
       ("libsoup" ,libsoup)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("libarchive" ,libarchive)
       ("libxml2" ,libxml2)))
    (synopsis "Totem Playlist Parser")
    (description "Totem-pl-parser is a simple GObject-based library to parse a
host of playlist formats, as well as save those.")
    (home-page "http://wiki.gnome.org/Apps/Videos")
    (license license:lgpl2.0+)))

(define-public aisleriot
  (package
    (name "aisleriot")
    (version "3.22.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/aisleriot/"
                                  (version-major+minor version) "/"
                                  "aisleriot-" version ".tar.xz"))
              (sha256
               (base32
                "0yzdh9cw5cjjgvfh75bihl968czlgfmpmn1z0fdk88sgvpjgzwji"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags
       '("--with-platform=gtk-only"
         "--with-card-theme-formats=svg")))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gtk+" ,gtk+)
       ("guile" ,guile-2.2)
       ("libcanberra" ,libcanberra)
       ("librsvg" ,librsvg)))
    (home-page "https://wiki.gnome.org/Apps/Aisleriot")
    (synopsis "Solitaire card games")
    (description
     "Aisleriot (also known as Solitaire or sol) is a collection of card games
which are easy to play with the aid of a mouse.")
    (license license:gpl3+)))

(define-public amtk
  (package
    (name "amtk")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/amtk/"
                       (version-major+minor version) "/"
                       "amtk-" version ".tar.xz"))
       (sha256
        (base32 "1wax6mim8dj0m21k8ima7ysm3bzzp54r00jganwbzakq8bfnnrgr"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* '("amtk-docs.xml.in"
                              "amtk-intro.xml.in")
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)))
    (synopsis "Actions, Menus and Toolbars Kit for GTK+ applications")
    (description "Amtk is the acronym for @acronym{Amtk, Actions Menus and
Toolbars Kit}.  It is a basic GtkUIManager replacement based on GAction.  It is
suitable for both a traditional UI or a modern UI with a GtkHeaderBar.")
    (home-page "https://wiki.gnome.org/Projects/Amtk")
    (license license:lgpl2.1+)))

(define-public devhelp
  (package
    (name "devhelp")
    (version "3.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0zpmn6fgkgiayvn4diia5df0s6s7dqrdnp3nrvpavsmgn0vhb4pg"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin") ; for glib-mkmenus
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("amtk" ,amtk)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("webkitgtk" ,webkitgtk)))
    (home-page "https://wiki.gnome.org/Apps/Devhelp")
    (synopsis "API documentation browser for GNOME")
    (description
     "Devhelp is an API documentation browser for GTK+ and GNOME.  It works
natively with GTK-Doc (the API reference system developed for GTK+ and used
throughout GNOME for API documentation).")
    (license license:gpl2+)))

(define-public cogl
  (package
    (name "cogl")
    (version "1.22.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/cogl/"
                       (version-major+minor version) "/"
                       "cogl-" version ".tar.xz"))
       (sha256
        (base32 "0nfph4ai60ncdx7hy6hl1i1cmp761jgnyjfhagzi0iqq36qb41d8"))))
    ;; NOTE: mutter exports a bundled fork of cogl, so when making changes to
    ;; cogl, corresponding changes may be appropriate in mutter as well.
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("libdrm" ,libdrm)))
    (propagated-inputs
     `(("cairo" ,cairo)
       ("glib" ,glib)
       ("gdk-pixbuf+svg" ,gdk-pixbuf+svg)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxdamage" ,libxdamage)
       ("libxcomposite" ,libxcomposite)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("pango" ,pango)
       ("wayland" ,wayland)))
    (arguments
     `(#:disallowed-references (,xorg-server-for-tests)
       #:configure-flags (list "--enable-cogl-gst"
                               "--enable-wayland-egl-platform"
                               "--enable-wayland-egl-server"
                               "--enable-gtk-doc"

                               (string-append "--with-html-dir="
                                (assoc-ref %outputs "doc")
                                "/share/gtk-doc/html")
                               ;; Arrange to pass an absolute file name to
                               ;; dlopen for libGL.so.
                               (string-append "--with-gl-libname="
                                              (assoc-ref %build-inputs "mesa")
                                              "/lib/libGL.so"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((xmldoc (string-append (assoc-ref inputs "docbook-xml")
                                           "/xml/dtd/docbook")))
               (with-directory-excursion "doc/reference"
                 (substitute*
                     '("cogl/cogl-docs.xml.in"
                       "cogl/blend-strings.xml"
                       "cogl-gst/cogl-gst-docs.xml.in"
                       "cogl-2.0-experimental/cogl-2.0-experimental-docs.xml.in"
                       "cogl-2.0-experimental/blend-strings.xml")
                   (("http://.*/docbookx\\.dtd")
                    (string-append xmldoc "/docbookx.dtd"))))
               #t)))
         (add-before 'check 'start-xorg-server
                     (lambda* (#:key tests? inputs #:allow-other-keys)
                       (if tests?
                           (begin
                             ;; The test suite requires a running X server.
                             (system (format #f "~a/bin/Xvfb :1 +extension GLX &"
                                             (assoc-ref inputs "xorg-server")))
                             (setenv "DISPLAY" ":1")
                             #t)
                           (format #t "test suite not run~%"))
                       #t)))))
    (synopsis "Hardware accelerated 3D graphics API")
    (description "Cogl is a small library for using 3D graphics hardware for
rendering.  The API departs from the flat state machine style of OpenGL and is
designed to make it easy to write orthogonal components that can render without
stepping on each others toes.")
    (home-page "https://www.cogl3d.org")
    (license
     (list
      license:expat                     ; most of the code
      license:bsd-3                     ; cogl/cogl-point-in-poly.c
      license:sgifreeb2.0               ; cogl-path/tesselator/
      license:asl2.0))))  ; examples/android/

(define-public clutter
  (package
    (name "clutter")
    (version "1.26.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1rn4cd1an6a9dfda884aqpcwcgq8dgydpqvb19nmagw4b70zlj4b"))))
    ;; NOTE: mutter exports a bundled fork of clutter, so when making changes
    ;; to clutter, corresponding changes may be appropriate in mutter as well.
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t    ; To wrap binaries and/or compile schemas.
       #:configure-flags
       (list
        "-Dwayland_compositor=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((xmldoc (string-append (assoc-ref inputs "docbook-xml")
                                           "/xml/dtd/docbook")))
               (substitute* "doc/reference/clutter-docs.xml"
                 (("http://.*/docbookx\\.dtd")
                  (string-append xmldoc "/docbookx.dtd")))
               #t)))
         (add-after 'unpack 'patch-cookbook-examples
           (lambda _
             (substitute* "doc/cookbook/meson.build"
               (("subdir\\('examples'\\)")
                ""))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-before 'check 'pre-check
           (lambda* (#:key tests? inputs #:allow-other-keys)
             (if tests?
                 (begin
                   ;; The test suite requires a running X server.
                   (system (format #f "~a/bin/Xvfb :1 +extension GLX &"
                                   (assoc-ref inputs "xorg-server")))
                   (setenv "DISPLAY" ":1")
                   #t)
                 (format #t "test suite not run~%"))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("libxslt" ,libxslt)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("ruby" ,ruby)
       ("xorg-server" ,xorg-server-for-tests)))
    (propagated-inputs
     `(("atk" ,atk)
       ("cogl" ,cogl)
       ("cairo" ,cairo)
       ("eudev" ,eudev)
       ("gdk-pixbuf+svg" ,gdk-pixbuf+svg)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libgudev" ,libgudev)
       ("libinput" ,libinput)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxkbcommon" ,libxkbcommon)
       ("pango" ,pango)
       ("wayland" ,wayland)))
    (synopsis "OpenGL-based interactive canvas library")
    (description "Clutter is an OpenGL-based interactive canvas library,
designed for creating fast, mainly 2D single window applications such as media
box UIs, presentations, kiosk style applications and so on.")
    (home-page "https://www.clutter-project.org")
    (license license:lgpl2.1+)))

(define-public clutter-gtk
  (package
    (name "clutter-gtk")
    (version "1.8.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "01ibniy4ich0fgpam53q252idm7f4fn5xg5qvizcfww90gn9652j"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t    ; To wrap binaries and/or compile schemas.
       #:configure-flags
       (list
        "-Denable_docs=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((xmldoc (string-append (assoc-ref inputs "docbook-xml")
                                           "/xml/dtd/docbook")))
               (substitute* "doc/clutter-gtk-1.0-docs.xml"
                 (("http://.*/docbookx\\.dtd")
                  (string-append xmldoc "/docbookx.dtd")))
               #t)))
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("clutter" ,clutter)
       ("gtk+" ,gtk+)))
    (synopsis "GTK+ integration library for Clutter")
    (description "Clutter-GTK is a library providing facilities to integrate
Clutter into GTK+ applications and vice versa.  It provides a GTK+ widget,
GtkClutterEmbed, for embedding the a Clutter stage into any GtkContainer; and
GtkClutterActor, a Clutter actor for embedding any GtkWidget inside a Clutter
stage.")
    (home-page "https://www.clutter-project.org")
    (license license:lgpl2.1+)))

(define-public clutter-gst
  (package
    (name "clutter-gst")
    (version "3.0.27")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/clutter-gst/"
                       (version-major+minor version) "/"
                       "clutter-gst-" version ".tar.xz"))
       (sha256
        (base32 "17czmpl92dzi4h3rn5rishk015yi3jwiw29zv8qan94xcmnbssgy"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--enable-gtk-doc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((xmldoc (string-append (assoc-ref inputs "docbook-xml")
                                           "/xml/dtd/docbook")))
               (substitute* "doc/reference/clutter-gst-docs.xml"
                 (("http://.*/docbookx\\.dtd")
                  (string-append xmldoc "/docbookx.dtd")))
               #t)))
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
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)))
    (propagated-inputs
     `(("clutter" ,clutter)
       ("cogl" ,cogl)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (synopsis "GStreamer integration library for Clutter")
    (description "Clutter-Gst is an integration library for using GStreamer with
Clutter.  It provides a GStreamer sink to upload frames to GL and an actor that
implements the ClutterGstPlayer interface using playbin.")
    (home-page "https://www.clutter-project.org")
    (license license:lgpl2.1+)))

(define-public libchamplain
  (package
    (name "libchamplain")
    (version "0.12.20")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://gnome/sources/libchamplain/0.12/libchamplain-"
         version ".tar.xz"))
       (sha256
        (base32 "0rihpb0npqpihqcdz4w03rq6xl7jdckfqskvv9diq2hkrnzv8ch2"))
       (patches
        (search-patches
         ;; To fix the upstream bug,
         ;; https://gitlab.gnome.org/GNOME/libchamplain/-/issues/55
         "libchamplain-memphis-demos.patch"))))
    (build-system meson-build-system)
    (outputs '("out" "demos" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dmemphis=true"
        "-Dgtk_doc=true"
        "-Ddemos=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* "champlain-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'enable-demos
           (lambda _
             (with-directory-excursion "demos"
               (substitute* "meson.build"
                 (("install: false,")
                  "install: true,")))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'move-demos
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (demos (assoc-ref outputs "demos")))
               (mkdir-p (string-append demos "/bin"))
               (rename-file
                (string-append out "/bin")
                (string-append demos "/bin"))
               #t))))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gjs" ,gjs)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("vapigen" ,vala)))
    (propagated-inputs
     `(("cairo" ,cairo)
       ("clutter" ,clutter)
       ("clutter-gtk" ,clutter-gtk)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libsoup" ,libsoup)
       ("memphis" ,memphis)
       ("sqlite" ,sqlite)))
    (synopsis "Map Widget")
    (description "LibChamplain is a Gtk+ widget displaying zoomable and pannable
maps that can be loaded from various network sources.  It supports overlay
layers, markers, and custom elements displayed on top of the maps.  The library
is written in C but other language mappings are also available thanks to
GObject-Introspection.")
    (home-page "https://https://wiki.gnome.org/Projects/libchamplain")
    (license license:lgpl2.1+)))

(define-public gom
  (package
    (name "gom")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/gom/"
                       (version-major+minor version) "/"
                       "gom-" version ".tar.xz"))
       (sha256
        (base32 "17ca07hpg7dqxjn0jpqim3xqcmplk2a87wbwrrlq3dd3m8381l38"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Denable-gtk-doc=true"
        (string-append "-Dpygobject-override-dir="
                       (assoc-ref %outputs "out")
                       "/lib/python"
                       ,(version-major+minor
                         (package-version python))
                       "/site-packages/gi/overrides"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "gom-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gjs" ,gjs)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("sqlite" ,sqlite)))
    (synopsis "GObject Data Mapper")
    (description "Gom provides an object mapper from GObjects to SQLite.  It
helps you write applications that need to store structured data as well as make
complex queries upon that data.")
    (home-page "https://wiki.gnome.org/Projects/Gom")
    (license license:lgpl2.1+)))

(define-public libgnome-games-support
  (package
    (name "libgnome-games-support")
    (version "1.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libgnome-games-support/"
                                  (version-major+minor version) "/"
                                  "libgnome-games-support-" version ".tar.xz"))
              (sha256
               (base32
                "1zkbmnrn161p74qg6jhsn9f66yjjzxfm13pl1klv9av8k1bax9pq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a writable HOME.
             (setenv "HOME" (getcwd))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (propagated-inputs
     ;; Required by libgnome-games-support-1.0.pc
     `(("gtk+" ,gtk+)
       ("libgee" ,libgee)))
    (home-page "https://www.gnome.org/")
    (synopsis "Useful functionality shared among GNOME games")
    (description
     "libgnome-games-support is a small library intended for internal use by
GNOME Games, but it may be used by others.")
    (license license:lgpl3+)))

(define-public gnome-klotski
  (package
    (name "gnome-klotski")
    (version "3.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0sbx0bzy32lh2c9jp8v7gz788wn9y1la8mr5a7gf7370szsl4d4f"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t)))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libgnome-games-support" ,libgnome-games-support)
       ("librsvg" ,librsvg)))
    (home-page "https://wiki.gnome.org/Apps/Klotski")
    (synopsis "Sliding block puzzles")
    (description
     "GNOME Klotski is a set of block sliding puzzles.  The objective is to move
the patterned block to the area bordered by green markers.  To do so, you will
need to slide other blocks out of the way.  Complete each puzzle in as few moves
as possible!")
    (license license:gpl2+)))

(define-public grilo
  (package
    (name "grilo")
    (version "0.3.12")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0w8sq5g6g1rg85h53vbll8va70fcp6082mlpmy98aa03444ddyyv"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/grilo"
               (substitute* '("environment-setup.xml"
                              "grilo-docs.sgml" "overview.xml"
                              "plugins-sources.xml" "plugins-testing.xml"
                              "quick-start.xml" "writing-apps.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("vapigen" ,vala)))
    (inputs
     `(("gtk+" ,gtk+)
       ("liboauth" ,liboauth)
       ("libsoup" ,libsoup)
       ("totem-pl-parser" ,totem-pl-parser)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("libxml2" ,libxml2)))
    (native-search-paths
     (list
      (search-path-specification
       (variable "GRL_PLUGIN_PATH")
       (files
        (list
         (string-append "lib/grilo-"
                        (version-major+minor version)))))))
    ;; To load grilo-plugins.
    (search-paths native-search-paths)
    (synopsis "Media Framework for GNOME")
    (description "Grilo is a framework for browsing and searching media content
from various sources using a single API.")
    (home-page "https://wiki.gnome.org/Projects/Grilo")
    (license license:lgpl2.1+)))

(define-public grilo-plugins
  (package
    (name "grilo-plugins")
    (version "0.3.11")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0wyd3n5mn7b77hxylkc3f62v01mlavh96901pz342hwrn42ydqnx"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "tests/meson.build"
               (("'chromaprint',")
                ""))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("itstool" ,itstool)
       ("lua" ,lua)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("avahi" ,avahi)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gmime" ,gmime)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("gom" ,gom)
       ("gperf" ,gperf)
       ("grilo" ,grilo)
       ("gssdp" ,gssdp)
       ("gstreamer" ,gstreamer)
       ("gupnp" ,gupnp)
       ("gupnp-av" ,gupnp-av)
       ("json-glib" ,json-glib)
       ("libarchive" ,libarchive)
       ("libdmapsharing" ,libdmapsharing)
       ("libgdata" ,libgdata)
       ("libmediaart" ,libmediaart)
       ("librest" ,rest)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("oauth" ,liboauth)
       ("sqlite" ,sqlite)
       ("totam-pl-parser" ,totem-pl-parser)
       ("tracker" ,tracker)))
    (synopsis "Plugins for Grilo")
    (description "Grilo-Plugins is a collection of plugins for Grilo implementing
Grilo's API for various multimedia content providers.")
    (home-page "https://wiki.gnome.org/Projects/Grilo")
    (license license:lgpl2.1+)))

(define-public totem
  (package
    (name "totem")
    (version "3.34.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/totem/"
                           (version-major+minor version) "/"
                           "totem-" version ".tar.xz"))
       (sha256
        (base32
         "028sc6xbyi7rs884862d8f3di6zhcm0lhvlpc3r69ifzjsq9my3b"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin")                   ;for 'glib-mkenums'
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (inputs
     `(("gtk+" ,gtk+)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("atk" ,atk)
       ("cairo" ,cairo)
       ("dbus-glib" ,dbus-glib)
       ("clutter" ,clutter)
       ("clutter-gtk" ,clutter-gtk)
       ("clutter-gst" ,clutter-gst)
       ("xorgproto" ,xorgproto)
       ("libxxf86vm" ,libxxf86vm)
       ("libxtst" ,libxtst)
       ("libxrandr" ,libxrandr)
       ("libxml2" ,libxml2)
       ("libsoup" ,libsoup)
       ("libpeas" ,libpeas)
       ("librsvg" ,librsvg)
       ("lirc" ,lirc)
       ("gnome-desktop" ,gnome-desktop)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("adwaita-icon-theme" ,adwaita-icon-theme)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)
       ("totem-pl-parser" ,totem-pl-parser)
       ("grilo" ,grilo)
       ("grilo-plugins" ,grilo-plugins)
       ("vala" ,vala)))
    (arguments
     `(#:glib-or-gtk? #t

       ;; Disable automatic GStreamer plugin installation via PackageKit and
       ;; all that.
       #:configure-flags '("-D" "enable-easy-codec-installation=no"

                           ;; Do not build .a files for the plugins, it's
                           ;; completely useless.  This saves 2 MiB.
                           "--default-library" "shared")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t))
         (add-before
          'install 'disable-cache-generation
          (lambda _
            (setenv "DESTDIR" "/")
            #t))
         (add-before
          'check 'pre-check
          (lambda _
            ;; Tests require a running X server.
            (system "Xvfb :1 &")
            (setenv "DISPLAY" ":1")
            #t))
         (add-after
          'install 'wrap-totem
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out             (assoc-ref outputs "out"))
                  (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH"))
                  (grl-plugin-path (getenv "GRL_PLUGIN_PATH")))
              (wrap-program (string-append out "/bin/totem")
                `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                `("GRL_PLUGIN_PATH"        ":" prefix (,grl-plugin-path)))
              (wrap-program (string-append out "/bin/totem-video-thumbnailer")
                `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
            #t)))))
    (home-page "https://wiki.gnome.org/Apps/Videos")
    (synopsis "Simple media player for GNOME based on GStreamer")
    (description "Totem is a simple yet featureful media player for GNOME
which can read a large number of file formats.")
    ;; GPL2+ with an exception clause for non-GPL compatible GStreamer plugins
    ;; to be used and distributed together with GStreamer and Totem.  See
    ;; file://COPYING in the source distribution for details.
    (license license:gpl2+)))

(define-public rhythmbox
 (package
   (name "rhythmbox")
   (version "3.4.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/rhythmbox/"
                                (version-major+minor version) "/"
                                "rhythmbox-" version ".tar.xz"))
            (sha256
             (base32
              "142xcvw4l19jyr5i72nbnrihs953pvrrzcbijjn9dxmxszbv03pf"))))
   (build-system glib-or-gtk-build-system)
   (arguments
    `(#:configure-flags
      (list "--enable-lirc"
            "--enable-python"
            "--enable-vala"
            "--with-brasero"
            "--with-gudev"
            "--with-libsecret")
      #:phases
      (modify-phases %standard-phases
        (add-after
         'install 'wrap-rhythmbox
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((out               (assoc-ref outputs "out"))
                 (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                 (gst-plugin-path   (getenv "GST_PLUGIN_SYSTEM_PATH"))
                 (grl-plugin-path   (getenv "GRL_PLUGIN_PATH"))
                 (python-path       (getenv "PYTHONPATH")))
             (wrap-program (string-append out "/bin/rhythmbox")
               `("GI_TYPELIB_PATH"        ":" prefix (,gi-typelib-path))
               `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
               `("GRL_PLUGIN_PATH"        ":" prefix (,grl-plugin-path))
               `("PYTHONPATH"             ":" prefix (,python-path))))
           #t)))))
   (propagated-inputs
    `(("dconf" ,dconf)))
   (native-inputs
    `(("itstool" ,itstool)
      ("intltool" ,intltool)
      ("glib" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("desktop-file-utils" ,desktop-file-utils)
      ("pkg-config" ,pkg-config)
      ("xmllint" ,libxml2)))
   (inputs
    `(("json-glib" ,json-glib)
      ("tdb" ,tdb)
      ("gnome-desktop" ,gnome-desktop)
      ("python" ,python)
      ("python-pygobject" ,python2-pygobject)
      ("vala" ,vala)
      ("gmime" ,gmime)
      ("adwaita-icon-theme" ,adwaita-icon-theme)
      ("grilo" ,grilo)
      ("grilo-plugins" ,grilo-plugins)
      ("gstreamer" ,gstreamer)
      ("gst-plugins-base" ,gst-plugins-base)
      ("gst-plugins-good" ,gst-plugins-good)
      ("totem-pl-parser" ,totem-pl-parser)
      ("libgudev" ,libgudev)
      ;;("libmtp" ,libmtp) FIXME: Not detected
      ("libsecret" ,libsecret)
      ("libsoup" ,libsoup)
      ("libnotify" ,libnotify)
      ("libpeas" ,libpeas)
      ("lirc" ,lirc)
      ;; TODO: clutter* only used by visualizer plugin, which also requires mx
      ;;("clutter" ,clutter)
      ;;("clutter-gtk" ,clutter-gtk)
      ;;("clutter-gst" ,clutter-gst)
      ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
      ("atk" ,atk)
      ("pango" ,pango)
      ("gtk+" ,gtk+)
      ;; TODO:
      ;;  * libgpod
      ;;  * mx
      ("brasero" ,brasero)))
   (home-page "https://wiki.gnome.org/Apps/Rhythmbox")
   (synopsis "Music player for GNOME")
   (description "Rhythmbox is a music playing application for GNOME.  It
supports playlists, song ratings, and any codecs installed through gstreamer.")
   (license license:gpl2+)))

(define-public eog
  (package
    (name "eog")
    (version "3.36.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1p1lrnsgk5iyw7h02qzax4s74dqqsh5lk85b0qsj7hwx91qm61xp"))))
    (build-system meson-build-system)
    (outputs '("out" "help" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       ;; Otherwise, the RUNPATH will lack the final 'eog' path component.
       (list
        "-Dgtk_doc=true"
        (string-append "-Dc_link_args=-Wl,-rpath="
                       (assoc-ref %outputs "out")
                       "/lib/eog"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/reference"
               (substitute* '("eog-docs.xml" "eog-docs.xml.in")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache")
                "true"))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/help")
                (string-append help "/share/help"))
               #t)))
         (add-after 'move-help 'wrap-eog
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/eog")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("exempi" ,exempi)
       ("gconf" ,gconf)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)
       ("gnome-desktop" ,gnome-desktop)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("lcms" ,lcms)
       ("libart" ,libart-lgpl)
       ("libexif" ,libexif)
       ("libjpeg" ,libjpeg-turbo)
       ("libpeas" ,libpeas)
       ("librsvg" ,librsvg-next)
       ("shared-mime-info" ,shared-mime-info)
       ("x11" ,libx11)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("gtk+" ,gtk+)))
    (synopsis "Eye of GNOME")
    (description "EoG is the GNOME image viewer.")
    (home-page "https://wiki.gnome.org/Apps/EyeOfGnome")
    (license license:gpl2+)))

(define-public eog-plugins
  ;; Note: EOG looks for its plugins (via libpeas) in ~/.local as well as
  ;; $DATA/lib/eog/plugins, where DATA is one of the entries in
  ;; $XDG_DATA_DIRS.  Thus, for EOG to find these, you have to have
  ;; 'XDG_DATA_DIRS' appropriately set.
  (package
    (name "eog-plugins")
    (version "3.26.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/eog-plugins/"
                                  (version-major+minor version) "/"
                                  "eog-plugins-" version ".tar.xz"))
              (sha256
               (base32
                "0pd7fqa4ciy5844k5s1c6rlsqkk8pxd8cchxjcjhxlsngm9lynnx"))))
    (build-system gnu-build-system)
    (home-page "https://wiki.gnome.org/Apps/EyeOfGnome/Plugins")
    (synopsis "Extensions for the Eye of GNOME image viewer")
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("eog" ,eog)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libpeas" ,libpeas)
       ("libexif" ,libexif)
       ("libchamplain" ,libchamplain)))
    (description
     "This package provides plugins for the Eye of GNOME (EOG) image viewer,
notably:

@itemize
@item @dfn{EXIF Display}, which displays camera (EXIF) information;
@item @dfn{Map}, which displays a map of where the picture was taken on the
side panel;
@item @dfn{Slideshow Shuffle}, to shuffle images in slideshow mode.
@end itemize\n")

    ;; XXX: eog-postasa-plugin-resources.c (which we don't build) contains a
    ;; long suspicious byte stream that goes to a
    ;; ".gresource.eog_postasa_plugin" ELF section.
    (license license:gpl2+)))

(define-public libgudev
  (package
    (name "libgudev")
    (version "233")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "00xvva04lgqamhnf277lg32phjn971wgpc9cxvgf5x13xdq4jz2q"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--enable-gtk-doc"
        "--disable-umockdev"            ; Due to circular-dependency
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* "gudev-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("udev" ,eudev)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "GObject bindings for libudev")
    (description "LibGudev is a library that provides GObject bindings for
libudev.  It was originally part of udev-extras, then udev, then systemd.
It's now a project on its own.")
    (home-page "https://wiki.gnome.org/Projects/libgudev")
    (license license:lgpl2.1+)))

(define-public gvfs
  (package
    (name "gvfs")
    (version "1.44.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/gvfs/"
                       (version-major+minor version) "/"
                       "gvfs-" version ".tar.xz"))
       (sha256
        (base32 "0ipv0jgzim6glsgizmfjkx0m3gd1l9lr242m0jj6cdmhs52k5vsh"))
       ;; This patch may be removed when upgrading to version 1.46.x.
       (patches
        (search-patches "gvfs-add-support-for-libplist-2.2.patch"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:configure-flags
       (list
        "-Dsystemduserunitdir=no"
        "-Dtmpfilesdir=no"
        "-Dman=true"
        ;; Otherwise, the RUNPATH will lack the final path component.
        (string-append "-Dc_link_args=-Wl,-rpath="
                       (assoc-ref %outputs "out") "/lib/gvfs"))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk-doc" ,gtk-doc)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("avahi" ,avahi)
       ("dbus" ,dbus)
       ("elogind" ,elogind)
       ("fuse" ,fuse)
       ("gcr" ,gcr)
       ("glib" ,glib)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libarchive" ,libarchive)
       ("libbluray" ,libbluray)
       ("libcap" ,libcap)
       ("libcdio-paranoia" ,libcdio-paranoia)
       ("libgcrypt" ,libgcrypt)
       ("libgdata" ,libgdata)
       ("libgphoto2" ,libgphoto2)
       ("libgudev" ,libgudev)
       ("libimobiledevice" ,libimobiledevice)
       ("libmtp" ,libmtp)
       ("libnfs" ,libnfs)
       ("libplist" ,libplist)
       ("libsecret" ,libsecret)
       ("libsmbclient" ,samba)
       ("libsoup" ,libsoup)
       ("libusb" ,libusb)
       ("libxml2" ,libxml2)
       ("openssh" ,openssh)
       ("polkit" ,polkit)
       ("udisks" ,udisks)))
    (synopsis "Virtual filesystem implementation for GIO")
    (description "GVfs is a userspace virtual filesystem implementation for GIO
(a library available in GLib).  GVfs comes with a set of backends, including
trash support, SFTP, SMB, HTTP, DAV, and many others.  GVfs also contains
modules for GIO that implement volume monitors and persistent metadata storage.
There is also FUSE support that provides limited access to the GVfs filesystems
for applications not using GIO.")
    (home-page "https://wiki.gnome.org/gvfs/")
    (license license:lgpl2.0+)))

(define-public gusb
  (package
    (name "gusb")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/hughsie/libgusb")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "002pg0p4qzzk5dkyiynm483ir26zxrn4k71c7f6j85mfsdzbgli7"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("gtk-doc" ,gtk-doc)))
    (propagated-inputs
     ;; Both of these are required by gusb.pc.
     `(("glib" ,glib)
       ("libusb" ,libusb)))
    (arguments
     `(#:tests? #f)) ;libusb fails to initialize.  Wonder what that is.
    (home-page "https://github.com/hughsie/libgusb")
    (synopsis "GLib binding for libusb1")
    (description
     "GUsb is a GObject wrapper for libusb1 that makes it easy to do
asynchronous control, bulk and interrupt transfers with proper cancellation
and integration into a mainloop.  This makes it easy to integrate low level
USB transfers with your high-level application or system daemon.")
    (license license:lgpl2.1+)))

(define-public simple-scan
  (package
    (name "simple-scan")
    (version "3.36.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/simple-scan/"
                           (version-major+minor version) "/"
                           "simple-scan-" version ".tar.xz"))
       (sha256
        (base32 "0gsz7jqk0fdj0mama3cnss9i1adw18cpdnlcjcjh4r5qijmvx0vh"))))
    (build-system meson-build-system)
    ;; TODO: Fix icons in home screen, About dialogue, and scan menu.
    (arguments
     '(#:glib-or-gtk? #t))
    (inputs
     `(("gtk" ,gtk+)
       ("zlib" ,zlib)
       ("cairo" ,cairo)
       ("colord" ,colord)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gusb" ,gusb)
       ("libsane" ,sane-backends)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("itstool" ,itstool)
       ("glib" ,glib "bin")             ; glib-compile-schemas, etc.
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (home-page "https://gitlab.gnome.org/GNOME/simple-scan")
    (synopsis "Document and image scanner")
    (description
     "Document Scanner is an easy-to-use application that lets you connect your
scanner and quickly capture images and documents in an appropriate format.  It
supports any scanner for which a suitable SANE driver is available, which is
almost all of them.")
    (license license:gpl3+)))

(define-public eolie
  (package
    (name "eolie")
    (version "0.9.98.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://adishatz.org/eolie/eolie-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1d844zva5w4p9pnp9c2g7zyb4vayr2g2drf78spxsdlc5lbd7lqr"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t))
         (add-after 'wrap 'wrap-more
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    ;; These libraries must be on LD_LIBRARY_PATH.
                    (libs '("gtkspell3" "webkitgtk" "libsoup" "libsecret"
                            "atk" "gtk+" "gsettings-desktop-schemas"
                            "gobject-introspection"))
                    (path (string-join
                           (map (lambda (lib)
                                  (string-append (assoc-ref inputs lib) "/lib"))
                                libs)
                           ":")))
               (wrap-program (string-append out "/bin/eolie")
                 `("LD_LIBRARY_PATH" ":" prefix (,path))
                 `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("glib:bin" ,glib "bin")))
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("glib-networking" ,glib-networking)
       ("cairo" ,cairo)
       ("gtk+" ,gtk+)
       ("atk" ,atk)    ; propagated by gtk+, but we need it in LD_LIBRARY_PATH
       ("python" ,python-wrapper)
       ("python-dateutil" ,python-dateutil)
       ("python-pyfxa" ,python-pyfxa)
       ("python-pygobject" ,python-pygobject)
       ("python-pycairo" ,python-pycairo)
       ("python-pycrypto" ,python-pycrypto)
       ("libsecret" ,libsecret)
       ("gtkspell3" ,gtkspell3)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("webkitgtk" ,webkitgtk)))
    (home-page "https://wiki.gnome.org/Apps/Eolie")
    (synopsis "Web browser for GNOME")
    (description
     "Eolie is a new web browser for GNOME.  It features Firefox sync support,
a secret password store, an adblocker, and a modern UI.")
    (license license:gpl3+)))

(define-public epiphany
  (package
    (name "epiphany")
    (version "3.36.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/epiphany/"
                       (version-major+minor version) "/"
                       "epiphany-" version ".tar.xz"))
       (sha256
        (base32 "0vz1j6yrjv0nmf5lk8prkkm10fbcmd35khy9zsd7d4a86wk5c6v2"))))
    (build-system meson-build-system)
    (outputs '("out" "help"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       ;; Otherwise, the RUNPATH will lack the final 'epiphany' path component.
       (list
        (string-append "-Dc_link_args=-Wl,-rpath="
                       (assoc-ref %outputs "out")
                       "/lib/epiphany"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "post_install.py"
               (("gtk-update-icon-cache")
                "true"))
             #t))
         (add-before 'configure 'disable-failing-tests
           (lambda _
             (substitute* "tests/meson.build"
               ;; embed_shell fails, because webkitgtk apparently no longer supports
               ;; overriding the ftp schema
               ;; web_app_utils fails due to missing network access
               (("(embed_shell|web_app_utils)_test,")
                "find_program('sh'), args: ['-c', 'exit 77'],"))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             #t))
       (add-after 'install 'move-help
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (help (assoc-ref outputs "help")))
             (mkdir-p (string-append help "/share"))
             (rename-file
              (string-append out "/share/help")
              (string-append help "/share/help"))
             #t))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server-for-tests)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("appstream-util" ,appstream-glib)
       ("avahi" ,avahi)
       ("cairo" ,cairo)
       ("gcr" ,gcr)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gnome-desktop" ,gnome-desktop)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("iso-codes" ,iso-codes)
       ("libdazzle" ,libdazzle)
       ("libhandy" ,libhandy)
       ("libnotify" ,libnotify)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("nettle" ,nettle)
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk)))
    (synopsis "GNOME web browser")
    (description "Epiphany is a GNOME web browser based on the WebKit rendering
engine.")
    (home-page "https://wiki.gnome.org/Apps/Web")
    (license license:gpl3+)))

(define-public d-feet
  (package
    (name "d-feet")
    (version "0.3.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1m8lwiwl5jhi0x7y6x5zmd3hjplgvdjrb8a8jg74rvkygslj1p7f"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:out-of-source? #f ; tests need to run in the source directory.
       #:phases
       (modify-phases %standard-phases
         (add-before
          'check 'pre-check
          (lambda _
            ;; The test suite requires a running X server.
            (system "Xvfb :1 &")
            (setenv "DISPLAY" ":1")
            ;; Don't fail on missing '/etc/machine-id'.
            (setenv "DBUS_FATAL_WARNINGS" "0")
            ;; tests.py and window.py don't meet E402:
            ;;   E402 module level import not at top of file
            (substitute* "src/tests/Makefile"
              (("--ignore=E123") "--ignore=E123,E402"))
            #t))
         (add-after
          'install 'wrap-program
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((prog (string-append (assoc-ref outputs "out")
                                       "/bin/d-feet")))
              (wrap-program prog
                `("PYTHONPATH" = (,(getenv "PYTHONPATH")))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
              #t))))))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("python-pep8" ,python-pep8)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("python" ,python-wrapper)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("python-pygobject" ,python-pygobject)))
    (home-page "https://wiki.gnome.org/Apps/DFeet")
    (synopsis "D-Bus debugger")
    (description
     "D-Feet is a D-Bus debugger, which can be used to inspect D-Bus interfaces
of running programs and invoke methods on those interfaces.")
    (license license:gpl2+)))

(define-public yelp-xsl
  (package
    (name "yelp-xsl")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1hsfj3q3a3kca0cf9i02xlq2qd4vy12qsjb89hh4r6mp6c11rrag"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--enable-doc")))
    (native-inputs
     `(("ducktype" ,mallard-ducktype)
       ("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)
       ("xsltproc" ,libxslt)))
    (synopsis "XSL stylesheets for Yelp")
    (description "Yelp-XSL is a collection of programs and data files to help
you build, maintain, and distribute documentation.  It provides XSLT stylesheets
that can be built upon for help viewers and publishing systems.  These
stylesheets output JavaScript and CSS content, and reference images
provided by yelp-xsl. It also redistributes copies of the jQuery and
jQuery.Syntax JavaScript libraries.")
    (home-page "https://wiki.gnome.org/Apps/Yelp")
    (license
     (list
      ;; XSLT
      license:gpl2+
      ;; Images
      license:lgpl2.1+
      ;; JavaScript
      license:expat))))

(define-public yelp
  (package
    (name "yelp")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "097djjyl096zmicjpxlb858yz6rd5cj813xc8azbxlhsscikwjzx"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* "libyelp/libyelp-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk)
       ("yelp-xsl" ,yelp-xsl)))
    (synopsis "GNOME help browser")
    (description "Yelp is the help viewer in Gnome.  It natively views Mallard,
DocBook, man, info, and HTML documents.  It can locate documents according to
the freedesktop.org help system specification.")
    (home-page "https://wiki.gnome.org/Apps/Yelp")
    (license license:gpl2+)))

(define-public yelp-tools
  (package
    (name "yelp-tools")
    (version "3.32.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1yg8f5g5wadhmy4yfd9yjhvd8vll4gq4l86ibp0b42qbxnsmcf0q"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("yelp-xsl" ,yelp-xsl)))
    (synopsis "Tools for Yelp")
    (description "Yelp-Tools contains small programs that help you create, edit,
manage, and publish your Mallard or DocBook documentation.")
    (home-page "https://wiki.gnome.org/Apps/Yelp/Tools")
    (license license:gpl2+)))

(define-public libgee
  (package
    (name "libgee")
    (version "0.20.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/libgee/"
                       (version-major+minor version) "/"
                       "libgee-" version ".tar.xz"))
       (sha256
        (base32
         "1pm525wm11dhwz24m8bpcln9547lmrigl6cxf3qsbg4cr3pyvdfh"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-introspection-install-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (substitute* "gee/Makefile.in"
                 (("@INTROSPECTION_GIRDIR@")
                  (string-append out "/share/gir-1.0/"))
                 (("@INTROSPECTION_TYPELIBDIR@")
                  (string-append out "/lib/girepository-1.0/")))))))))
    (native-inputs
     `(("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "GObject collection library")
    (description "Libgee is a utility library providing GObject-based interfaces
and classes for commonly used data structures.")
    (home-page "https://wiki.gnome.org/Projects/Libgee")
    (license license:lgpl2.1+)))

(define-public gexiv2
  (package
    (name "gexiv2")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0xxxq8xdkgkn146my307jgws4qgxx477h0ybg1mqza1ycmczvsla"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* "gexiv2-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("python2" ,python-2)
       ("python3" ,python)
       ("vala" ,vala)))
    (propagated-inputs
     `(("exiv2" ,exiv2)
       ("glib" ,glib)))
    (synopsis "GObject-based Exiv2 wrapper")
    (description "Gexiv2 is a GObject wrapper around the Exiv2 photo metadata
library.  It allows for GNOME applications to easily inspect and update EXIF,
IPTC, and XMP metadata in photo and video files of various formats.")
    (home-page "https://wiki.gnome.org/Projects/gexiv2")
    (license license:gpl2+)))

(define-public shotwell
  (package
    (name "shotwell")
    (version "0.30.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/shotwell/"
                                  (version-major+minor version) "/"
                                  "shotwell-" version ".tar.xz"))
              (sha256
               (base32
                "1y556yyzfya0310v5wqjkf17hy5lhf028iminvvgi2pdfva344id"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/meson/postinstall.py"
               (("gtk-update-icon-cache") (which "true"))
               (("update-desktop-database") (which "true")))
             #t)))))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("gcr" ,gcr)
       ("gexiv2" ,gexiv2)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gstreamer" ,gstreamer)
       ("json-glib" ,json-glib)
       ("libgdata" ,libgdata)
       ("libgee" ,libgee)
       ("libgphoto2" ,libgphoto2)
       ("libgudev" ,libgudev)
       ("libraw" ,libraw)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk)))
    (home-page "https://wiki.gnome.org/Apps/Shotwell")
    (synopsis "Photo manager for GNOME 3")
    (description
     "Shotwell is a digital photo manager designed for the GNOME desktop
environment.  It allows you to import photos from disk or camera, organize
them by keywords and events, view them in full-window or fullscreen mode, and
share them with others via social networking and more.")
    (license license:lgpl2.1+)))

(define-public file-roller
  (package
    (name "file-roller")
    (version "3.36.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/file-roller/"
                       (version-major+minor version) "/"
                       "file-roller-" version ".tar.xz"))
       (sha256
        (base32 "1lkb0m8ys13sy3b6c1kj3cqrqf5d1dqvhbp8spz8v9yjv3d7z3r6"))))
    (build-system meson-build-system)
    (outputs '("out" "help"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-nautilus-extension
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "nautilus/meson.build"
               (("libnautilus_extension_dep\\.get_pkgconfig_variable\\('extensiondir'\\)")
                (string-append "'"
                               (assoc-ref outputs "out")
                               "/lib/nautilus/extensions-"
                               ,(version-major (package-version nautilus))
                               ".0" "'")))
             #t))
         (add-before 'configure 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "postinstall.py"
               (("gtk-update-icon-cache")
                "true"))
             #t))
         (add-after 'install 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/help")
                (string-append help "/share/help"))
               #t))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (inputs
     `(("cpio" ,cpio)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libarchive" ,libarchive)
       ("libnautilus-extension" ,nautilus)
       ("libnotify" ,libnotify)
       ("nettle" ,nettle)))
    (synopsis "Archive management utility")
    (description "File Roller is an archive manager for the GNOME environment.
This means that you can create and modify archives; view the content of an
archive; view and modify a file contained in the archive; extract files from
the archive.")
    (home-page "https://wiki.gnome.org/Apps/FileRoller")
    (license license:gpl2+)))

(define-public gnome-session
  (package
    (name "gnome-session")
    (version "3.34.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1qgqp97f8k2zi2ydvhds14zsjzfj7cv521r6wx5sw0qacn0p7dwb"))))
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Use elogind instead of systemd.
             (substitute* "meson.build"
               (("libsystemd-login") "libelogind")
               (("and libsystemd_daemon_dep.found.*") ","))
             (substitute* "gnome-session/gsm-systemd.c"
               (("#include <systemd/sd-login.h>")
                "#include <elogind/sd-login.h>"))
             ;; Remove uses of the systemd daemon.
             (substitute* "gnome-session/gsm-autostart-app.c"
               (("#ifdef HAVE_SYSTEMD") "#if 0"))
             #t))
         (add-after 'install 'wrap-gnome-session
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'gnome-session' finds the 'gsettings' program.
             (let ((glib (assoc-ref inputs "glib:bin"))
                   (out  (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/gnome-session")
                 `("PATH" ":" prefix (,(string-append glib "/bin"))))
               #t))))

       #:configure-flags
       '("-Ddocbook=false" ; FIXME: disabled because of docbook validation error
         "-Dman=false" ; FIXME: disabled because of docbook validation error
         "-Dsystemd_journal=false")))
    (build-system meson-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("xsltproc" ,libxslt)
       ("libxml2" ,libxml2) ;for 'XML_CATALOG_FILES'
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml)
       ("xmlto" ,xmlto)))
    (inputs
     `(("elogind" ,elogind)
       ("gnome-desktop" ,gnome-desktop)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libsm" ,libsm)
       ("libxcomposite" ,libxcomposite)
       ("libxtst" ,libxtst)
       ("mesa" ,mesa)
       ("upower" ,upower)
       ("xtrans" ,xtrans)))
    (synopsis "Session manager for GNOME")
    (description
     "This package contains the GNOME session manager, as well as a
configuration program to choose applications starting on login.")
    (home-page "https://wiki.gnome.org/Projects/SessionManagement")
    (license license:gpl2+)))

(define-public gjs
  (package
    (name "gjs")
    (version "1.58.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "10gkmxbhwpnq27db0gkn25b0gw28n05msjkzwjg3sdhpdisfpcvz"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags
       (list
        "--enable-code-coverage"
        "--enable-asan"
        "--enable-ubsan")
       #:phases
       (modify-phases %standard-phases
         (add-before
             'check 'pre-check
           (lambda _
             ;; The test suite requires a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             ;; For the missing /etc/machine-id.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             ;; Our mozjs package does not compile the required Intl API
             ;; support for these failing tests.
             (substitute* "installed-tests/js/testLocale.js"
               ((".*toBeDefined.*") "")
               ((".*expect\\(datestr\\).*") ""))
             (substitute* "installed-tests/scripts/testCommandLine.sh"
               (("Valentín") "")
               (("☭") ""))
             #t)))))
    (native-inputs
     `(("dbus-launch" ,dbus)
       ("dconf" ,dconf)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("lcov" ,lcov)
       ("pkg-config" ,pkg-config)
       ("uuidgen" ,util-linux)
       ("xmllint" ,libxml2)
       ("xvfb" ,xorg-server-for-tests)))
    (inputs
     `(("gtk+" ,gtk+)
       ("ncurses" ,ncurses)
       ("readline" ,readline)
       ("sysprof" ,sysprof)))
    (propagated-inputs
     `(("cairo" ,cairo)
       ("glib" ,glib)
       ("libffi" ,libffi)
       ("mozjs" ,mozjs-60)))
    (synopsis "Javascript bindings for GNOME")
    (description "Gjs is a javascript binding for GNOME.  It's mainly based on
spidermonkey javascript engine and the GObject introspection framework.")
    (home-page "https://wiki.gnome.org/Projects/Gjs")
    (license
     ;; The project is dual-licensed.
     (list
      license:expat
      license:lgpl2.0+))))

(define-public gedit
  (package
    (name "gedit")
    (version "3.36.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "15s1almlhjlgl3m8lxg6jpzln8jhgdxxjr635a3b7cf58d35b1v8"))))
    (build-system meson-build-system)
    (outputs '("out" "help" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dgtk_doc=true"
        ;; Otherwise, the RUNPATH will lack the final path component.
        (string-append "-Dc_link_args=-Wl,-rpath="
                       (assoc-ref %outputs "out")
                       "/lib/gedit"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-libgd-fetch
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((libgd (assoc-ref inputs "libgd")))
               ;; Calling git is unnecessary because libgd is fetched as a
               ;; native input to this package.
               (substitute* "meson.build"
                 ((".*git.*")
                  ""))
               (copy-recursively libgd "subprojects/libgd")
               #t)))
         (add-after 'patch-libgd-fetch 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* '("api-breaks.xml" "gedit-docs.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml-4.1.2")
                                 "/xml/dtd/docbook/"))
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml-4.3")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/meson/post_install.py"
               (("gtk-update-icon-cache")
                (which "true")))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/help")
                (string-append help "/share/help"))
               #t)))
         (add-after 'move-help 'wrap-gedit
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gtksourceview (assoc-ref inputs "gtksourceview"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                    (python-path (getenv "PYTHONPATH")))
               (wrap-program (string-append out "/bin/gedit")
                 ;; For plugins.
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                 `("PYTHONPATH" ":" prefix (,python-path))
                 ;; For language-specs.
                 `("XDG_DATA_DIRS" ":" prefix (,(string-append gtksourceview
                                                               "/share")))))
             #t)))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("docbook-xml-4.1.2" ,docbook-xml-4.1.2)
       ("docbook-xml-4.3" ,docbook-xml-4.3)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("libgd"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://gitlab.gnome.org/GNOME/libgd")
             (commit "c7c7ff4e05d3fe82854219091cf116cce6b19de0")))
           (file-name (git-file-name "libgd" version))
           (sha256
            (base32 "16yld0ap7qj1n96h4f2sqkjmibg7xx5xwkqxdfzam2nmyfdlrrrs"))))
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (inputs
     `(("adwaita-icon-theme" ,adwaita-icon-theme)
       ("appstream-util" ,appstream-glib)
       ("glib" ,glib)
       ("gnome-desktop" ,gnome-desktop)
       ("gspell" ,gspell)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("iso-codes" ,iso-codes)
       ("libsoup" ,libsoup)
       ("tepl" ,tepl)
       ("x11" ,libx11)))
    (propagated-inputs
     `(("gtksourceview" ,gtksourceview)
       ("libpeas" ,libpeas)))
    (synopsis "GNOME text editor")
    (description "Gedit is the text editor of the GNOME desktop environment.
The first goal of gedit is to be easy to use, with a simple interface by default.
More advanced features are available by enabling plugins.")
    (home-page "https://wiki.gnome.org/Apps/Gedit")
    (license license:gpl2+)))

(define-public zenity
  (package
    (name "zenity")
    (version "3.32.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/zenity/"
                       (version-major+minor version) "/"
                       "zenity-" version ".tar.xz"))
       (sha256
        (base32 "15fdh8xfdhnwcynyh4byx3mrjxbyprqnwxzi7qn3g5wwaqryg1p7"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "help"))
    (arguments
     `(#:configure-flags
       (list
        "--enable-libnotify"
        "--enable-webkitgtk"
        (string-append "--with-help-dir="
                       (assoc-ref %outputs "help")
                       "/share/help"))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("itstool" ,itstool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libnotify" ,libnotify)
       ("x11" ,libx11)
       ("webkitgtk" ,webkitgtk)))
    (synopsis "Rewrite of gdialog")
    (description "Zenity is a tool that allows you to display GTK dialog boxes
in commandline and shell scripts.")
    (home-page "https://wiki.gnome.org/Projects/Zenity")
    (license license:lgpl2.0+)))

(define-public mutter
  (package
    (name "mutter")
    (version "3.34.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0b8bz5kvs7rlwvqsg87cf6jhrrj95vgd1l235mjx8rip35ipfvrd"))))
    ;; NOTE: Since version 3.21.x, mutter now bundles and exports forked
    ;; versions of cogl and clutter.  As a result, many of the inputs,
    ;; propagated-inputs, and configure flags used in cogl and clutter are
    ;; needed here as well.
    (build-system meson-build-system)
    (arguments
     '(;; XXX: All mutter tests fail with the following error:
       ;;   Settings schema 'org.gnome.mutter' is not installed
       #:tests? #f
       #:glib-or-gtk? #t
       #:configure-flags
       ;; TODO: Enable profiler when Sysprof is packaged.
       (list "-Dprofiler=false"
             ;; Otherwise, the RUNPATH will lack the final path component.
             (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib:"
                            (assoc-ref %outputs "out") "/lib/mutter-5")

             ;; The following flags are needed for the bundled clutter
             (string-append "-Dxwayland_path="
                            (assoc-ref %build-inputs "xorg-server-xwayland")
                            "/bin/Xwayland")

             ;; the remaining flags are needed for the bundled cogl
             (string-append "-Dopengl_libname="
                            (assoc-ref %build-inputs "mesa")
                            "/lib/libGL.so"))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-build-with-mesa-20
                    (lambda _
                      ;; Mimic upstream commit a444a4c5f58ea516ad for
                      ;; compatibility with Mesa 20.  Remove for 3.36.
                      (substitute* '("src/backends/meta-egl-ext.h"
                                     "src/backends/meta-egl.c"
                                     "src/backends/meta-egl.h")
                        (("#include <EGL/eglext\\.h>" all)
                         (string-append all "\n#include <EGL/eglmesaext.h>")))
                      (substitute* "cogl/cogl/meson.build"
                        (("#include <EGL/eglext\\.h>" all)
                         (string-append all "\\n#include <EGL/eglmesaext.h>")))
                      #t)))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server-for-tests)
       ;; For git build
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (propagated-inputs
     `(;; libmutter.pc refers to these:
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ;; mutter-clutter-1.0.pc and mutter-cogl-1.0.pc refer to these:
       ("atk" ,atk)
       ("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("json-glib" ,json-glib)
       ("libinput" ,libinput)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxkbcommon" ,libxkbcommon)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("pango" ,pango)
       ("udev" ,eudev)
       ("xinput" ,xinput)))
    (inputs
     `(("elogind" ,elogind)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("libcanberra-gtk" ,libcanberra)
       ("libgudev" ,libgudev)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("libwacom" ,libwacom)
       ("libxkbfile" ,libxkbfile)
       ("libxrandr" ,libxrandr)
       ("libxtst" ,libxtst)
       ("pipewire" ,pipewire)
       ("startup-notification" ,startup-notification)
       ("upower-glib" ,upower)
       ("xkeyboard-config" ,xkeyboard-config)
       ("xorg-server-xwayland" ,xorg-server-xwayland)
       ("zenity" ,zenity)))
    (synopsis "Window and compositing manager")
    (home-page "https://www.gnome.org")
    (description
     "Mutter is a window and compositing manager that displays and manages your
desktop via OpenGL.  Mutter combines a sophisticated display engine using the
Clutter toolkit with solid window-management logic inherited from the Metacity
window manager.")
    (license license:gpl2+)))

(define-public gnome-online-accounts
  (package
    (name "gnome-online-accounts")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0bigfi225g1prnxpb9lcc1i7mdcrkplwb05vilc43jik12cn53qw"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "lib" "doc"))
    (arguments
     `(#:configure-flags
       (list
        (string-append "--libdir="
                       (assoc-ref %outputs "out")
                       "/lib")
        "--disable-static"
        "--enable-documentation"
        "--enable-gtk-doc"
        "--enable-inspector"
        "--enable-media-server"
        "--enable-lastfm"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* '("goa-daemon.xml"
                              "goa-docs.xml" "goa-overview.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'patch-libgoa-output
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((lib (assoc-ref outputs "lib")))
               (substitute* '("src/goa/Makefile.in" "src/goa/goa-1.0.pc.in")
                 (("@prefix@") lib)
                 (("@exec_prefix@") lib)
                 (("@libdir@") (string-append lib "/lib"))
                 (("@includedir@") (string-append lib "/include"))
                 (("@datadir@") (string-append lib "/share")))
               ;; Make sure gobject-introspection knows about the output
               ;; too (see <https://bugs.gnu.org/36535>).
               (setenv "outputs" "out lib")
               #t))))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("docbook-xsl" ,docbook-xsl)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("dbus" ,dbus)
       ("gcr" ,gcr)
       ("json-glib" ,json-glib)
       ("krb5" ,mit-krb5)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("rest" ,rest)
       ("webkitgtk" ,webkitgtk)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gtk+" ,gtk+)))
    (synopsis "Single sign-on framework for GNOME")
    (description "GNOME Online Accounts aims to provide a way for users to setup
online accounts to be used by the core system and core applications only.
Calendar entries show up in GNOME Shell, e-mail in Evolution, online storages
are exposed as GVolumes, and so on.")
    (home-page "https://wiki.gnome.org/Projects/GnomeOnlineAccounts")
    (license license:lgpl2.0+)))

(define-public evolution-data-server
  (package
    (name "evolution-data-server")
    (version "3.36.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "03sc7r6hpi62kcxpnzm5gv1ky3hmslh4fnf2vy2qghb5xqg3zy1r"))))
    (build-system cmake-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:imported-modules
       (,@%cmake-build-system-modules
        (guix build glib-or-gtk-build-system))
       #:modules
       ((guix build cmake-build-system)
        ((guix build glib-or-gtk-build-system)
         #:prefix glib-or-gtk:)
        (guix build utils))
       #:configure-flags
       (list
        (string-append "-DSENDMAIL_PATH="
                       (assoc-ref %build-inputs "sendmail"))
        "-DWITH_SYSTEMDUSERUNITDIR=OFF"
        "-DENABLE_INTROSPECTION=ON"
        "-DENABLE_GTK_DOC=ON"
        "-DWITH_PRIVATE_DOCS=ON"
        "-DENABLE_BACKEND_PER_PROCESS=ON"
        "-DENABLE_VALA_BINDINGS=ON"
        ;; Required for RUNPATH validation.
        (string-append "-DCMAKE_INSTALL_RPATH="
                       (assoc-ref %outputs "out")
                       "/lib"
                       ":"
                       (assoc-ref %outputs "out")
                       "/lib/evolution-data-server"
                       ":"
                       (assoc-ref %outputs "out")
                       "/lib/evolution-data-server/addressbook-backends"
                       ":"
                       (assoc-ref %outputs "out")
                       "/lib/evolution-data-server/calendar-backends"
                       ":"
                       (assoc-ref %outputs "out")
                       "/lib/evolution-data-server/camel-providers"
                       ":"
                       (assoc-ref %outputs "out")
                       "/lib/evolution-data-server/credential-modules"
                       ":"
                       (assoc-ref %outputs "out")
                       "/lib/evolution-data-server/registry-modules"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* '("camel/camel-docs.sgml.in"
                              "evolution-data-server/evolution-data-server-docs.sgml.in"
                              "private/libedbus-private-docs.sgml.in")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'patch-docbook-xml 'fix-errors
           (lambda _
             ;; Entity not available.
             (substitute* "docs/reference/evolution-data-server/evolution-data-server-docs.sgml.in"
               (("<xi:include href=\"xml/e-cal-time-util.xml\"/>")
                ""))
             ;; CMakeLists.txt hard-codes runpath to just the libdir.
             ;; Remove it so the configure flag is respected.
             (substitute* "CMakeLists.txt"
               (("SET\\(CMAKE_INSTALL_RPATH \"\\$\\{privlibdir\\}\"\\)")
                ""))
             ;; Fix references to 'rm' program.
             (substitute* '("tests/test-server-utils/e-test-server-utils.c"
                            "tests/libedata-book/data-test-utils.c"
                            "tests/libedata-book/test-book-cache-utils.c"
                            "tests/libedata-cal/test-cal-cache-utils.c")
               (("/bin/rm") (which "rm")))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk-doc" ,gtk-doc)
       ("gtk+:bin" ,gtk+ "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("vala" ,vala)))
    (inputs
     `(("gcr" ,gcr)
       ("gweather" ,libgweather)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("gperf" ,gperf)
       ("icu" ,icu4c)
       ("json-glib" ,json-glib)
       ("krb5" ,mit-krb5)
       ("libcanberra" ,libcanberra)
       ("libdb" ,bdb)
       ("libgdata" ,libgdata)
       ("oauth" ,liboauth)
       ("openldap" ,openldap)
       ("sendmail" ,sendmail)
       ("webkitgtk" ,webkitgtk)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gtk+" ,gtk+)
       ("libical" ,libical)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("nspr" ,nspr)
       ("nss" ,nss)
       ("sqlite" ,sqlite)))
    (synopsis "Backend for Evolution")
    (description "Evolution-Data-Server package provides a unified backend for
programs that work with contacts, tasks, and calendar information.  It provides
a single database for common, desktop-wide information, such as a user's address
book or calendar events.  It was originally developed for Evolution, but is now
used by other packages as well.")
    (home-page "https://wiki.gnome.org/Apps/Evolution")
    (license license:lgpl2.0)))

(define-public caribou
  (package
    (name "caribou")
    (version "0.4.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0mfychh1q3dx0b96pjz9a9y112bm9yqyim40yykzxx1hppsdjhww"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before
          'build 'pre-build
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              ;; Use absolute shared library path in Caribou-1.0.typelib.
              (substitute* "libcaribou/Makefile"
                (("--shared-library=libcaribou.so")
                 (string-append "--shared-library="
                                out "/lib/libcaribou.so")))
              #t)))
         (add-after 'install 'wrap-programs
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (python-path (getenv "PYTHONPATH"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
              (for-each
               (lambda (prog)
                 (wrap-program prog
                   `("PYTHONPATH"      ":" prefix (,python-path))
                   `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
               (list (string-append out "/bin/caribou-preferences")
                     (string-append out "/libexec/antler-keyboard"))))
            #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     ;; caribou-1.0.pc refers to all these.
     `(("libgee" ,libgee)
       ("libxklavier" ,libxklavier)
       ("libxtst" ,libxtst)
       ("gtk+" ,gtk+)))
    (inputs
     `(("clutter" ,clutter)
       ("dconf" ,dconf)
       ("gtk+-2" ,gtk+-2)
       ("python-pygobject" ,python-pygobject)))
    (synopsis "Text entry and UI navigation application")
    (home-page "https://wiki.gnome.org/Projects/Caribou")
    (description
     "Caribou is an input assistive technology intended for switch and pointer
users.")
    (license license:lgpl2.1)))

(define-public network-manager
  (package
    (name "network-manager")
    (version "1.26.0")
    (properties '((upstream-name . "NetworkManager")))
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/NetworkManager/"
                       (version-major+minor version) "/"
                       "NetworkManager-" version ".tar.xz"))
       (sha256
        (base32 "0isdqwp58d7r92sqsk7l2vlqwy518n8b7c7z94jk9gc1bdmjf8sj"))
       (patches
        (search-patches "network-manager-plugin-path.patch"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        ;; System Paths
        "-Dsystemdsystemunitdir=no"
        (string-append "-Dsystem_ca_path="
                       (assoc-ref %build-inputs "nss-certs")
                       "/etc/ssl/certs")
        (string-append "-Dudev_dir="
                       (assoc-ref %outputs "out")
                       "/lib/udev")
        (string-append "-Ddbus_conf_dir="
                       (assoc-ref %outputs "out")
                       "/share/dbus-1/system.d")
        (string-append "-Dkernel_firmware_dir="
                       (assoc-ref %outputs "out")
                       "/lib/firmware")
        (string-append "-Diptables="
                       (assoc-ref %build-inputs "iptables"))
        (string-append "-Ddnsmasq="
                       (assoc-ref %build-inputs "dnsmasq"))
        (string-append "-Ddnssec_trigger="
                       (assoc-ref %build-inputs "dnssec-trigger:nm")
                       "/libexec/dnssec-trigger-script")
        ;; Platform
        "-Dsession_tracking_consolekit=false"
        "-Dsession_tracking=elogind"
        "-Dsuspend_resume=upower"
        "-Dconfig_auth_polkit_default=true"
        "-Dmodify_system=true"
        "-Dpolkit_agent=true"
        "-Dsystemd_journal=false"
        "-Dconfig_logging_backend_default=syslog"
        "-Dlibaudit=yes"
        ;; Features
        "-Diwd=true"
        "-Dofono=true"
        "-Dnm_cloud_setup=true"
        "-Dbluez5_dun=true"
        "-Debpf=true"
        ;; Handlers for resolv.conf
        "-Dnetconfig=no"
        "-Dconfig_dns_rc_manager_default=resolvconf"
        ;; DHCP Clients
        "-Ddhcpcanon=no"
        "-Ddhcpcd=no"
        "-Dconfig_dhcp_default=dhclient"
        ;; Miscellaneous
        "-Ddocs=true"
        "-Dcrypto=gnutls"
        "-Dqt=false"
        ;; Otherwise, the RUNPATH will lack the final 'NetworkManager' path
        ;; component.
        (string-append "-Dc_link_args=-Wl,-rpath="
                       (assoc-ref %outputs "out")
                       "/lib"
                       ":"
                       (assoc-ref %outputs "out")
                       "/lib/NetworkManager/" ,version))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute*
                 '("docs/api/network-manager-docs.xml"
                   "docs/libnm/libnm-docs.xml")
               (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                (string-append (assoc-ref inputs "docbook-xml-4.1.2")
                               "/xml/dtd/docbook/")))
             (substitute*
                 '("docs/api/network-manager-docs.xml"
                   "docs/libnm/libnm-docs.xml"
                   "man/NetworkManager.conf.xml"
                   "man/NetworkManager.xml"
                   "man/nm-initrd-generator.xml"
                   "man/nm-online.xml"
                   "man/nm-openvswitch.xml"
                   "man/nmcli-examples.xml"
                   "man/nmcli.xml"
                   "man/nmtui.xml")
               (("http://www.oasis-open.org/docbook/xml/4.2/")
                (string-append (assoc-ref inputs "docbook-xml-4.2")
                               "/xml/dtd/docbook/")))
             (substitute*
                 '("docs/api/settings-spec.xsl"
                   "man/nm-settings-dbus.xsl"
                   "man/nm-settings-ifcfg-rh.xsl"
                   "man/nm-settings-keyfile.xsl"
                   "man/nm-settings-nmcli.xsl")
               (("http://www.oasis-open.org/docbook/xml/4.3/")
                (string-append (assoc-ref inputs "docbook-xml-4.3")
                               "/xml/dtd/docbook/")))
             #t))
         (add-before 'configure 'pre-configure
           (lambda _
             ;; These tests try to test aspects of network-manager's
             ;; functionality within restricted containers, but they don't
             ;; cope with being already in the Guix build jail as that jail
             ;; lacks some features that they would like to proxy over (like
             ;; a /sys mount).
             (substitute* "src/platform/tests/meson.build"
               ((".*test-address-linux.*")
                "")
               ((".*test-cleanup-linux.*")
                "")
               ((".*test-link-linux.*")
                "")
               ((".*test-route-linux.*")
                "")
               ((".*test-tc-linux.*")
                ""))
             (substitute* "src/devices/tests/meson.build"
               ((".*test-acd.*")
                "")
               ((".*test-lldp.*")
                ""))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; For the missing /etc/machine-id.
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
     `(("docbook-xml-4.1.2" ,docbook-xml-4.1.2)
       ("docbook-xml-4.2" ,docbook-xml-4.2)
       ("docbook-xml-4.3" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
       ("gjs" ,gjs)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("lua" ,lua)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("python-dbus" ,python-dbus)
       ("python-pygobject" ,python-pygobject)
       ("ruby" ,ruby)
       ("vala" ,vala)
       ("xmllint" ,libxml2)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("audit" ,audit)
       ("bluez" ,bluez)
       ("dbus" ,dbus)
       ("dnsmasq" ,dnsmasq)
       ("dnssec-trigger:nm" ,dnssec-trigger "nm")
       ("gnutls" ,gnutls)
       ("iptables" ,iptables)
       ("isc-dhcp" ,isc-dhcp)
       ("iwd" ,iwd)
       ("jansson" ,jansson)
       ("libcurl" ,curl)
       ("libelogind" ,elogind)
       ("libndp" ,libndp)
       ("libnewt" ,newt)
       ("libnl" ,libnl)
       ("libpsl" ,libpsl)
       ("libselinux" ,libselinux)
       ("libsoup" ,libsoup)
       ("libudev" ,eudev)
       ("mm-glib" ,modem-manager)
       ("mobile-broadband-provider-info" ,mobile-broadband-provider-info)
       ("nss" ,nss)
       ("nss-certs" ,nss-certs)
       ("openresolv" ,openresolv)
       ("polkit" ,polkit)
       ("ppp" ,ppp)
       ("readline" ,readline)
       ("upower" ,upower)
       ("util-linux" ,util-linux)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)))
    (synopsis "Network Management Daemon")
    (description "NetworkManager daemon attempts to make networking
configuration and operation as painless and automatic as possible by managing
the primary network connection and other network interfaces, like Ethernet,
Wi-Fi, and Mobile Broadband devices.  It will connect any network device when a
connection for that device becomes available, unless that behavior is disabled.
Information about networking is exported via a D-Bus interface to any interested
application, providing a rich API with which to inspect and control network
settings and operation.")
    (home-page "https://wiki.gnome.org/Projects/NetworkManager")
    (license
     (list
      ;; Documentation
      license:fdl1.1+
      ;; Library
      license:lgpl2.1+
      ;; Others
      license:gpl2+))))

(define-public network-manager-openvpn
  (package
    (name "network-manager-openvpn")
    (version "1.8.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-openvpn/"
                    (version-major+minor version)
                    "/NetworkManager-openvpn-" version ".tar.xz"))
              (sha256
               (base32
                "062kh4zj7jfbwy4zzcwpq2m457bzbpm3l18s0ysnw3mgia3siz8f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-absolute-paths" "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-path
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (let* ((ovpn (string-append (assoc-ref inputs "openvpn")
                                         "/sbin/openvpn"))
                    (modprobe (string-append (assoc-ref inputs "kmod")
                                             "/bin/modprobe"))
                    (pretty-ovpn (string-append "\"" ovpn "\"")))
               (for-each
                (lambda (file)
                  (substitute* file
                    (("\"/usr/local/sbin/openvpn\"") pretty-ovpn)
                    (("\"/usr/sbin/openvpn\"") pretty-ovpn)
                    (("\"/sbin/openvpn\"") pretty-ovpn)
                    (("/sbin/modprobe") modprobe)))
                '("src/nm-openvpn-service.c" "properties/nm-openvpn-editor.c")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("gtk+" ,gtk+)
       ("kmod" ,kmod)
       ("openvpn" ,openvpn)
       ("network-manager" ,network-manager)
       ("libnma" ,libnma)
       ("libsecret" ,libsecret)))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "OpenVPN plug-in for NetworkManager")
    (description
     "This extension of NetworkManager allows it to take care of connections
to virtual private networks (VPNs) via OpenVPN.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-openvpn")))))

(define-public network-manager-vpnc
  (package
    (name "network-manager-vpnc")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-vpnc/"
                    (version-major+minor version)
                    "/NetworkManager-vpnc-" version ".tar.xz"))
              (sha256
               (base32
                "1js5lwcsqws4klgypfxl4ikmakv7v7xgddij1fj6b0y0qicx0kyy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-absolute-paths"
                           "--localstatedir=/var"
                           ;; libnm-glib has been removed from network-manager
                           ;; 1de8383ad9fdfc8f552117e5d109bdfa7005634b
                           "--with-libnm-glib=no")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-path
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (let* ((vpnc (string-append (assoc-ref inputs "vpnc")
                                         "/sbin/vpnc"))
                    (modprobe (string-append (assoc-ref inputs "kmod")
                                             "/bin/modprobe"))
                    (pretty-ovpn (string-append "\"" vpnc "\"")))
               (substitute* "src/nm-vpnc-service.c"
                    (("\"/usr/local/sbin/vpnc\"") pretty-ovpn)
                    (("\"/usr/sbin/vpnc\"") pretty-ovpn)
                    (("\"/sbin/vpnc\"") pretty-ovpn)
                    (("/sbin/modprobe") modprobe)))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("gtk+" ,gtk+)
       ("kmod" ,kmod)
       ("vpnc" ,vpnc)
       ("network-manager" ,network-manager)
       ("libnma" ,libnma)
       ("libsecret" ,libsecret)))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "VPNC plug-in for NetworkManager")
    (description
     "Support for configuring virtual private networks based on VPNC.
Compatible with Cisco VPN concentrators configured to use IPsec.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-vpnc")))))

(define-public network-manager-openconnect
  (package
    (name "network-manager-openconnect")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-openconnect/"
                    (version-major+minor version)
                    "/NetworkManager-openconnect-" version ".tar.xz"))
              (sha256
               (base32
                "0nlp290nkawc4wqm978n4vhzg3xdqi8kpjjx19l855vab41rh44m"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-absolute-paths" "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-path
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (let* ((openconnect (string-append (assoc-ref inputs "openconnect")
                                         "/sbin/openconnect"))
                    (modprobe (string-append (assoc-ref inputs "kmod")
                                             "/bin/modprobe"))
                    (pretty-ovpn (string-append "\"" openconnect "\"")))
               (substitute* "src/nm-openconnect-service.c"
                 (("\"/usr(/local)?/s?bin/openconnect\"") pretty-ovpn)
                 (("/sbin/modprobe") modprobe)))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("libnma" ,libnma)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gcr" ,gcr)
       ("gtk+" ,gtk+)
       ("kmod" ,kmod)
       ("libsecret" ,libsecret)
       ("libxml2" ,libxml2)
       ("lz4" ,lz4)
       ("network-manager" ,network-manager)
       ("openconnect" ,openconnect)))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "OpenConnect plug-in for NetworkManager")
    (description
     "This extension of NetworkManager allows it to take care of connections
to @acronym{VPNs, virtual private networks} via OpenConnect, an open client for
Cisco's AnyConnect SSL VPN.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-openconnect")

                  ;; The 'etc/dbus-1/system.d/nm-openconnect-service.conf'
                  ;; file refers to account "nm-openconnect".  Specify it here
                  ;; so that 'network-manager-service-type' creates it.
                  (user-accounts . ("nm-openconnect"))))))

(define-public mobile-broadband-provider-info
  (package
    (name "mobile-broadband-provider-info")
    (version "20190618")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://gnome/sources/"
         "mobile-broadband-provider-info/" version "/"
         "mobile-broadband-provider-info-" version ".tar.xz"))
       (sha256
        (base32 "0nyf7kxxhig80myi7j45yg34nqqqxzm2xvdnxjg5p5yicnjn8bf2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)
       ("xsltproc" ,libxslt)))
    (synopsis "Database of broadband connection configuration")
    (description "Mobile-Broadband-Provider-Info contains mobile broadband
settings for different service providers in different countries.")
    (home-page "https://gitlab.gnome.org/GNOME/mobile-broadband-provider-info")
    (license license:public-domain)))

(define-public network-manager-applet
  (package
    (name "network-manager-applet")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/network-manager-applet/"
                       (version-major+minor version) "/"
                       "network-manager-applet-" version ".tar.xz"))
       (sha256
        (base32 "12xiy8g8qk18jvxvn78mvq03zvzp06bww49na765jjw0rq541fyx"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))   ; To wrap binaries and/or compile schemas
    (native-inputs
     `(("desktop-file-vaildate" ,desktop-file-utils)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("appindicator" ,libappindicator)
       ("appstream-util" ,appstream-glib)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gtk+" ,gtk+)
       ("gudev" ,libgudev)
       ("jansson" ,jansson)
       ("libnm" ,network-manager)
       ("libnma" ,libnma)
       ("libnotify" ,libnotify)
       ("libsecret" ,libsecret)
       ("libselinux" ,libselinux)
       ("mm-glib" ,modem-manager)))
    (synopsis "Applet for NetworkManager")
    (description "Network-Manager-Applet is a tray applet and an advanced
network connection editor.")
    (home-page "https://wiki.gnome.org/Projects/NetworkManager")
    (license license:gpl2+)))

(define-public libxml++
  (package
    (name "libxml++")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "19kik79fmg61nv0by0a5f9wchrcfjwzvih4v2waw01hqflhqvp0r"))))
    (build-system gnu-build-system)
    ;; libxml++-3.0.pc refers to all these.
    (propagated-inputs
     `(("libxml2" ,libxml2)
       ("glibmm" ,glibmm)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (home-page "http://libxmlplusplus.sourceforge.net/")
    (synopsis "C++ wrapper for XML parser library libxml2")
    (description
     "This package provides a C++ wrapper for the XML parser library
libxml2.")
    (license license:lgpl2.1+)))

(define-public libxml++-2
  (package
    (inherit libxml++)
    (name "libxml++")
    (version "2.40.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1sb3akryklvh2v6m6dihdnbpf1lkx441v972q9hlz1sq6bfspm2a"))))))

(define-public gdm
  (package
    (name "gdm")
    (version "3.34.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1lyqvcwxhwxklbxn4xjswjzr6fhjix6h28mi9ypn34wdm9bzcpg8"))
              (patches (search-patches "gdm-default-session.patch"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags
       `("--without-plymouth"
         "--disable-systemd-journal"

         ;; Using --with-initial-vt=7 allows GDM to run alongside TTY 1,
         ;; instead of having to replace it (i.e., stopping the mingetty
         ;; service for TTY 1 before starting GDM).
         "--with-initial-vt=7"

         ;; Use '/etc/environment' for locale settings instead of the
         ;; systemd-specific '/etc/locale.conf'.
         "--with-lang-file=/etc/environment"

         ,(string-append "--with-udevdir="
                         (assoc-ref %outputs "out") "/lib/udev")

         "--localstatedir=/var"
         ,(string-append "--with-default-path="
                         (string-join '("/run/setuid-programs"
                                        "/run/current-system/profile/bin"
                                        "/run/current-system/profile/sbin")
                                      ":"))
         ;; Put GDM in bindir so that glib-or-gtk-build-system wraps the
         ;; XDG_DATA_DIRS so that it finds its schemas.
         "--sbindir" ,(string-append (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'pre-configure
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; We don't have <systemd/sd-daemon.h>.
            (substitute* '("common/gdm-log.c"
                           "daemon/gdm-server.c"
                           "daemon/gdm-session-worker.c"
                           "daemon/gdm-session-worker-job.c")
              (("#include <systemd/sd-daemon\\.h>") ""))
            ;; Use elogind for sd-login.
            (substitute* '("common/gdm-common.c"
                           "daemon/gdm-local-display-factory.c"
                           "daemon/gdm-manager.c"
                           "libgdm/gdm-user-switching.c")
              (("#include <systemd/sd-login\\.h>")
               "#include <elogind/sd-login.h>"))
            ;; Check for elogind.
            (substitute* '("configure")
              (("libsystemd")
               "libelogind"))
            ;; Look for system-installed sessions in
            ;; /run/current-system/profile/share.
            (substitute* '("libgdm/gdm-sessions.c"
                           "daemon/gdm-session.c"
                           "daemon/gdm-display.c"
                           "daemon/gdm-launch-environment.c")
              (("DATADIR \"/x")
               "\"/run/current-system/profile/share/x")
              (("DATADIR \"/wayland")
               "\"/run/current-system/profile/share/wayland")
              (("DATADIR \"/gnome")
               "\"/run/current-system/profile/share/gnome"))
            (let ((propagate '("GDM_CUSTOM_CONF"
                               "GDM_DBUS_DAEMON"
                               "GDM_X_SERVER"
                               "GDM_X_SESSION"
                               ;; XXX: Remove this once GNOME Shell is
                               ;; a dependency of GDM.
                               "XDG_DATA_DIRS")))
              (substitute* "daemon/gdm-session.c"
                (("set_up_session_environment \\(self\\);")
                 (apply string-append
                        "set_up_session_environment (self);\n"
                        (map (lambda (name)
                               (string-append
                                "gdm_session_set_environment_variable "
                                "(self, \"" name "\","
                                "g_getenv (\"" name "\"));\n"))
                             propagate)))))
            ;; Find the configuration file using an environment variable.
            (substitute* '("common/gdm-settings.c")
              (("GDM_CUSTOM_CONF")
               (string-append "(g_getenv(\"GDM_CUSTOM_CONF\") != NULL"
                              " ? g_getenv(\"GDM_CUSTOM_CONF\")"
                              " : GDM_CUSTOM_CONF)")))
            ;; Use service-supplied path to X.
            (substitute* '("daemon/gdm-server.c")
              (("\\(X_SERVER X_SERVER_ARG_FORMAT")
               "(\"%s\" X_SERVER_ARG_FORMAT, g_getenv (\"GDM_X_SERVER\")"))
            (substitute* '("daemon/gdm-wayland-session.c"
                           "daemon/gdm-x-session.c")
              (("\"dbus-daemon\"")
               "g_getenv (\"GDM_DBUS_DAEMON\")")
              (("X_SERVER")
               "g_getenv (\"GDM_X_SERVER\")")
              (("GDMCONFDIR \"/Xsession\"")
               "g_getenv (\"GDM_X_SESSION\")"))
            ;; Use an absolute path for GNOME Session.
            (substitute* "daemon/gdm-launch-environment.c"
              (("\"gnome-session\"")
               (string-append "\"" (assoc-ref inputs "gnome-session")
                              "/bin/gnome-session\"")))
            ;; Do not automatically select the placeholder session.
            (substitute* "daemon/gdm-session.c"
              (("!g_str_has_suffix [(]base_name, \"\\.desktop\"[)]")
               (string-append "!g_str_has_suffix (base_name, \".desktop\") || "
                              "(g_strcmp0(search_dirs[i], \""
                              (assoc-ref outputs "out") "/share/gdm/BuiltInSessions/"
                              "\") == 0 && "
                              "g_strcmp0(base_name, \"fail.desktop\") == 0)"))
              (("g_error [(]\"GdmSession: no session desktop files installed, aborting\\.\\.\\.\"[)];")
               "{ self->fallback_session_name = g_strdup(\"fail\"); goto out; }"))
            #t))
         ;; GDM requires that there be at least one desktop entry
         ;; file.  This phase installs a hidden one that simply
         ;; fails.  This enables users to use GDM with a
         ;; '~/.xsession' script with no other desktop entry files.
         ;; See <https://bugs.gnu.org/35068>.
         (add-after 'install 'install-placeholder-desktop-entry
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sessions (string-append out "/share/gdm/BuiltInSessions"))
                    (fail (string-append sessions "/fail.desktop")))
               (mkdir-p sessions)
               (with-output-to-file fail
                 (lambda ()
                   (for-each
                    display
                    '("[Desktop Entry]\n"
                      "Encoding=UTF-8\n"
                      "Type=Application\n"
                      "Name=Fail\n"
                      "Comment=This session fails immediately.\n"
                      "NoDisplay=true\n"
                      "Exec=false\n"))))
               #t)))
         ;; GDM needs GNOME Session to run these applications.  We link
         ;; their autostart files in `share/gdm/greeter/autostart'
         ;; because GDM explicitly tells GNOME Session to look there.
         ;;
         ;; XXX: GNOME Shell should be linked here too, but currently
         ;; GNOME Shell depends on GDM.
         (add-after 'install 'link-autostart-files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (autostart (string-append out "/share/gdm/"
                                              "greeter/autostart"))
                    (settings (assoc-ref inputs "gnome-settings-daemon")))
               (mkdir-p autostart)
               (with-directory-excursion autostart
                 (for-each (lambda (desktop)
                             (symlink desktop (basename desktop)))
                           (find-files (string-append settings "/etc/xdg"))))
               #t))))))
    (native-inputs
     `(("dconf" ,dconf)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("accountsservice" ,accountsservice)
       ("check" ,check) ; for testing
       ("elogind" ,elogind)
       ("gnome-session" ,gnome-session)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("gtk+" ,gtk+)
       ("iso-codes" ,iso-codes)
       ("libcanberra" ,libcanberra)
       ("linux-pam" ,linux-pam)))
    (synopsis "Display manager for GNOME")
    (home-page "https://wiki.gnome.org/Projects/GDM/")
    (description
     "GNOME Display Manager is a system service that is responsible for
providing graphical log-ins and managing local and remote displays.")
    (license license:gpl2+)))

(define-public libgtop
  (package
    (name "libgtop")
    (version "2.40.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/libgtop/"
                       (version-major+minor version) "/"
                       "libgtop-" version ".tar.xz"))
       (sha256
        (base32 "1m6jbqk8maa52gxrf223442fr5bvvxgb7ham6v039i3r1i62gwvq"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/reference"
               (substitute* "libgtop-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("x11" ,libx11)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "System Monitoring Library")
    (description "LibGTop is a library for collecting system monitoring data.")
    (home-page "https://gitlab.gnome.org/GNOME/libgtop")
    (license license:gpl2+)))

(define-public gnome-bluetooth
  (package
    (name "gnome-bluetooth")
    (version "3.34.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "11nk8nvz5yrbx7wp75vsiaf4rniv7ik2g3nwmgwx2b42q9v11j9y"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dicon_update=false"
        "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference/libgnome-bluetooth"
               (substitute* "gnome-bluetooth-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
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
     `(("docbook-xml" ,docbook-xml-4.3)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("bluez" ,bluez)
       ("dbus-glib" ,dbus-glib)
       ("gconf" ,gconf)
       ("libcanberra" ,libcanberra)
       ("libnotify" ,libnotify)
       ("libunique" ,libunique)
       ("udev" ,eudev)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)))
    (synopsis "GNOME Bluetooth")
    (description "GNOME-Bluetooth is a fork of bluez-gnome focused on
integration with the GNOME desktop environment.")
    (home-page "https://wiki.gnome.org/Projects/GnomeBluetooth")
    (license
     (list
      ;; Library
      license:lgpl2.1+
      ;; Others
      license:gpl2+))))

(define-public gnome-control-center
  (package
    (name "gnome-control-center")
    (version "3.36.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0m7pxjgymc7aqqz0vcmlq91nxnwzd1v7v1gdhrfam49krxmk80mc"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Ddocumentation=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "man"
               (substitute* '("gnome-control-center.xml" "meson.build")
                 (("http://docbook.sourceforge.net/release/xsl/current")
                  (string-append (assoc-ref inputs "docbook-xsl")
                                 "/xml/xsl/docbook-xsl-"
                                 ,(package-version docbook-xsl)))
                 (("http://www.oasis-open.org/docbook/xml/4.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'patch-docbook 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((libc (assoc-ref inputs "libc"))
                    (tzdata (assoc-ref inputs "tzdata"))
                    (libgnomekbd (assoc-ref inputs "libgnomekbd"))
                    (nm-applet (assoc-ref inputs "network-manager-applet"))
                    (gnome-desktop (assoc-ref inputs "gnome-desktop")))
               (substitute* "panels/datetime/tz.h"
                 (("/usr/share/zoneinfo/zone.tab")
                  (string-append tzdata "/share/zoneinfo/zone.tab")))
               (substitute* "tests/datetime/test-endianess.c"
                 (("/usr/share/locale")
                  (string-append libc "/share/locale")))
               (substitute* "panels/region/cc-region-panel.c"
                 (("\"gkbd-keyboard-display")
                  (string-append "\"" libgnomekbd
                                 "/bin/gkbd-keyboard-display")))
               (substitute* '("panels/network/net-device-wifi.c"
                              "panels/network/connection-editor/net-connection-editor.c")
                 (("\"nm-connection-editor")
                  (string-append "\"" nm-applet
                                 "/bin/nm-connection-editor")))
               (substitute* '("panels/user-accounts/run-passwd.c")
                 (("/usr/bin/passwd")
                  "/run/setuid-programs/passwd"))
               #t)))
         (add-before 'configure 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/meson/meson_post_install.py"
               (("gtk-update-icon-cache")
                (which "true")))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("python-dbusmock" ,python-dbusmock)
       ("xorg-server" ,xorg-server-for-tests)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("accountsservice" ,accountsservice)
       ("cairo" ,cairo)
       ("cheese" ,cheese)
       ("colord" ,colord)
       ("colord-gtk" ,colord-gtk)
       ("cups" ,cups)
       ("epoxy" ,libepoxy)
       ("fontconfig" ,fontconfig)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)
       ("gnome-bluetooth" ,gnome-bluetooth)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("grilo" ,grilo)
       ("gsound" ,gsound)
       ("gtk+" ,gtk+)
       ("gudev" ,libgudev)
       ("ibus" ,ibus)
       ("krb5" ,mit-krb5)
       ("libgnomekbd" ,libgnomekbd)
       ("libgtop" ,libgtop)
       ("libhandy" ,libhandy)
       ("libnm" ,network-manager)
       ("libnma" ,libnma)
       ("libpulse" ,pulseaudio)
       ("libpwquality" ,libpwquality)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("libwacom" ,libwacom)
       ("mm-glib" ,modem-manager)
       ("network-manager-applet" ,network-manager-applet)
       ("polkit" ,polkit)
       ("smbclient" ,samba)
       ("tzdata" ,tzdata)
       ("udisks" ,udisks)
       ("upower-glib" ,upower)
       ("x11" ,libx11)
       ("xi" ,libxi)))
    (synopsis "GNOME Settings")
    (description "GNOME-Control-Center is the GNOME's main interface for
configuration of various aspects of your desktop.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-control-center/")
    (license license:gpl2+)))

(define-public gnome-shell
  (package
    (name "gnome-shell")
    (version "3.34.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0k9vq2gh1nhdd6fpp7jnwx37qxaakawiqw1xnlfjvq5g5zdn8ckh"))
              (patches (search-patches "gnome-shell-theme.patch"
                                       "gnome-shell-disable-test.patch"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   ;; Copy images for use on the GDM log-in screen.
                   (copy-file #$(file-append %artwork-repository
                                             "/slim/0.x/background.png")
                              "data/theme/guix-background.png")
                   (copy-file #$(file-append %artwork-repository
                                             "/logo/Guix-horizontal-white.svg")
                              "data/theme/guix-logo.svg")
                   #t))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:disallowed-references ((,glib "bin")
                                ,inkscape ,libxslt
                                ,ruby-sass)
       #:configure-flags
       (list "-Dsystemd=false"
             ;; Otherwise, the RUNPATH will lack the final path component.
             (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/gnome-shell"))

       #:modules ((guix build meson-build-system)
                  (guix build utils)
                  (srfi srfi-1))

       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-keysdir
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (keysdir (string-append
                              out "/share/gnome-control-center/keybindings")))
               (substitute* "meson.build"
                 (("keysdir =.*")
                  (string-append "keysdir = '" keysdir "'\n")))
               #t)))
         (add-before 'configure 'convert-logo-to-png
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Convert the logo from SVG to PNG.
             (invoke "inkscape" "--export-png=data/theme/guix-logo.png"
                     "data/theme/guix-logo.svg")))
         (add-before 'configure 'record-absolute-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "js/misc/ibusManager.js"
               (("'ibus-daemon'")
                (string-append "'" (assoc-ref inputs "ibus")
                               "/bin/ibus-daemon'")))
             (substitute* "js/ui/status/keyboard.js"
               (("'gkbd-keyboard-display'")
                (string-append "'" (assoc-ref inputs "libgnomekbd")
                               "/bin/gkbd-keyboard-display'")))
             #t))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             #t))
         (add-after 'install 'wrap-programs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out              (assoc-ref outputs "out"))
                   (gi-typelib-path  (getenv "GI_TYPELIB_PATH"))
                   (python-path      (getenv "PYTHONPATH")))
               (wrap-program (string-append out "/bin/gnome-shell")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                 ;; FIXME: gnome-shell loads these libraries with unqualified
                 ;; names only, so they need to be on LD_LIBRARY_PATH.  The
                 ;; alternative might be to patch gnome-shell.
                 `("LD_LIBRARY_PATH" ":" prefix
                   ,(map (lambda (pkg)
                           (string-append (assoc-ref inputs pkg) "/lib"))
                         '("gdk-pixbuf"
                           "gnome-bluetooth" "librsvg" "libgweather"))))
               (for-each
                (lambda (prog)
                  (wrap-program (string-append out "/bin/" prog)
                    `("PYTHONPATH"      ":" prefix (,python-path))
                    `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                '("gnome-shell-extension-tool" "gnome-shell-perf-tool"))
               #t)))
         (replace 'glib-or-gtk-wrap
           (let ((wrap (assoc-ref %standard-phases 'glib-or-gtk-wrap)))
             (lambda* (#:key inputs outputs #:allow-other-keys #:rest rest)
               ;; By default Inkscape et al. would end up in the XDG_DATA_DIRS
               ;; settings of the wrappers created by the 'glib-or-gtk-wrap'
               ;; phase.  Fix that since we don't need these.
               (wrap #:inputs (fold alist-delete inputs
                                    '("inkscape" "intltool" "glib:bin"))
                     #:outputs outputs)))))))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("gobject-introspection" ,gobject-introspection)
       ("inkscape" ,inkscape)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("ruby-sass" ,ruby-sass)
       ("sassc" ,sassc)
       ("xsltproc" ,libxslt)
       ;; For tests
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("accountsservice" ,accountsservice)
       ("caribou" ,caribou)
       ("docbook-xsl" ,docbook-xsl)
       ("evolution-data-server" ,evolution-data-server)
       ("gcr" ,gcr)
       ("gdm" ,gdm)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("gjs" ,gjs)
       ("gnome-autoar" ,gnome-autoar)
       ("gnome-bluetooth" ,gnome-bluetooth)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("gst-plugins-base" ,gst-plugins-base)
       ("ibus" ,ibus)
       ("libcanberra" ,libcanberra)
       ("libcroco" ,libcroco)
       ("libgnomekbd" ,libgnomekbd)               ;for gkbd-keyboard-display
       ("libgweather" ,libgweather)
       ("libnma" ,libnma)
       ("libsoup" ,libsoup)
       ("mesa-headers" ,mesa-headers)
       ("mutter" ,mutter)
       ("network-manager-applet" ,network-manager-applet)
       ("polkit" ,polkit)
       ("pulseaudio" ,pulseaudio)
       ("python-pygobject" ,python-pygobject)
       ("startup-notification" ,startup-notification)
       ("telepathy-logger" ,telepathy-logger)
       ("upower" ,upower)
       ;; XXX: These requirements were added in 3.24, but no mention in NEWS.
       ;; Missing propagation? See also: <https://bugs.gnu.org/27264>
       ("librsvg" ,librsvg)
       ("geoclue" ,geoclue)))
    (synopsis "Desktop shell for GNOME")
    (home-page "https://wiki.gnome.org/Projects/GnomeShell")
    (description
     "GNOME Shell provides core user interface functions for the GNOME desktop,
like switching to windows and launching applications.")
    (license license:gpl2+)))

(define-public gtk-vnc
  (package
    (name "gtk-vnc")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1060ws037v556rx1qhfrcg02859rscksrzr8fq11himdg4d1y6m8"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))   ; To wrap binaries and/or compile schemas
    (native-inputs
     `(("gjs" ,gjs)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("node" ,node)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("vala" ,vala)))
    (inputs
     `(("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)
       ("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt)
       ("libsasl" ,cyrus-sasl)
       ("pulseaudio" ,pulseaudio)
       ("x11" ,libx11)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("gtk+" ,gtk+)))
    (synopsis "VNC client viewer widget for GTK+")
    (description "GTK-VNC is a project providing client side APIs for the RFB
protocol / VNC remote desktop technology.  It is built using coroutines allowing
it to be completely asynchronous while remaining single threaded.  It provides a
core C library, and bindings for Python (PyGTK).")
    (home-page "https://wiki.gnome.org/Projects/gtk-vnc")
    (license license:lgpl2.1+)))

(define-public gnome-autoar
  (package
    (name "gnome-autoar")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0yk56ch46n3wfy633mq31kif9n7v06rlij4vqbsbn6l4z1vw6d0a"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "out")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* "gnome-autoar-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vapigen" ,vala)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libarchive" ,libarchive)))
    (synopsis "Archive management for GNOME")
    (description "GNOME Autoar provides functions, widgets, and gschemas for
GNOME applications which want to use archives as a convenient method to transfer
directories over the internet.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-autoar")
    (license license:lgpl2.1+)))

(define-public tracker
  (package
    (name "tracker")
    (version "2.3.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/tracker/"
                       (version-major+minor version) "/"
                       "tracker-" version ".tar.xz"))
       (sha256
        (base32 "0vai0qz9jn3z5dlzysynwhbbmslp84ygdql81f5wfxxr98j54yap"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list
        "-Ddocs=true"
        "-Dunicode_support=icu"
        "-Dsystemd_user_services=no"
        ;; Otherwise, the RUNPATH will lack the final path component.
        (string-append "-Dc_link_args=-Wl,-rpath="
                       (assoc-ref %outputs "out")
                       "/lib"
                       ":"
                       (assoc-ref %outputs "out")
                       "/lib/tracker-"
                       ,(version-major version)
                       ".0"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute*
                   '("libtracker-control/libtracker-control-docs.xml"
                     "libtracker-control/migrating-1to2.xml"
                     "libtracker-miner/libtracker-miner-docs.xml"
                     "libtracker-miner/migrating-1to2.xml"
                     "libtracker-sparql/libtracker-sparql-docs.xml"
                     "libtracker-sparql/migrating-1to2.xml"
                     "ontology/ontology-docs.sgml")
                 (("http://www.oasis-open.org/docbook/xml/4.5/")
                  (string-append (assoc-ref inputs "docbook-xml-4.5")
                                 "/xml/dtd/docbook/"))
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml-4.3")
                                 "/xml/dtd/docbook/"))
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml-4.1.2")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Some tests expect to write to $HOME.
             (setenv "HOME" (getcwd))
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
       ("docbook-xml-4.5" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("python-pygobject" ,python-pygobject)
       ("vala" ,vala)))
    (inputs
     `(("dbus" ,dbus)
       ("icu-uc" ,icu4c)
       ("json-glib" ,json-glib)
       ("libnm" ,network-manager)
       ("libsoup" ,libsoup)
       ("libstemmer" ,libstemmer)
       ("libxml2" ,libxml2)
       ("sqlite" ,sqlite)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "Search Engine and Triplestore")
    (description "Tracker is a filesystem indexer, metadata storage system and
search tool.  This package provides Tracker-Core, that contains the database
(tracker-store), the database ontologies, the commandline user interface
(tracker), and several support libraries.")
    (home-page "https://wiki.gnome.org/Projects/Tracker")
    (license
     (list
      ;; LibTracker, Tracker-Extract, Tracker-Utils and GVDB
      license:lgpl2.1+
      ;; LibStemmer
      license:bsd-3
      ;; Others
      license:gpl2+))))

(define-public tracker-miners
  (package
    (name "tracker-miners")
    (version "2.3.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/tracker-miners/"
                       (version-major+minor version)
                       "/tracker-miners-" version ".tar.xz"))
       (sha256
        (base32 "06abxrnrz7xayykrabn135rpsm6z0fqw7gibrb9j09l6swlalwkl"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dfunctional_tests=false"
        "-Dguarantee_metadata=true"
        "-Dbattery_detection=upower"
        "-Dcharset_detection=icu"
        "-Dgeneric_media_extractor=gstreamer"
        "-Dgstreamer_backend=discoverer"
        "-Dsystemd_user_services=no"
        ;; Ensure the RUNPATH contains all installed library locations.
        (string-append "-Dc_link_args=-Wl,-rpath="
                       (assoc-ref %outputs "out")
                       "/lib/tracker-miners-"
                       ,(version-major version)
                       ".0"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "tests/libtracker-miners-common/meson.build"
               (("'file-utils',")
                ""))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python-pygobject" ,python-pygobject)))
    (inputs
     `(("dbus" ,dbus)
       ("exempi" ,exempi)
       ("flac" ,flac)
       ("gexiv2" ,gexiv2)
       ("giflib" ,giflib)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("icu-uc" ,icu4c)
       ("libcue" ,libcue)
       ("libexif" ,libexif)
       ("libgrss" ,libgrss)
       ("libgsf" ,libgsf)
       ("libgxps" ,libgxps)
       ("libiptcdata" ,libiptcdata)
       ("libjpeg" ,libjpeg-turbo)
       ("libosinfo" ,libosinfo)
       ("libpng" ,libpng)
       ("libseccomp" ,libseccomp)
       ("libtiff" ,libtiff)
       ("libxml2" ,libxml2)
       ("poppler-glib" ,poppler)
       ("taglib" ,taglib)
       ("totem-plparser" ,totem-pl-parser)
       ("tracker" ,tracker)
       ("upower-glib" ,upower)
       ("vorbisfile" ,libvorbis)
       ("zlib" ,zlib)))
    (synopsis "Search Engine and Triplestore")
    (description "Tracker is a filesystem indexer, metadata storage system and
search tool.  This package provides Tracker-Miners, that contains the indexer
daemon (tracker-miner-fs) and tools to extract metadata from many different
filetypes.")
    (home-page "https://wiki.gnome.org/Projects/Tracker")
    (license
     (list
      ;; LibTracker-Extract, LibTracker-Miners-Common
      ;; and Tracker-Extract
      license:lgpl2.1+
      ;; Miners-Apps and Miners-FS
      license:gpl2+))))

(define-public nautilus
  (package
    (name "nautilus")
    (version "3.34.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "19zqwq4qyyncc5wq3xls0f7rsavnw741k336p2h7kx35p4kf41mv"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson/postinstall.py"
               (("gtk-update-icon-cache") "true"))
             #t)))
       ;; XXX: FAIL: check-nautilus
       ;;   Settings schema 'org.gnome.nautilus.preferences' is not installed
       #:tests? #f))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("glib:bin" ,glib "bin")         ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dconf" ,dconf)
       ("gexiv2" ,gexiv2)
       ("gvfs" ,gvfs)
       ("exempi" ,exempi)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-autoar" ,gnome-autoar)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libseccomp" ,libseccomp)
       ("libselinux" ,libselinux)
       ("tracker" ,tracker)
       ("tracker-miners" ,tracker-miners)
       ;; XXX: gtk+ is required by libnautilus-extension.pc
       ;;
       ;; Don't propagate it to reduces "profile pollution" of the 'gnome' meta
       ;; package.  See:
       ;; <http://lists.gnu.org/archive/html/guix-devel/2016-03/msg00283.html>.
       ("gtk+" ,gtk+)
       ("libexif" ,libexif)
       ("libxml2" ,libxml2)))
    (synopsis "File manager for GNOME")
    (home-page "https://wiki.gnome.org/Apps/Nautilus")
    (description
     "Nautilus (Files) is a file manager designed to fit the GNOME desktop
design and behaviour, giving the user a simple way to navigate and manage its
files.")
    (license license:gpl2+)))

(define-public baobab
  (package
    (name "baobab")
    (version "3.34.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://gnome/sources/" name "/"
         (version-major+minor version) "/"
         name "-" version ".tar.xz"))
       (sha256
        (base32 "1i90gc1cpw5507zn54k46hj4mjgdxsq8cvpnlgxq0ksy2d7iv63z"))))
    (build-system meson-build-system)
    (outputs '("out" "help"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/help")
                (string-append help "/share/help"))
               #t))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+-bin" ,gtk+ "bin")
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)))
    (synopsis "Disk Usage Analyzer")
    (description "Baobab scans folders, devices or remote locations and reports
on the disk space consumed by each element.  It provides both a tree-like and a
graphical representation.")
    (home-page "https://wiki.gnome.org/Apps/DiskUsageAnalyzer")
    (license
     (list
      ;; Documentation
      license:fdl1.1+
      ;; Others
      license:gpl2+))))

(define-public gnome-backgrounds
  (package
    (name "gnome-backgrounds")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "02xvfwfi3133mjljl731z2hj9hfmjas3a1jl4fxmy24xrzj83jxq"))))
    (build-system meson-build-system)
    (native-inputs
     `(("intltool" ,intltool)))
    (synopsis "Default wallpaper set for GNOME")
    (description "GNOME backgrounds package contains a set of backgrounds
packaged with the GNOME desktop.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-backgrounds")
    (license
     (list
      license:gpl2+
      license:cc-by2.0
      license:cc-by-sa2.0
      license:cc-by-sa3.0))))

(define-public gnome-screenshot
  (package
    (name "gnome-screenshot")
    (version "3.34.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1rmiq890j7gfn5mcz31xy6jfnnxgc17dq67bhn2k9m5ylbvza2n8"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/postinstall.py"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("intltool" ,intltool)
       ("appstream-glib" ,appstream-glib)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libx11" ,libx11)
       ("libxext" ,libxext)))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-screenshot")
    (synopsis "Take pictures of your screen")
    (description
     "GNOME Screenshot is a utility used for taking screenshots of the entire
screen, a window or a user defined area of the screen, with optional
beautifying border effects.")
    (license license:gpl2+)))

(define-public dconf-editor
  (package
    (name "dconf-editor")
    (version "3.34.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0pwxjada2vaf69ihpjgp9nky54iykvxq63lp1vl8pxjanif2mk6f"))))
    (build-system meson-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, gio-2.0.
       ("gtk+-bin" ,gtk+ "bin") ; for gtk-update-icon-cache
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("dconf" ,dconf)
       ("gtk+" ,gtk+)
       ("libxml2" ,libxml2)))
    (home-page "https://gitlab.gnome.org/GNOME/dconf-editor")
    (synopsis "Graphical editor for GNOME's dconf configuration system")
    (description
     "Dconf-editor is a graphical tool for browsing and editing the dconf
configuration system for GNOME.  It allows users to configure desktop
software that do not provide their own configuration interface.")
    (license license:lgpl2.1+)))

(define-public gnome-default-applications
  (package
    (name "gnome-default-applications")
    (version "0")
    (build-system trivial-build-system)
    (source #f)
    (propagated-inputs
     `(("nautilus" ,nautilus)
       ("evince" ,evince)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (apps (string-append out "/share/applications")))
           (mkdir-p apps)
           (call-with-output-file (string-append apps "/gnome-mimeapps.list")
             (lambda (port)
               (format port "[Default Applications]\n")
               (format port "inode/directory=org.gnome.Nautilus.desktop\n")
               (format port "application/pdf=evince.desktop\n")
               (format port "application/postscript=evince.desktop\n")))
           #t))))
    (synopsis "Default MIME type associations for the GNOME desktop")
    (description
     "Given many installed packages which might handle a given MIME type, a
user running the GNOME desktop probably has some preferences: for example,
that folders be opened by default by the Nautilus file manager, not the Baobab
disk usage analyzer.  This package establishes that set of default MIME type
associations for GNOME.")
    (license license:gpl3+)
    (home-page #f)))

(define-public libgovirt
  (package
   (name "libgovirt")
   (version "0.3.6")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "19pb71pag3vsi83kbv8h08kimwym4hpw36kjl6a5ik5nk50mc8sg"))))
   (build-system glib-or-gtk-build-system)
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
      ("gtk+:bin" ,gtk+ "bin")
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("glib-networking" ,glib-networking) ; GIO plugin--for the tests
      ("librest" ,rest)))
   (synopsis "GoVirt Library")
   (description "GoVirt is a GObject wrapper for the oVirt REST API.")
   (home-page "https://gitlab.gnome.org/GNOME/libgovirt")
   (license license:gpl2+)))

(define-public gnome-weather
  (package
   (name "gnome-weather")
   (version "3.34.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "1g63xzs17i36if923b36k9fwbk0nqa5vz6zh1k6q2axrzhhpx1i4"))))
   (build-system meson-build-system)
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("gtk+:bin" ,gtk+ "bin")
      ("pkg-config" ,pkg-config)))
   (inputs
    `( ;("adwaita-icon-theme" ,adwaita-icon-theme)
      ("appstream-glib" ,appstream-glib)
      ("geoclue" ,geoclue)
      ("gdk-pixbuf" ,gdk-pixbuf)
      ("gjs" ,gjs)
      ("gnome-desktop" ,gnome-desktop)
      ("libgweather" ,libgweather)))
   (arguments
    `(#:glib-or-gtk? #t
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'fix-desktop-file
          ;; FIXME: "gapplication launch org.gnome.Weather" fails for some reason.
          ;; See https://issues.guix.gnu.org/issue/39324.
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (applications (string-append out "/share/applications")))
              (substitute* (string-append applications "/org.gnome.Weather.desktop")
                (("Exec=.*") "Exec=gnome-weather\n"))
              #t)))
        (add-after 'install 'wrap
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out               (assoc-ref outputs "out"))
                  (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
              ;; GNOME Weather needs the typelib files of GTK+, Pango etc at runtime.
              (wrap-program (string-append out "/bin/gnome-weather")
                `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))
              #t))))))
   (synopsis "Weather monitoring for GNOME desktop")
   (description "GNOME Weather is a small application that allows you to
monitor the current weather conditions for your city, or anywhere in the
world.")
   (home-page "https://wiki.gnome.org/Apps/Weather")
   (license license:gpl2+)))

(define-public gnome
  (package
    (name "gnome")
    (version (package-version gnome-shell))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (begin (mkdir %output) #t)))
    (propagated-inputs
     ;; TODO: Add or remove packages according to:
     ;;       <https://calc.disroot.org/2nu6mpf88ynq.html>.
     `(
     ;; GNOME-Core-OS-Services
       ("accountsservice" ,accountsservice)
       ("network-manager" ,network-manager)
       ("packagekit" ,packagekit)
       ("upower" ,upower)
     ;; GNOME-Core-Shell
       ("adwaita-icon-theme" ,adwaita-icon-theme)
       ("gdm" ,gdm)
       ("glib-networking" ,glib-networking)
       ("gnome-backgrounds" ,gnome-backgrounds)
       ("gnome-bluetooth" ,gnome-bluetooth)
       ("gnome-color-manager" ,gnome-color-manager)
       ("gnome-control-center" ,gnome-control-center)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-getting-started-docs" ,gnome-getting-started-docs)
       ("gnome-initial-setup" ,gnome-initial-setup)
       ("gnome-keyring" ,gnome-keyring)
       ("gnome-menus" ,gnome-menus)
       ("gnome-session" ,gnome-session)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("gnome-shell-extensions" ,gnome-shell-extensions)
       ("gnome-shell" ,gnome-shell)
       ("gnome-themes-extra" ,gnome-themes-extra)
       ("gnome-user-docs" ,gnome-user-docs)
       ("gnome-user-share" ,gnome-user-share)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gvfs" ,gvfs)
       ("mutter" ,mutter)
       ("orca" ,orca)
       ("rygel" ,rygel)
       ("sushi" ,sushi)
     ;; GNOME-Core-Utilities
       ("baobab" ,baobab)
       ("cheese" ,cheese)
       ;; XXX: EoG requires librsvg-next, which depends on Rust, which currently
       ;; only works on x86_64, so exclude it on other architectures.
       ,@(if (string-prefix? "x86_64" (%current-system))
             `(("eog" ,eog))
             '())
       ("epiphany" ,epiphany)
       ("evince" ,evince)
       ("file-roller" ,file-roller)
       ("gedit" ,gedit)
       ("gnome-boxes" ,gnome-boxes)
       ("gnome-calculator" ,gnome-calculator)
       ("gnome-calendar" ,gnome-calendar)
       ("gnome-characters" ,gnome-characters)
       ("gnome-clocks" ,gnome-clocks)
       ("gnome-contacts" ,gnome-contacts)
       ("gnome-disk-utility" ,gnome-disk-utility)
       ("gnome-font-viewer" ,gnome-font-viewer)
       ("gnome-maps" ,gnome-maps)
       ; TODO: ("gnome-music" ,gnome-music)
       ; TODO: ("gnome-photos" ,gnome-photos)
       ("gnome-screenshot" ,gnome-screenshot)
       ("gnome-system-monitor" ,gnome-system-monitor)
       ("gnome-terminal" ,gnome-terminal)
       ("gnome-weather" ,gnome-weather)
       ("nautilus" ,nautilus)
       ("simple-scan" ,simple-scan)
       ("totem" ,totem)
       ("yelp" ,yelp)
     ;; Others
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("gnome-online-accounts" ,gnome-online-accounts)

       ;; Packages not part of GNOME proper but that are needed for a good
       ;; experience.  See <https://bugs.gnu.org/39646>.
       ;; XXX: Find out exactly which ones are needed and why.
       ("font-cantarell"            ,font-cantarell)
       ("font-dejavu"               ,font-dejavu)
       ("at-spi2-core"              ,at-spi2-core)
       ("dbus"                      ,dbus)
       ("dconf"                     ,dconf)
       ("desktop-file-utils"        ,desktop-file-utils)
       ("gnome-default-applications" ,gnome-default-applications)
       ("gnome-themes-standard"     ,gnome-themes-standard)
       ("gst-plugins-base"          ,gst-plugins-base)
       ("gst-plugins-good"          ,gst-plugins-good)
       ("gucharmap"                 ,gucharmap)
       ("pinentry-gnome3"           ,pinentry-gnome3)
       ("pulseaudio"                ,pulseaudio)
       ("shared-mime-info"          ,shared-mime-info)
       ("system-config-printer"     ,system-config-printer)
       ("xdg-user-dirs"             ,xdg-user-dirs)
       ("yelp"                      ,yelp)
       ("zenity"                    ,zenity)))
    (synopsis "The GNU desktop environment")
    (home-page "https://www.gnome.org/")
    (description
     "GNOME is the graphical desktop for GNU.  It includes a wide variety of
applications for browsing the web, editing text and images, creating
documents and diagrams, playing media, scanning, and much more.")
    (license license:gpl2+)))

(define-public byzanz
  ;; The last stable release of Byzanz was in 2011, but there have been many
  ;; useful commits made to the Byzanz repository since then that it would be
  ;; silly to use such an old release.
  (let ((commit "f7af3a5bd252db84af8365bd059c117a7aa5c4af"))
    (package
      (name "byzanz")
      (version (string-append "0.2-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.gnome.org/browse/byzanz")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1l60myzxf9cav27v5v3nsijlslz9r7ip6d5kiirfpkf9k0w26hz3"))))
      (build-system glib-or-gtk-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'bootstrap 'build-without-Werror
             (lambda _
               ;; The build system cleverly detects that we're not building from
               ;; a release tarball and turns on -Werror for GCC.
               ;; Unsurprisingly, there is a warning during compilation that
               ;; causes the build to fail unnecessarily, so we remove the flag.
               (substitute* '("configure.ac")
                 (("-Werror") ""))
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("gnome-common" ,gnome-common)
         ("intltool" ,intltool)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)
         ("which" ,which)))
      (inputs
       `(("glib" ,glib)
         ("gstreamer" ,gstreamer)
         ("gst-plugins-base" ,gst-plugins-base)
         ("gtk+" ,gtk+)))
      (synopsis "Desktop recording program")
      (description "Byzanz is a simple desktop recording program with a
command-line interface.  It can record part or all of an X display for a
specified duration and save it as a GIF encoded animated image file.")
      (home-page "https://git.gnome.org/browse/byzanz")
      (license license:gpl2+))))

(define-public gsound
  (package
    (name "gsound")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0lwfwx2c99qrp08pfaj59pks5dphsnxjgrxyadz065d8xqqgza5v"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* "gsound-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("glib" ,glib)
       ("libcanberra" ,libcanberra)))
    (synopsis "GObject wrapper for libcanberra")
    (description "GSound is a small library for playing system sounds.  It's
designed to be used via GObject Introspection, and is a thin wrapper around the
libcanberra C library.")
    (home-page "https://wiki.gnome.org/Projects/GSound")
    (license license:lgpl2.1+)))

(define-public libzapojit
  (package
    (name "libzapojit")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0zn3s7ryjc3k1abj4k55dr2na844l451nrg9s6cvnnhh569zj99x"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* "libzapojit-0.0-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.1.2)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("json-glib" ,json-glib)
       ("libsoup" ,libsoup)
       ("rest" ,rest)))
    (synopsis "REST API Wrapper")
    (description "LibZapojit is a GLib/GObject wrapper for the SkyDrive and
Hotmail REST APIs.")
    (home-page "https://wiki.gnome.org/Projects/Zapojit")
    (license license:lgpl2.1+)))

(define-public gnome-clocks
  (package
    (name "gnome-clocks")
    (version "3.36.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1rjicycgh9jvkqir2m8hx9m4jlaa3w5lqs43k185wa0zxhi1n6zi"))))
    (build-system meson-build-system)
    (outputs '("out" "help"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/post-install.py"
               (("gtk-update-icon-cache")
                "true"))
             #t))
         (add-after 'install 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/help")
                (string-append help "/share/help"))
               #t))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("appstream-util" ,appstream-glib)
       ("geocode-glib" ,geocode-glib)
       ("glib" ,glib)
       ("gnome-desktop" ,gnome-desktop)
       ("gtk+" ,gtk+)
       ("gsound" ,gsound)
       ("libgeoclue" ,geoclue)
       ("libgweather" ,libgweather)
       ("libhandy" ,libhandy)))
    (synopsis "Time for Clocks")
    (description "GNOME-Clocks is a simple clock application for GNOME.
It includes world clocks, alarms, a stopwatch and a timer.")
    (home-page "https://wiki.gnome.org/Apps/Clocks")
    (license license:gpl2+)))

(define-public gnome-calendar
  (package
    (name "gnome-calendar")
    (version "3.36.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "07sc1kn65dzxsxpv0vl5dj1a5awljjsfl9jldrg0hnjmq12m7c6h"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/meson/meson_post_install.py"
               (("gtk-update-icon-cache")
                "true"))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib-bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("evolution-data-server" ,evolution-data-server)
       ("geocode-glib" ,geocode-glib)
       ("glib" ,glib)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("libdazzle" ,libdazzle)
       ("libgeoclue" ,geoclue)
       ("libhandy" ,libhandy)
       ("libical" ,libical)
       ("libsoup", libsoup)
       ("libedataserverui" ,evolution-data-server)
       ("libgweather" ,libgweather)))
    (synopsis "Calendar application for GNOME")
    (description "GNOME Calendar is a simple and beautiful calendar application
designed to perfectly fit the GNOME desktop.  By reusing the components which
the GNOME desktop is build on, Calendar nicely integrates with the GNOME
ecosystem.")
    (home-page "https://wiki.gnome.org/Apps/Calendar")
    (license license:gpl3+)))

(define-public gnome-todo
  (package
    (name "gnome-todo")
    (version "3.28.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "08ygqbib72jlf9y0a16k54zz51sncpq2wa18wp81v46q8301ymy7"))
              (patches
               (search-patches "gnome-todo-delete-esource-duplicate.patch"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases (modify-phases %standard-phases
                  (add-after
                      'install 'wrap-gnome-todo
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out               (assoc-ref outputs "out"))
                            (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                            (python-path       (getenv "PYTHONPATH")))
                        (wrap-program (string-append out "/bin/gnome-todo")
                          ;; XXX: gi plugins are broken.
                          ;; See https://bugzilla.gnome.org/show_bug.cgi?id=787212
                          ;; For plugins.
                          `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                          `("PYTHONPATH" ":" prefix (,python-path))))
                      #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin")         ; For glib-compile-resources
       ("gtk+-bin" ,gtk+ "bin")         ; For gtk-update-icon-cache
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("rest" ,rest)                   ; For Todoist plugin
       ("json-glib" ,json-glib)         ; For Todoist plugin
       ("libedataserverui" ,evolution-data-server)
       ("libical" ,libical)
       ("libpeas" ,libpeas)
       ("python-pygobject" ,python-pygobject)
       ("evolution-data-server" ,evolution-data-server)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (home-page "https://wiki.gnome.org/Apps/Todo")
    (synopsis "GNOME's ToDo Application")
    (description
     "GNOME To Do is a simplistic personal task manager designed to perfectly
fit the GNOME desktop.")
    (license license:gpl3+)))

(define-public gnome-dictionary
  (package
    (name "gnome-dictionary")
    (version "3.26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "007k2bq8iplg4psdpz074r5d4zxvn4s95qym4rw9hs6giljbrf0n"))))
    (build-system meson-build-system)
    (arguments '(#:glib-or-gtk? #t
                 #:phases (modify-phases %standard-phases
                            (add-after 'unpack 'patch-install-script
                              (lambda _
                                ;; This script attempts to compile glib schemas
                                ;; and create an empty MIME database.  We do
                                ;; that elsewhere, so prevent it from running.
                                (substitute* "build-aux/post-install.sh"
                                  (("\\[ -z \"\\$DESTDIR\" \\]") "false"))
                                #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)))
    (home-page "https://wiki.gnome.org/Apps/Dictionary")
    (synopsis "Look up words in dictionary sources")
    (description
     "GNOME Dictionary can look for the definition or translation of a word in
existing databases over the internet.")
    (license license:gpl3+)))

(define-public gnome-tweaks
  (package
    (name "gnome-tweaks")
    (version "3.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-tweaks/"
                                  (version-major+minor version) "/"
                                  "gnome-tweaks-" version ".tar.xz"))
              (patches
               (list (search-patch "gnome-tweaks-search-paths.patch")))
              (sha256
               (base32
                "0l2j42ba7v866iknygamnkiq7igh0fjvq92r93cslvvfnkx2ccq0"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags '("-Dlocalstatedir=/tmp"
                           "-Dsysconfdir=/tmp")
       #:imported-modules ((guix build python-build-system)
                           ,@%meson-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson-postinstall.py"
               (("gtk-update-icon-cache") "true"))
             #t))
         (add-after 'install 'wrap
           (@@ (guix build python-build-system) wrap))
         (add-after 'wrap 'wrap-gi-typelib
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/gnome-tweaks")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gnome-desktop" ,gnome-desktop)
       ("gtk+" ,gtk+)
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libhandy" ,libhandy)
       ("libnotify" ,libnotify)
       ("libsoup" ,libsoup)
       ("nautilus" ,nautilus)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)))
    (synopsis "Customize advanced GNOME 3 options")
    (home-page "https://wiki.gnome.org/Apps/Tweaks")
    (description
     "GNOME Tweaks allows adjusting advanced configuration settings in
GNOME 3.  This includes things like the fonts used in user interface elements,
alternative user interface themes, changes in window management behavior,
GNOME Shell appearance and extension, etc.")
    (license license:gpl3+)))

(define-public gnome-shell-extensions
  (package
    (name "gnome-shell-extensions")
    (version "3.34.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1f5l35l3kdkzrv49xmg1sh11vwmgjbg7hx3gb91i39bfl1snxqd1"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags '("-Dextension_set=all")))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "Extensions for GNOME Shell")
    (description "GNOME Shell extensions modify and extend GNOME Shell
functionality and behavior.")
    (home-page "https://extensions.gnome.org/")
    (license license:gpl3+)))

(define-public arc-theme
  (package
    (name "arc-theme")
    (version "20190917")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NicoHood/arc-theme")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qgpk4p2hi5hd4yy0hj93kq1vs0b32wb8qkaj1wi90c8gwddq5wa"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; autogen.sh calls configure at the end of the script.
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vfi")))
         (add-before 'build 'set-home   ;placate Inkscape
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("glib" ,glib "bin")             ; for glib-compile-resources
       ("gnome-shell" ,gnome-shell)
       ("gtk+" ,gtk+)
       ("inkscape" ,inkscape)
       ("optipng" ,optipng)
       ("pkg-config" ,pkg-config)
       ("sassc" ,sassc/libsass-3.5)))
    (synopsis "A flat GTK+ theme with transparent elements")
    (description "Arc is a flat theme with transparent elements for GTK 3, GTK
2, and GNOME Shell which supports GTK 3 and GTK 2 based desktop environments
like GNOME, Unity, Budgie, Pantheon, XFCE, Mate, etc.")
    (home-page "https://github.com/horst3180/arc-theme")
    ;; No "or later" language found.
    (license license:gpl3+)))

(define-public faba-icon-theme
  (package
    (name "faba-icon-theme")
    (version "4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snwh/faba-icon-theme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xh6ppr73p76z60ym49b4d0liwdc96w41cc5p07d48hxjsa6qd6n"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-post-install
           (lambda _
             (substitute* "meson.build"
               (("meson.add_install_script.*") "")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (synopsis "Faba icon theme")
    (description
     "Faba is a minimal icon set used as a basis for other themes such as
Moka")
    (home-page "https://snwh.org/moka")
    (license (list license:lgpl3+
                   license:cc-by-sa4.0))))

(define-public moka-icon-theme
  (package
    (inherit faba-icon-theme)
    (name "moka-icon-theme")
    (version "5.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snwh/moka-icon-theme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "015l02im4mha5z91dbchxf6xkp66d346bg3xskwg0rh3lglhjsrd"))))
    (propagated-inputs
     ;; Moka is based on Faba by using it as a fallback icon set instead of
     ;; bundling it, so we need to add it as a propagated input.
     `(("faba-icon-theme" ,faba-icon-theme)))
    (synopsis "Moka icon theme")
    (description "Moka is a stylized desktop icon set, designed to be clear,
simple and consistent.")
    (license (list license:gpl3+
                   license:cc-by-sa4.0))))

(define-public arc-icon-theme
  (package
    (name "arc-icon-theme")
    (version "20161122")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/horst3180/arc-icon-theme")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ch3hp08qri93510hypzz6m2x4xgg2h15wvnhjwh1x1s1b7jvxjd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-configure-during-bootstrap
           (lambda _
             (substitute* "autogen.sh"
               (("^\"\\$srcdir/configure\".*") ""))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    ;; When Arc is missing an icon, it looks in the Moka icon theme for it.
    (propagated-inputs
     `(("moka-icon-theme" ,moka-icon-theme)))
    (synopsis "Arc icon theme")
    (description "The Arc icon theme provides a set of icons matching the
style of the Arc GTK theme.  Icons missing from the Arc theme are provided by
the Moka icon theme.")
    (home-page "https://github.com/horst3180/arc-icon-theme")
    (license license:gpl3+)))

(define-public folks
  (package
    (name "folks")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/folks/"
                       (version-major+minor version) "/"
                       "folks-" version ".tar.xz"))
       (sha256
        (base32 "1f9b52vmwnq7s51vj26w2618dn2ph5g12ibbkbyk6fvxcgd7iryn"))))
    (build-system meson-build-system)
    (outputs '("out" "doc" "help"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dtracker_backend=true"
        "-Dzeitgeist=true"
        "-Ddocs=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           ;; Telepathy and Tracker tests require networking.
           (lambda _
             (substitute* "tests/meson.build"
               (("subdir\\('telepathy'\\)")
                "")
               (("subdir\\('tracker'\\)")
                ""))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/devhelp")
                (string-append help "/share/devhelp"))
               #t))))))
    (native-inputs
     `(("cmake" ,cmake)
       ("docbook-xml" ,docbook-xml-4.3)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("gtk+:bin" ,gtk+ "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("python-dbusmock" ,python-dbusmock)
       ("vala" ,vala)))
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("libxml2" ,libxml2)
       ("readline" ,readline)
       ("zeitgeist" ,zeitgeist)))
    (propagated-inputs
     `(("evolution-data-server" ,evolution-data-server)
       ("glib" ,glib)
       ("gee" ,libgee)
       ("telepathy-glib" ,telepathy-glib)
       ("tracker-sparql" ,tracker)))
    (synopsis "Contact aggregation library")
    (description "Libfolks is a library that aggregates people from multiple
sources (eg, Telepathy connection managers for IM contacts, Evolution Data
Server for local contacts, etc.) to create metacontacts.  It's written in Vala,
which generates C code when compiled.")
    (home-page "https://wiki.gnome.org/Projects/Folks")
    (license license:lgpl2.1+)))

(define-public gfbgraph
  (package
    (name "gfbgraph")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/gfbgraph/"
                    (version-major+minor version) "/"
                    "gfbgraph-" version ".tar.xz"))
              (sha256
               (base32
                "0yck7dwvjk16a52nafjpi0a39rxwmg0w833brj45acz76lgkjrb0"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f                      ; Tests require networking
       #:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        "--enable-introspection"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* "gfbgraph-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("json-glib" ,json-glib)
       ("libsoup" ,libsoup)
       ("rest" ,rest)))
    (synopsis "GObject library for Facebook Graph API")
    (description "GFBGraph is a GLib/GObject wrapper for the Facebook API.")
    (home-page "https://wiki.gnome.org/Projects/GFBGraph")
    (license license:lgpl2.1+)))

(define-public libgnomekbd
  (package
    (name "libgnomekbd")
    (version "3.26.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/libgnomekbd/"
                       (version-major+minor version)  "/"
                       "libgnomekbd-" version ".tar.xz"))
       (sha256
        (base32 "0y962ykn3rr9gylj0pwpww7bi20lmhvsw6qvxs5bisbn2mih5jpp"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static")))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libxklavier" ,libxklavier)))
    (synopsis "GNOME keyboard configuration library")
    (description "Libgnomekbd is a keyboard configuration library for the
GNOME desktop environment, which can notably display keyboard layouts.")
    (home-page "https://gitlab.gnome.org/GNOME/libgnomekbd")
    (license license:lgpl2.0+)))

;;; This package is no longer maintained:
;;; https://wiki.gnome.org/Attic/LibUnique
;;; "Unique is now in maintenance mode, and its usage is strongly discouraged.
;;; Applications should use the GtkApplication class provided by GTK+ 3.0."
(define-public libunique
  (package
    (name "libunique")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0f70lkw66v9cj72q0iw1s2546r6bwwcd8idcm3621fg2fgh2rw58"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags '("--disable-static"
                           "--disable-dbus" ; use gdbus
                           "--enable-introspection")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin")
       ("gtk-doc" ,gtk-doc)))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     `(("gtk+" ,gtk+)))
    (home-page "https://wiki.gnome.org/Attic/LibUnique")
    (synopsis "Library for writing single instance applications")
    (description
     "Libunique is a library for writing single instance applications.  If you
launch a single instance application twice, the second instance will either just
quit or will send a message to the running instance.  Libunique makes it easy to
write this kind of application, by providing a base class, taking care of all
the IPC machinery needed to send messages to a running instance, and also
handling the startup notification side.")
    (license license:lgpl2.1+)))

(define-public gnome-calculator
  (package
    (name "gnome-calculator")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1cqd4b25qp1i0p04m669jssg1l5sdapc1mniv9jssvw5r7wk1s52"))))
    (build-system meson-build-system)
    (outputs '("out" "help"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache")
                "true"))
             #t))
         (add-after 'install 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/help")
                (string-append help "/share/help"))
               (rename-file
                (string-append out "/share/devhelp")
                (string-append help "/share/devhelp"))
               #t))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("gtksourceview" ,gtksourceview)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("mpc" ,mpc)
       ("mpfr" ,mpfr)))
    (propagated-inputs
     `(("gee" ,libgee)
       ("glib" ,glib)
       ("gtk+" ,gtk+)))
    (synopsis "GNOME Calculator")
    (description "GNOME-Calculator is an application that solves mathematical
equations and is suitable as a default application in a Desktop environment.")
    (home-page "https://wiki.gnome.org/Apps/Calculator")
    (license license:gpl3+)))

(define-public xpad
  (package
    (name "xpad")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/xpad/trunk/"
                           version "/+download/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32
         "0l0g5x8g6dwhf5ksnqqrjjsycy57kcvdslkmsr6bl3vrsjd7qml3"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gtk+:bin" ,gtk+ "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtksourceview" ,gtksourceview-3)
       ("libsm" ,libsm)))
    (home-page "https://wiki.gnome.org/Apps/Xpad")
    (synopsis "Virtual sticky note")
    (description
     "Xpad is a sticky note that strives to be simple, fault tolerant,
and customizable.  Xpad consists of independent pad windows, each is
basically a text box in which notes can be written.")
    (license license:gpl3+)))

(define-public gucharmap
  (let ((unicode-files
         '(("Blocks.txt"
            "041sk54v6rjzb23b9x7yjdwzdp2wc7gvfz7ybavgg4gbh51wm8x1")
           ("DerivedAge.txt"
            "04j92xp07v273z3pxkbfmi1svmw9kmnjl9nvz9fv0g5ybk9zk7r6")
           ("NamesList.txt"
            "0vsq8gx7hws8mvxy3nlglpwxw7ky57q0fs09d7w9xgb2ylk7fz61")
           ("Scripts.txt"
            "18c63hx4y5yg408a8d0wx72d2hfnlz4l560y1fsf9lpzifxpqcmx")
           ("UnicodeData.txt"
            "07d1kq190kgl92ispfx6zmdkvwvhjga0ishxsngzlw8j3kdkz4ap")
           ("Unihan.zip"
            "1kfdhgg2gm52x3s07bijb5cxjy0jxwhd097k5lqhvzpznprm6ibf"))))
    (package
      (name "gucharmap")
      (version "12.0.1")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "mirror://gnome/sources/" name "/"
                             (version-major+minor version) "/"
                             name "-" version ".tar.xz"))
         (sha256
          (base32
           "0m915hm2b2d6r3vs1l80rqpssvg78pv8j6nv54yg62kzknnqmpir"))))
      (build-system glib-or-gtk-build-system)
      (arguments
       `(#:modules ((ice-9 match)
                    (guix build glib-or-gtk-build-system)
                    (guix build utils))
         #:configure-flags
         (list "--with-unicode-data=../unicode-data")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'prepare-unicode-data
             (lambda* (#:key inputs #:allow-other-keys)
               (mkdir-p "../unicode-data")
               (with-directory-excursion "../unicode-data"
                 (for-each (match-lambda
                             ((file _)
                              (install-file (assoc-ref inputs file) ".")))
                           ',unicode-files))
               #t)))))
      (native-inputs
       `(("desktop-file-utils" ,desktop-file-utils)
         ("glib:bin" ,glib "bin")       ; for glib-compile-resources.
         ("gobject-introspection" ,gobject-introspection)
         ("intltool" ,intltool)
         ("itstool" ,itstool)
         ("pkg-config" ,pkg-config)
         ,@(map (match-lambda
                  ((file hash)
                   `(,file
                     ,(origin
                        (method url-fetch)
                        (uri (string-append
                              "http://www.unicode.org/Public/12.0.0/ucd/"
                              file))
                        (sha256 (base32 hash))))))
                unicode-files)
         ("unzip" ,unzip)))
      (inputs
       `(("gtk+" ,gtk+)
         ("xmllint" ,libxml2)))
      (home-page "https://wiki.gnome.org/Apps/Gucharmap")
      (synopsis "Unicode character picker and font browser")
      (description
       "This program allows you to browse through all the available Unicode
characters and categories for the installed fonts, and to examine their
detailed properties.  It is an easy way to find the character you might
only know by its Unicode name or code point.")
      (license license:gpl3+))))

(define-public bluefish
  (package
    (name "bluefish")
    (version "2.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.bennewitz.com/bluefish/stable/source/"
                           "bluefish-" version ".tar.gz"))
       (sha256
        (base32 "0a7kf78q4cj2ap4igjks9kbmmr74brsrl4y2f9wbxpl0b0v2ck2x"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("enchant" ,enchant)
       ("gtk+" ,gtk+)
       ("python" ,python-wrapper)
       ("xmllint" ,libxml2)
       ("gucharmap" ,gucharmap)))
    (home-page "http://bluefish.openoffice.nl")
    (synopsis "Web development studio")
    (description
     "Bluefish is an editor targeted towards programmers and web developers,
with many options to write web sites, scripts and other code.
Bluefish supports many programming and markup languages.")
    (license license:gpl3+)))

(define-public gnome-system-monitor
  (package
    (name "gnome-system-monitor")
    (version "3.32.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1wd43qdgjav6xamq5z5cy8fri5zr01jga3plc9w95gcia0rk3ha8"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:configure-flags '("-Dsystemd=false")))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums.
       ("gtk+" ,gtk+ "bin") ; gtk-update-icon-cache
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("libgtop" ,libgtop)
       ("polkit" ,polkit)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf) ; for loading SVG files.
       ("gtk+" ,gtk+)
       ("gtkmm" ,gtkmm)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)
       ("libwnck" ,libwnck)))
    (home-page "https://wiki.gnome.org/Apps/SystemMonitor")
    (synopsis "Process viewer and system resource monitor for GNOME")
    (description
     "GNOME System Monitor is a GNOME process viewer and system monitor with
an attractive, easy-to-use interface.  It has features, such as a tree view
for process dependencies, icons for processes, the ability to hide processes,
graphical time histories of CPU/memory/swap usage and the ability to
kill/reinice processes.")
    (license license:gpl2+)))

(define-public python-pyatspi
  (package
    (name "python-pyatspi")
    (version "2.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/pyatspi/"
                    (version-major+minor version)
                    "/pyatspi-" version ".tar.xz"))
              (sha256
               (base32
                "0j3f75j0zd6ca8msg7yr19qsfirqkn9fk8pqbjnlhqrpri455g4p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-atk-load
           (lambda _
             (substitute* "pyatspi/__init__.py"
               (("from gi.repository import Atspi")
                "gi.require_version('Gtk', '3.0')
from gi.repository import Gtk
from gi.repository import Atspi"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python)))
    (propagated-inputs
     `(("python-pygobject" ,python-pygobject)))
    (synopsis "Python client bindings for D-Bus AT-SPI")
    (home-page "https://wiki.linuxfoundation.org/accessibility\
/atk/at-spi/at-spi_on_d-bus")
    (description
     "This package includes a python client library for the AT-SPI D-Bus
accessibility infrastructure.")
    (license license:lgpl2.0)
    (properties '((upstream-name . "pyatspi")))))

(define-public python2-pyatspi
  (package
    (inherit python-pyatspi)
    (name "python2-pyatspi")
    (inputs
     `(("python" ,python-2)))
    (propagated-inputs
     `(("python-pygobject" ,python2-pygobject)))))

(define-public orca
  (package
    (name "orca")
    (version "3.34.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1q38n7hyshkiszmn361skxjynxr31lcms7a1iny6d0zlpmh1vnk4"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'qualify-xkbcomp
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xkbcomp (string-append
                             (assoc-ref inputs "xkbcomp") "/bin/xkbcomp")))
               (substitute* "src/orca/orca.py"
                 (("'xkbcomp'") (format #f "'~a'" xkbcomp))))
             #t))
         (add-after 'install 'wrap-orca
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (prog (string-append out "/bin/orca")))
               (wrap-program prog
                 `("GI_TYPELIB_PATH" ":" prefix
                   (,(getenv "GI_TYPELIB_PATH")))
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix
                   (,(getenv "GST_PLUGIN_SYSTEM_PATH")))
                 `("PYTHONPATH" ":" prefix
                   (,(getenv "PYTHONPATH")))))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("at-spi2-atk" ,at-spi2-atk)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gtk+" ,gtk+)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)
       ("python-pyatspi" ,python-pyatspi)
       ("python-speechd" ,speech-dispatcher)
       ("xkbcomp" ,xkbcomp)))
    (synopsis
     "Screen reader for individuals who are blind or visually impaired")
    (home-page "https://wiki.gnome.org/Projects/Orca")
    (description
     "Orca is a screen reader that provides access to the graphical desktop
via speech and refreshable braille.  Orca works with applications and toolkits
that support the Assistive Technology Service Provider Interface (AT-SPI).")
    (license license:lgpl2.1+)))

(define-public gspell
  (package
    (name "gspell")
    (version "1.8.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1s1dns070pz8dg04ppshdbx1r86n9406vkxcfs8hdghn0bfi9ras"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* '("gspell-docs.xml.in" "intro.xml.in")
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'patch-docbook-xml 'disable-failing-tests
           (lambda _
             (substitute* "testsuite/test-checker.c"
               (("g_test_add_func \\(\"/checker/dashes\", test_dashes\\);")
                ""))
             #t))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; For the missing /etc/machine-id.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             ;; Allow Enchant and its Aspell backend to find the en_US
             ;; dictionary.
             (setenv "ASPELL_DICT_DIR"
                     (string-append (assoc-ref inputs "aspell-dict-en")
                                    "/lib/aspell"))
             #t)))))
    (native-inputs
     `(("aspell-dict-en" ,aspell-dict-en)
       ("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("iso-codes" ,iso-codes)))
    (propagated-inputs
     `(("enchant" ,enchant)
       ("glib" ,glib)
       ("gtk+" ,gtk+)))
    (synopsis "Spell-checking library for GTK+ applications")
    (description "GSpell provides a flexible API to add spell-checking to a GTK+
application.")
    (home-page "https://wiki.gnome.org/Projects/gspell")
    (license license:lgpl2.1+)))

(define-public gnome-planner
  (package
    (name "gnome-planner")
    (version "0.14.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/planner/"
                                  (version-major+minor version) "/planner-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "15h6ps58giy5r1g66sg1l4xzhjssl362mfny2x09khdqsvk2j38k"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     ;; Disable the Python bindings because the Planner program functions
     ;; without them, and (as of 2017-06-13) we have not packaged all of
     ;; packages that are necessary for building the Python bindings.
     `(#:configure-flags
       (list "--disable-python"
             ,@(if (string=? "aarch64-linux" (%current-system))
                   '("--build=aarch64-unknown-linux-gnu")
                   '()))))
    (inputs
     `(("libgnomecanvas" ,libgnomecanvas)
       ("libgnomeui" ,libgnomeui)
       ("libglade" ,libglade)
       ("gnome-vfs" ,gnome-vfs)
       ("gconf" ,gconf)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("gtk+" ,gtk+)
       ("glib" ,glib)))
    (native-inputs
     `(("intltool" ,intltool)
       ("scrollkeeper" ,scrollkeeper)
       ("pkg-config" ,pkg-config)))
    (home-page "https://wiki.gnome.org/Apps/Planner")
    (synopsis "Project management software for the GNOME desktop")
    (description
     "GNOME Planner is a project management tool based on the Work Breakdown
Structure (WBS).  Its goal is to enable you to easily plan projects.  Based on
the resources, tasks, and constraints that you define, Planner generates
various views into a project.  For example, Planner can show a Gantt chart of
the project.  It can show a detailed summary of tasks including their
duration, cost, and current progress.  It can also show a report of resource
utilization that highlights under-utilized and over-utilized resources.  These
views can be printed as PDF or PostScript files, or exported to HTML.")
    (license license:gpl2+)))

(define-public lollypop
  (package
    (name "lollypop")
    (version "1.2.32")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://adishatz.org/lollypop/"
                           "lollypop-" version ".tar.xz"))
       (sha256
        (base32 "1ng9492k8754vlqggbfsyzbmfdx4w17fzc4ad21fr92710na0w5a"))))
    (build-system meson-build-system)
    (arguments
     `(#:imported-modules
       (,@%meson-build-system-modules
        (guix build python-build-system))
       #:modules
       ((guix build meson-build-system)
        ((guix build python-build-system) #:prefix python:)
        (guix build utils))
       #:glib-or-gtk? #t
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/lollypop")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
             #t))
         (add-after 'install 'wrap-python
           (assoc-ref python:%standard-phases 'wrap)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib:bin" ,glib "bin")         ; For glib-compile-resources
       ("gtk+:bin" ,gtk+ "bin")         ; For gtk-update-icon-cache
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libnotify" ,libnotify)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("python" ,python)
       ("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-gst" ,python-gst)
       ("python-pil" ,python-pillow)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("python-pylast" ,python-pylast)
       ("totem-pl-parser" ,totem-pl-parser)
       ("webkitgtk" ,webkitgtk)))
    (propagated-inputs
     `(;; gst-plugins-base is required to start Lollypop,
       ;; the others are required to play streaming.
       ("gst-plugins-good" ,gst-plugins-good)
       ("gst-plugins-ugly" ,gst-plugins-ugly)))
    (home-page "https://wiki.gnome.org/Apps/Lollypop")
    (synopsis "GNOME music playing application")
    (description
     "Lollypop is a music player designed to play well with GNOME desktop.
Lollypop plays audio formats such as mp3, mp4, ogg and flac and gets information
from artists and tracks from the web.  It also fetches cover artworks
automatically and it can stream songs from online music services and charts.")
    (license license:gpl3+)))

(define-public gnome-video-effects
  (package
    (name "gnome-video-effects")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/" name "-"
                       version ".tar.xz"))
       (sha256
        (base32 "1j6h98whgkcxrh30bwvnxvyqxrxchgpdgqhl0j71xz7x72dqxijd"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))   ; To wrap binaries and/or compile schemas
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("perl" ,perl)
       ("perl-xml-parser" ,perl-xml-parser)
       ("pkg-config" ,pkg-config)))
    (synopsis "GNOME Video Effects")
    (description "GNOME-Video-Effects is a collection of GStreamer effects to be
used in different GNOME Modules.")
    (home-page "https://wiki.gnome.org/Projects/GnomeVideoEffects")
    (license license:gpl2+)))

(define-public cheese
  (package
    (name "cheese")
    (version "3.34.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/" name "-"
                       version ".tar.xz"))
       (sha256
        (base32 "0wvyc9wb0avrprvm529m42y5fkv3lirdphqydc9jw0c8mh05d1ni"))))
    (build-system meson-build-system)
    (outputs '("out" "help" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/reference"
               (substitute* '("cheese-docs.xml" "cheese.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'configure 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache")
                (which "true")))
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/help")
                (string-append help "/share/help"))
               #t)))
         (add-after 'move-doc 'wrap-cheese
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
               (wrap-program (string-append out "/bin/cheese")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (inputs
     `(("dbus" ,dbus)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-video-effects" ,gnome-video-effects)
       ("libcanberra" ,libcanberra)
       ("xext" ,libxext)
       ("xtst" ,libxtst)))
    (propagated-inputs
     `(("clutter" ,clutter)
       ("clutter-gtk" ,clutter-gtk)
       ("clutter-gst" ,clutter-gst)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-bad" ,gst-plugins-bad)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gtk+" ,gtk+)
       ("x11" ,libx11)))
    (synopsis "Webcam application")
    (description "Cheese is a program for the GNOME Desktop which allows you to
take photos, videos, and anything else you can think of with your webcam.")
    (home-page "https://wiki.gnome.org/Apps/Cheese")
    (license license:gpl2+)))

(define-public sound-juicer
  (package
    (name "sound-juicer")
    (version "3.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "19qg4xv0f9rkq34lragkmhii1llxsa87llbl28i759b0ks4f6sny"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gtk+" ,gtk+)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("iso-codes" ,iso-codes)
       ("libbrasero-media3" ,brasero)
       ("libcanberra" ,libcanberra)
       ("libdiscid" ,libdiscid)
       ("libmusicbrainz" ,libmusicbrainz)
       ("neon" ,neon)))
    (home-page "https://wiki.gnome.org/Apps/SoundJuicer")
    (synopsis "Audio music cd ripper")
    (description "Sound Juicer extracts audio from compact discs and convert it
into audio files that a personal computer or digital audio player can play.
It supports ripping to any audio codec supported by a GStreamer plugin, such as
mp3, Ogg Vorbis and FLAC")
    (license license:gpl2+)))

(define-public soundconverter
  (package
    (name "soundconverter")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/soundconverter/trunk/"
                           version "/+download/"
                           "soundconverter-" version ".tar.xz"))

       (sha256
        (base32 "1jv8m82hi23ilrgdznlc1jhp2jm8bw1yrw0chh3qw2l0sixvkl11"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           (guix build glib-or-gtk-build-system)
                           ,@%gnu-build-system-modules)

       #:modules ((guix build glib-or-gtk-build-system)
                  (guix build utils)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  ((guix build python-build-system) #:prefix python:))

       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-soundconverter-for-python
           (assoc-ref python:%standard-phases 'wrap))
         (add-after 'install 'wrap-soundconverter
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                   (gst-plugin-path   (getenv "GST_PLUGIN_SYSTEM_PATH")))
               (wrap-program (string-append out "/bin/soundconverter")
                 `("GI_TYPELIB_PATH"        ":" prefix (,gi-typelib-path))
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin")))
    (inputs
     `(("gtk+" ,gtk+)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (home-page "https://soundconverter.org/")
    (synopsis "Convert between audio formats with a graphical interface")
    (description
     "SoundConverter supports converting between many audio formats including
Opus, Ogg Vorbis, FLAC and more.  It supports parallel conversion, and
configurable file renaming. ")
    (license license:gpl3)))

(define-public workrave
  (package
    (name "workrave")
    (version "1.10.42")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rcaelers/workrave")
             (commit (string-append "v" (string-map
                                         (match-lambda (#\. #\_) (chr chr))
                                         version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03i9kk8r1wgrfkkbwikx8wxaw4r4kn62vismr2zdq5g34fkkjh95"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     ;; The only tests are maintainer tests (in po/), which fail.
     `(#:tests? #f))
    (inputs `(("glib" ,glib)
              ("gtk+" ,gtk+)
              ("gdk-pixbuf" ,gdk-pixbuf)
              ("gtkmm" ,gtkmm)
              ("glibmm" ,glibmm)
              ("libx11" ,libx11)
              ("libxtst" ,libxtst)
              ("dconf" ,dconf)
              ("libice" ,libice)
              ("libsm" ,libsm)
              ("libxscrnsaver" ,libxscrnsaver)))
    (native-inputs `(("boost" ,boost)
                     ("pkg-config" ,pkg-config)
                     ("gettext" ,gettext-minimal)
                     ("autoconf" ,autoconf)
                     ("autoconf-archive" , autoconf-archive)
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("intltool" ,intltool)
                     ("gobject-introspection" ,gobject-introspection)
                     ("python3" ,python-3)
                     ("python-jinja2" ,python-jinja2)))
    (synopsis "Tool to help prevent repetitive strain injury (RSI)")
    (description
     "Workrave is a program that assists in the recovery and prevention of
repetitive strain injury (@dfn{RSI}).  The program frequently alerts you to take
micro-pauses and rest breaks, and restricts you to your daily limit.")
    (home-page "http://www.workrave.org")
    (license license:gpl3+)))

(define-public ghex
  (package
    (name "ghex")
    (version "3.18.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/ghex/"
                                  (version-major+minor version) "/"
                                  "ghex-" version ".tar.xz"))
              (sha256
               (base32
                "1h1pjrr9wynclfykizqd78dbi785wjz6b63p31k87kjvzy8w3nf2"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas
       ("gnome-common" ,gnome-common)
       ("which" ,which)
       ("intltool" ,intltool)
       ("yelp-tools" ,yelp-tools)
       ("desktop-file-utils" ,desktop-file-utils))) ; for 'desktop-file-validate'
    (inputs
     `(("atk" ,atk)
       ("gtk" ,gtk+)))
    (synopsis "GNOME hexadecimal editor")
    (description "The GHex program can view and edit files in two ways:
hexadecimal or ASCII.  It is useful for editing binary files in general.")
    (home-page "https://wiki.gnome.org/Apps/Ghex")
    (license license:gpl2)))

(define-public libdazzle
  (package
    (name "libdazzle")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/libdazzle/"
                       (version-major+minor version) "/"
                       "libdazzle-" version ".tar.xz"))
       (sha256
        (base32 "0n6r16a07in82cnzw91vl675pbjzbvazkxwbqxq2kihganzipcw2"))))
    (build-system meson-build-system)
    (outputs '("out" "tools" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Denable_rdtscp=true"
        "-Denable_gtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "dazzle-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'move-tools
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (tools (assoc-ref outputs "tools")))
               (mkdir-p (string-append tools "/bin"))
               (rename-file
                (string-append out "/bin")
                (string-append tools "/bin"))
               #t))))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vapigen" ,vala)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("glib" ,glib)))
    (propagated-inputs
     `(("gtk+" ,gtk+)))
    (synopsis "Companion library to GObject and Gtk+")
    (description "LibDazzle is a companion library to GObject and Gtk+.  It
provides various features that the authors wish were in the underlying library
but cannot for various reasons.  In most cases, they are wildly out of scope for
those libraries.  In other cases, they are not quite generic enough to work for
everyone.")
    (home-page "https://gitlab.gnome.org/GNOME/libdazzle")
    (license license:gpl3+)))

(define-public evolution
  (package
    (name "evolution")
    (version "3.34.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/evolution/"
                                  (version-major+minor version) "/"
                                  "evolution-" version ".tar.xz"))
              (sha256
               (base32
                "164vy8h432pjglafn8y2ms4gsvk3kbgc63h5qp0mk5dv4smsp29c"))))
    (build-system cmake-build-system)
    (arguments
     `(#:imported-modules (,@%cmake-build-system-modules
                           (guix build glib-or-gtk-build-system))
       #:modules ((guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:configure-flags
       (list "-DENABLE_PST_IMPORT=OFF"    ; libpst is not packaged
             "-DENABLE_LIBCRYPTUI=OFF")   ; libcryptui hasn't seen a release
                                          ; in four years and cannot be built.
       #:phases
       (modify-phases %standard-phases
         ;; The build system attempts to install user interface modules to the
         ;; output directory of the "evolution-data-server" package.  This
         ;; change redirects that change.
         (add-after 'unpack 'patch-ui-module-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/modules/alarm-notify/CMakeLists.txt"
               (("\\$\\{edsuimoduledir\\}")
                (string-append (assoc-ref outputs "out")
                               "/lib/evolution-data-server/ui-modules")))
             #t))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     `(("glib" ,glib "bin")             ; glib-mkenums
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("itstool" ,itstool)))
    (inputs
     `(("enchant" ,enchant)
       ("evolution-data-server" ,evolution-data-server) ; must be the same version
       ("gcr" ,gcr)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gnome-autoar" ,gnome-autoar)
       ("gnome-desktop" ,gnome-desktop)
       ("gspell" ,gspell)
       ("highlight" ,highlight)
       ("libcanberra" ,libcanberra)
       ("libgweather" ,libgweather)
       ("libnotify" ,libnotify)
       ("libsoup" ,libsoup)
       ("nss" ,nss)
       ("openldap" ,openldap)
       ("webkitgtk" ,webkitgtk)
       ("ytnef" ,ytnef)))
    (home-page "https://gitlab.gnome.org/GNOME/evolution")
    (synopsis "Manage your email, contacts and schedule")
    (description "Evolution is a personal information management application
that provides integrated mail, calendaring and address book
functionality.")
    ;; See COPYING for details.
    (license (list license:lgpl2.1 license:lgpl3 ; either one of these
                   license:openldap2.8 ; addressbook/gui/component/openldap-extract.h
                   license:lgpl2.1+))))  ; smime/lib/*

(define-public gthumb
  (package
    (name "gthumb")
    (version "3.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gthumb/"
                                  (version-major+minor version) "/"
                                  "gthumb-" version ".tar.xz"))
              (sha256
               (base32
                "15wqks35ks5dm7zj046dfd45vvrilan2ayfy2sxiprv7q74cip2q"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       ;; Ensure the RUNPATH contains all installed library locations.
       (list (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/gthumb/extensions")
             (string-append "-Dcpp_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/gthumb/extensions"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin")                   ; for glib-compile-resources
       ("gtk+:bin" ,gtk+ "bin")                   ; for gtk-update-icon-cache
       ("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("intltool" ,intltool)
       ("itstool" ,itstool)))
    (inputs
     `(("exiv2" ,exiv2)
       ("gtk" ,gtk+)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gstreamer" ,gstreamer)
       ("clutter" ,clutter)
       ("clutter-gst" ,clutter-gst)
       ("clutter-gtk" ,clutter-gtk)
       ("libjpeg" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("libraw" ,libraw)))
    (home-page "https://wiki.gnome.org/Apps/Gthumb")
    (synopsis "GNOME image viewer and browser")
    (description "GThumb is an image viewer, browser, organizer, editor and
advanced image management tool")
    (license license:gpl2+)))

(define-public terminator
  (package
    (name "terminator")
    (version "1.91")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/" name "/"
                                  "gtk3/" version "/" "+download/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sdyqwydmdnh7j6mn74vrywz35m416kqsbxbrqcnv5ak08y6xxwm"))))
    (build-system python-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("glib:bin" ,glib "bin")                   ; for glib-compile-resources
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("python2-pycairo" ,python2-pycairo)
       ("python2-pygobject" ,python2-pygobject)
       ("python2-psutil" ,python2-psutil)
       ("vte" ,vte)))
    (arguments
     `(#:python ,python-2                          ; Python 3 isn't supported
       #:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
       #:modules ((guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after
          'install 'wrap-program
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((prog (string-append (assoc-ref outputs "out")
                                       "/bin/terminator")))
              (wrap-program prog
                `("PYTHONPATH" = (,(getenv "PYTHONPATH")))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
              #t)))
         (add-after 'wrap-program 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (home-page "https://gnometerminator.blogspot.com/")
    (synopsis "Store and run multiple GNOME terminals in one window")
    (description
     "Terminator allows you to run multiple GNOME terminals in a grid and
+tabs, and it supports drag and drop re-ordering of terminals.")
    (license license:gpl2)))

(define-public libhandy
  (package
    (name "libhandy")
    (version "0.0.13")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.gnome.org/GNOME/libhandy.git")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y23k623sjkldfrdiwfarpchg5mg58smcy1pkgnwfwca15wm1ra5"))))
    (build-system meson-build-system)
    (outputs '("out" "examples" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute*
                   '("build-howto.xml"
                     "handy-docs.xml"
                     "visual-index.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'patch-docbook-xml 'patch-glade
           (lambda _
             (substitute* "glade/meson.build"
               (("gladeui_dep\\.get_pkgconfig_variable\\('moduledir'\\)")
                "libdir / 'glade' / 'modules'")
               (("gladeui_dep\\.get_pkgconfig_variable\\('catalogdir'\\)")
                "datadir / 'glade' / 'catalogs'"))
             (substitute* '("glade/glade-hdy-header-group.c"
                            "glade/glade-hdy-swipe-group.c")
               (("GPC_OBJECT_DELIMITER")
                "G_STR_DELIMITERS"))
             #t))
         (add-before 'configure 'disable-failing-tests
           (lambda _
             (substitute* "tests/meson.build"
               (("'test-avatar',")
                ""))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             #t))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t)))
         (add-after 'move-doc 'move-examples
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (examples (assoc-ref outputs "examples")))
               (mkdir-p (string-append examples "/bin"))
               (rename-file
                (string-append out "/bin")
                (string-append examples "/bin"))
               #t))))))
    (native-inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)
       ("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("glade" ,glade)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)))
    (synopsis "Building blocks for modern adaptive GNOME apps")
    (description "LibHandy aims to help with developing UI for mobile devices
using GTK/GNOME.")
    (home-page "https://gitlab.gnome.org/GNOME/libhandy")
    (license license:lgpl2.1+)))

(define-public libgit2-glib
  (package
    (name "libgit2-glib")
    (version "0.99.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1pmrcnsa7qdda73c3dxf47733mwprmj5ljpw3acxbj6r8k27anp0"))))
    (build-system meson-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ;; For glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("python-pygobject" ,python-pygobject)
       ("python-wrapper" ,python-wrapper)
       ("vala" ,vala)))
    (inputs
     `(("glib" ,glib)
       ("libssh2" ,libssh2)))
    (propagated-inputs
     `(;; In Requires of libgit2-glib.pc.
       ("libgit2" ,libgit2)))
    (synopsis "GLib wrapper around the libgit2 Git access library")
    (description "libgit2-glib is a GLib wrapper library around the libgit2 Git
access library.  It only implements the core plumbing functions, not really the
higher level porcelain stuff.")
    (home-page "https://wiki.gnome.org/Projects/Libgit2-glib")
    (license license:gpl2+)))

(define-public gitg
  (package
    (name "gitg")
    (version "3.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0npg4kqpwl992fgjd2cn3fh84aiwpdp9kd8z7rw2xaj2iazsm914"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-post-install-partially
           (lambda _
             (substitute* "meson_post_install.py"
               (("'python'") ; there are no python sources to compile
                (string-append "'" (which "true") "'"))
               (("gtk-update-icon-cache") (which "true")))
             #t))
         (add-after 'unpack 'fix-test-sources
           (lambda _
             (substitute* "tests/libgitg/test-commit.vala"
               (("/bin/bash") (which "bash")))
             #t))
         (add-after 'glib-or-gtk-wrap 'wrap-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/gitg")))
               (wrap-program prog
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
               #t))))))
    (inputs
     `(("glib" ,glib)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("gtkspell3" ,gtkspell3)
       ("gtksourceview" ,gtksourceview-3)
       ("json-glib" ,json-glib)
       ("libdazzle" ,libdazzle)
       ("libgee" ,libgee)
       ("libgit2-glib" ,libgit2-glib)
       ("libpeas" ,libpeas)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (synopsis "Graphical user interface for git")
    (description
     "gitg is a graphical user interface for git.  It aims at being a small,
fast and convenient tool to visualize the history of git repositories.
Besides visualization, gitg also provides several utilities to manage your
repository and commit your work.")
    (home-page "https://wiki.gnome.org/Apps/Gitg")
    (license license:gpl2+)))

(define-public gamin
  (package
    (name "gamin")
    (version "0.1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "02n1zr9y8q9lyczhcz0nxar1vmf8p2mmbw8kq0v43wg21jr4i6d5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-deprecated-macro
           (lambda _
             (substitute* '("server/gam_node.c"
                            "server/gam_subscription.h"
                            "server/gam_node.h"
                            "server/gam_subscription.c")
               (("G_CONST_RETURN") "const"))
             #t)))))
    (inputs
     `(("glib" ,glib)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://people.gnome.org/~veillard/gamin/")
    (synopsis "File alteration monitor")
    (description
     "Gamin is a file and directory monitoring system defined to be a subset
of the FAM (File Alteration Monitor) system.  This is a service provided by a
library which detects when a file or a directory has been modified.")
    (license license:gpl2+)))

(define-public gnome-mahjongg
  (package
    (name "gnome-mahjongg")
    (version "3.35.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/gnome-mahjongg.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "019a66a4m5w4kkb0sm6gxj0wi54n06zdxdlmyqw7h8kbakjizv7l"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))
    (native-inputs
     `(("appstream-glib" ,appstream-glib)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin") ;; For glib-compile-resources
       ("gtk+" ,gtk+ "bin")     ;; For gtk-update-icon-cache
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("librsvg" ,librsvg)))
    (synopsis "Mahjongg tile-matching game")
    (description "GNOME Mahjongg is a game based on the classic Chinese
tile-matching game Mahjong.  It features multiple board layouts, tile themes,
and a high score table.")
    (home-page "https://wiki.gnome.org/Apps/Mahjongg")
    (license license:gpl2+)))

(define-public gnome-themes-extra
  (package
    (name "gnome-themes-extra")
    (version "3.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/" name "-"
                           version ".tar.xz"))
       (sha256
        (base32
         "06aqg9asq2vqi9wr29bs4v8z2bf4manhbhfghf4nvw01y2zs0jvw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; Don't create 'icon-theme.cache'.
       (let* ((coreutils (assoc-ref %build-inputs "coreutils"))
              (true      (string-append coreutils "/bin/true")))
         (list (string-append "GTK_UPDATE_ICON_CACHE=" true)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-themes-extra")
    (synopsis "GNOME Extra Themes")
    (description "This package provides themes and related elements that don't
really fit in other upstream packages.  It offers legacy support for GTK+ 2
versions of Adwaita, Adwaita-dark and HighContrast themes.  It also provides
index files needed for Adwaita to be used outside of GNOME.")
    (license license:lgpl2.1+)))

(define-public polari
  (package
    (name "polari")
    (version "3.32.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/polari/"
                                  (version-major+minor version)
                                  "/polari-" version ".tar.xz"))
              (sha256
               (base32
                "0h0w9j3y067l911gpj446b3a2w1i2vzr1w2a7cz7i5rhn6qkf2sp"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson/meson-postinstall.sh"
               (("gtk-update-icon-cache") (which "true")))
             #t))
         (add-after 'glib-or-gtk-wrap 'wrap-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/polari")))
               (wrap-program prog
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
               #t))))))
    (inputs
     `(("glib" ,glib)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gspell" ,gspell)
       ("gtk+" ,gtk+)
       ("gjs" ,gjs)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("telepathy-glib" ,telepathy-glib)
       ("telepathy-logger" ,telepathy-logger)))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("yelp-tools" ,yelp-tools)))
    (propagated-inputs
     `(("telepathy-idle" ,telepathy-idle)
       ("telepathy-mission-control" ,telepathy-mission-control)))
    (synopsis "Simple IRC Client")
    (description
     "Polari is a simple Internet Relay Chat (IRC) client that is designed to
integrate seamlessly with the GNOME desktop.")
    (home-page "https://wiki.gnome.org/Apps/Polari")
    (license license:gpl2+)))

(define-public gnome-boxes
  (package
    (name "gnome-boxes")
    (version "3.36.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/gnome-boxes/"
                       (version-major+minor version) "/"
                       "gnome-boxes-" version ".tar.xz"))
       (sha256
        (base32 "0c3cw90xqqcpacc2z06ljs0gg8saxizfgjzg9alhpwgz3gl4c5pg"))))
    (build-system meson-build-system)
    (outputs '("out" "help"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        ;; For run-path validation.
        (string-append "-Dc_link_args=-Wl,-rpath="
                       (assoc-ref %outputs "out")
                       "/lib/gnome-boxes"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/post_install.py"
               (("gtk-update-icon-cache")
                "true"))
             #t))
         (add-after 'install 'move-help
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (help (assoc-ref outputs "help")))
               (mkdir-p (string-append help "/share"))
               (rename-file
                (string-append out "/share/help")
                (string-append help "/share/help"))
               #t))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("vala" ,vala)))
    (inputs
     `(("appstream-glib" ,appstream-glib)
       ("libarchive" ,libarchive)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk" ,gtk+)
       ("gtk-vnc" ,gtk-vnc)
       ("gudev" ,libgudev)
       ("libarchive" ,libarchive)
       ("libosinfo" ,libosinfo)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("libusb" ,libusb)
       ("libvirt" ,libvirt)
       ("libvirt-glib" ,libvirt-glib)
       ("libxml2" ,libxml2)
       ("sparql-query" ,sparql-query)
       ("spice-gtk" ,spice-gtk)
       ("tracker" ,tracker)
       ("vte" ,vte)
       ("webkitgtk" ,webkitgtk)
       ("winpr" ,freerdp)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)))
    (synopsis "Virtualization made simple")
    (description "GNOME Boxes is a simple application to view, access, and
manage remote and virtual systems.  Note that this application requires the
@code{libvirt} and @code{virtlog} daemons to run.  Use the command
@command{info '(guix) Virtualization Services'} to learn how to configure
these services on the Guix System.")
    (license (list
              ;; For data/icons/empty-boxes.png.
              license:cc-by2.0
              ;; For all others.
              license:lgpl2.0+))
    (home-page "https://wiki.gnome.org/Apps/Boxes")))

(define-public geary
  (package
    (name "geary")
    (version "3.34.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/geary")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01cc921kyh3zxz07biqbdzkjgmdcc36kwjyajm4y382a75cl5zg7"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-failing-tests
                    (lambda _
                      (substitute* "test/meson.build"
                        (("test\\('client-tests', geary_test_client_bin\\)")
                         ""))
                      #t))
                  (add-after 'unpack 'disable-postinstall-script
                    (lambda _
                      (substitute* "meson.build"
                        (("meson.add_install_script\\(\
join_paths\\('build-aux', 'post_install.py'\\)\\)")
                         ""))
                      #t))
                  (add-before 'check 'setup-xvfb
                    (lambda _
                      (system "Xvfb :1 &")
                      (setenv "DISPLAY" ":1")
                      #t)))))
    (inputs
     `(("enchant" ,enchant)
       ("folks" ,folks)
       ("gcr" ,gcr)
       ("glib" ,glib)
       ("gmime" ,gmime-2.6)
       ("gnome-online-accounts:lib"
        ,gnome-online-accounts "lib")
       ("gspell" ,gspell)
       ("gtk+" ,gtk+)
       ("iso-codes" ,iso-codes)
       ("json-glib" ,json-glib)
       ("libcanberra" ,libcanberra)
       ("libgee" ,libgee)
       ("libhandy" ,libhandy)
       ("libpeas" ,libpeas)
       ("libsecret" ,libsecret)
       ("libunwind" ,libunwind)
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk)
       ("ytnef" ,ytnef)))
    (native-inputs
     `(("appstream-glib" ,appstream-glib)
       ("cmake-minimal" ,cmake-minimal)
       ("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("itstool" ,itstool)
       ("libarchive" ,libarchive)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xvfb" ,xorg-server-for-tests)))
    (synopsis "GNOME email application built around conversations")
    (description
     "Geary collects related messages together into conversations,
making it easy to find and follow your discussions.  Full-text and keyword
search makes it easy to find the email you are looking for.  Geary's
full-featured composer lets you send rich, styled text with images, links, and
lists, but also send lightweight, easy to read text messages.  Geary
automatically picks up your existing GNOME Online Accounts, and adding more is
easy.  Geary has a clean, fast, modern interface that works like you want it
to.")
    (home-page "https://wiki.gnome.org/Apps/Geary")
    (license (list
              ;; geary
              license:lgpl2.1+
              ;; icons
              license:cc-by3.0
              license:cc-by-sa3.0
              license:public-domain
              ;; snowball
              license:bsd-2))))

(define-public glabels
  (package
    (name "glabels")
    (version "3.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           "glabels-" version ".tar.xz"))
       (sha256
        (base32 "0f2rki8i27pkd9r0gz03cdl1g4vnmvp0j49nhxqn275vi8lmgr0q"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)))
    (home-page "https://glabels.org/")
    (synopsis "Program for creating labels and business cards")
    (description
     "gLabels is a program for creating labels and business cards.  It is
designed to work with various laser/ink-jet peel-off label and business
card sheets that you’ll find at most office supply stores.")
    (license license:gpl3+)))

(define-public libratbag
  (package
    (name "libratbag")
    (version "0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libratbag/libratbag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18y8mfr63d91278m1kcid0wvrxa1sgjs8na9af1ks2n28ssvciwq"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list "-Dsystemd=false"
             "-Dlogind-provider=elogind")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site (string-append
                           "/lib/python"
                           ,(version-major+minor (package-version python))
                           "/site-packages"))
                    (evdev (string-append
                            (assoc-ref inputs "python-evdev") site))
                    (pygo (string-append
                           (assoc-ref inputs "python-pygobject") site))
                    (python-wrap
                     `("PYTHONPATH" = (,evdev ,pygo))))
               (wrap-program (string-append out "/bin/" "ratbagctl")
                 python-wrap)
               #t))))))
    (native-inputs
     `(("check" ,check)
       ("pkg-config" ,pkg-config)
       ("swig" ,swig)
       ("valgrind" ,valgrind)))
    (inputs
     `(("glib" ,glib)
       ("json-glib" ,json-glib)
       ("libevdev" ,libevdev)
       ("libsystemd" ,elogind)
       ("libunistring" ,libunistring)
       ("python-evdev" ,python-evdev)
       ("python-pygobject" ,python-pygobject)
       ("udev" ,eudev)))
    (home-page "https://github.com/libratbag/libratbag")
    (synopsis "DBus daemon and utility for configuring gaming mice")
    (description "libratbag provides @command{ratbagd}, a DBus daemon to
configure input devices, mainly gaming mice.  The daemon provides a generic
way to access the various features exposed by these mice and abstracts away
hardware-specific and kernel-specific quirks.  There is also the
@command{ratbagctl} command line interface for configuring devices.

libratbag currently supports devices from Logitech, Etekcity, GSkill, Roccat,
Steelseries.

The ratbagd DBus service can be enabled by adding the following service to
your operating-system definition:

  (simple-service 'ratbagd dbus-root-service-type (list libratbag))")
    (license license:expat)))

(define-public piper
  (package
    (name "piper")
    (version "0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libratbag/piper")
             (commit version)))
       (sha256
        (base32 "00vrcsbsv2477l1ncpyzc61lhxgac84dsgr3sjs8qxw3nh1gaasv"))
       (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("python-flake8" ,python-flake8)))
    (inputs
     `(("adwaita-icon-theme" ,adwaita-icon-theme)
       ("gtk" ,gtk+)
       ("gtk:bin" ,gtk+ "bin")
       ("librsvg" ,librsvg)
       ("python-evdev" ,python-evdev)
       ("python-lxml" ,python-lxml)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)))
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%meson-build-system-modules)
       #:modules (((guix build python-build-system) #:prefix python:)
                  (guix build meson-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-update-gtk-icon-cache
           (lambda _
             (substitute* "meson.build"
               (("meson.add_install_script('meson_install.sh')") ""))
             #t))
         ;; TODO: Switch to wrap-script when it is fixed.
         (add-after 'install 'wrap-python
           (assoc-ref python:%standard-phases 'wrap))
         (add-after 'wrap-python 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program
                 (string-append (assoc-ref outputs "out" )"/bin/piper")
               `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
             #t)))))
    (home-page "https://github.com/libratbag/piper/")
    (synopsis "Configure bindings and LEDs on gaming mice")
    (description "Piper is a GTK+ application for configuring gaming mice with
onboard configuration for key bindings via libratbag.  Piper requires
a @command{ratbagd} daemon running with root privileges.  It can be run
manually as root, but is preferably configured as a DBus service that can
launch on demand.  This can be configured by enabling the following service,
provided there is a DBus service present:

  (simple-service 'ratbagd dbus-root-service-type (list libratbag))")
    (license license:gpl2)))

(define-public parlatype
  (package
    (name "parlatype")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gkarsay/parlatype.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c15ja0rwz3jj8bnqdq0nmqka39iwrhy8krdv2a2x8nl4shfpmv0"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:tests? #f                      ;require internet access
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-parlatype
           ;; Add gstreamer plugin provided in this package to system's
           ;; plugins.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gst-plugin-path (string-append
                                      out "/lib/gstreamer-1.0/"
                                      ":"
                                      (getenv "GST_PLUGIN_SYSTEM_PATH"))))
               (wrap-program (string-append out "/bin/parlatype")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" = (,gst-plugin-path))))
             #t)))))
    (native-inputs
     `(("appstream-glib" ,appstream-glib)
       ("desktop-file-utils" ,desktop-file-utils) ;for desktop-file-validate
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")             ;for glib-compile-resources
       ("pkg-config" ,pkg-config)
       ("yelp-tools" ,yelp-tools)))
    (inputs
     `(("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gstreamer" ,gstreamer)
       ("gtk+" ,gtk+)
       ("pocketsphinx" ,pocketsphinx)
       ("pulseaudio" ,pulseaudio)
       ("sphinxbase" ,sphinxbase)))
    (home-page "http://gkarsay.github.io/parlatype/")
    (synopsis "GNOME audio player for transcription")
    (description "Parlatype is an audio player for the GNOME desktop
environment.  Its main purpose is the manual transcription of spoken
audio files.")
    (license license:gpl3+)))

(define-public jsonrpc-glib
  (package
    (name "jsonrpc-glib")
    (version "3.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                   name "-" version ".tar.xz"))
              (sha256
               (base32
                "0j05x4xv2cp3cbmp30m68z8g4rdw7b030ip4wszyfj9ya15v5kni"))))
    (build-system meson-build-system)
    (inputs
     `(("json-glib" ,json-glib)
       ("glib" ,glib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin") ; for glib-genmarshal, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("vala" ,vala)))
    (home-page "https://gitlab.gnome.org/GNOME/jsonrpc-glib")
    (synopsis "JSON-RPC library for GLib")
    (description "Jsonrpc-GLib is a library to communicate with JSON-RPC based
peers in either a synchronous or asynchronous fashion.  It also allows
communicating using the GVariant serialization format instead of JSON when
both peers support it.  You might want that when communicating on a single
host to avoid parser overhead and memory-allocator fragmentation.")
    (license license:lgpl2.1+)))

(define-public feedbackd
  (package
    (name "feedbackd")
    (version "0.0.0+git20200527")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://source.puri.sm/Librem5/feedbackd.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wbkzxnqjydfgjvp7vz4ghczcz740zcb1yn90cb6gb5md4n6qx2y"))))
    (build-system meson-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("dbus" ,dbus)
       ("gsound" ,gsound)
       ("json-glib" ,json-glib)
       ("libgudev" ,libgudev)))
    (propagated-inputs
     `(("glib" ,glib))) ; in Requires of libfeedback-0.0.pc
    (synopsis "Haptic/visual/audio feedback via DBus")
    (description "Feedbackd provides a DBus daemon to act on events to provide
haptic, visual and audio feedback.  It offers the libfeedbackd library and
GObject introspection bindings.")
     (home-page "https://source.puri.sm/Librem5/feedbackd")
     (license (list license:lgpl2.1+   ; libfeedbackd
                    license:lgpl3+)))) ; the rest

(define-public sysprof
  (package
    (name "sysprof")
    (version "3.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/sysprof/"
                           (version-major+minor version) "/"
                           "sysprof-" version ".tar.xz"))
       (sha256
        (base32 "024i0gzqnm79rpr4gqxdvcj6gvf82xdlcp2p1k9ikcppmi6xnw46"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-Dsystemdunitdir="
                            %output
                            "/share/systemd"))
       #:tests? #f ; 3/4 test-model-filter barfs some dbus nonsense
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-install-script
           (lambda _
             (substitute* "build-aux/meson/post_install.sh"
               (("gtk-update-icon-cache") "true")
               (("update-desktop-database") "true"))
             #t)))))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libdazzle" ,libdazzle)
       ("polkit" ,polkit)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin") ; for gdbus-codegen, etc.
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    ;; This home page is so woefully out of date as to be essentially useless.
    ;; (home-page "http://www.sysprof.com")
    (home-page "https://wiki.gnome.org/Apps/Sysprof")
    (synopsis "System-wide performance profiler for GNU/Linux")
    (description
     "Sysprof performs detailed, accurate, and fast CPU profiling of an entire
GNU/Linux system including the kernel and all user-space applications.  This
helps find the function(s) in which a program spends most of its time.

It uses the kernel's built-in @code{ptrace} feature and handles shared
libraries.  Applications do not need to be recompiled--or even restarted.")
    (license license:gpl3+)))

(define-public gnome-builder
  (package
    (name "gnome-builder")
    (version "3.36.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "17pvmd5jypar8dkr6w56hvf7jnq4l1wih2wwgkrv7sblr7rkkar2"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags (list "-Dnetwork_tests=false"
                               ;; TODO: Enable all plugins...
                               "-Dplugin_clang=false"
                               "-Dplugin_flatpak=false"
                               "-Dplugin_glade=false"
                               ;; ... except this one.
                               "-Dplugin_update_manager=false")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-meson
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build-aux/meson/post_install.py"
               (("gtk-update-icon-cache") "true")
               (("update-desktop-database") "true"))
             (substitute* "src/libide/meson.build"
               (("/usr/lib")
                (string-append (assoc-ref inputs "python-pygobject")
                               "/lib")))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             #t)))))
    (inputs
     `(("devhelp" ,devhelp)
       ("gspell" ,gspell)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("jsonrpc-glib" ,jsonrpc-glib)
       ("libdazzle" ,libdazzle)
       ("libgit2-glib" ,libgit2-glib)
       ("libpeas" ,libpeas)
       ("python-pygobject" ,python-pygobject)
       ("sysprof" ,sysprof)
       ("template-glib" ,template-glib)
       ("vte" ,vte)
       ("webkitgtk" ,webkitgtk)))
    (propagated-inputs
     `(("gtksourceview" ,gtksourceview)))         ;needed for settings
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils) ;for desktop-file-validate
       ("glib:bin" ,glib "bin")
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xorg-server" ,xorg-server-for-tests)))
    (home-page "https://wiki.gnome.org/Apps/Builder")
    (synopsis "Toolsmith for GNOME-based applications")
    (description "Builder aims to be an integrated development
environment (IDE) for writing GNOME-based software.  It features fuzzy search,
auto-completion, a mini code map, documentation browsing, Git integration, an
integrated profiler via Sysprof, debugging support, and more.")
    (license license:gpl3+)))
