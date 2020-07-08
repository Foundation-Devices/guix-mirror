;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Fabian Harfert <fhmgufs@web.de>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
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

(define-module (gnu packages libcanberra)
  #:use-module ((guix licenses)
                #:select (lgpl2.1+ gpl2 gpl2+ cc-by-sa4.0 cc-by3.0))
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xiph)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

(define-public libcanberra
  (package
    (name "libcanberra")
    (version "0.30")
    (source
     (origin
       (method git-fetch)
       ;; The original source is no longer available.
       (uri
        (git-reference
         (url "https://salsa.debian.org/gnome-team/libcanberra.git")
         (commit
          (string-append "upstream/" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "14bsgi9pf9hbscsg4ad7pzv9k9iyb5nkns474syx9sw8kfgzx6cc"))
       ;; "sound-theme-freedesktop" is the default and fall-back sound theme for
       ;; XDG desktops and should always be present.
       ;; http://www.freedesktop.org/wiki/Specifications/sound-theme-spec/
       ;; We make sure libcanberra will find it.
       ;;
       ;; We add the default sounds store directory to the code dealing with
       ;; XDG_DATA_DIRS and not XDG_DATA_HOME. This is because XDG_DATA_HOME
       ;; can only be a single directory and is inspected first.  XDG_DATA_DIRS
       ;; can list an arbitrary number of directories and is only inspected
       ;; later.  This is designed to allows the user to modify any theme at
       ;; his pleasure.
       (patch-flags '("-p0"))
       (patches
        (search-patches "libcanberra-sound-theme-freedesktop.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-default-sounds-directory
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/sound-theme-spec.c"
               (("@SOUND_THEME_DIRECTORY@")
                (string-append
                 (assoc-ref inputs "sound-theme-freedesktop")
                 "/share")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("libltdl" ,libltdl)
       ("libvorbis" ,libvorbis)
       ("pulseaudio" ,pulseaudio)
       ("tdb" ,tdb)
       ("udev" ,eudev)
       ("sound-theme-freedesktop" ,sound-theme-freedesktop)))
    (propagated-inputs
     `(("gtk+2" ,gtk+-2)
       ("gtk+3" ,gtk+)))
    (synopsis "FreeDesktop.org Sound Theme and Name Specifications")
    (description "Libcanberra is an implementation of the XDG Sound Theme and
Name Specifications, for generating event sounds on free desktops, such as
GNOME.  It comes with several backends (ALSA, PulseAudio, OSS, GStreamer, null)
and is designed to be portable.")
    (home-page "http://0pointer.de/lennart/projects/libcanberra/")
    (license lgpl2.1+)))

(define-public libcanberra/gtk+-2
  (package (inherit libcanberra)
    (name "libcanberra-gtk2")
    (inputs `(,@(alist-delete "gtk+" (package-inputs libcanberra))
              ("gtk+" ,gtk+-2)))))

(define-public sound-theme-freedesktop
  (package
    (name "sound-theme-freedesktop")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://people.freedesktop.org/~mccann/dist/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "054abv4gmfk9maw93fis0bf605rc56dah7ys5plc4pphxqh8nlfb"))))
    (build-system gnu-build-system)
    (native-inputs `(("intltool" ,intltool)))
    (synopsis "Audio samples for use as a desktop sound theme")
    (description
     "This package provides audio samples that can be used by libcanberra as
sounds for various system events.")

    ;; The license of the various sounds is given in the 'CREDITS' file.
    (license (list cc-by-sa4.0 cc-by3.0 gpl2 gpl2+))

    (home-page "https://www.freedesktop.org/wiki/Specifications/sound-theme-spec/")))

(define-public python-pycanberra
  (package
    (name "python-pycanberra")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://ftp.n0.is/pub/releases/"
                           "pycanberra-" version ".tar.xz"))
       (sha256
        (base32
         "16jjf8fcgaprmz6jacsxrh17l1ad891fns38bxv49lg3s3mn1nj2"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;No tests included.
    (propagated-inputs
     `(("libcanberra" ,libcanberra)))
    (synopsis "Ctypes wrapper for the libcanberra API")
    (description
     "Pycanberra is a basic Python wrapper for libcanberra.")
    (home-page "http://c.n0.is/ng0/pycanberra/")
    (license lgpl2.1+)))
