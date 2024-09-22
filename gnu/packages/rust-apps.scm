;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2020-2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.ccom>
;;; Copyright © 2021, 2022 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2021, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021, 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2021 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Mathieu Laparie <mlaparie@disr.it>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Greg Hogan <code@greghogan.com>
;;; Copyright © 2023 Arnav Andrew Jose <arnav.jose@gmail.com>
;;; Copyright © 2023 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2023, 2024 Jaeme Sifat <jaeme@runbox.com>
;;; Copyright © 2023 Steve George <steve@futurile.net>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2024 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2024 Suhail Singh <suhail@bayesians.ca>
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

(define-module (gnu packages rust-apps)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-tls)
  #:use-module (gnu packages crates-vcs)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg))

(define-public aardvark-dns
  (package
    (name "aardvark-dns")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aardvark-dns" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ldqv9v3v9a1m2kka660d5v15y2zasy5z7m4fh5hif74r089cx6x"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* (find-files "." "^Cargo\\.toml$")
                  (("\"~([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-async-broadcast" ,rust-async-broadcast-0.6)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-hickory-client" ,rust-hickory-client-0.24)
                       ("rust-hickory-proto" ,rust-hickory-proto-0.24)
                       ("rust-hickory-server" ,rust-hickory-server-0.24)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nix" ,rust-nix-0.27)
                       ("rust-resolv-conf" ,rust-resolv-conf-0.7)
                       ("rust-signal-hook" ,rust-signal-hook-0.3)
                       ("rust-syslog" ,rust-syslog-6)
                       ("rust-tokio" ,rust-tokio-1))))
    (home-page "https://github.com/containers/aardvark-dns")
    (synopsis "Container-focused DNS A/AAAA record server")
    (description
     "Aardvark-dns is an authoritative DNS server for A/AAAA container
records.  It can forward other requests to configured resolvers.")
    (license license:asl2.0)))

(define-public agate
  (package
    (name "agate")
    (version "3.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "agate" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ja2lvlcvkcbjn8r9da4k0ppy7pb7xad8j4b0a4fhg0mbp244f9s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-configparser" ,rust-configparser-3)
        ("rust-env-logger" ,rust-env-logger-0.11)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-getopts" ,rust-getopts-0.2)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime-guess" ,rust-mime-guess-2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-rcgen" ,rust-rcgen-0.12)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.25)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-gemini-fetch" ,rust-gemini-fetch-0.2))))
    (home-page "https://github.com/mbrubeck/agate")
    (synopsis "Very simple server for the Gemini hypertext protocol")
    (description
     "Agate is a server for the Gemini network protocol, built with the Rust
programming language.  It has very few features, and can only serve static
files.  It uses async I/O, and should be quite efficient even when running on
low-end hardware and serving many concurrent requests.")
    (license (list license:expat license:asl2.0))))

(define-public alfis
  (package
    (name "alfis")
    (version "0.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Revertron/Alfis")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zqszjyiab0b76m2b8zfzpndg393hn311whq1fs9syfn53hp7nh4"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Use a packaged version of web-view.
                 (substitute* "Cargo.toml"
                   (("git = .*,") "version = \"0.7.3\","))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=dns::client::tests::test_tcp_client"
         "--skip=dns::client::tests::test_udp_client")
       #:cargo-inputs
       (("rust-getopts" ,rust-getopts-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-simplelog" ,rust-simplelog-0.12)
        ("rust-toml" ,rust-toml-0.7)
        ("rust-digest" ,rust-digest-0.10)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-ed25519-dalek" ,rust-ed25519-dalek-1)
        ("rust-x25519-dalek" ,rust-x25519-dalek-1)
        ("rust-ecies-ed25519" ,rust-ecies-ed25519-0.5)
        ("rust-chacha20poly1305" ,rust-chacha20poly1305-0.9)
        ("rust-signature" ,rust-signature-2)
        ("rust-blakeout" ,rust-blakeout-0.3)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-byteorder" ,rust-byteorder-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-bincode" ,rust-bincode-1)
        ("rust-serde-cbor" ,rust-serde-cbor-0.11)
        ("rust-base64" ,rust-base64-0.21)
        ("rust-num-bigint" ,rust-num-bigint-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rand-0.7" ,rust-rand-0.7) ;For ed25519-dalek
        ("rust-sqlite" ,rust-sqlite-0.30)
        ("rust-uuid" ,rust-uuid-1)
        ("rust-mio" ,rust-mio-0.8)
        ("rust-ureq" ,rust-ureq-2)
        ("rust-lru" ,rust-lru-0.9)
        ("rust-derive-more" ,rust-derive-more-0.99)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-spmc" ,rust-spmc-0.3)
        ("rust-tinyfiledialogs" ,rust-tinyfiledialogs-3)
        ("rust-web-view" ,rust-web-view-0.7)
        ("rust-open" ,rust-open-3)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-thread-priority" ,rust-thread-priority-0.10)
        ("rust-winres" ,rust-winres-0.1))
       #:cargo-development-inputs
       (("rust-serde-bytes" ,rust-serde-bytes-0.11)
        ("rust-serde-derive" ,rust-serde-derive-1))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list at-spi2-core
           gtk
           glib
           pango
           sqlite
           webkitgtk-with-libsoup2))
    (home-page "https://github.com/Revertron/Alfis")
    (synopsis "Alternative Free Identity System")
    (description
     "This project represents a minimal blockchain without cryptocurrency,
capable of sustaining any number of domain names in a bunch of original
alternative zones.")
    (license license:agpl3+)))

(define-public bat
  (package
    (name "bat")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bat" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "11nc2iv2qhd1bs16yijqq934864ybnmg485rny70scy26xb9xk4x"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (("\"~([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-inputs
      `(("rust-ansi-colours" ,rust-ansi-colours-1)
        ("rust-bincode" ,rust-bincode-1)
        ("rust-bugreport" ,rust-bugreport-0.5)
        ("rust-bytesize" ,rust-bytesize-1)
        ("rust-clap" ,rust-clap-4)
        ("rust-clircle" ,rust-clircle-0.4)
        ("rust-console" ,rust-console-0.15)
        ("rust-content-inspector" ,rust-content-inspector-0.2)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-etcetera" ,rust-etcetera-0.8)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-git2" ,rust-git2-0.18)
        ("rust-globset" ,rust-globset-0.4)
        ("rust-grep-cli" ,rust-grep-cli-0.1)
        ("rust-home" ,rust-home-0.5)
        ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.49)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-os-str-bytes" ,rust-os-str-bytes-6)
        ("rust-path-abs" ,rust-path-abs-0.5)
        ("rust-plist" ,rust-plist-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-run-script" ,rust-run-script-0.10)
        ("rust-semver" ,rust-semver-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-yaml" ,rust-serde-yaml-0.9)
        ("rust-shell-words" ,rust-shell-words-1)
        ("rust-syntect" ,rust-syntect-5)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-wild" ,rust-wild-2))
      #:cargo-development-inputs
      `(("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-expect-test" ,rust-expect-test-1)
        ("rust-nix" ,rust-nix-0.26)
        ("rust-predicates" ,rust-predicates-3)
        ("rust-serial-test" ,rust-serial-test-2)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-wait-timeout" ,rust-wait-timeout-0.2))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pre-build
            (lambda _
              (setenv "BAT_ASSETS_GEN_DIR" "target")))
          (add-after 'install 'install-extras
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (share (string-append out "/share"))
                     (bash-completions-dir
                      (string-append share "/bash-completion/completions"))
                     (zsh-completions-dir
                      (string-append share "/zsh/site-functions"))
                     (fish-completions-dir
                      (string-append share "/fish/vendor_completions.d"))
                     (man1 (string-append share "/man/man1")))
                (mkdir-p bash-completions-dir)
                (mkdir-p zsh-completions-dir)
                (mkdir-p fish-completions-dir)
                (copy-file "target/assets/completions/bat.bash"
                           (string-append bash-completions-dir "/bat"))
                (copy-file "target/assets/completions/bat.zsh"
                           (string-append zsh-completions-dir "/_bat"))
                (install-file "target/assets/completions/bat.fish"
                              fish-completions-dir)
                (install-file "target/assets/manual/bat.1" man1)))))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.7 zlib))
    (home-page "https://github.com/sharkdp/bat")
    (synopsis "@command{cat} clone with syntax highlighting and git integration")
    (description
     "@command{bat} is a drop-in @command{cat} replacement featuring syntax
highlighting for a large number of languages, git integration, and automatic
paging.")
    (license (list license:expat license:asl2.0))))

(define-public cargo-machete
  (package
    (name "cargo-machete")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri name version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0n45j6q36zjw149a84wz2yyygfhcjsnvfaiskwph00684fzlbnbm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;Error: No such file or directory (os error 2)
       #:install-source? #f
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-argh" ,rust-argh-0.1)
                       ("rust-cargo-metadata" ,rust-cargo-metadata-0.18)
                       ("rust-cargo-toml" ,rust-cargo-toml-0.19)
                       ("rust-grep" ,rust-grep-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.5)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-toml-edit" ,rust-toml-edit-0.22)
                       ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/est31/cargo-udeps")
    (synopsis "Find unused dependencies in Cargo.toml")
    (description "@code{cargo-machete} finds unused dependencies in Cargo.toml.")
    (license (list license:expat license:asl2.0))))

(define-public complgen
  (package
    (name "complgen")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/adaszko/complgen")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "026bv2cync6qgdkn1ifhspy6z9v25plziqhnn58rlgffx2a0dqn2"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-inputs `(("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-chic" ,rust-chic-1)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-env-logger" ,rust-env-logger-0.10)
                       ("rust-hashbrown" ,rust-hashbrown-0.13)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-nom-locate" ,rust-nom-locate-4)
                       ("rust-railroad" ,rust-railroad-0.2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-roaring" ,rust-roaring-0.10)
                       ("rust-slice-group-by" ,rust-slice-group-by-0.3)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-ustr" ,rust-ustr-0.9))
      #:cargo-development-inputs `(("rust-proptest" ,rust-proptest-1))))
    (native-inputs (list git))
    (home-page "https://github.com/adaszko/complgen")
    (synopsis "Declarative bash/fish/zsh completions without writing shell
scripts")
    (description
     "@command{complgen} is a tool that allows you to generate
completion scripts for all major shells (@code{bash}, @code{fish}, @code{zsh})
from a single, concise, @code{EBNF}-like grammar.")
    (license license:asl2.0)))

(define-public diffr
  (package
    (name "diffr")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "diffr" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "1kdngd5g1ssdiq7d10jr3jwg0sx740x3vmhq3j594a5kd467ikib"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       ;; https://github.com/mookid/diffr/issues/79
       #:cargo-test-flags
       '("--release" "--"
         "--skip=tests_cli::color_invalid_attribute_name"
         "--skip=tests_cli::color_invalid_color_not_done"
         "--skip=tests_cli::color_invalid_color_value_ansi"
         "--skip=tests_cli::color_invalid_color_value_name"
         "--skip=tests_cli::color_invalid_color_value_rgb"
         "--skip=tests_cli::color_invalid_face_name"
         "--skip=tests_cli::color_ok"
         "--skip=tests_cli::color_ok_multiple"
         "--skip=tests_cli::color_only_face_name"
         "--skip=tests_cli::debug_flag"
         "--skip=tests_cli::line_numbers_style"
         "--skip=tests_cli::test_bad_argument")
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-termcolor" ,rust-termcolor-1))))
    (home-page "https://github.com/mookid/diffr")
    (synopsis "Longest Common Sequence based diff highlighting tool")
    (description
     "This package provides an @acronym{LCS, longest common sequence} based diff
highlighting tool to ease code review from your terminal.")
    (license license:expat)))

(define-public drill
  (package
    (name "drill")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "drill" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0jp9r19zc9m3hgxc7a98fhyi1ga0qwjprxjsqaxiykmjpb86bxf3"))))
    (build-system cargo-build-system)
    (arguments
      `(#:install-source? #f
        #:cargo-inputs
        (("rust-async-trait" ,rust-async-trait-0.1)
         ("rust-clap" ,rust-clap-2)
         ("rust-colored" ,rust-colored-2)
         ("rust-csv" ,rust-csv-1)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-hdrhistogram" ,rust-hdrhistogram-7)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-linked-hash-map" ,rust-linked-hash-map-0.5)
         ("rust-num-cpus" ,rust-num-cpus-1)
         ("rust-openssl-sys" ,rust-openssl-sys-0.9)
         ("rust-rand" ,rust-rand-0.8)
         ("rust-regex" ,rust-regex-1)
         ("rust-reqwest" ,rust-reqwest-0.11)
         ("rust-serde" ,rust-serde-1)
         ("rust-serde-json" ,rust-serde-json-1)
         ("rust-tokio" ,rust-tokio-1)
         ("rust-url" ,rust-url-2)
         ("rust-yaml-rust" ,rust-yaml-rust-0.4))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://github.com/fcsonline/drill")
    (synopsis "HTTP load testing application")
    (description
     "Drill is a HTTP load testing application written in Rust inspired by
Ansible syntax.  Benchmark files can be written in YAML.")
    (license license:gpl3)))

(define-public dutree
  (package
    (name "dutree")
    (version "0.2.18")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dutree" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1611h27i8fm3jndscd6w65z8z7w09nnrm61vdgs9kb8ln57gqm8x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-getopts" ,rust-getopts-0.2)
        ("rust-regex" ,rust-regex-1)
        ("rust-signal-hook" ,rust-signal-hook-0.1)
        ("rust-terminal-size" ,rust-terminal-size-0.1)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
        ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://ownyourbits.com/2018/03/25/analyze-disk-usage-with-dutree/")
    (synopsis "Command line tool to analyze disk usage")
    (description
     "@command{dutree} is command line tool to analyze disk usage.
Features include:
@enumerate
@item coloured output, according to the @code{LS_COLORS} environment variable.
@item display the file system tree.
@item ability to aggregate small files.
@item ability to exclude files or directories.
@item ability to compare different directories.
@item fast, written in Rust.
@end enumerate\n")
    (license license:gpl3)))

(define-public emacs-lsp-booster
  (package
    (name "emacs-lsp-booster")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blahgeek/emacs-lsp-booster")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xx32ms3mpi1clxf74mx7nanj0iw9qkmhi0a53fx8fkz0jw2fq8f"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-inputs `(("rust-anyhow" ,rust-anyhow-1)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-verbosity-flag" ,rust-clap-verbosity-flag-2)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-env-logger" ,rust-env-logger-0.10))
      #:cargo-development-inputs `(("rust-emacs" ,rust-emacs-0.18)
                                   ("rust-tempfile" ,rust-tempfile-3))
      #:install-source? #f))
    (native-inputs (list emacs))    ; Not emacs-minimal
    (home-page "https://github.com/blahgeek/emacs-lsp-booster")
    (synopsis "Emacs LSP performance booster")
    (description
     "@code{emacs-lsp-booster} improves the performance of @code{lsp-mode} and
@code{eglot} Emacs packages using a wrapper executable.  See the home-page for
configuration instructions.")
    (license license:expat)))

(define-public evremap
  (let ((commit "cc618e8b973f5c6f66682d1477b3b868a768c545")) ;version bump
    (package
      (name "evremap")
      (version "0.1.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wez/evremap")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "182ry573q8kjsxx2hvxk9d6clahpll1vh50zvs5g652jd6a2f038"))))
      (build-system cargo-build-system)
      (arguments
       (list #:cargo-inputs `(("rust-anyhow" ,rust-anyhow-1)
                              ("rust-clap" ,rust-clap-4)
                              ("rust-env-logger" ,rust-env-logger-0.11)
                              ("rust-evdev-rs" ,rust-evdev-rs-0.6)
                              ("rust-libc" ,rust-libc-0.2)
                              ("rust-log" ,rust-log-0.4)
                              ("rust-serde" ,rust-serde-1)
                              ("rust-thiserror" ,rust-thiserror-1)
                              ("rust-toml" ,rust-toml-0.8))))
      (native-inputs (list pkg-config))
      (inputs (list libevdev))
      (home-page "https://github.com/wez/evremap")
      (synopsis "Keyboard input remappper")
      (description
       "Evremap is a keyboard input remapper.  It works by grabbing exclusive
access to an input device and maintaining a model of the keys that are
pressed.  It then applies your remapping configuration to produce the
effective set of pressed keys and emits appropriate changes to a virtual
output device.

Its remapping is effective system-wide: in Wayland, X11 and the Linux
console.")
      (license license:expat))))

(define-public eza
  (package
    (name "eza")
    (version "0.19.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "eza" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "16zc0j1x7clbjlrg1kc1szy0x1lbsfshij0qhdq8vx0zj6b7dlys"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-inputs `(("rust-ansi-width" ,rust-ansi-width-0.1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-dirs" ,rust-dirs-5)
                       ("rust-git2" ,rust-git2-0.19)
                       ("rust-glob" ,rust-glob-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-locale" ,rust-locale-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-natord" ,rust-natord-1)
                       ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
                       ("rust-number-prefix" ,rust-number-prefix-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-palette" ,rust-palette-0.7)
                       ("rust-path-clean" ,rust-path-clean-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-phf" ,rust-phf-0.11)
                       ("rust-plist" ,rust-plist-1)
                       ("rust-proc-mounts" ,rust-proc-mounts-0.3)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-norway" ,rust-serde-norway-0.9)
                       ("rust-terminal-size" ,rust-terminal-size-0.3)
                       ("rust-timeago" ,rust-timeago-0.4)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-uutils-term-grid" ,rust-uutils-term-grid-0.6)
                       ("rust-uzers" ,rust-uzers-0.12)
                       ("rust-windows-sys" ,rust-windows-sys-0.59)
                       ("rust-zoneinfo-compiled" ,rust-zoneinfo-compiled-0.5))
      #:cargo-development-inputs `(("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-trycmd" ,rust-trycmd-0.15))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'build 'build-manual
                     (lambda* (#:key inputs #:allow-other-keys)
                       (when (assoc-ref inputs "pandoc")
                         (map (lambda (page)
                                (with-output-to-file page
                                  (lambda _
                                    (invoke "pandoc" "--standalone"
                                            "-f" "markdown"
                                            "-t" "man"
                                            (string-append "man/" page ".md")))))
                              (list "eza.1"
                                    "eza_colors.5"
                                    "eza_colors-explanation.5")))))
                   (add-after 'install 'install-extras
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (share (string-append out "/share"))
                              (bash-completions-dir (string-append share
                                                     "/bash-completion/completions"))
                              (zsh-completions-dir (string-append share
                                                    "/zsh/site-functions"))
                              (fish-completions-dir (string-append share
                                                     "/fish/vendor_completions.d"))
                              (man1 (string-append share "/man/man1"))
                              (man5 (string-append share "/man/man5")))
                         (when (file-exists? "eza.1")
                           (install-file "eza.1" man1))
                         (when (file-exists? "eza_colors.5")
                           (install-file "eza_colors.5" man5))
                         (when (file-exists? "eza_colors-explanation.5")
                           (install-file "eza_colors-explanation.5" man5))
                         (mkdir-p bash-completions-dir)
                         (mkdir-p zsh-completions-dir)
                         (mkdir-p fish-completions-dir)
                         (copy-file "completions/bash/eza"
                                    (string-append bash-completions-dir "/eza"))
                         (copy-file "completions/zsh/_eza"
                                    (string-append zsh-completions-dir "/_eza"))
                         (copy-file "completions/fish/eza.fish"
                                    (string-append fish-completions-dir
                                                   "/eza.fish"))))))))
    (native-inputs
     (append (list pkg-config)
             (if (supported-package? pandoc)
                 (list pandoc)
                 '())))
    (inputs (list libgit2-1.8 zlib))
    (home-page "https://github.com/eza-community/eza")
    (synopsis "Modern replacement for ls")
    (description
     "@code{eza} is a modern replacement for the command-line
program @code{ls}.  It uses colours to distinguish file types and
metadata.  It also knows about symlinks, extended attributes, and Git.
This package is the community maintained fork of @code{exa}.")
    (license license:expat)))

(define-public exa
  (deprecated-package "exa" eza))

(define-public fd
  (package
    (name "fd")
    (version "10.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fd-find" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0d5zv6pmxxsblbvk4pzxjbj072d2bg3byhss57699y2s37xdw26y"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-test-flags
      ;; No user 'root' in the build environment.
      '(list "--release" "--"
             "--skip=test_owner_root")
      #:install-source? #f
      #:cargo-inputs
      `(("rust-aho-corasick" ,rust-aho-corasick-1)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-argmax" ,rust-argmax-0.3)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete" ,rust-clap-complete-4)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-ctrlc" ,rust-ctrlc-3)
        ("rust-etcetera" ,rust-etcetera-0.8)
        ("rust-faccess" ,rust-faccess-0.2)
        ("rust-globset" ,rust-globset-0.4)
        ("rust-humantime" ,rust-humantime-2)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-jemallocator" ,rust-jemallocator-0.5)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-lscolors" ,rust-lscolors-0.19)
        ("rust-nix" ,rust-nix-0.29)
        ("rust-normpath" ,rust-normpath-1)
        ("rust-nu-ansi-term" ,rust-nu-ansi-term-0.50)
        ("rust-regex" ,rust-regex-1)
        ("rust-regex-syntax" ,rust-regex-syntax-0.8)
        ("rust-version-check" ,rust-version-check-0.9))
      #:cargo-development-inputs
      `(("rust-diff" ,rust-diff-0.1)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-test-case" ,rust-test-case-3))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'override-jemalloc
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((jemalloc (assoc-ref inputs "jemalloc")))
                ;; This flag is needed when not using the bundled jemalloc.
                ;; https://github.com/tikv/jemallocator/issues/19
                (setenv "CARGO_FEATURE_UNPREFIXED_MALLOC_ON_SUPPORTED_PLATFORMS" "1")
                (setenv "JEMALLOC_OVERRIDE"
                        (string-append jemalloc "/lib/libjemalloc.so")))))
          (add-after 'install 'install-extras
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                ;; Manpages
                (install-file "doc/fd.1" (string-append out "/share/man/man1"))
                ;; Completions require running the built binary.
                (unless #$(%current-target-system)
                  (invoke "make" "completions")
                  (install-file "autocomplete/fd.bash"
                                (string-append out "/etc/bash_completion.d"))
                  (install-file "autocomplete/fd.fish"
                                (string-append out "/share/fish/vendor_completions.d"))
                  (install-file "autocomplete/_fd"
                                (string-append out "/share/zsh/site-functions"))
                  (rename-file (string-append out "/etc/bash_completion.d/fd.bash")
                               (string-append out "/etc/bash_completion.d/fd")))))))))
     (inputs (list jemalloc))
     (home-page "https://github.com/sharkdp/fd")
     (synopsis "Simple, fast and user-friendly alternative to find")
     (description
      "@code{fd} is a simple, fast and user-friendly alternative to @code{find}.
While it does not seek to mirror all of find's powerful functionality, it provides
defaults for 80% of the use cases.")
     (license (list license:expat license:asl2.0))))

(define-public gitui
  (package
    (name "gitui")
    (version "0.25.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gitui" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19xv6bvp0hs1m5y8a0myifvh8xrxrv14wd4gknlsrka0l7d8ijg7"))))
    (build-system cargo-build-system)
    (arguments
     `(;; disable vendor-openssl from default flags
       ;; use oniguruma regex lib which is faster and makes gitui 25% smaller
       #:features '("ghemoji" "regex-onig" "trace-libgit")
       #:cargo-build-flags
       '("--release" "--no-default-features")
       #:cargo-test-flags
       '("--release" "--no-default-features"
         "--features" "ghemoji regex-onig trace-libgit"
         "--"
         ;; this test fails with permission denied error
         "--skip=test_symbolic_links")
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
          (replace 'install
             ;; Add --no-default-features to the install phase.
             (lambda* (#:key outputs features #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (invoke "cargo" "install" "--no-track"
                          "--path" "."
                          "--root" out
                          "--no-default-features"
                          "--features" (string-join features))))))
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-asyncgit" ,rust-asyncgit-0.25)
                       ("rust-backtrace" ,rust-backtrace-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-bugreport" ,rust-bugreport-0.5)
                       ("rust-bwrap" ,rust-bwrap-1)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossterm" ,rust-crossterm-0.27)
                       ("rust-dirs" ,rust-dirs-5)
                       ("rust-easy-cast" ,rust-easy-cast-0.5)
                       ("rust-filetreelist" ,rust-filetreelist-0.5)
                       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)
                       ("rust-gh-emoji" ,rust-gh-emoji-1)
                       ("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itertools" ,rust-itertools-0.12)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-notify" ,rust-notify-6)
                       ("rust-notify-debouncer-mini" ,rust-notify-debouncer-mini-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-ratatui" ,rust-ratatui-0.24)
                       ("rust-rayon-core" ,rust-rayon-core-1)
                       ("rust-ron" ,rust-ron-0.8)
                       ("rust-scopeguard" ,rust-scopeguard-1)
                       ("rust-scopetime" ,rust-scopetime-0.1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-shellexpand" ,rust-shellexpand-3)
                       ("rust-simplelog" ,rust-simplelog-0.12)
                       ("rust-struct-patch" ,rust-struct-patch-0.4)
                       ("rust-syntect" ,rust-syntect-5)
                       ("rust-tui-textarea" ,rust-tui-textarea-0.4)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-truncate" ,rust-unicode-truncate-0.2)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-which" ,rust-which-6))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2-1.7 libssh2 openssl zlib))
    (home-page "https://github.com/extrawurst/gitui")
    (synopsis "Terminal UI for git")
    (description "This package provides a fast Terminal UI for git.")
    (license license:expat)))

(define-public helvum
  (package
    (name "helvum")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/pipewire/helvum")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q8gkx7djrfdl8fykppsqkxiadsq47v0xhj612nxlrvjz8n77ygn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-glib" ,rust-glib-0.18)
                       ("rust-libadwaita" ,rust-libadwaita-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-once-cell" ,rust-once-cell-1)
                       ("rust-pipewire" ,rust-pipewire-0.7))
       #:imported-modules (,@%glib-or-gtk-build-system-modules
                           ,@%cargo-build-system-modules)
       #:modules ((guix build cargo-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-extra
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (source (assoc-ref inputs "source"))
                    (share (string-append out "/share"))
                    (hicolor (string-append share "/icons/hicolor")))
               (mkdir-p hicolor)
               (with-directory-excursion hicolor
                 (mkdir-p "scalable/apps")
                 (install-file
                  (string-append source "/data/icons/org.pipewire.Helvum.svg")
                  "scalable/apps")
                 (mkdir-p "symbolic/apps")
                 (install-file
                  (string-append
                   source "/data/icons/org.pipewire.Helvum-symbolic.svg")
                  "symbolic/apps"))
               (with-directory-excursion share
                 (mkdir-p "applications")
                 (with-directory-excursion "applications"
                   (install-file
                    (string-append
                     source "/data/org.pipewire.Helvum.desktop.in") ".")
                   (substitute* "org.pipewire.Helvum.desktop.in"
                     (("@icon@") "org.pipewire.Helvum")
                     (("Exec=helvum")
                      (string-append "Exec="
                                     (string-append out "/bin/helvum"))))
                   (rename-file "org.pipewire.Helvum.desktop.in"
                                "org.pipewire.Helvum.desktop"))
                 (mkdir-p "metainfo")
                 (with-directory-excursion "metainfo"
                   (install-file
                    (string-append
                     source
                     "/data/org.pipewire.Helvum.metainfo.xml.in") ".")
                   (substitute* "org.pipewire.Helvum.metainfo.xml.in"
                     (("@app-id@") "org.pipewire.Helvum"))
                   (rename-file "org.pipewire.Helvum.metainfo.xml.in"
                                "org.pipewire.Helvum.metainfo.xml"))))))
         (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
           (assoc-ref glib-or-gtk:%standard-phases
                      'generate-gdk-pixbuf-loaders-cache-file))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs (list pkg-config clang))
    (inputs (list glib gtk libadwaita pipewire))
    (home-page "https://gitlab.freedesktop.org/pipewire/helvum")
    (synopsis "GTK patchbay for pipewire")
    (description "This package provides a GTK patchbay for pipewire.")
    (license license:gpl3)))

(define-public hexyl
  (package
    (name "hexyl")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hexyl" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0fhbc4ibpbbgcgx2v6wzxcn63jz76cvdp2f8jdg747h65hvp5bcm"))
        (snippet
         #~(begin (use-modules (guix build utils))
                  (substitute* "doc/hexyl.1.md"
                    (("0\\.12\\.0") "0.14.0"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-clap" ,rust-clap-4)
        ("rust-const-format" ,rust-const-format-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-owo-colors" ,rust-owo-colors-3)
        ("rust-supports-color" ,rust-supports-color-2)
        ("rust-terminal-size" ,rust-terminal-size-0.2)
        ("rust-thiserror" ,rust-thiserror-1))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-predicates" ,rust-predicates-3)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1))
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'install-manual
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((man1 (string-append (assoc-ref outputs "out")
                                       "/share/man/man1")))
              (when (assoc-ref inputs "pandoc")
                (mkdir-p man1)
                (with-output-to-file (string-append man1 "/hexyl.1")
                  (lambda _
                    (invoke "pandoc" "--standalone"
                            "--from" "markdown"
                            "--to" "man"
                            "doc/hexyl.1.md"))))))))))
    (native-inputs
     (if (supported-package? pandoc)
         (list pandoc)
         '()))
    (home-page "https://github.com/sharkdp/hexyl")
    (synopsis "Command-line hex viewer")
    (description
     "This package provides a command line hex viewer.  It uses a colored output
for distinguishing different kinds of bytes such as NULL bytes, printable ASCII
characters, ASCII whitespace characters, other ASCII characters and non-ASCII.")
    (license (list license:expat license:asl2.0))))

(define-public hyperfine
  (package
    (name "hyperfine")
    (version "1.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hyperfine" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0m5lrvx6wwkxqdc5digm1k4diiaqcg5j4pia77s5nw1aam7k51hy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:modules ((guix build cargo-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:install-source? #f
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-clap" ,rust-clap-2)
        ("rust-colored" ,rust-colored-2)
        ("rust-csv" ,rust-csv-1)
        ("rust-indicatif" ,rust-indicatif-0.15)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rust-decimal" ,rust-rust-decimal-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-statistical" ,rust-statistical-1)
        ("rust-version-check" ,rust-version-check-0.9)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-approx" ,rust-approx-0.3))
       #:phases
       (modify-phases %standard-phases
        (add-after 'install 'install-more
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out   (assoc-ref outputs "out"))
                   (share (string-append out "/share/"))
                   (man   (string-append share "man/man1"))
                   (bash  (string-append share "bash-completion/completions"))
                   (fish  (string-append share "fish/vendor_completions.d"))
                   (zsh   (string-append share "zsh/site-functions")))
              (install-file "doc/hyperfine.1" man)
              (for-each (cut install-file <> bash)
                        (find-files "target" "^hyperfine.bash$"))
              (rename-file (string-append bash "/hyperfine.bash")
                           (string-append bash "/hyperfine"))
              (for-each (cut install-file <> fish)
                        (find-files "target" "^hyperfine.fish$"))
              (for-each (cut install-file <> zsh)
                        (find-files "target" "^_hyperfine$"))))))))
    (home-page "https://github.com/sharkdp/hyperfine")
    (synopsis "Command-line benchmarking tool")
    (description
     "This package provides a command-line benchmarking tool.")
    (license (list license:expat license:asl2.0))))

(define-public i3status-rust
  (package
    (name "i3status-rust")
    (version "0.32.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/greshake/i3status-rust")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11n32kkfwlv38mj9018hp5rbg4w3d1vq9f6x8rhqahs19mm5amqa"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            ;; This comes a few commits after the 0.32.3 tag.
            (substitute* "Cargo.toml"
               (("^(wayrs-client = \\{ version = \")0\\.12\"" all most)
                (string-append most "1.0\""))
               (("^(wayrs-protocols = \\{ version = \")0\\.12\"" all most)
                (string-append most "0.13\"")))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)              ; Dependency of xtask.
        ("rust-async-once-cell" ,rust-async-once-cell-0.5)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-backon" ,rust-backon-0.4)
        ("rust-calibright" ,rust-calibright-0.1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-chrono-tz" ,rust-chrono-tz-0.8)
        ("rust-clap" ,rust-clap-4)                  ; Dependency of xtask also.
        ("rust-clap-mangen" ,rust-clap-mangen-0.2)  ; Dependency of xtask.
        ("rust-dirs" ,rust-dirs-5)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-hyper" ,rust-hyper-0.14)
        ("rust-inotify" ,rust-inotify-0.10)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libpulse-binding" ,rust-libpulse-binding-2)
        ("rust-log" ,rust-log-0.4)
        ("rust-maildir" ,rust-maildir-0.6)
        ("rust-neli" ,rust-neli-0.6)
        ("rust-neli-wifi" ,rust-neli-wifi-0.6)
        ("rust-nix" ,rust-nix-0.27)
        ("rust-nom" ,rust-nom-7)
        ("rust-notmuch" ,rust-notmuch-0.8)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pandoc" ,rust-pandoc-0.8)            ; Dependency of xtask.
        ("rust-regex" ,rust-regex-1)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-sensors" ,rust-sensors-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-with" ,rust-serde-with-3)
        ("rust-shellexpand" ,rust-shellexpand-3)
        ("rust-signal-hook" ,rust-signal-hook-0.3)
        ("rust-signal-hook-tokio" ,rust-signal-hook-tokio-0.3)
        ("rust-smart-default" ,rust-smart-default-0.7)
        ("rust-swayipc-async" ,rust-swayipc-async-2)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-toml" ,rust-toml-0.8)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
        ("rust-wayrs-client" ,rust-wayrs-client-1)
        ("rust-wayrs-protocols" ,rust-wayrs-protocols-0.13)
        ("rust-zbus" ,rust-zbus-3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-optional-icu-deps
           (lambda _
             (substitute* "Cargo.toml"
               (("^icu_calendar.*") "")
               (("^icu_datetime.*") "")
               (("^icu_locid.*") ""))))
         (add-after 'unpack 'fix-resources-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share")))
               (substitute* "src/util.rs"
                 (("/usr/share/i3status-rust") share)))))
         (add-after 'unpack 'substitute-package-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (define* (substitute-command-block* file command full-command)
               (substitute* file
                 (((string-append "Command::new\\(\"" command "\"\\)"))
                  (string-append "Command::new(\"" full-command "\")"))))
             (substitute-command-block* "src/blocks/keyboard_layout/set_xkb_map.rs"
               "setxkbmap" (search-input-file inputs "/bin/setxkbmap"))
             (substitute-command-block* "src/blocks/sound/alsa.rs"
               "alsactl" (search-input-file inputs "/sbin/alsactl"))
             (substitute-command-block* "src/blocks/sound/alsa.rs"
               "amixer" (search-input-file inputs "/bin/amixer"))
             (substitute-command-block* "src/blocks/speedtest.rs"
               "speedtest-cli" (search-input-file inputs "/bin/speedtest-cli"))
             (substitute-command-block* "src/blocks/xrandr.rs"
               "xrandr" (search-input-file inputs "/bin/xrandr"))
             (substitute-command-block* "src/util.rs"
               "sh" (search-input-file inputs "/bin/sh"))
             (substitute-command-block* "src/subprocess.rs"
               "sh" (search-input-file inputs "/bin/sh"))))
         (add-after 'install 'install-resources
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "files" (string-append out "/share")))))
         (add-after 'install 'wrap-i3status
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (paths (map
                           (lambda (input)
                             (string-append
                               (assoc-ref inputs input) "/bin"))
                           '("iproute2" "kdeconnect"))))
               (wrap-program (string-append out "/bin/i3status-rs")
                 `("PATH" prefix ,paths))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-utils
           bash-minimal
           dbus
           iproute
           kdeconnect
           (list lm-sensors "lib")
           pulseaudio
           openssl
           setxkbmap
           speedtest-cli
           xrandr))
    (home-page "https://github.com/greshake/i3status-rust/")
    (synopsis "Replacement for i3status, written in Rust")
    (description "@code{i3status-rs} is a feature-rich and resource-friendly
replacement for i3status, written in pure Rust.  It provides a way to display
@code{blocks} of system information (time, battery status, volume, etc) on the i3
bar.  It is also compatible with sway.")
    (license license:gpl3)))

(define-public just
  (package
    (name "just")
    (version "1.35.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "just" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0q5a94wrkvb01q0rcz59w0qzsdh7wp698nk0crdqn0j1vwwy7r50"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=backticks::trailing_newlines_are_stripped"
         "--skip=completions::bash"
         "--skip=completions::replacements"
         "--skip=functions::env_var_functions"
         "--skip=string::shebang_backtick")
       #:install-source? #f
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-blake3" ,rust-blake3-1)
        ("rust-camino" ,rust-camino-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete" ,rust-clap-complete-4)
        ("rust-clap-mangen" ,rust-clap-mangen-0.2)
        ("rust-ctrlc" ,rust-ctrlc-3)
        ("rust-derivative" ,rust-derivative-2)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-dotenvy" ,rust-dotenvy-0.15)
        ("rust-edit-distance" ,rust-edit-distance-2)
        ("rust-heck" ,rust-heck-0.5)
        ("rust-lexiclean" ,rust-lexiclean-0.0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-regex" ,rust-regex-1)
        ("rust-semver" ,rust-semver-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-shellexpand" ,rust-shellexpand-3)
        ("rust-similar" ,rust-similar-2)
        ("rust-snafu" ,rust-snafu-0.8)
        ("rust-strum" ,rust-strum-0.26)
        ("rust-target" ,rust-target-2)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-typed-arena" ,rust-typed-arena-2)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-uuid" ,rust-uuid-1))
       #:cargo-development-inputs
       (("rust-executable-path" ,rust-executable-path-1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-temptree" ,rust-temptree-0.2)
        ("rust-which" ,rust-which-6))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-hardcoded-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (cons "src/justfile.rs"
                                (find-files "tests/" "\\.rs$"))
               (("/bin/sh")
                (search-input-file inputs "/bin/sh"))
               (("/usr/bin/env sh")
                (search-input-file inputs "/bin/sh"))
               (("/usr/bin/env")
                (search-input-file inputs "/bin/env"))
               (("/bin/echo")
                (search-input-file inputs "/bin/echo")))))
         (add-after 'install 'install-extras
           (lambda* (#:key native-inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (man1 (string-append share "/man/man1"))
                    (bash-completions-dir
                     (string-append out "/etc/bash_completion.d/"))
                    (zsh-completions-dir
                     (string-append share "/zsh/site-functions"))
                    (fish-completions-dir
                     (string-append share "/fish/vendor_completions.d"))
                    (elvish-completions-dir
                     (string-append share "/elvish/lib"))
                    (just (if ,(%current-target-system)
                          (search-input-file native-inputs "/bin/just")
                          (string-append out "/bin/just"))))
               (mkdir "man")
               (with-output-to-file "man/just.1"
                 (lambda _ (invoke just "--man")))
               (install-file "man/just.1" man1)

               (mkdir-p bash-completions-dir)
               (with-output-to-file
                 (string-append bash-completions-dir "/just")
                 (lambda _ (invoke just "--completions" "bash")))
               ;; This seems to be broken currently
               ;(mkdir-p zsh-completions-dir)
               ;(with-output-to-file
               ;  (string-append zsh-completions-dir "/_just")
               ;  (lambda _ (invoke just "--completions" "zsh")))
               (mkdir-p fish-completions-dir)
               (with-output-to-file
                 (string-append fish-completions-dir "/just.fish")
                 (lambda _ (invoke just "--completions" "fish")))
               (mkdir-p elvish-completions-dir)
               (with-output-to-file
                 (string-append elvish-completions-dir "/just")
                 (lambda _ (invoke just "--completions" "elvish")))))))))
    (native-inputs (if (%current-target-system)
                       (list this-package)
                       '()))
    (inputs (list bash-minimal coreutils-minimal))
    (home-page "https://github.com/casey/just")
    (synopsis "Just a command runner")
    (description "This package provides @code{just}, a command runner.
@code{just} is a handy way to save and run project-specific commands.")
    (license license:cc0)))

(define-public kibi
  (package
    (name "kibi")
    (version "0.2.2")
    (source
     (origin
       ;; crates.io doesn't have the config files
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ilai-deutel/kibi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s9ka3pfhpssms2y5707f33n59ljnqqwp7jarh2l55a9dhlnl7d3"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-test-flags `(list "--release" "--"
                                "--skip=syntax::tests::syntax_d_files")
      #:cargo-inputs `(("rust-libc" ,rust-libc-0.2)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-winapi" ,rust-winapi-0.3)
                       ("rust-winapi-util" ,rust-winapi-util-0.1))
      #:cargo-development-inputs `(("rust-serial-test" ,rust-serial-test-0.5)
                                   ("rust-tempfile" ,rust-tempfile-3))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'install-extras
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (share (string-append out "/share"))
                              (syntax.d (string-append share "/syntax.d"))
                              (etc (string-append out "/etc")))
                         (mkdir-p syntax.d)
                         (copy-recursively "syntax.d" syntax.d)
                         (rename-file "config_example.ini" "config.ini")
                         (install-file "config.ini" etc)))))))
    (home-page "https://github.com/ilai-deutel/kibi")
    (synopsis "Featureful text editor in less than 1024 lines of code")
    (description
     "Inspired by the kilo text editor in C, this package provides a text
editor in less than 1024 lines of code with syntax higlighting, search and
more.")
    (license (list license:expat license:asl2.0))))

(define-public macchina
  (package
    (name "macchina")
    (version "6.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "macchina" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "044bygdazv8l1d5sf7pxn2xp26pmnx2b65122qzb37m1ylb1ksg6"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-inputs `(("rust-ansi-to-tui" ,rust-ansi-to-tui-2)
                       ("rust-atty" ,rust-atty-0.2)
                       ("rust-bytesize" ,rust-bytesize-1)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-color-to-tui" ,rust-color-to-tui-0.2)
                       ("rust-colored" ,rust-colored-2)
                       ("rust-dirs" ,rust-dirs-4)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-libmacchina" ,rust-libmacchina-6)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-shellexpand" ,rust-shellexpand-3)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-toml" ,rust-toml-0.5)
                       ("rust-tui" ,rust-tui-0.19)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-vergen" ,rust-vergen-7))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'install-extras
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (share (string-append out "/share"))
                              (contrib (string-append share "/contrib")))
                         (mkdir-p contrib)
                         (copy-recursively "contrib" contrib)))))))
    (native-inputs (list pkg-config))
    (inputs (list libgit2 sqlite zlib))
    (home-page "https://github.com/Macchina-CLI/macchina")
    (synopsis "System information fetcher with an emphasis on performance")
    (description
     "This package provides a system information fetcher with an emphasis on
performance.  Similar to neofetch, this package prints out system information
on the terminal in a visually appealing way.")
    (license license:expat)))

(define-public maturin
  (package
    (name "maturin")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "maturin" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ia5xziazpcpc1wwg8jlz5nmza87cz7nb039gg38jgw3704p4dls"))
              (patches (search-patches "maturin-no-cross-compile.patch"))
              (snippet
               #~(begin (use-modules (guix build utils))
                        ;; Remove support for x86_64h-apple-darwin.
                        ;; This target causes maturin to fail to build.
                        (substitute* "src/target.rs"
                          (("\\| Architecture::X86_64h ") ""))))))
    (build-system cargo-build-system)
    (arguments
     `(#:modules ((guix build cargo-build-system)
                  ((guix build pyproject-build-system) #:prefix py:)
                  (guix build utils))
       #:imported-modules ((guix build cargo-build-system)
                           (guix build cargo-utils)
                           ,@%pyproject-build-system-modules)
       #:install-source? #f
       #:cargo-test-flags
       '("--release" "--"
         ;; Not all files are included.
         "--skip=build_options::test::test_find_bridge_bin"
         "--skip=build_options::test::test_find_bridge_cffi"
         "--skip=build_options::test::test_find_bridge_pyo3"
         "--skip=build_options::test::test_find_bridge_pyo3_abi3"
         "--skip=build_options::test::test_find_bridge_pyo3_feature"
         "--skip=metadata::test::test_implicit_readme"
         "--skip=metadata::test::test_merge_metadata_from_pyproject_dynamic_license_test"
         "--skip=metadata::test::test_merge_metadata_from_pyproject_toml"
         "--skip=metadata::test::test_merge_metadata_from_pyproject_toml_with_customized_python_source_dir"
         "--skip=pyproject_toml::tests::test_warn_missing_maturin_version")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-base64" ,rust-base64-0.21)
        ("rust-bytesize" ,rust-bytesize-1)
        ("rust-cargo-config2" ,rust-cargo-config2-0.1)
        ("rust-cargo-options" ,rust-cargo-options-0.7)
        ;("rust-cargo-xwin" ,rust-cargo-xwin-0.16)
        ;("rust-cargo-zigbuild" ,rust-cargo-zigbuild-0.18)
        ("rust-cargo-metadata" ,rust-cargo-metadata-0.18)
        ("rust-cbindgen" ,rust-cbindgen-0.26)
        ("rust-cc" ,rust-cc-1)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete-command" ,rust-clap-complete-command-0.5)
        ("rust-configparser" ,rust-configparser-3)
        ("rust-console" ,rust-console-0.15)
        ("rust-dialoguer" ,rust-dialoguer-0.11)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-fat-macho" ,rust-fat-macho-0.4)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-fs-err" ,rust-fs-err-2)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-goblin" ,rust-goblin-0.7)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-indexmap" ,rust-indexmap-2)
        ("rust-itertools" ,rust-itertools-0.12)
        ("rust-keyring" ,rust-keyring-2)
        ("rust-lddtree" ,rust-lddtree-0.3)
        ("rust-minijinja" ,rust-minijinja-1)
        ("rust-multipart" ,rust-multipart-0.18)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-normpath" ,rust-normpath-1)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-path-slash" ,rust-path-slash-0.2)
        ("rust-pep440-rs" ,rust-pep440-rs-0.3)
        ("rust-pep508-rs" ,rust-pep508-rs-0.2)
        ("rust-platform-info" ,rust-platform-info-2)
        ("rust-pyproject-toml" ,rust-pyproject-toml-0.8)
        ("rust-python-pkginfo" ,rust-python-pkginfo-0.6)
        ("rust-regex" ,rust-regex-1)
        ("rust-rustc-version" ,rust-rustc-version-0.4)
        ("rust-rustls" ,rust-rustls-0.21)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
        ("rust-semver" ,rust-semver-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-tar" ,rust-tar-0.4)
        ("rust-target-lexicon" ,rust-target-lexicon-0.12)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-textwrap" ,rust-textwrap-0.16)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-toml" ,rust-toml-0.8)
        ("rust-toml-edit" ,rust-toml-edit-0.21)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-ureq" ,rust-ureq-2)
        ("rust-url" ,rust-url-2)
        ("rust-wild" ,rust-wild-2)
        ("rust-zip" ,rust-zip-0.6))
       #:cargo-development-inputs
       (("rust-expect-test" ,rust-expect-test-1)
        ("rust-indoc" ,rust-indoc-2)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-trycmd" ,rust-trycmd-0.14)
        ("rust-which" ,rust-which-5))
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-python-module
           (lambda _
             ;; Match the features from the cargo-build-system and Cargo.toml.
             (setenv "MATURIN_SETUP_ARGS" "--features=default")
             ((assoc-ref py:%standard-phases 'build))))

         ;; We can't use the pyproject install phase because maturin is a
         ;; binary, not a python script.
         (add-after 'install 'install-python-module
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (wheel (car (find-files "dist" "\\.whl$")))
                   (site-dir (py:site-packages inputs outputs))
                   (pyversion
                     (string-append "python"
                                    (py:python-version
                                      (assoc-ref inputs "python-wrapper")))))
               (invoke "python" "-m" "zipfile" "-e" wheel site-dir)
               (mkdir-p (string-append out "/bin"))
               (for-each delete-file
                         (find-files (string-append out "/lib/" pyversion)
                                     "^maturin$")))))
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (maturin (string-append out "/bin/maturin")))
               ;; TODO? fig, nushell, powershell
               (mkdir-p (string-append share "/bash-completion/completions"))
               (with-output-to-file
                 (string-append share "/bash-completion/completions/maturin")
                 (lambda _ (invoke maturin "completions" "bash")))
               (mkdir-p (string-append share "/fish/vendor_completions.d"))
               (with-output-to-file
                 (string-append share "/fish/vendor_completions.d/maturin.fish")
                 (lambda _ (invoke maturin "completions" "fish")))
               (mkdir-p (string-append share "/zsh/site-functions"))
               (with-output-to-file
                 (string-append share "/zsh/site-functions/_maturin")
                 (lambda _ (invoke maturin "completions" "zsh")))
               (mkdir-p (string-append share "/elvish/lib"))
               (with-output-to-file
                 (string-append share "/elvish/lib/maturin")
                 (lambda _ (invoke maturin "completions" "elvish")))))))))
    (propagated-inputs
     (list python-tomli))
    (inputs (list bzip2))
    (native-inputs
     (list python-wheel
           python-wrapper
           python-setuptools-rust))
    (home-page "https://github.com/pyo3/maturin")
    (synopsis "Build and publish crates and python packages")
    (description
     "Build and publish crates with @code{pyo3}, @code{rust-cpython} and
@code{cffi} bindings as well as rust binaries as python packages.")
    (license (list license:expat license:asl2.0))))

(define-public netavark
  (package
    (name "netavark")
    (version "1.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "netavark" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1viyj9xqq9hkcsghrfx7wjmky3hkxfr96952f9favd4zg9ih64yw"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* (find-files "." "^Cargo\\.toml$")
                  (("\"~([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-env-logger" ,rust-env-logger-0.11)
                       ("rust-fs2" ,rust-fs2-0.4)
                       ("rust-futures-channel" ,rust-futures-channel-0.3)
                       ("rust-futures-core" ,rust-futures-core-0.3)
                       ("rust-futures-util" ,rust-futures-util-0.3)
                       ("rust-ipnet" ,rust-ipnet-2)
                       ("rust-iptables" ,rust-iptables-0.5)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-mozim" ,rust-mozim-0.2)
                       ("rust-netlink-packet-core" ,rust-netlink-packet-core-0.7)
                       ("rust-netlink-packet-route" ,rust-netlink-packet-route-0.18)
                       ("rust-netlink-packet-utils" ,rust-netlink-packet-utils-0.5)
                       ("rust-netlink-sys" ,rust-netlink-sys-0.8)
                       ("rust-nftables" ,rust-nftables-0.3)
                       ("rust-nispor" ,rust-nispor-1)
                       ("rust-nix" ,rust-nix-0.27)
                       ("rust-prost" ,rust-prost-0.12)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-value" ,rust-serde-value-0.7)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-sysctl" ,rust-sysctl-0.5)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-tonic" ,rust-tonic-0.10)
                       ("rust-tonic-build" ,rust-tonic-build-0.10)
                       ("rust-tower" ,rust-tower-0.4)
                       ("rust-url" ,rust-url-2)
                       ("rust-zbus" ,rust-zbus-3))
       #:cargo-development-inputs (("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list protobuf))
    (home-page "https://github.com/containers/netavark")
    (synopsis "Container network stack")
    (description "Netavark is a rust based network stack for containers.  It
is being designed to work with Podman but is also applicable for other OCI
container management applications.")
    (license license:asl2.0)))

(define-public ripgrep
  (package
    (name "ripgrep")
    (version "14.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ripgrep" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n81lnfzy556f63pgnizay2sgx8fgn4mmailbybjfiaqvhr80yzp"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-inputs `(("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bstr" ,rust-bstr-1)
                       ("rust-grep" ,rust-grep-0.3)
                       ("rust-ignore" ,rust-ignore-0.4)
                       ("rust-jemallocator" ,rust-jemallocator-0.5)
                       ("rust-lexopt" ,rust-lexopt-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-termcolor" ,rust-termcolor-1)
                       ("rust-textwrap" ,rust-textwrap-0.16))
      #:cargo-development-inputs `(("rust-serde" ,rust-serde-1)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-walkdir" ,rust-walkdir-2))
      #:install-source? #f
      ;; Note: the built target 'rg' binary is required for 'install-extras
      #:phases #~(modify-phases %standard-phases
                   (add-after 'install 'install-extras
                     (lambda* (#:key native-inputs outputs #:allow-other-keys)
                       (let* ((out (assoc-ref outputs "out"))
                              (share (string-append out "/share"))
                              (bash-completions-dir
                                (string-append out "/etc/bash_completion.d/"))
                              (zsh-completions-dir
                                (string-append share "/zsh/site-functions"))
                              (fish-completions-dir
                                (string-append share "/fish/vendor_completions.d"))
                              (man1 (string-append share "/man/man1"))
                              (rg (if #$(%current-target-system)
                                    (search-input-file native-inputs "/bin/rg")
                                    (string-append out "/bin/rg"))))
                           (mkdir-p man1)
                           (with-output-to-file (string-append man1 "/rg.1")
                             (lambda _
                               (invoke rg "--generate" "man")))
                           (mkdir-p bash-completions-dir)
                           (with-output-to-file (string-append
                                                  bash-completions-dir "/rg")
                             (lambda _
                               (invoke rg "--generate" "complete-bash")))
                           (mkdir-p zsh-completions-dir)
                           (with-output-to-file (string-append
                                                  zsh-completions-dir "/_rg")
                             (lambda _
                               (invoke rg "--generate" "complete-zsh")))
                           (mkdir-p fish-completions-dir)
                           (with-output-to-file
                             (string-append fish-completions-dir "/rg.fish")
                             (lambda _
                               (invoke rg "--generate" "complete-fish")))))))
      #:features '(list "pcre2")))
    (inputs (list pcre2))
    (native-inputs (cons* pkg-config (if (%current-target-system)
                                         (list this-package)
                                         '())))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "Line-oriented search tool and Rust successor to @command{grep}")
    (description
     "@code{ripgrep} (@command{rg}) is a line-oriented search tool that
recursively searches your current directory for a regex pattern while
respecting your gitignore rules. @code{ripgrep} is similar to other popular
search tools like The Silver Searcher, @command{ack} and @command{grep}.")
    (license (list license:unlicense license:expat))))

(define-public rot8
  (package
    (name "rot8")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rot8" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bvb87sr9pkf6sj5ghgmga4nrp5kwiqnllzi672da5vs915xh8li"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-clap" ,rust-clap-3)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-wayland-client" ,rust-wayland-client-0.31)
        ("rust-wayland-protocols-wlr" ,rust-wayland-protocols-wlr-0.2))))
    (home-page "https://github.com/efernau/rot8/")
    (synopsis "Automatic display rotation using built-in accelerometer")
    (description "@command{rot8} is a daemon that automates rotating screen and
associated input devices using the built-in accelerometer; handy for convertible
touchscreen devices.")
    (license license:expat)))

(define-public rust-swc
  (package
    (name "rust-swc")
    (version "1.2.124")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swc-project/swc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1cb65vl437sy7shflsazi2k4sz53v3r85dj8rb32ny1j6njczj4h"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* (find-files "." "^Cargo\\.toml$")
                  (("\"=([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-build-flags
       '("--release" "-p" "swc_cli")
       #:cargo-test-flags
       '("--release" "-p" "swc_cli")
       #:cargo-inputs
       (("rust-abi-stable" ,rust-abi-stable-0.10)
        ("rust-ahash" ,rust-ahash-0.7)
        ("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-arrayvec" ,rust-arrayvec-0.5)
        ("rust-arbitrary" ,rust-arbitrary-1)
        ("rust-auto-impl" ,rust-auto-impl-0.5)
        ("rust-auto-impl" ,rust-auto-impl-0.4)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-bincode" ,rust-bincode-1)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-browserslist-rs" ,rust-browserslist-rs-0.6)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-console-error-panic-hook" ,rust-console-error-panic-hook-0.1)
        ("rust-copyless" ,rust-copyless-0.1)
        ("rust-crc" ,rust-crc-1)
        ("rust-darling" ,rust-darling-0.10)
        ("rust-dashmap" ,rust-dashmap-4)
        ("rust-debug-unreachable" ,rust-debug-unreachable-0.1)
        ("rust-difference" ,rust-difference-2)
        ("rust-either" ,rust-either-1)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-inflector" ,rust-inflector-0.11)
        ("rust-is-macro" ,rust-is-macro-0.1)
        ("rust-lexical" ,rust-lexical-5)
        ("rust-libloading" ,rust-libloading-0.7)
        ("rust-lru" ,rust-lru-0.7)
        ("rust-mimalloc-rust" ,rust-mimalloc-rust-0.1)
        ("rust-napi" ,rust-napi-2)
        ("rust-napi-build" ,rust-napi-build-1)
        ("rust-napi-derive" ,rust-napi-derive-2)
        ("rust-nom" ,rust-nom-5)
        ("rust-normpath" ,rust-normpath-0.2)
        ("rust-num-bigint" ,rust-num-bigint-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-ordered-float" ,rust-ordered-float-2)
        ("rust-owning-ref" ,rust-owning-ref-0.4)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-parking-lot" ,rust-parking-lot-0.7)
        ("rust-parking-lot-core" ,rust-parking-lot-core-0.8)
        ("rust-path-clean" ,rust-path-clean-0.1)
        ("rust-paw" ,rust-paw-1)
        ("rust-petgraph" ,rust-petgraph-0.5)
        ("rust-phf" ,rust-phf-0.8)
        ("rust-pmutil" ,rust-pmutil-0.5)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.7)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.6)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-radix-fmt" ,rust-radix-fmt-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-relative-path" ,rust-relative-path-1)
        ("rust-retain-mut" ,rust-retain-mut-0.1)
        ("rust-rustc-hash" ,rust-rustc-hash-1)
        ("rust-scoped-tls" ,rust-scoped-tls-1)
        ("rust-semver" ,rust-semver-0.9)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-regex" ,rust-serde-regex-1)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-smallvec" ,rust-smallvec-1)
        ("rust-sourcemap" ,rust-sourcemap-6)
        ("rust-st-map" ,rust-st-map-0.1)
        ("rust-string-cache" ,rust-string-cache-0.8)
        ("rust-string-cache-codegen" ,rust-string-cache-codegen-0.5)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-syn" ,rust-syn-1)
        ("rust-termcolor" ,rust-termcolor-1)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-typed-arena" ,rust-typed-arena-2)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-unicode-xid" ,rust-unicode-xid-0.2)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-dashmap" ,rust-dashmap-4)
        ("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-ntest" ,rust-ntest-0.7)
        ("rust-path-clean" ,rust-path-clean-0.1)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.7)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.6)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-sourcemap" ,rust-sourcemap-6)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-url" ,rust-url-2)
        ("rust-walkdir" ,rust-walkdir-2))
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (swc (car (find-files "target" "^swc$"))))
               (install-file swc bin)))))))
    (home-page "https://swc.rs/")
    (synopsis "Typescript/javascript compiler")
    (description "@code{rust-swc} is a typescript/javascript compiler.  It
consumes a javascript or typescript file which uses recently added features
like async-await and emits javascript code which can be executed on old
browsers.")
    (license (list license:expat
                   license:asl2.0))))

(define-deprecated rust-swc-1 rust-swc)

(define-public rust-cargo-edit
  (package
    (name "rust-cargo-edit")
    (version "0.12.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cargo-edit" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03lxi7z1n9xq287lqvqnhzg5r0yv1fi3569ryw3jqcrvv8nqs0c2"))
              (patches (search-patches "rust-cargo-edit-remove-ureq.patch"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       ;; error[E0463]: can't find crate for `cargo_test_macro`
       #:tests? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-cargo-metadata" ,rust-cargo-metadata-0.15)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-cargo" ,rust-clap-cargo-0.12)
        ("rust-concolor-control" ,rust-concolor-control-0.0.7)
        ("rust-crates-index" ,rust-crates-index-0.19)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-env-proxy" ,rust-env-proxy-0.4)
        ("rust-git2" ,rust-git2-0.17)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-home" ,rust-home-0.5)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-pathdiff" ,rust-pathdiff-0.2)
        ("rust-regex" ,rust-regex-1)
        ("rust-semver" ,rust-semver-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-subprocess" ,rust-subprocess-0.2)
        ("rust-termcolor" ,rust-termcolor-1)
        ("rust-toml" ,rust-toml-0.7)
        ("rust-toml-edit" ,rust-toml-edit-0.19)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-assert-fs" ,rust-assert-fs-1)
        ("rust-predicates" ,rust-predicates-3)
        ("rust-snapbox" ,rust-snapbox-0.4)
        ("rust-trycmd" ,rust-trycmd-0.14)
        ("rust-url" ,rust-url-2))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-default-to-vendored-libgit2
           (lambda _
             (substitute* "Cargo.toml"
               ((".*\"vendored-libgit2\".*") "")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libgit2-1.6
           libssh2
           openssl
           zlib))
    (home-page "https://github.com/killercup/cargo-edit")
    (synopsis "Add and remove dependencies from the command line")
    (description
     "This package extends Cargo to allow you to add and remove dependencies
by modifying your @file{Cargo.toml} file from the command line.")
    (license (list license:asl2.0 license:expat))))

(define-deprecated rust-cargo-edit-0.8 rust-cargo-edit)

(define-public git-interactive-rebase-tool
  (package
    (name "git-interactive-rebase-tool")
    (version "2.1.0")
    (source
     (origin
       ;; crates.io does not provide the test data.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mitmaro/git-interactive-rebase-tool")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "173spqqpyc00kvfmldjmjfqizh9b4spq4xw4bskd4dny8qcpz28d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-test-flags
       ;; https://github.com/MitMaro/git-interactive-rebase-tool/issues/586
       '("--release" "--" "--skip=tests::success")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-crossterm" ,rust-crossterm-0.19)
        ("rust-git2" ,rust-git2-0.13)
        ("rust-num-format" ,rust-num-format-0.4)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-xi-unicode" ,rust-xi-unicode-0.3))
       #:cargo-development-inputs
       (("rust-concat-idents" ,rust-concat-idents-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rstest" ,rust-rstest-0.6)
        ("rust-serial-test" ,rust-serial-test-0.5)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libgit2-1.3 zlib))
    (home-page "https://gitrebasetool.mitmaro.ca/")
    (synopsis "Terminal based sequence editor for git interactive rebase")
    (description
     "This application is a terminal-based sequence editor for git interactive
rebase.")
    (license license:gpl3+)))

(define-public rust-cbindgen-0.26
  (package
    (name "rust-cbindgen")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cbindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jdbxmn5h5nlr4bifx85gny309djv5djs9q78fa1d7sj0wdw2sys"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clap" ,rust-clap-3)
                       ("rust-heck" ,rust-heck-0.4)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-tempfile" ,rust-tempfile-3)
                       ("rust-toml" ,rust-toml-0.5))
       #:cargo-development-inputs (("rust-serial-test" ,rust-serial-test-0.5))))
    (native-inputs (list python-cython))
    (home-page "https://github.com/eqrion/cbindgen/")
    (synopsis "Tool for generating C bindings to Rust code")
    (description
     "This package provides a tool for generating C/C++ bindings to Rust code.")
    (license license:mpl2.0)))

(define-public rust-cbindgen-0.24
  (package
    (inherit rust-cbindgen-0.26)
    (name "rust-cbindgen")
    (version "0.24.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "cbindgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13g1k2zljdp326b0cv1nhyh7jsacd364fh0cr2g828hiyfm2z4jb"))))))

(define-public rust-cbindgen-0.23
  (package
    (inherit rust-cbindgen-0.24)
    (name "rust-cbindgen")
    (version "0.23.0")
    (source (origin
             (method url-fetch)
             (uri (crate-uri "cbindgen" version))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "006rn3fn4njayjxr2vd24g1awssr9i3894nbmfzkybx07j728vav"))))))

(define-public rust-cbindgen rust-cbindgen-0.26)

(define-public rust-bindgen-cli
  (package
    (name "rust-bindgen-cli")
    (version "0.69.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00dfny07m4xfnqbfn7yr7cqwilj6935lbyg5d39yxjfj8jglfcax"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-complete" ,rust-clap-complete-4)
                       ("rust-env-logger" ,rust-env-logger-0.10)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-shlex" ,rust-shlex-1))
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
                    (bindgen (string-append bin "/bindgen"))
                    (llvm-dir (string-append
                                (assoc-ref inputs "clang") "/lib")))
               (install-file "target/release/bindgen" bin)
               (wrap-program bindgen
                 `("LIBCLANG_PATH" = (,llvm-dir))))))
         (add-after 'install 'install-completions
           (lambda* (#:key native-inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (bindgen (string-append out "/bin/bindgen")))
               (mkdir-p (string-append share "/bash-completion/completions"))
               (with-output-to-file
                 (string-append share "/bash-completion/completions/bindgen")
                 (lambda _ (invoke bindgen "--generate-shell-completions" "bash")))
               (mkdir-p (string-append share "/fish/vendor_completions.d"))
               (with-output-to-file
                 (string-append share "/fish/vendor_completions.d/bindgen.fish")
                 (lambda _ (invoke bindgen "--generate-shell-completions" "fish")))
               (mkdir-p (string-append share "/zsh/site-functions"))
               (with-output-to-file
                 (string-append share "/zsh/site-functions/_bindgen")
                 (lambda _ (invoke bindgen "--generate-shell-completions" "zsh")))
               (mkdir-p (string-append share "/elvish/lib"))
               (with-output-to-file
                 (string-append share "/elvish/lib/bindgen")
                 (lambda _
                   (invoke bindgen "--generate-shell-completions" "elvish")))))))))
    (inputs (list bash-minimal clang))
    (home-page "https://rust-lang.github.io/rust-bindgen/")
    (synopsis "Generate Rust FFI bindings to C and C++ libraries")
    (description "This package can be used to automatically generate Rust FFI
bindings to C and C++ libraries.  This package provides the @command{bindgen}
command.")
    (license license:bsd-3)))

(define-public sniffglue
  (package
    (name "sniffglue")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sniffglue" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0q63dysxzzqyknm3kqk0dff1vm8j6g05dkjwn7kqaglmf9ksd7v3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-bstr" ,rust-bstr-1)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete" ,rust-clap-complete-4)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-dhcp4r" ,rust-dhcp4r-0.2)
        ("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-dns-parser" ,rust-dns-parser-0.8)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-httparse" ,rust-httparse-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-nix" ,rust-nix-0.27)
        ("rust-nom" ,rust-nom-7)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-pcap-sys" ,rust-pcap-sys-0.1)
        ("rust-pktparse" ,rust-pktparse-0.7)
        ("rust-reduce" ,rust-reduce-0.1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-syscallz" ,rust-syscallz-0.17)
        ("rust-tls-parser" ,rust-tls-parser-0.11)
        ("rust-toml" ,rust-toml-0.8)
        ("rust-uzers" ,rust-uzers-0.11))
       #:cargo-development-inputs
       (("rust-boxxy" ,rust-boxxy-0.13))))
    (inputs
     (list libpcap libseccomp))
    (home-page "https://github.com/kpcyrd/sniffglue")
    (synopsis "Secure multithreaded packet sniffer")
    (description
     "This package provides a network sniffer written in Rust.  Packets
are parsed concurrently using a thread pool to utilize all cpu cores.  A goal
of the project is to be runnable on untrusted networks without crashing.")
    (license license:gpl3)))

(define-public tectonic
  (package
    (name "tectonic")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tectonic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q4mz2c32gfypx33zlzgd1q9h4322jrk13fzvsf8h676ylclqzpc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-byte-unit" ,rust-byte-unit-4)
        ("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-error-chain" ,rust-error-chain-0.12)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-fs2" ,rust-fs2-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-md-5" ,rust-md-5-0.9)
        ("rust-open" ,rust-open-1)
        ("rust-quick-xml" ,rust-quick-xml-0.22)
        ("rust-serde" ,rust-serde-1)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-tectonic-bridge-core" ,rust-tectonic-bridge-core-0.3)
        ("rust-tectonic-bundles" ,rust-tectonic-bundles-0.3)
        ("rust-tectonic-docmodel" ,rust-tectonic-docmodel-0.2)
        ("rust-tectonic-engine-bibtex" ,rust-tectonic-engine-bibtex-0.1)
        ("rust-tectonic-engine-spx2html" ,rust-tectonic-engine-spx2html-0.1)
        ("rust-tectonic-engine-xdvipdfmx" ,rust-tectonic-engine-xdvipdfmx-0.4)
        ("rust-tectonic-engine-xetex" ,rust-tectonic-engine-xetex-0.4)
        ("rust-tectonic-errors" ,rust-tectonic-errors-0.2)
        ("rust-tectonic-geturl" ,rust-tectonic-geturl-0.3)
        ("rust-tectonic-io-base" ,rust-tectonic-io-base-0.4)
        ("rust-tectonic-status-base" ,rust-tectonic-status-base-0.2)
        ("rust-tectonic-xdv" ,rust-tectonic-xdv-0.2)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-termcolor" ,rust-termcolor-1)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-url" ,rust-url-2)
        ("rust-watchexec" ,rust-watchexec-1)
        ("rust-zip" ,rust-zip-0.5))
       #:cargo-development-inputs
       (("rust-filetime" ,rust-filetime-0.2)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-headers" ,rust-headers-0.2)
        ("rust-hyper" ,rust-hyper-0.12)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-tokio" ,rust-tokio-0.1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (copy-recursively "docs/src" doc)))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list fontconfig
           freetype
           graphite2
           harfbuzz
           icu4c
           libpng
           openssl
           zlib))
    (home-page "https://tectonic-typesetting.github.io/")
    (synopsis "Complete, embeddable TeX/LaTeX engine")
    (description
     "This package provides a modernized, complete, embeddable
TeX/LaTeX engine.  Tectonic is forked from the XeTeX extension to the
classic Web2C implementation of TeX and uses the TeXLive distribution
of support files.")
    (license license:expat)))

(define-public treefmt
  (package
    (name "treefmt")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "treefmt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pfx8kgaf0rc8ijps2fqb61gjnak3sf430hvg52bnby9qqyd51h8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-verbosity-flag" ,rust-clap-verbosity-flag-2)
        ("rust-console" ,rust-console-0.13)
        ("rust-directories" ,rust-directories-3)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-globset" ,rust-globset-0.4)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-log" ,rust-log-0.4)
        ("rust-path-clean" ,rust-path-clean-0.1)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-which" ,rust-which-4))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-mockall" ,rust-mockall-0.11))))
    (home-page "https://numtide.github.io/treefmt")
    (synopsis "Command-line application to format the code tree")
    (description
     "This application provides a way to unify the formatting process of the
codebase.  It is nice for large code trees where using multiple formatters are
common.  @command{treefmt} comes with the following features.

@itemize
@item Unified CLI and output.
@item Runs formatters in parallel.
@item Cache changed files for performance.
@end itemize

The application does have some design decisions to keep in mind.

@itemize
@item The source code is kept under version control, making it possible to
revert and check changes.
@item Only one formatter per file, making outputs idempotent.
@end itemize")
    (license license:expat)))

(define-public hex
  (package
    (name "hex")
    (version "0.6.0")
    (source
     (origin
       ;; crates.io does not provide the test data.
       ;; Not all releases are pushed to crates.io.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sitkevij/hex")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kv07ghibifs6rnskg1na6a0hdb0f8vqfbpv5k8g09lc2075gjv1"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (substitute* "Cargo.toml"
                   ;; rusty-hook provides a git hook for CI.
                   ((".*rusty-hook.*") ""))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-clap" ,rust-clap-4)
        ("rust-no-color" ,rust-no-color-0.1))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "hx.1" (string-append out "/share/man/man1"))))))))
    (home-page "https://github.com/sitkevij/hex")
    (synopsis "Hexadecimal colorized view of a file")
    (description
     "@command{hx} accepts a file path as input and outputs a hexadecimal
colorized view to stdout.")
    (license license:expat)))

(define-public tokei
  (package
    (name "tokei")
    (version "12.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokei" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "000w549v1bpw7r57xw656p40ywf1gimvxxx5cjnri2js0xg927x4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-aho-corasick" ,rust-aho-corasick-0.7)
        ("rust-clap" ,rust-clap-2)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
        ("rust-dashmap" ,rust-dashmap-4)
        ("rust-dirs" ,rust-dirs-3)
        ("rust-encoding-rs-io" ,rust-encoding-rs-io-0.1)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-grep-searcher" ,rust-grep-searcher-0.1)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-log" ,rust-log-0.4)
        ("rust-num-format" ,rust-num-format-0.4)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-cbor" ,rust-serde-cbor-0.11)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ("rust-tera" ,rust-tera-1)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-toml" ,rust-toml-0.5))
       #:cargo-development-inputs
       (("rust-git2" ,rust-git2-0.13)
        ("rust-regex" ,rust-regex-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libgit2 openssl zlib))
    (home-page "https://tokei.rs")
    (synopsis "Count code, quickly")
    (description
     "Tokei is a program that displays statistics about your code.  Tokei will
show number of files, total lines within those files and code, comments, and
blanks grouped by language.")
    (license (list license:expat license:asl2.0))))

(define-public vivid
  (package
    (name "vivid")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "vivid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01fds6dm19bqgqydaa6n051v9l4wh9rb5d6sr9akwp2cc0fs43b7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-colours" ,rust-ansi-colours-1)
        ("rust-clap" ,rust-clap-2)
        ("rust-dirs" ,rust-dirs-3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rust-embed" ,rust-rust-embed-5)
        ("rust-yaml-rust" ,rust-yaml-rust-0.4))))
    (home-page "https://github.com/sharkdp/vivid")
    (synopsis "LS_COLORS environment variable manager")
    (description
     "vivid is a generator for the @code{LS_COLORS} environment variable that
controls the colorized output of ls, tree, fd, bfs, dust and many other tools.

It uses a YAML configuration format for the filetype-database and the color
themes.  In contrast to @command{dircolors}, the database and the themes are
organized in different files.  This allows users to choose and customize color
themes independent from the collection of file extensions.  Instead of using
cryptic ANSI escape codes, colors can be specified in the RRGGBB format and
will be translated to either truecolor (24-bit) ANSI codes or 8-bit codes for
older terminal emulators.")
    (license (list license:expat license:asl2.0))))

(define-public watchexec
  (package
    (name "watchexec")
    (version "1.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "watchexec-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1wp424gzw1zmax5yy5gya15knl24rjx8gi9c7palvq807q3cnj65"))))
    (build-system cargo-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (zsh (string-append out "/share/zsh/site-functions/_watchexec"))
                    (doc (string-append out "/share/doc/watchexec-" ,version)))
               (mkdir-p (dirname zsh))
               ;; FIXME: The crates.io source does not provide zsh
               ;; completions.  But the GitHub source does not compile.
               ;;
               ;; (copy-file "completions/zsh" zsh)
               (install-file "README.md" doc)))))
       #:cargo-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-embed-resource" ,rust-embed-resource-1)
        ("rust-env-logger" ,rust-env-logger-0.8)
        ("rust-log" ,rust-log-0.4)
        ("rust-watchexec" ,rust-watchexec-1))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-1)
        ("rust-insta" ,rust-insta-1))))
    (home-page "https://github.com/watchexec/watchexec")
    (synopsis "Executes commands in response to file modifications")
    (description
     "@command{watchexec} is a simple, standalone tool that watches a path and
runs a command whenever it detects modifications.")
    (license license:asl2.0)))

(define-public rbw
  (package
    (name "rbw")
    (version "1.9.0")
    (outputs '("out" "scripts"))
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rbw" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0rlp55kcac9k0rz1zfhyslkfgsim1ka6bkllfzqrayvdfyxqq51i"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (("\"=([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-aes" ,rust-aes-0.8)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-argon2" ,rust-argon2-0.5)
        ("rust-arrayvec" ,rust-arrayvec-0.7)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-base32" ,rust-base32-0.4)
        ("rust-base64" ,rust-base64-0.21)
        ("rust-block-padding" ,rust-block-padding-0.3)
        ("rust-cbc" ,rust-cbc-0.1)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete" ,rust-clap-complete-4)
        ("rust-copypasta" ,rust-copypasta-0.10)
        ("rust-daemonize" ,rust-daemonize-0.5)
        ("rust-directories" ,rust-directories-5)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-channel" ,rust-futures-channel-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-hkdf" ,rust-hkdf-0.12)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-humantime" ,rust-humantime-2)
        ("rust-is-terminal" ,rust-is-terminal-0.4)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-nix" ,rust-nix-0.26)
        ("rust-pbkdf2" ,rust-pbkdf2-0.12)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-pkcs8" ,rust-pkcs8-0.10)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-region" ,rust-region-3)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-rmpv" ,rust-rmpv-1)
        ("rust-rsa" ,rust-rsa-0.9)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-path-to-error" ,rust-serde-path-to-error-0.1)
        ("rust-serde-repr" ,rust-serde-repr-0.1)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-terminal-size" ,rust-terminal-size-0.3)
        ("rust-textwrap" ,rust-textwrap-0.16)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-tokio-stream" ,rust-tokio-stream-0.1)
        ("rust-tokio-tungstenite" ,rust-tokio-tungstenite-0.20)
        ("rust-totp-lite" ,rust-totp-lite-2)
        ("rust-url" ,rust-url-2)
        ("rust-uuid" ,rust-uuid-1)
        ("rust-zeroize" ,rust-zeroize-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key native-inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (rbw (if ,(%current-target-system)
                          (search-input-file native-inputs "/bin/rbw")
                          (string-append out "/bin/rbw"))))
               (mkdir-p (string-append share "/bash-completion/completions"))
               (with-output-to-file
                 (string-append share "/bash-completion/completions/rbw")
                 (lambda _ (invoke rbw "gen-completions" "bash")))
               (mkdir-p (string-append share "/fish/vendor_completions.d"))
               (with-output-to-file
                 (string-append share "/fish/vendor_completions.d/rbw.fish")
                 (lambda _ (invoke rbw "gen-completions" "fish")))
               (mkdir-p (string-append share "/zsh/site-functions"))
               (with-output-to-file
                 (string-append share "/zsh/site-functions/_rbw")
                 (lambda _ (invoke rbw "gen-completions" "zsh")))
               (mkdir-p (string-append share "/elvish/lib"))
               (with-output-to-file
                 (string-append share "/elvish/lib/rbw")
                 (lambda _ (invoke rbw "gen-completions" "elvish"))))))
         (add-after 'install 'install-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (scripts (assoc-ref outputs "scripts")))
               (for-each (lambda (file)
                           (install-file file (string-append scripts "/bin")))
                         (find-files "bin"))
               (for-each (lambda (file)
                           (wrap-script file
                             ;; TODO: Do we want to wrap these with more programs?
                             ;; pass git fzf libsecret xclip rofi
                             `("PATH" prefix
                                (,(string-append out "/bin")
                                 ,(dirname (search-input-file inputs "/bin/grep"))
                                 ,(dirname (search-input-file inputs "/bin/sed"))
                                 ,(dirname (search-input-file inputs "/bin/perl"))
                                 ,(dirname (search-input-file inputs "/bin/xargs"))
                                 ,(dirname (search-input-file inputs "/bin/sort"))))))
                         (find-files (string-append scripts "/bin")))))))))
    (native-inputs
     (cons* perl (if (%current-target-system)
                   (list this-package)
                   '())))
    (inputs
     (list coreutils-minimal findutils grep perl sed))
    (home-page "https://git.tozt.net/rbw")
    (synopsis "Unofficial Bitwarden CLI")
    (description "This package is an unofficial command line client for
Bitwarden.  Although Bitwarden ships with a command line client, but
it's limited by being stateless, which makes it very difficult to use.  This
client avoids that problem by maintaining a background process which is able
to hold the keys in memory, similar to the way that ssh-agent or gpg-agent
work.  This allows the client to be used in a much simpler way, with the
background agent taking care of maintaining the necessary state.")
    (license license:expat)))

;; Note: Keep rust-cargo and rust-cargo-c in sync with our current
;; rust:cargo version.
(define-public rust-cargo
  (package
    (name "rust-cargo")
    (version "0.78.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p6564hg38xxbpjiiqxmnm1kmysxfjh0kbm5g56n85c3s0wmwc6n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; unresolved import `cargo_test_support`
       #:cargo-inputs
       (("rust-annotate-snippets" ,rust-annotate-snippets-0.10)
        ("rust-anstream" ,rust-anstream-0.6)
        ("rust-anstyle" ,rust-anstyle-1)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-base64" ,rust-base64-0.21)
        ("rust-bytesize" ,rust-bytesize-1)
        ("rust-cargo-credential" ,rust-cargo-credential-0.4)
        ("rust-cargo-credential-libsecret" ,rust-cargo-credential-libsecret-0.4)
        ("rust-cargo-credential-macos-keychain" ,rust-cargo-credential-macos-keychain-0.4)
        ("rust-cargo-credential-wincred" ,rust-cargo-credential-wincred-0.4)
        ("rust-cargo-platform" ,rust-cargo-platform-0.1)
        ("rust-cargo-util" ,rust-cargo-util-0.2)
        ("rust-cargo-util-schemas" ,rust-cargo-util-schemas-0.2)
        ("rust-clap" ,rust-clap-4)
        ("rust-color-print" ,rust-color-print-0.3)
        ("rust-crates-io" ,rust-crates-io-0.40)
        ("rust-curl" ,rust-curl-0.4)
        ("rust-curl-sys" ,rust-curl-sys-0.4)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-git2" ,rust-git2-0.18)
        ("rust-git2-curl" ,rust-git2-curl-0.19)
        ("rust-gix" ,rust-gix-0.57)
        ("rust-gix-features" ,rust-gix-features-0.37)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-hmac" ,rust-hmac-0.12)
        ("rust-home" ,rust-home-0.5)
        ("rust-http-auth" ,rust-http-auth-0.1)
        ("rust-humantime" ,rust-humantime-2)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-im-rc" ,rust-im-rc-15)
        ("rust-indexmap" ,rust-indexmap-2)
        ("rust-itertools" ,rust-itertools-0.12)
        ("rust-jobserver" ,rust-jobserver-0.1)
        ("rust-lazycell" ,rust-lazycell-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libgit2-sys" ,rust-libgit2-sys-0.16)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-opener" ,rust-opener-0.6)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-os-info" ,rust-os-info-3)
        ("rust-pasetors" ,rust-pasetors-0.6)
        ("rust-pathdiff" ,rust-pathdiff-0.2)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-regex" ,rust-regex-1)
        ("rust-rusqlite" ,rust-rusqlite-0.30)
        ("rust-rustfix" ,rust-rustfix-0.8)
        ("rust-semver" ,rust-semver-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-untagged" ,rust-serde-untagged-0.1)
        ("rust-serde-ignored" ,rust-serde-ignored-0.1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-sha1" ,rust-sha1-0.10)
        ("rust-shell-escape" ,rust-shell-escape-0.1)
        ("rust-supports-hyperlinks" ,rust-supports-hyperlinks-2)
        ("rust-tar" ,rust-tar-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-time" ,rust-time-0.3)
        ("rust-toml" ,rust-toml-0.8)
        ("rust-toml-edit" ,rust-toml-edit-0.21)
        ("rust-tracing" ,rust-tracing-0.1)
        ("rust-tracing-subscriber" ,rust-tracing-subscriber-0.3)
        ("rust-unicase" ,rust-unicase-2)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-url" ,rust-url-2)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs (("rust-same-file" ,rust-same-file-1)
                                   ("rust-snapbox" ,rust-snapbox-0.4))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl libssh2 libgit2-1.7 openssl zlib))
    (home-page "https://crates.io")
    (synopsis "Package manager for Rust")
    (description "Cargo, a package manager for Rust.  This package provides
the library crate of Cargo.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cargo-c
  (package
    (name "rust-cargo-c")
    (version "0.9.31+cargo-0.78.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cargo-c" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1y60hhjikkzk5s36gskgbxbyzr6ik7w0dn5j84mvqlilcs3ab0lj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-cargo" ,rust-cargo)
        ("rust-cargo-util" ,rust-cargo-util-0.2)
        ("rust-cbindgen" ,rust-cbindgen-0.26)
        ("rust-cc" ,rust-cc-1)
        ("rust-clap" ,rust-clap-4)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-itertools" ,rust-itertools-0.12)
        ("rust-log" ,rust-log-0.4)
        ("rust-regex" ,rust-regex-1)
        ("rust-semver" ,rust-semver-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-toml" ,rust-toml-0.8)
        ("rust-windows-sys" ,rust-windows-sys-0.52))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl libgit2-1.7 libssh2 openssl zlib))
    (home-page "https://github.com/lu-zero/cargo-c")
    (synopsis "Build and install C-compatible libraries")
    (description
     "This package produces and installs a correct pkg-config file, a static
library and a dynamic library, and a C header to be used by any C (and
C-compatible) software.")
    (license license:expat)))

(define-public rtss
  (package
    (name "rtss")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rtss" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1r1b6fynkjnpj5p3k209sa13mjvh4k0ghzwnribm48dh9v7lfnnv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-memchr" ,rust-memchr-2))))
    (home-page "https://github.com/Freaky/rtss")
    (synopsis "Annotate stdout/stderr with elapsed times")
    (description "@code{rtss} annotates its output with relative durations between
consecutive lines and since program start.")
    (license license:expat)))

(define-public skim
  (package
    (name "skim")
    (version "0.10.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "skim" version))
       ;; Keep the same file name as the crate in crates-io
       (file-name (string-append "rust-skim-" version ".tar.gz"))
       (sha256
        (base32 "0chgv9nr8cmlf2mg2k94igh3m5svjsfxxwbnl21xsb6blvh8vlp5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs (("rust-atty" ,rust-atty-0.2)
                       ("rust-beef" ,rust-beef-0.5)
                       ("rust-bitflags" ,rust-bitflags-1)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-3)
                       ("rust-crossbeam" ,rust-crossbeam-0.8)
                       ("rust-defer-drop" ,rust-defer-drop-1)
                       ("rust-derive-builder" ,rust-derive-builder-0.11)
                       ("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-fuzzy-matcher" ,rust-fuzzy-matcher-0.3)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nix" ,rust-nix-0.25)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-shlex" ,rust-shlex-1)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-timer" ,rust-timer-0.2)
                       ("rust-tuikit" ,rust-tuikit-0.5)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-vte" ,rust-vte-0.11))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-extras
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (share (string-append out "/share"))
                             (man (string-append out "/share/man"))
                             (vimfiles (string-append share
                                        "/vim/vimfiles/pack/guix/start/skim/plugin"))
                             (bash-completion (string-append share
                                               "/bash-completions/completions"))
                             (zsh-site (string-append share
                                                      "/zsh/site-functions"))
                             (fish-vendor (string-append share
                                           "/fish/vendor-completions.d")))
                        ;; Binaries
                        (for-each (lambda (binary)
                                    (install-file binary bin))
                                  (find-files "bin"))
                        (mkdir-p share)
                        ;; Manpages
                        (copy-recursively "man" man)
                        ;; Vim plugins
                        (mkdir-p vimfiles)
                        (copy-recursively "plugin" vimfiles)
                        ;; Completions
                        (mkdir-p bash-completion)
                        (copy-file "shell/completion.bash"
                                   (string-append bash-completion "/skim"))
                        (copy-file "shell/key-bindings.bash"
                                   (string-append bash-completion
                                                  "/skim-bindings"))
                        (mkdir-p zsh-site)
                        (copy-file "shell/completion.zsh"
                                   (string-append zsh-site "/_skim"))
                        (copy-file "shell/key-bindings.zsh"
                                   (string-append zsh-site "/_skim-bindings"))
                        (mkdir-p fish-vendor)
                        (copy-file "shell/key-bindings.fish"
                                   (string-append fish-vendor
                                                  "/skim-bindings.fish"))))))))
    (home-page "https://github.com/lotabout/skim")
    (synopsis "Fuzzy Finder in Rust")
    (description "This package provides a fuzzy finder in Rust.")
    (license license:expat)))

(define-public spotifyd
  (package
    (name "spotifyd")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "spotifyd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1g6k8xmx8xvc2dpak14y8cc2221djhdflzsjczygvqa9gk5jiadd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs (("rust-alsa" ,rust-alsa-0.7)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-color-eyre" ,rust-color-eyre-0.6)
                       ("rust-daemonize" ,rust-daemonize-0.5)
                       ("rust-dbus" ,rust-dbus-0.9)
                       ("rust-dbus-crossroads" ,rust-dbus-crossroads-0.5)
                       ("rust-dbus-tokio" ,rust-dbus-tokio-0.7)
                       ("rust-fern" ,rust-fern-0.6)
                       ("rust-futures" ,rust-futures-0.3)
                       ("rust-gethostname" ,rust-gethostname-0.4)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-keyring" ,rust-keyring-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-librespot-audio" ,rust-librespot-audio-0.4)
                       ("rust-librespot-connect" ,rust-librespot-connect-0.4)
                       ("rust-librespot-core" ,rust-librespot-core-0.4)
                       ("rust-librespot-discovery" ,rust-librespot-discovery-0.4)
                       ("rust-librespot-playback" ,rust-librespot-playback-0.4)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-rspotify" ,rust-rspotify-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-sha-1" ,rust-sha-1-0.10)
                       ("rust-structopt" ,rust-structopt-0.3)
                       ("rust-syslog" ,rust-syslog-6)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
                       ("rust-toml" ,rust-toml-0.7)
                       ("rust-url" ,rust-url-2)
                       ("rust-whoami" ,rust-whoami-1)
                       ("rust-xdg" ,rust-xdg-2))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.10))
       #:features (list "alsa_backend"
                        "dbus_keyring"
                        ;"dbus_mpris"   ; Conflicts with rust-chrono-0.4 version.
                        "pulseaudio_backend"
                        "rodio_backend")))
    (native-inputs (list pkg-config))
    (inputs (list alsa-lib dbus pulseaudio))
    (home-page "https://github.com/Spotifyd/spotifyd")
    (synopsis "Spotify streaming daemon with Spotify Connect support")
    (description
     "This package provides a light-weight daemon that connects to the Spotify
music service.  A Spotifyd instance can be controlled by clients that use the
Spotify Connect protocol, which includes the official Spotify mobile apps.")
    (license license:gpl3)))

(define-public svd2rust
  (package
    (name "svd2rust")
    (version "0.19.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "svd2rust" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0q8slfgjfhpljzlk2myb0i538mfq99q1ljn398jm17r1q2pjjxhv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-cast" ,rust-cast-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-clap-conf" ,rust-clap-conf-0.1)
        ("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-inflections" ,rust-inflections-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-1)
        ("rust-svd-parser" ,rust-svd-parser-0.10)
        ("rust-syn" ,rust-syn-1)
        ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/rust-embedded/svd2rust/")
    (synopsis
     "Generate Rust register maps (`struct`s) from SVD files")
    (description
     "This program can be used to generate Rust register maps (`struct`s) from SVD
files.")
    (license (list license:expat license:asl2.0))))

(define-public swayhide
  (package
    (name "swayhide")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "swayhide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x172ffj0lfmbv5nix708l1mfsizxzy74gpxp5amvx0bbaq0p78s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-exitcode" ,rust-exitcode-1)
        ("rust-swayipc" ,rust-swayipc-2))))
    (home-page "https://github.com/NomisIV/swayhide/")
    (synopsis "Swallow windows on swaywm")
    (description "swayhide hides the currently active terminal (by moving it
to the scratchpad), then it executes the supplied command.  When the child
process has finished, the terminal is moved back.  This is useful if your
workflow includes opening graphical programs from the terminal, as the locked
terminal won't have to take up any space.")
    (license license:gpl3+)))

(define-public swayr
  (package
   (name "swayr")
   (version "0.27.3")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "swayr" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "1akmq1qa3v8jxn7qgwmr70dhgamb1mvn2jkqdawc6i8accz33gca"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-test-flags
      '("--release" "--"
        "--skip=config::test_load_swayr_config")
      #:install-source? #f
      #:cargo-inputs
      (("rust-clap" ,rust-clap-4)
       ("rust-directories" ,rust-directories-5)
       ("rust-env-logger" ,rust-env-logger-0.10)
       ("rust-log" ,rust-log-0.4)
       ("rust-once-cell" ,rust-once-cell-1)
       ("rust-peg" ,rust-peg-0.8)
       ("rust-rand" ,rust-rand-0.8)
       ("rust-regex" ,rust-regex-1)
       ("rust-rt-format" ,rust-rt-format-0.3)
       ("rust-serde" ,rust-serde-1)
       ("rust-serde-json" ,rust-serde-json-1)
       ("rust-swayipc" ,rust-swayipc-3)
       ("rust-toml" ,rust-toml-0.8))))
   (home-page "https://sr.ht/~tsdh/swayr/")
   (synopsis "Window-switcher for the sway window manager")
   (description
    "This package provides a last-recently-used window-switcher for the sway
window manager.  Swayr consists of a daemon, and a client.  The swayrd daemon
records window/workspace creations, deletions, and focus changes using sway's
JSON IPC interface.  The swayr client offers subcommands, and sends them to the
daemon which executes them.")
   (license license:gpl3+)))

(define-public tealdeer
  (package
    (name "tealdeer")
    (version "1.6.1")
    (source
     (origin
       ;; Completions aren't in the release tarball.
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dbrgn/tealdeer")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ipd23b30pqvyh20mxfd13ps0rnvg7zfpysv7wambfbb92xdh36d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bash (string-append out "/etc/bash_completion.d/"))
                    (fish (string-append out "/share/fish/vendor_completions.d/"))
                    (zsh  (string-append out "/share/zsh/site-functions/")))
               (mkdir-p bash)
               (mkdir-p fish)
               (mkdir-p zsh)
               (copy-file "completion/bash_tealdeer"
                          (string-append bash "tealdeer"))
               (copy-file "completion/fish_tealdeer"
                          (string-append fish "tealdeer.fish"))
               (copy-file "completion/zsh_tealdeer"
                          (string-append zsh "_tealdeer"))))))
       #:install-source? #f
       #:cargo-test-flags
       '("--release" "--"
         ;; These tests go to the network
         "--skip=test_quiet_old_cache"
         "--skip=test_quiet_cache"
         "--skip=test_quiet_failures"
         "--skip=test_pager_flag_enable"
         "--skip=test_markdown_rendering"
         "--skip=test_spaces_find_command"
         "--skip=test_autoupdate_cache"
         "--skip=test_update_cache"
         "--skip=test_create_cache_directory_path")
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-app-dirs2" ,rust-app-dirs2-2)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-clap" ,rust-clap-3)
        ("rust-env-logger" ,rust-env-logger-0.9)
        ("rust-log" ,rust-log-0.4)
        ("rust-pager" ,rust-pager-0.16)
        ("rust-reqwest" ,rust-reqwest-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-yansi" ,rust-yansi-0.5)
        ("rust-zip" ,rust-zip-0.6))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-escargot" ,rust-escargot-0.5)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-predicates" ,rust-predicates-2)
        ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://github.com/dbrgn/tealdeer/")
    (synopsis "Fetch and show tldr help pages for many CLI commands")
    (description
     "This package fetches and shows tldr help pages for many CLI commands.
Full featured offline client with caching support.")
    (license (list license:expat license:asl2.0))))

(define-public git-absorb
  (package
    (name "git-absorb")
    (version "0.6.11")
    (source
     (origin
       ;; crates.io does not include the manual page.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tummychow/git-absorb")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mgqmbk2rz87blas86k340nshiy0zbw9pq76b8nqknpgghm4k029"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (substitute* "Cargo.toml"
                   (("\"~") "\""))
                 (delete-file "Documentation/git-absorb.1")))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-clap" ,rust-clap-2)
        ("rust-git2" ,rust-git2-0.18)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-slog" ,rust-slog-2)
        ("rust-slog-async" ,rust-slog-async-2)
        ("rust-slog-term" ,rust-slog-term-2))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-manual-page
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (man   (string-append out "/share/man/man1")))
               (with-directory-excursion "Documentation"
                 (invoke "a2x" "-L" "-d" "manpage" "-f" "manpage" "git-absorb.txt"))
               (install-file "Documentation/git-absorb.1" man)))))))
    (native-inputs
     (list asciidoc pkg-config))
    (inputs
     (list libgit2-1.7 zlib))
    (home-page "https://github.com/tummychow/git-absorb")
    (synopsis "Git tool for making automatic fixup commits")
    (description
     "@code{git absorb} automatically absorbs staged changes into their
current branch.  @code{git absorb} will automatically identify which commits
are safe to modify, and which staged changes belong to each of those commits.
It will then write @code{fixup!} commits for each of those changes.")
    (license license:bsd-3)))

(define-public git-delta
  (package
    (name "git-delta")
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "git-delta" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1bmjan13lm1d6vcy8mh0iryl2rnvh39ml5y4alf6s728xdzc2yhj"))
       (modules '((guix build utils)))
       (snippet
        '(begin (substitute* "Cargo.toml"
                  (("\"=([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                   (string-append "\"^" version)))))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-test-flags
      '(list "--release" "--"
             "--skip=ansi::tests::test_measure_text_width"
             "--skip=features::line_numbers::tests::test_line_numbers_continue_correctly_after_wrapping"
             "--skip=features::side_by_side::tests::test_two_plus_lines_exact_fit"
             "--skip=handlers::diff_header::tests::test_diff_header_relative_paths"
             "--skip=tests::test_example_diffs::tests::test_binary_file_added"
             "--skip=tests::test_example_diffs::tests::test_binary_file_removed"
             "--skip=tests::test_example_diffs::tests::test_binary_files_differ"
             "--skip=tests::test_example_diffs::tests::test_binary_files_differ_after_other"
             "--skip=wrapping::tests::test_alignment_1_line_vs_3_lines"
             "--skip=wrapping::tests::test_alignment_2_lines_vs_3_lines"
             "--skip=wrapping::tests::test_wrap_line_newlines")
      #:cargo-inputs
      `(("rust-ansi-colours" ,rust-ansi-colours-1)
        ("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-anstyle-parse" ,rust-anstyle-parse-0.2)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-bat" ,rust-bat-0.24)
        ("rust-bitflags" ,rust-bitflags-2)
        ("rust-box-drawing" ,rust-box-drawing-0.1)
        ("rust-bytelines" ,rust-bytelines-2)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-chrono-humanize" ,rust-chrono-humanize-0.2)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete" ,rust-clap-complete-4)
        ("rust-console" ,rust-console-0.15)
        ("rust-ctrlc" ,rust-ctrlc-3)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-git2" ,rust-git2-0.18)
        ("rust-grep-cli" ,rust-grep-cli-0.1)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-palette" ,rust-palette-0.7)
        ("rust-pathdiff" ,rust-pathdiff-0.2)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-shell-words" ,rust-shell-words-1)
        ("rust-smol-str" ,rust-smol-str-0.1)
        ("rust-syntect" ,rust-syntect-5)
        ("rust-sysinfo" ,rust-sysinfo-0.29)
        ("rust-terminal-colorsaurus" ,rust-terminal-colorsaurus-0.4)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-xdg" ,rust-xdg-2))
      #:cargo-development-inputs `(("rust-insta" ,rust-insta-1)
                                   ("rust-rstest" ,rust-rstest-0.21))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-extras
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (share (string-append out "/share"))
                     (bash-completions-dir
                      (string-append out "/etc/bash-completion.d"))
                     (zsh-completions-dir
                      (string-append share "/zsh/site-functions"))
                     (fish-completions-dir
                      (string-append share "/fish/vendor_completions.d")))
                (mkdir-p bash-completions-dir)
                (mkdir-p zsh-completions-dir)
                (mkdir-p fish-completions-dir)
                (copy-file "etc/completion/completion.bash"
                           (string-append bash-completions-dir "/delta"))
                (copy-file "etc/completion/completion.zsh"
                           (string-append zsh-completions-dir "/_delta"))
                (copy-file "etc/completion/completion.fish"
                           (string-append fish-completions-dir "/delta.fish"))))))))
    (native-inputs (list git-minimal pkg-config))
    (inputs (list libgit2-1.7 openssl zlib))
    (home-page "https://github.com/dandavison/delta")
    (synopsis "Syntax-highlighting pager for git")
    (description
     "This package provides a syntax-highlighting pager for @command{git}.  It
uses @command{bat} for syntax highlighting and provides many features such as
advanced keybindings, word-level diff highlighting, syntax highlighting for
@command{grep} and a stylized box presentation.")
    (license license:expat)))

(define-public rust-xremap
  (package
    (name "rust-xremap")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xremap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0a89vdldswd5zawln17gfrfqxjn4pacqlkn4hf5kn8r5znr0ap01"))))
    (build-system cargo-build-system)
    (arguments
     `(#:features '()
       #:install-source? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete" ,rust-clap-complete-4)
        ("rust-derive-where" ,rust-derive-where-1)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-evdev" ,rust-evdev-0.12)
        ("rust-fork" ,rust-fork-0.2)
        ("rust-indoc" ,rust-indoc-2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-nix" ,rust-nix-0.26)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-with" ,rust-serde-with-3)
        ("rust-serde-yaml" ,rust-serde-yaml-0.9)
        ("rust-toml" ,rust-toml-0.8)
        ("rust-wayland-client" ,rust-wayland-client-0.30)
        ("rust-wayland-protocols-wlr" ,rust-wayland-protocols-wlr-0.1)
        ("rust-x11rb" ,rust-x11rb-0.13)
        ("rust-zbus" ,rust-zbus-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (xremap (string-append out "/bin/xremap")))
               (mkdir-p (string-append share "/bash-completion/completions"))
               (with-output-to-file
                 (string-append share "/bash-completion/completions/xremap")
                 (lambda _ (invoke xremap "--completions" "bash")))
               (mkdir-p (string-append share "/fish/vendor_completions.d"))
               (with-output-to-file
                 (string-append share "/fish/vendor_completions.d/xremap.fish")
                 (lambda _ (invoke xremap "--completions" "fish")))
               (mkdir-p (string-append share "/zsh/site-functions"))
               (with-output-to-file
                 (string-append share "/zsh/site-functions/_xremap")
                 (lambda _ (invoke xremap "--completions" "zsh")))
               (mkdir-p (string-append share "/elvish/lib"))
               (with-output-to-file
                 (string-append share "/elvish/lib/xremap")
                 (lambda _ (invoke xremap "--completions" "elvish")))))))))
    (home-page "https://github.com/k0kubun/xremap")
    (synopsis "Dynamic key remapp for X and Wayland")
    (description "This package provides dynamic key remapp for X and Wayland.")
    (license license:expat)))

(define-public xremap-gnome
  (package
    (inherit rust-xremap)
    (name "xremap-gnome")
    (arguments
     (substitute-keyword-arguments (package-arguments rust-xremap)
       ((#:features _) '(list "gnome"))))))

(define-public xremap-wlroots
  (package
    (inherit rust-xremap)
    (name "xremap-wlroots")
    (arguments
     (substitute-keyword-arguments (package-arguments rust-xremap)
       ((#:features _) '(list "wlroots"))))))

(define-public xremap-hyprland
  (deprecated-package "xremap-hyprland" xremap-wlroots))

(define-public xremap-sway
  (deprecated-package "xremap-sway" xremap-wlroots))

(define-public xremap-x11
  (package
    (inherit rust-xremap)
    (name "xremap-x11")
    (arguments
     (substitute-keyword-arguments (package-arguments rust-xremap)
       ((#:features _) '(list "x11"))))))

(define-public xsv
  (package
    (name "xsv")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "xsv" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pvzr7x5phlya6m5yikvy13vgbazshw0plysckz9zmf2ly5x4jl8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1)
        ("rust-chan" ,rust-chan-0.1)
        ("rust-csv" ,rust-csv-1)
        ("rust-csv-index" ,rust-csv-index-0.1)
        ("rust-docopt" ,rust-docopt-1)
        ("rust-filetime" ,rust-filetime-0.1)
        ("rust-num-cpus" ,rust-num-cpus-1)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-streaming-stats" ,rust-streaming-stats-0.2)
        ("rust-tabwriter" ,rust-tabwriter-1)
        ("rust-threadpool" ,rust-threadpool-1))
       #:cargo-development-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-quickcheck" ,rust-quickcheck-0.6))))
    (home-page "https://github.com/BurntSushi/xsv")
    (synopsis "High performance CSV command line toolkit")
    (description
     "This package provides a high performance CSV command line toolkit.")
    (license (list license:unlicense license:expat))))

(define-public zoxide
  (package
    (name "zoxide")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zoxide" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dqndbxpnv0g82d52jaszsgh62y9cv9hq8rgypsimgkk9yxhn4rw"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f
           #:cargo-inputs
           `(("rust-anyhow" ,rust-anyhow-1)
             ("rust-askama" ,rust-askama-0.12)
             ("rust-bincode" ,rust-bincode-1)
             ("rust-clap" ,rust-clap-4)
             ("rust-clap-complete" ,rust-clap-complete-4)
             ("rust-clap-complete-fig" ,rust-clap-complete-fig-4)
             ("rust-color-print" ,rust-color-print-0.3)
             ("rust-dirs" ,rust-dirs-5)
             ("rust-dunce" ,rust-dunce-1)
             ("rust-fastrand" ,rust-fastrand-2)
             ("rust-glob" ,rust-glob-0.3)
             ("rust-nix" ,rust-nix-0.26)
             ("rust-ouroboros" ,rust-ouroboros-0.17)
             ("rust-serde" ,rust-serde-1)
             ("rust-which" ,rust-which-4))
           #:cargo-development-inputs
           `(("rust-assert-cmd" ,rust-assert-cmd-2)
             ("rust-rstest" ,rust-rstest-0.18)
             ("rust-rstest-reuse" ,rust-rstest-reuse-0.6)
             ("rust-tempfile" ,rust-tempfile-3))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-more
                 (lambda _
                   (let* ((out #$output)
                          (share (string-append out "/share"))
                          (man1 (string-append share "/man/man1"))
                          (bash-completions-dir
                            (string-append out "/etc/bash-completion.d"))
                          (zsh-completions-dir
                            (string-append share "/zsh/site-functions"))
                          (fish-completions-dir
                            (string-append share "/fish/vendor_completions.d"))
                          (elvish-completions-dir
                            (string-append share "/elvish/lib")))
                     ;; The completions are generated in build.rs.
                     (mkdir-p man1)
                     (mkdir-p bash-completions-dir)
                     (mkdir-p elvish-completions-dir)
                     (for-each (lambda (file)
                                 (install-file file man1))
                               (find-files "man/man1"))
                     (copy-file "contrib/completions/zoxide.bash"
                                (string-append bash-completions-dir "/zoxide"))
                     (install-file "contrib/completions/zoxide.fish"
                                   fish-completions-dir)
                     (install-file "contrib/completions/_zoxide"
                                   zsh-completions-dir)
                     (copy-file "contrib/completions/zoxide.elv"
                                (string-append elvish-completions-dir
                                               "/zoxide"))))))))
    (home-page "https://github.com/ajeetdsouza/zoxide/")
    (synopsis "Fast way to navigate your file system")
    (description
     "Zoxide is a fast replacement for your @command{cd} command.  It keeps
track of the directories you use most frequently, and uses a ranking algorithm
to navigate to the best match.")
    (license license:expat)))

(define-public htmlq
  (package
    (name "htmlq")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "htmlq" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0912cdkz5xji1hzfj1cf42zh1kd860b52xmwwhb7q2jhp6qk25jh"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-clap" ,rust-clap-2)
         ("rust-html5ever" ,rust-html5ever-0.25)
         ("rust-kuchiki" ,rust-kuchiki-0.8)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-url" ,rust-url-2))))
    (home-page "https://github.com/mgdm/htmlq")
    (synopsis "Like jq, but for HTML")
    (description "Extract content from HTML files using CSS selectors.")
    (license license:expat)))
