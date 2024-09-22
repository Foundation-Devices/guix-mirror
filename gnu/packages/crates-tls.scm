;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2021, 2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 VÖRÖSKŐI András <voroskoi@gmail.com>
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

(define-module (gnu packages crates-tls)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages tls))

(define-public rust-asn1-0.15
  (package
    (name "rust-asn1")
    (version "0.15.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asn1" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nzg1gjiyfvpvrf3i7i8j21165snf5livqg6x2sjf9m2i77cngmf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-asn1-derive" ,rust-asn1-derive-0.15))
       #:cargo-development-inputs
       (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/alex/rust-asn1")
    (synopsis "ASN.1 (DER) parser and writer")
    (description
     "This is a Rust library for parsing and generating ASN.1 data (DER only).")
    (license license:bsd-3)))

(define-public rust-asn1-0.13
  (package
    (inherit rust-asn1-0.15)
    (name "rust-asn1")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1idxxw14h3dvrj72k4g0hx1aqigd986a00cg0yxfw2gfc9gbmzra"))))
    (arguments
     `(#:cargo-inputs
       (("rust-asn1-derive" ,rust-asn1-derive-0.13)
        ("rust-chrono" ,rust-chrono-0.4))
       #:cargo-development-inputs
       (("rust-libc" ,rust-libc-0.2))))))

(define-public rust-asn1-derive-0.15
  (package
    (name "rust-asn1-derive")
    (version "0.15.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asn1_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1771kfjycjs4g2acqvxpjy3igfcgg8hychczl1lsqq64za4gj6l6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/alex/rust-asn1")
    (synopsis "#[derive] support for asn1")
    (description
     "This package provides #[derive] support for @code{asn1}.")
    (license license:bsd-3)))

(define-public rust-asn1-derive-0.13
  (package
    (inherit rust-asn1-derive-0.15)
    (name "rust-asn1-derive")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bvqriazb23gysygpzng1dhzjgnlv274q2yj5gpmlpl7jp0pkaxz"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-asn1-rs-0.5
  (package
    (name "rust-asn1-rs")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1-rs" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w7zq0392qs7kkv0nzw50bfqvq7q9zxv48fsp3sxyl83mzfxavvz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-asn1-rs-derive" ,rust-asn1-rs-derive-0.4)
        ("rust-asn1-rs-impl" ,rust-asn1-rs-impl-0.1)
        ("rust-bitvec" ,rust-bitvec-1)
        ("rust-cookie-factory" ,rust-cookie-factory-0.3)
        ("rust-displaydoc" ,rust-displaydoc-0.2)
        ("rust-nom" ,rust-nom-7)
        ("rust-num-bigint" ,rust-num-bigint-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rusticata-macros" ,rust-rusticata-macros-4)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs
       (("rust-colored" ,rust-colored-2)
        ("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-oid-registry" ,rust-oid-registry-0.6)
        ("rust-pem" ,rust-pem-1)
        ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/rusticata/asn1-rs")
    (synopsis "Parser/encoder for ASN.1 BER/DER data")
    (description "Parser/encoder for ASN.1 BER/DER data")
    (license (list license:expat license:asl2.0))))

(define-public rust-asn1-rs-0.3
  (package
    (inherit rust-asn1-rs-0.5)
    (name "rust-asn1-rs")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asn1-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0czsk1nd4dx2k83f7jzkn8klx05wbmblkx1jh51i4c170akhbzrh"))))
    (arguments
     `(#:cargo-inputs (("rust-asn1-rs-derive" ,rust-asn1-rs-derive-0.1)
                       ("rust-asn1-rs-impl" ,rust-asn1-rs-impl-0.1)
                       ("rust-bitvec" ,rust-bitvec-1)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-displaydoc" ,rust-displaydoc-0.2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs (("rust-colored" ,rust-colored-2)
                                   ("rust-hex-literal" ,rust-hex-literal-0.3)
                                   ("rust-oid-registry" ,rust-oid-registry-0.3)
                                   ("rust-pem" ,rust-pem-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))))

(define-public rust-asn1-rs-derive-0.4
  (package
    (name "rust-asn1-rs-derive")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1-rs-derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v7fgmnzk7jjxv51grhwzcx5bf167nlqwk3vcmq7xblf5s4karbj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1)
        ("rust-synstructure" ,rust-synstructure-0.12))))
    (home-page "https://github.com/rusticata/asn1-rs")
    (synopsis "Derive macros for the `asn1-rs` crate")
    (description
     "This package provides derive macros for the @code{asn1-rs} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-asn1-rs-derive-0.1
  (package
    (inherit rust-asn1-rs-derive-0.4)
    (name "rust-asn1-rs-derive")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "asn1-rs-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1gzf9vab06lk0zjvbr07axx64fndkng2s28bnj27fnwd548pb2yv"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-synstructure" ,rust-synstructure-0.12))))))

(define-public rust-asn1-rs-impl-0.1
  (package
    (name "rust-asn1-rs-impl")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "asn1-rs-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1va27bn7qxqp4wanzjlkagnynv6jnrhnwmcky2ahzb1r405p6xr7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rusticata/asn1-rs")
    (synopsis "Implementation details for the `asn1-rs` crate")
    (description
     "This package provides implementation details for the @code{asn1-rs} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-native-tls-0.3
  (package
    (name "rust-async-native-tls")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cwv4vbrvcbv58b51y1azfbszzgzhrzxx92q5nl6hk6kkf97m7ly"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Tests want internet access.
       #:cargo-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-url" ,rust-url-2))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-tokio" ,rust-tokio-0.2))))
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (home-page "https://docs.rs/crate/async-native-tls/")
    (synopsis "Native TLS using futures")
    (description "Native TLS using futures.")
    (license (list license:expat license:asl2.0))))

(define-public rust-async-tls-0.10
  (package
    (name "rust-async-tls")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0lnc61fb16wg76hbqh2kjzc4d9kqkh8mz51zzn78gkpcl329fnnq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; unresolved import `async_std::sync::channel`
       #:cargo-inputs
       (("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-rustls" ,rust-rustls-0.18)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.20))
       #:cargo-development-inputs
       (("rust-async-std" ,rust-async-std-1)
        ("rust-futures-executor" ,rust-futures-executor-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1))))
    (home-page "https://github.com/async-std/async-tls")
    (synopsis "Asynchronous TLS/SSL streams using Rustls")
    (description
     "This package provides asynchronous TLS/SSL streams using Rustls.")
    (license (list license:expat license:asl2.0))))

;; TODO: Unbundle aws-lc-fips.
(define-public rust-aws-lc-fips-sys-0.12
  (package
    (name "rust-aws-lc-fips-sys")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aws-lc-fips-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fv2z5gcm9wm45hbsjhm657p6diqiq1vw5a2rzrzfg8j4vxdzz07"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-fs-extra" ,rust-fs-extra-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-paste" ,rust-paste-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-directory
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (native-inputs (list cmake-minimal go perl))
    (home-page "https://github.com/aws/aws-lc-rs")
    (synopsis
     "AWS-LC is a general-purpose cryptographic library (FIPS version)")
    (description
     "AWS-LC is a general-purpose cryptographic library maintained by the AWS
Cryptography team for AWS and their customers.  This is the FIPS validated
version of AWS-LC.")
    (license (list license:isc license:openssl license:asl2.0))))

(define-public rust-aws-lc-rs-1
  (package
    (name "rust-aws-lc-rs")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aws-lc-rs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jmwpin66yibpq0ha7i61g2ryz9gp4y6by4337fdjj2ckhwbm55v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f          ; Not all files included.
       #:cargo-inputs (("rust-aws-lc-fips-sys" ,rust-aws-lc-fips-sys-0.12)
                       ("rust-aws-lc-sys" ,rust-aws-lc-sys-0.13)
                       ("rust-mirai-annotations" ,rust-mirai-annotations-1)
                       ("rust-paste" ,rust-paste-1)
                       ("rust-untrusted" ,rust-untrusted-0.7)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-clap" ,rust-clap-4)
                                   ("rust-hex" ,rust-hex-0.4)
                                   ("rust-lazy-static" ,rust-lazy-static-1)
                                   ("rust-regex" ,rust-regex-1))))
    (native-inputs (list cmake-minimal))
    (home-page "https://github.com/awslabs/aws-lc-rs")
    (synopsis "AWS-LC is a general-purpose cryptographic library")
    (description
     "@code{aws-lc-rs} is a cryptographic library using AWS-LC for its
cryptographic operations.  This library strives to be API-compatible with the
popular Rust library named ring.")
    (license (list license:isc license:openssl license:asl2.0))))

;; TODO: Unbundle aws-lc.
(define-public rust-aws-lc-sys-0.13
  (package
    (name "rust-aws-lc-sys")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "aws-lc-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05a7z5hg00zpk4gvqggzv7j6bvljplx2c1kw44ifmxjnf5469rdn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bindgen" ,rust-bindgen-0.69)
                       ("rust-cmake" ,rust-cmake-0.1)
                       ("rust-dunce" ,rust-dunce-1)
                       ("rust-fs-extra" ,rust-fs-extra-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-paste" ,rust-paste-1))))
    (native-inputs (list cmake-minimal))
    (home-page "https://github.com/aws/aws-lc-rs")
    (synopsis "AWS-LC is a general-purpose cryptographic library")
    (description
     "AWS-LC is a general-purpose cryptographic library maintained by the AWS
Cryptography team for AWS and their customers.  It is based on code from the
Google @code{BoringSSL} project and the @code{OpenSSL} project.")
    (license (list license:isc license:openssl license:asl2.0))))

(define-public rust-der-0.7
  (package
    (name "rust-der")
    (version "0.7.8")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "der" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "070bwiyr80800h31c5zd96ckkgagfjgnrrdmz3dzg2lccsd3dypz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-arbitrary" ,rust-arbitrary-1)
        ("rust-bytes" ,rust-bytes-1)
        ("rust-const-oid" ,rust-const-oid-0.9)
        ("rust-der-derive" ,rust-der-derive-0.7)
        ("rust-flagset" ,rust-flagset-0.4)
        ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.7)
        ("rust-time" ,rust-time-0.3)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-proptest" ,rust-proptest-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/der")
    (synopsis
     "Implementation of the Distinguished Encoding Rules (DER)")
    (description
     "This package provides a pure Rust embedded-friendly implementation of
the Distinguished Encoding Rules (DER) for Abstract Syntax Notation One
(ASN.1) as described in ITU X.690 with full support for heapless no_std
targets")
    (license (list license:asl2.0 license:expat))))

(define-public rust-der-0.6
  (package
    (inherit rust-der-0.7)
    (name "rust-der")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "der" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pnl3y52m1s6srxpfrfbazf6qilzq8fgksk5dv79nxaybjk6g97i"))))
    (arguments
     `(#:cargo-inputs
       (("rust-const-oid" ,rust-const-oid-0.9)
        ("rust-der-derive" ,rust-der-derive-0.6)
        ("rust-flagset" ,rust-flagset-0.4)
        ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.6)
        ("rust-time" ,rust-time-0.3)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-proptest" ,rust-proptest-1))))))

(define-public rust-der-0.5
  (package
    (inherit rust-der-0.7)
    (name "rust-der")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "der" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0p3h7nszn7jhjacpmkjrcyx5g8p3ma1qhxfy3397m7l3fdfq26b9"))))
    (arguments
     `(#:cargo-inputs (("rust-const-oid" ,rust-const-oid-0.7)
                       ("rust-crypto-bigint" ,rust-crypto-bigint-0.3)
                       ("rust-der-derive" ,rust-der-derive-0.5)
                       ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.3)
                       ("rust-time" ,rust-time-0.3))
       #:cargo-development-inputs (("rust-hex-literal" ,rust-hex-literal-0.3)
                                   ("rust-proptest" ,rust-proptest-1))))))

(define-public rust-der-0.4
  (package
    (inherit rust-der-0.7)
    (name "rust-der")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "der" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x4k0jln8va1657cghl40l6p7hyvr1ixz71v9cd6imwmgp51rdvr"))))
    (arguments
     `(#:skip-build?
       #t                               ; FIXME
       #:cargo-inputs
       (("rust-const-oid" ,rust-const-oid-0.6)
        ("rust-crypto-bigint" ,rust-crypto-bigint-0.2)
        ("rust-der-derive" ,rust-der-derive-0.4))))))

(define-public rust-der-derive-0.7
  (package
    (name "rust-der-derive")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "der_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jg0y3k46bpygwc5cqha07axz5sdnsx5116g3nxf0rwrabj7rs2z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro-error" ,rust-proc-macro-error-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/der/derive")
    (synopsis
      "Custom derive support for the `der` crate's `Choice` and `Sequence` traits")
    (description
      "This package provides a custom derive support for the `der` crate's
`Choice` and `Sequence` traits.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-der-derive-0.6
  (package
    (inherit rust-der-derive-0.7)
    (name "rust-der-derive")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "der_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fg3dv4cjjwd4a6dh62ch2gb477s1pvwh5s8wbg567rsbgdivxwf"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro-error" ,rust-proc-macro-error-1)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))))

(define-public rust-der-derive-0.5
  (package
    (inherit rust-der-derive-0.7)
    (name "rust-der-derive")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "der_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1zw4p6yqklv4i76ms2a0gcmna648337r379d5ljgpbir5cyqylrs"))))
    (arguments
     `(#:cargo-inputs (("rust-proc-macro-error" ,rust-proc-macro-error-1)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))))

(define-public rust-der-derive-0.4
  (package
    (inherit rust-der-derive-0.7)
    (name "rust-der-derive")
    (version "0.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "der_derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0snv85yfy9iln05qsgbhwr1159gd0jfrgzj5dkrnricdc0y3pvca"))))
    (arguments
      `(#:skip-build?
        #t                              ; FIXME
        #:cargo-inputs
        (("rust-proc-macro2" ,rust-proc-macro2-1)
         ("rust-quote" ,rust-quote-1)
         ("rust-syn" ,rust-syn-1)
         ("rust-synstructure" ,rust-synstructure-0.12))))))

(define-public rust-der-oid-macro-0.5
  (package
    (name "rust-der-oid-macro")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "der-oid-macro" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0dply8g2p72hfhyymkrkr7fjqy844drj19xbrfkqrp55nq4z4fn7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-num-bigint" ,rust-num-bigint-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rusticata/der-parser")
    (synopsis "Macro to encode DER oids at compile time")
    (description
     "This crate provides a macro to encode DER oids at compile time.")
    (license (list license:expat license:asl2.0))))

(define-public rust-der-parser-8
  (package
    (name "rust-der-parser")
    (version "8.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "der-parser" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07mnz9y395zyxwj7nam2dbzkqdngfraxp2i7y2714dxmpbxpdmnv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-asn1-rs" ,rust-asn1-rs-0.5)
        ("rust-cookie-factory" ,rust-cookie-factory-0.3)
        ("rust-displaydoc" ,rust-displaydoc-0.2)
        ("rust-nom" ,rust-nom-7)
        ("rust-num-bigint" ,rust-num-bigint-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rusticata-macros" ,rust-rusticata-macros-4))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-test-case" ,rust-test-case-3))))
    (home-page "https://github.com/rusticata/der-parser")
    (synopsis "BER/DER parser written in pure Rust")
    (description "This crate provides a parser for Basic Encoding Rules (BER
[X.690]) and Distinguished Encoding Rules(DER [X.690]), implemented with the
@code{nom} parser combinator framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-der-parser-7
  (package
    (inherit rust-der-parser-8)
    (name "rust-der-parser")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "der-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10kfa2gzl3x20mwgrd43cyi79xgkqxyzcyrh0xylv4apa33qlfgy"))))
    (arguments
     `(#:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.3)
                       ("rust-cookie-factory" ,rust-cookie-factory-0.3)
                       ("rust-displaydoc" ,rust-displaydoc-0.2)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-test-case" ,rust-test-case-1))))))

(define-public rust-der-parser-6
  (package
    (inherit rust-der-parser-8)
    (name "rust-der-parser")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "der-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cik6kfx0bpp5gsw59h9b61widy09pqbggifn2r13d00ywhg3pac"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitvec" ,rust-bitvec-0.22)
        ("rust-cookie-factory" ,rust-cookie-factory-0.3)
        ("rust-der-oid-macro" ,rust-der-oid-macro-0.5)
        ("rust-nom" ,rust-nom-7)
        ("rust-num-bigint" ,rust-num-bigint-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rusticata-macros" ,rust-rusticata-macros-4))
       #:cargo-development-inputs
       (("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-test-case" ,rust-test-case-1))))))

(define-public rust-native-tls-0.2
  (package
    (name "rust-native-tls")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "native-tls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bmrlg0fmzxaycjpkgkchi93av07v2yf9k33gc12ca9gqdrn28h7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f                      ; tests require network access
       #:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9)
        ("rust-schannel" ,rust-schannel-0.1)
        ("rust-security-framework" ,rust-security-framework-2)
        ("rust-security-framework-sys" ,rust-security-framework-sys-2)
        ("rust-tempfile" ,rust-tempfile-3))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3)
        ("rust-test-cert-gen" ,rust-test-cert-gen-0.7))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl))
    (home-page "https://github.com/sfackler/rust-native-tls")
    (synopsis "Wrapper over a platform's native TLS implementation")
    (description
     "This package provides a wrapper over a platform's native TLS
implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-oid-0.2
  (package
    (name "rust-oid")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hh61lx2kr0ca2rvkhf5j94asxxvb6pfwfxm06hdn4w8b4y906cw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-serde" ,rust-serde-1))
       #:cargo-development-inputs (("rust-bincode" ,rust-bincode-1)
                                   ("rust-serde-xml-rs" ,rust-serde-xml-rs-0.4)
                                   ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://labs.unnecessary.engineering/oid")
    (synopsis
     "Library for building, parsing, and formating Object Identifiers (OIDs)")
    (description
     "This package provides a Rust-native library for building, parsing, and
formating Object Identifiers (OIDs).")
    (license (list license:expat license:asl2.0))))

(define-public rust-oid-registry-0.6
  (package
    (name "rust-oid-registry")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "oid-registry" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zwvjp3ad6gzn8g8w2hcn9a2xdap0lkzckhlnwp6rabbzdpz7vcv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.5))))
    (home-page "https://github.com/rusticata/oid-registry")
    (synopsis "Object Identifier (OID) database")
    (description "This crate is a helper crate, containing a database of
OID objects.  These objects are intended for use when manipulating ASN.1
grammars and BER/DER encodings, for example.")
    (license (list license:expat license:asl2.0))))

(define-public rust-oid-registry-0.4
  (package
    (inherit rust-oid-registry-0.6)
    (name "rust-oid-registry")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oid-registry" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0akbah3j8231ayrp2l1y5d9zmvbvqcsj0sa6s6dz6h85z8bhgqiq"))))
    (arguments
     `(#:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.3))))))

(define-public rust-oid-registry-0.3
  (package
    (inherit rust-oid-registry-0.6)
    (name "rust-oid-registry")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "oid-registry" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jxr4nqpcsgirl53yndhfhch1gzddkjh27z0p7rbsr2xngcpyj9n"))))
    (arguments
     `(#:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.3))))))

(define-public rust-oid-registry-0.2
  (package
    (inherit rust-oid-registry-0.4)
    (name "rust-oid-registry")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "oid-registry" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "058qip5j5y0i95ckmw67mp73372rq16ci0lcczyq9irv76r4qmgy"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-der-parser" ,rust-der-parser-6))))))

(define-public rust-rcgen-0.12
  (package
    (name "rust-rcgen")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rcgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qg10xvayyxhkcjk1x3g6n59a5rq6iaw6vmmrmyvqg0zmjw6sh28"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Don't use a vendored botan.
                  (substitute* "Cargo.toml"
                    ((".*vendored.*") ""))))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-aws-lc-rs" ,rust-aws-lc-rs-1)
        ("rust-pem" ,rust-pem-3)
        ("rust-ring" ,rust-ring-0.17)
        ("rust-time" ,rust-time-0.3)
        ("rust-x509-parser" ,rust-x509-parser-0.15)
        ("rust-yasna" ,rust-yasna-0.5)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-botan" ,rust-botan-0.10)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-ring" ,rust-ring-0.17)
        ("rust-rsa" ,rust-rsa-0.9)
        ("rust-rustls-webpki" ,rust-rustls-webpki-0.101)
        ("rust-x509-parser" ,rust-x509-parser-0.15))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list botan openssl))
    (home-page "https://github.com/rustls/rcgen")
    (synopsis "Rust X.509 certificate generator")
    (description "Rust X.509 certificate generator.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rcgen-0.11
  (package
    (inherit rust-rcgen-0.12)
    (name "rust-rcgen")
    (version "0.11.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rcgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1iivv3xycr9mjfmp522xjqj47nsl5amlzzsfpbxpvg53984g7i2j"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Don't use a vendored botan.
                  (substitute* "Cargo.toml"
                    ((".*vendored.*") ""))))))
    (arguments
     `(#:cargo-inputs
       (("rust-pem" ,rust-pem-3)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-time" ,rust-time-0.3)
        ("rust-x509-parser" ,rust-x509-parser-0.15)
        ("rust-yasna" ,rust-yasna-0.5)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-botan" ,rust-botan-0.10)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rsa" ,rust-rsa-0.9)
        ("rust-rustls-webpki" ,rust-rustls-webpki-0.101)
        ("rust-x509-parser" ,rust-x509-parser-0.15))))))

(define-public rust-rcgen-0.10
  (package
    (inherit rust-rcgen-0.11)
    (name "rust-rcgen")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rcgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nvqgr697xzdzaavkcwcl59kxw7xfx9zdhdzx49fm3gkwbpq9gpz"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Don't use a vendored botan.
                 (substitute* "Cargo.toml"
                   ((".*vendored.*") ""))))))
    (arguments
     `(#:cargo-inputs (("rust-pem" ,rust-pem-1)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-x509-parser" ,rust-x509-parser-0.14)
                       ("rust-yasna" ,rust-yasna-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-botan" ,rust-botan-0.8)
                                   ("rust-openssl" ,rust-openssl-0.10)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rsa" ,rust-rsa-0.6)
                                   ("rust-webpki" ,rust-webpki-0.22)
                                   ("rust-x509-parser" ,rust-x509-parser-0.14))))))

(define-public rust-rcgen-0.9
  (package
    (inherit rust-rcgen-0.11)
    (name "rust-rcgen")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rcgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pdwrza2j6bv94nxcfrb6dpbrrrjdnspb3hkwr137rfy3vgg64v4"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Don't use a vendored botan.
                 (substitute* "Cargo.toml"
                   ((".*vendored.*") ""))))))
    (arguments
     `(#:cargo-inputs (("rust-pem" ,rust-pem-1)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-x509-parser" ,rust-x509-parser-0.13)
                       ("rust-yasna" ,rust-yasna-0.5)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-botan" ,rust-botan-0.8)
                                   ("rust-openssl" ,rust-openssl-0.10)
                                   ("rust-rand" ,rust-rand-0.8)
                                   ("rust-rsa" ,rust-rsa-0.5)
                                   ("rust-webpki" ,rust-webpki-0.22)
                                   ("rust-x509-parser" ,rust-x509-parser-0.13))))))

(define-public rust-rcgen-0.8
  (package
    (inherit rust-rcgen-0.11)
    (name "rust-rcgen")
    (version "0.8.14")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rcgen" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "19qvlcz8kl046q85xa40p3xg7l78jganj83hdbawjhs17x0d24ar"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Don't use a vendored botan.
                  (substitute* "Cargo.toml"
                    ((".*vendored.*") ""))))))
    (arguments
     `(#:cargo-test-flags
       '("--release" "--"
         "--skip=tests::test_dt_to_generalized"
         "--skip=tests::test_dt_utc_strip_nanos")
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-pem" ,rust-pem-1)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-x509-parser" ,rust-x509-parser-0.12)
        ("rust-yasna" ,rust-yasna-0.4)
        ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs
       (("rust-botan" ,rust-botan-0.8)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rsa" ,rust-rsa-0.5)
        ("rust-webpki" ,rust-webpki-0.22)
        ("rust-x509-parser" ,rust-x509-parser-0.12))))))

(define-public rust-rustls-0.22
  (package
    (name "rust-rustls")
    (version "0.22.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0hcxyhq6ynvws9v5b2h81s1nwmijmya7a3vyyyhsy1wqpmb9jz78"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f          ; Not all files included.
       #:cargo-inputs (("rust-aws-lc-rs" ,rust-aws-lc-rs-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-ring" ,rust-ring-0.17)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-rustls-webpki" ,rust-rustls-webpki-0.102)
                       ("rust-rustversion" ,rust-rustversion-1)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-base64" ,rust-base64-0.21)
                                   ("rust-bencher" ,rust-bencher-0.1)
                                   ("rust-env-logger" ,rust-env-logger-0.10)
                                   ("rust-log" ,rust-log-0.4)
                                   ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
                                   ("rust-webpki-roots" ,rust-webpki-roots-0.26))))
    (home-page "https://github.com/rustls/rustls")
    (synopsis "Modern TLS library written in Rust")
    (description
     "This package provides a modern TLS library written in Rust.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-0.21
  (package
    (inherit rust-rustls-0.22)
    (name "rust-rustls")
    (version "0.21.10")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fmpzk3axnhkd99saqkvraifdfms4pkyi56lkihf8n877j0sdmgr"))))
    (arguments
     `(#:tests? #f          ; Not all files included.
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.17)
        ("rust-rustls-webpki" ,rust-rustls-webpki-0.101)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-sct" ,rust-sct-0.7))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.21)
        ("rust-bencher" ,rust-bencher-0.1)
        ("rust-env-logger" ,rust-env-logger-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-webpki-roots" ,rust-webpki-roots-0.25))))))

(define-public rust-rustls-0.20
  (package
    (inherit rust-rustls-0.21)
    (name "rust-rustls")
    (version "0.20.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "16byazb8jfr06kgbijy92bdk0ila806g6a00a6l9x64mqpgf700v"))))
    (arguments
     `(#:tests? #f          ; Not all files included.
       #:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-sct" ,rust-sct-0.7)
        ("rust-webpki" ,rust-webpki-0.22))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.13)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-env-logger" ,rust-env-logger-0.9)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-webpki-roots" ,rust-webpki-roots-0.22))))))

(define-public rust-rustls-0.19
  (package
    (inherit rust-rustls-0.20)
    (name "rust-rustls")
    (version "0.19.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mx6nzbplydy9khll4clsl35m6c1a2cgz9czr74swfgfzrsvdv9m"))))
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.13)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-sct" ,rust-sct-0.6)
        ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.3)
                                   ("rust-env-logger" ,rust-env-logger-0.8)
                                   ("rust-log" ,rust-log-0.4)
                                   ("rust-webpki-roots" ,rust-webpki-roots-0.21))))))

(define-public rust-rustls-0.18
  (package
    (inherit rust-rustls-0.19)
    (name "rust-rustls")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "108cf3bfw5high066shz9xrfv4jz7djdmnwqs3kwx4wfypf2c4ax"))))
    (arguments
     `(#:tests? #f          ; Not all files included.
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.12)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-sct" ,rust-sct-0.6)
        ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-log" ,rust-log-0.4)
        ("rust-webpki-roots" ,rust-webpki-roots-0.20))))))

(define-public rust-rustls-0.17
  (package
    (inherit rust-rustls-0.18)
    (name "rust-rustls")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1q8m835viqrf4bbd2fa8rnmaj48fkd984saxf0238hb8blgs7m60"))))
    (arguments
     `(#:tests? #f          ; Not all files included.
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.11)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-sct" ,rust-sct-0.6)
        ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-log" ,rust-log-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-webpki-roots" ,rust-webpki-roots-0.19))))))

(define-public rust-rustls-0.16
  (package
    (inherit rust-rustls-0.17)
    (name "rust-rustls")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "17n0fx3fpkg4fhpdplrdhkissnl003kj90vzbqag11vkpyqihnmj"))))
    (arguments
     `(#:tests? #f ;; 1/114 tests fail (test file not found)
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-sct" ,rust-sct-0.6)
        ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-webpki-roots" ,rust-webpki-roots-0.17))))))

(define-public rust-rustls-0.15
  (package
    (inherit rust-rustls-0.16)
    (name "rust-rustls")
    (version "0.15.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustls" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0vh93fhqfbn4ysw4xzkpkpqdz36xixz4mhs1qllgldfq5iay6wgj"))))
    (arguments
     `(#:tests? #f      ; API tests panic
       #:cargo-test-flags
       '("--release" "--"
         "--skip=msgs::message_test::test_read_fuzz_corpus")
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.14)
        ("rust-sct" ,rust-sct-0.5)
        ("rust-untrusted" ,rust-untrusted-0.6)
        ("rust-webpki" ,rust-webpki-0.19))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-webpki-roots" ,rust-webpki-roots-0.16))))))

(define-public rust-rustls-0.14
  (package
    (inherit rust-rustls-0.18)
    (name "rust-rustls")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1nal4qca7f7mhwnvx3m824ymdj6qmzfcl64sxmrmpis32dwr2y4b"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.9)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.13)
        ("rust-sct" ,rust-sct-0.4)
        ("rust-untrusted" ,rust-untrusted-0.6)
        ("rust-webpki" ,rust-webpki-0.18))))))

(define-public rust-rustls-ffi-0.8
  (package
    (name "rust-rustls-ffi")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls-ffi" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "06kqrvm1d5ps9pml26zdd2hm8hh20j6svwvqibpnx7m5rh3jg9cx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-num-enum" ,rust-num-enum-0.5)
        ("rust-rustls" ,rust-rustls-0.20)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-0.2)
        ("rust-sct" ,rust-sct-0.7)
        ("rust-webpki" ,rust-webpki-0.22))
        #:cargo-development-inputs
        (("rust-cbindgen" ,rust-cbindgen-0.26))
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'adjust-cbindgen-requirement
            ;; The Cargo.toml in the git repository doesn't specify
            ;; a version requirement for cbindgen.
            (lambda _
              (substitute* "Cargo.toml"
                (("0\\.19\\.0") "*")))))))
    (home-page "https://github.com/rustls/rustls-ffi")
    (synopsis "Rustls bindings for non-Rust languages")
    (description "Rustls bindings for non-Rust languages")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-native-certs-0.7
  (package
    (name "rust-rustls-native-certs")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-native-certs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14ip15dcr6fmjzi12lla9cpln7mmkdid4a7wsp344v4kz9gbh7wg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f      ; Tests want network access.
       #:cargo-inputs (("rust-openssl-probe" ,rust-openssl-probe-0.1)
                       ("rust-rustls-pemfile" ,rust-rustls-pemfile-2)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-schannel" ,rust-schannel-0.1)
                       ("rust-security-framework" ,rust-security-framework-2))
       #:cargo-development-inputs (("rust-ring" ,rust-ring-0.16)
                                   ("rust-rustls" ,rust-rustls-0.22)
                                   ("rust-rustls-webpki" ,rust-rustls-webpki-0.102)
                                   ("rust-serial-test" ,rust-serial-test-2)
                                   ("rust-untrusted" ,rust-untrusted-0.7)
                                   ("rust-webpki-roots" ,rust-webpki-roots-0.26)
                                   ("rust-x509-parser" ,rust-x509-parser-0.15))))
    (home-page "https://github.com/ctz/rustls-native-certs")
    (synopsis "Use the platform native certificate store with rustls")
    (description "@code{rustls-native-certs} allows rustls to use the platform
native certificate store.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-native-certs-0.6
  (package
    (inherit rust-rustls-native-certs-0.7)
    (name "rust-rustls-native-certs")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-native-certs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "007zind70rd5rfsrkdcfm8vn09j8sg02phg9334kark6rdscxam9"))))
    (arguments
     `(#:tests? #f      ; Tests want network access.
       #:cargo-inputs
       (("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
        ("rust-schannel" ,rust-schannel-0.1)
        ("rust-security-framework" ,rust-security-framework-2))
       #:cargo-development-inputs
       (("rust-ring" ,rust-ring-0.16)
        ("rust-rustls" ,rust-rustls-0.21)
        ("rust-rustls-webpki" ,rust-rustls-webpki-0.100)
        ("rust-serial-test" ,rust-serial-test-2)
        ("rust-untrusted" ,rust-untrusted-0.7)
        ("rust-webpki-roots" ,rust-webpki-roots-0.23)
        ("rust-x509-parser" ,rust-x509-parser-0.15))))))

(define-public rust-rustls-native-certs-0.5
  (package
    (inherit rust-rustls-native-certs-0.6)
    (name "rust-rustls-native-certs")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-native-certs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14i0bbbigk6r6262hvc51vz4dvqk1f3vg2f264wfvn2vi30vf1ss"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-rustls" ,rust-rustls-0.19)
        ("rust-schannel" ,rust-schannel-0.1)
        ("rust-security-framework" ,rust-security-framework-2))))))

(define-public rust-rustls-native-certs-0.4
  (package
    (inherit rust-rustls-native-certs-0.5)
    (name "rust-rustls-native-certs")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-native-certs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1f2rkvdkz92qcmwryyqiw9phkqkf95g4962ljpfq5nkjfsd477b2"))))
    (arguments
     `(#:tests? #f      ; Tests want network access
       #:cargo-inputs
       (("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-rustls" ,rust-rustls-0.18)
        ("rust-schannel" ,rust-schannel-0.1)
        ("rust-security-framework"
         ,rust-security-framework-1))
       #:cargo-development-inputs
       (("rust-ring" ,rust-ring-0.16)
        ("rust-untrusted" ,rust-untrusted-0.7)
        ("rust-webpki" ,rust-webpki-0.21)
        ("rust-webpki-roots" ,rust-webpki-roots-0.20))))))

(define-public rust-rustls-pemfile-2
  (package
    (name "rust-rustls-pemfile")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pemfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1x34xidvzn4br2vl8f8xwmhgbjv4lmlb0ggv5whlnk4yl87rir1m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base64" ,rust-base64-0.21)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1))
       #:cargo-development-inputs (("rust-bencher" ,rust-bencher-0.1))))
    (home-page "https://github.com/rustls/pemfile")
    (synopsis "Basic parser for PEM formatted keys and certificates")
    (description "This package provides a very basic parser for the
PEM-encodings commonly used to store keys and certificates at rest.")
    (license (list license:asl2.0 license:isc license:expat))))

(define-public rust-rustls-pemfile-1
  (package
    (inherit rust-rustls-pemfile-2)
    (name "rust-rustls-pemfile")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pemfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.21))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1))))))

(define-public rust-rustls-pemfile-0.2
  (package
    (inherit rust-rustls-pemfile-1)
    (name "rust-rustls-pemfile")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pemfile" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jfi97lqnnnnxhmfy6ygrsp0x70m8wsdpaw45svvz1qc6vmymssy"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.13))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3))))))

(define-public rust-rustls-pki-types-1
  (package
    (name "rust-rustls-pki-types")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-pki-types" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0banlc9xzwqrx8n0h4bd0igmq3z5hc72rn941lf22cp3gkkraqlp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-web-time" ,rust-web-time-1))))
    (home-page "https://github.com/rustls/pki-types")
    (synopsis "Shared types for the rustls PKI ecosystem")
    (description
     "This crate provides shared types for the rustls PKI ecosystem.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustls-webpki-0.102
  (package
    (name "rust-rustls-webpki")
    (version "0.102.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls-webpki" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nz9d3xhy8cg6anmvq64scyiva8bglrc6j3v6bdxw2f96xha4k7g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-aws-lc-rs" ,rust-aws-lc-rs-1)
                       ("rust-ring" ,rust-ring-0.17)
                       ("rust-rustls-pki-types" ,rust-rustls-pki-types-1)
                       ("rust-untrusted" ,rust-untrusted-0.9))
       #:cargo-development-inputs (("rust-base64" ,rust-base64-0.21)
                                   ("rust-bencher" ,rust-bencher-0.1)
                                   ("rust-bzip2" ,rust-bzip2-0.4)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-rcgen" ,rust-rcgen-0.12)
                                   ("rust-serde" ,rust-serde-1)
                                   ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://github.com/rustls/webpki")
    (synopsis "Web PKI X.509 Certificate Verification")
    (description "Web PKI X.509 Certificate Verification.")
    (license license:isc)))

(define-public rust-rustls-webpki-0.101
  (package
    (inherit rust-rustls-webpki-0.102)
    (name "rust-rustls-webpki")
    (version "0.101.7")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls-webpki" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))))
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-ring" ,rust-ring-0.17)
        ("rust-untrusted" ,rust-untrusted-0.9))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.21)
        ("rust-bencher" ,rust-bencher-0.1)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-rcgen" ,rust-rcgen-0.11)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))))))

(define-public rust-rustls-webpki-0.100
  (package
    (inherit rust-rustls-webpki-0.101)
    (name "rust-rustls-webpki")
    (version "0.100.3")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustls-webpki" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qsa7bgwbkgiqh7zq7lihd9jsvjl8mlm25ihzrnjghgib315ysjz"))))
    (arguments
     `(#:tests? #f      ; Not all files included.
       #:cargo-inputs
       (("rust-ring" ,rust-ring-0.16)
        ("rust-untrusted" ,rust-untrusted-0.7))
       #:cargo-development-inputs (("rust-base64" ,rust-base64-0.13)
                                   ("rust-rcgen" ,rust-rcgen-0.11))))))

(define-public rust-tls-parser-0.11
  (package
    (name "rust-tls-parser")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tls-parser" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1q0vd41sjf6pbcygp5bqlpqrxbqdd0qsqi4sm5zbzvb4vvi0d4j0"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Accept newer versions of rust-clap.
                 (substitute* "Cargo.toml"
                   (("~2\\.33") "^2.33"))))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;; requires assets not included in crates.io tarball
       #:cargo-inputs
       (("rust-cookie-factory" ,rust-cookie-factory-0.3)
        ("rust-enum-primitive" ,rust-enum-primitive-0.1)
        ("rust-nom" ,rust-nom-7)
        ("rust-nom-derive" ,rust-nom-derive-0.10)
        ("rust-phf" ,rust-phf-0.10)
        ("rust-phf-codegen" ,rust-phf-codegen-0.10)
        ("rust-rusticata-macros" ,rust-rusticata-macros-4))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-hex-literal" ,rust-hex-literal-0.3)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.7))))
    (home-page "https://github.com/rusticata/tls-parser")
    (synopsis "Parser for the TLS protocol")
    (description "This package provides a Rust parser for the TLS protocol.")
    (license (list license:expat license:asl2.0))))

(define-public rust-x509-cert-0.2
  (package
    (name "rust-x509-cert")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x509-cert" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "155f42vm6m7phn8w7s2wmk9vli3ws45dqpk5z3jilw0a04syj08k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-const-oid" ,rust-const-oid-0.9)
                       ("rust-der" ,rust-der-0.7)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-spki" ,rust-spki-0.7)
                       ("rust-tls-codec" ,rust-tls-codec-0.4))
       #:cargo-development-inputs
       (("rust-ecdsa" ,rust-ecdsa-0.16)
        ("rust-hex-literal" ,rust-hex-literal-0.4)
        ("rust-p256" ,rust-p256-0.13)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rsa" ,rust-rsa-0.9)
        ("rust-rstest" ,rust-rstest-0.18)
        ("rust-sha2" ,rust-sha2-0.10)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/x509-cert")
    (synopsis "X.509 Public Key Infrastructure Certificate format in Rust")
    (description
     "This package provides a pure Rust implementation of the X.509
Public Key Infrastructure Certificate format as described in RFC 5280.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-x509-parser-0.15
  (package
    (name "rust-x509-parser")
    (version "0.15.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "x509-parser" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nk3ryam7yzsza735xdypkv1i4c35gqlygax5jyr74bbnsjznsbh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-asn1-rs" ,rust-asn1-rs-0.5)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-der-parser" ,rust-der-parser-8)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-nom" ,rust-nom-7)
        ("rust-oid-registry" ,rust-oid-registry-0.6)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-rusticata-macros" ,rust-rusticata-macros-4)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/rusticata/x509-parser")
    (synopsis "X.509 parser written in pure Rust")
    (description "This crate provides a parser for the X.509 v3 format (RFC
5280 certificates).")
    (license (list license:expat license:asl2.0))))

(define-public rust-x509-parser-0.14
  (package
    (inherit rust-x509-parser-0.15)
    (name "rust-x509-parser")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x509-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1j7b3xxpwik38y9rajglmhis551gj3zz5irw1vj1bqkwnsvvxv70"))))
    (arguments
     `(#:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.5)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-der-parser" ,rust-der-parser-8)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-oid-registry" ,rust-oid-registry-0.6)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))))))

(define-public rust-x509-parser-0.13
  (package
    (inherit rust-x509-parser-0.15)
    (name "rust-x509-parser")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x509-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "077bi0xyaa8cmrqf3rrw1z6kkzscwd1nxdxgs7mgz2ambg7bmfcz"))))
    (arguments
     `(#:cargo-inputs (("rust-asn1-rs" ,rust-asn1-rs-0.3)
                       ("rust-base64" ,rust-base64-0.13)
                       ("rust-data-encoding" ,rust-data-encoding-2)
                       ("rust-der-parser" ,rust-der-parser-7)
                       ("rust-lazy-static" ,rust-lazy-static-1)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-oid-registry" ,rust-oid-registry-0.4)
                       ("rust-ring" ,rust-ring-0.16)
                       ("rust-rusticata-macros" ,rust-rusticata-macros-4)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-time" ,rust-time-0.3))))))

(define-public rust-x509-parser-0.12
  (package
    (inherit rust-x509-parser-0.15)
    (name "rust-x509-parser")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x509-parser" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1vanwazknxwd1kmlp443bpph9qyas021ayqk6iljxdscm0v0ijgz"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.13)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-data-encoding" ,rust-data-encoding-2)
        ("rust-der-parser" ,rust-der-parser-6)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-nom" ,rust-nom-7)
        ("rust-oid-registry" ,rust-oid-registry-0.2)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-rusticata-macros" ,rust-rusticata-macros-4)
        ("rust-thiserror" ,rust-thiserror-1))))))

(define-public rust-x509-signature-0.5
  (package
    (name "rust-x509-signature")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x509-signature" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "063mjzgddfam4xb88wr0li3k0q6nzyq3mvkiykajr69dj0mbrclz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f  ; `const_err` has been removed: converted into hard error
       #:cargo-test-flags '("--release" "--"
                            "--skip=tests::parses_openssl_generated_cert")
       #:cargo-inputs (("rust-ring" ,rust-ring-0.16)
                       ("rust-rustls" ,rust-rustls-0.18)
                       ("rust-untrusted" ,rust-untrusted-0.7)
                       ("rust-webpki" ,rust-webpki-0.21))
       #:cargo-development-inputs (("rust-chrono" ,rust-chrono-0.4))))
    (home-page "https://github.com/paritytech/x509-signature")
    (synopsis "Low-level X.509 parsing and signature verification library")
    (description "This package provides a low-level X.509 parsing and signature
verification library.")
    (license (list license:expat license:asl2.0))))
