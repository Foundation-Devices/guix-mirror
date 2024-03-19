;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Luis Henrique Gomes Higino <luishenriquegh2701@gmail.com>
;;; Copyright © 2022, 2023 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2023, 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023, 2024 Raven Hallsby <karl@hallsby.com>
;;; Copyright © 2024 Foundation Devices, Inc. <hello@foundation.xyz>
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

(define-module (gnu packages tree-sitter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-vcs)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages node)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system tree-sitter)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public python-tree-sitter
  (package
    (name "python-tree-sitter")
    (version "0.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/py-tree-sitter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rc8zqiry4n52xlf7pwx4s56ka9vwjzwgn7blwbkiscqdwvsai92"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-tree-sitter-lib-path
            (lambda _
              (let ((tree-sitter #$(this-package-input "tree-sitter")))
                (substitute* "setup.py"
                  (((string-append
                     "( *)\\[\"tree_sitter\\/core\\/lib\\/src\\/lib\\.c\", "
                     "\"tree_sitter\\/binding\\.c\"\\],") all tabs)
                   (string-append
                    tabs "[\"tree_sitter/binding.c\"],\n"
                    tabs "library_dirs=[\"" tree-sitter "/lib\"],\n"
                    tabs "libraries=[\"tree-sitter\"],"))
                  (("include_dirs=.*")
                   (string-append
                    "include_dirs=[\"" tree-sitter "/include\"],\n"))))))
          (add-before 'check 'set-test-lib-paths
            (lambda _
              (let ((py #$(this-package-native-input "tree-sitter-python"))
                    (js #$(this-package-native-input "tree-sitter-javascript")))
                (substitute* "tests/test_tree_sitter.py"
                  (("Language\\.build_library")
                   "_ =")
                  (("LIB_PATH(, \"python\")" all name)
                   (string-append
                    "\"" py "/lib/tree-sitter/libtree-sitter-python.so\"" name))
                  (("LIB_PATH(, \"javascript\")" all name)
                   (string-append
                    "\"" js "/lib/tree-sitter/libtree-sitter-javascript.so\""
                    name)))))))))
    (inputs (list tree-sitter))
    (native-inputs
     (list tree-sitter-python tree-sitter-javascript))
    (home-page "https://github.com/tree-sitter/py-tree-sitter")
    (synopsis "Python bindings to the Tree-sitter parsing library")
    (description "This package provides Python bindings to the
Tree-sitter parsing library.")
    (license license:expat)))

(define-public tree-sitter
  (package
    (name "tree-sitter")
    (version "0.22.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dbndz3r9h11mr2nsivbf4v6snm3v37rp74ynf5wfvq21d53f4s6"))
              (modules '((guix build utils)))
              (snippet #~(begin
                           ;; Remove bundled ICU parts
                           (delete-file-recursively "lib/src/unicode")))))
    (build-system gnu-build-system)
    (inputs (list icu4c))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               ;; The library uses -fvisibility=hidden to compile, but this
               ;; symbol is needed by the Rust bindings.
               (add-after 'unpack 'patch-_ts_dup-visibility
                 (lambda _
                   (substitute* "lib/src/tree.c"
                     (("int _ts_dup")
                       (string-append
                         "int __attribute__ ((visibility (\"default\"))) "
                         "_ts_dup"))))))
           #:tests? #f ; there are no tests for the runtime library
           #:make-flags
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "CC=" #$(cc-for-target)))))
    (home-page "https://tree-sitter.github.io/tree-sitter/")
    (synopsis "Incremental parsing system for programming tools")
    (description
     "Tree-sitter is a parser generator tool and an incremental parsing
library.  It can build a concrete syntax tree for a source file and
efficiently update the syntax tree as the source file is edited.

Tree-sitter aims to be:

@itemize
@item General enough to parse any programming language
@item Fast enough to parse on every keystroke in a text editor
@item Robust enough to provide useful results even in the presence of syntax errors
@item Dependency-free so that the runtime library (which is written in pure C)
can be embedded in any application
@end itemize

This package includes the @code{libtree-sitter} runtime library.")
    (license license:expat)))

(define-public tree-sitter-cli
  (package
    (inherit tree-sitter)
    (name "tree-sitter-cli")
    (source (origin
              (inherit (package-source tree-sitter))
              (snippet
               #~(begin
                   ;; Don't build the runtime library and dynamically link to
                   ;; it instead.
                   (delete-file "lib/binding_rust/build.rs")
                   (with-output-to-file "lib/binding_rust/build.rs"
                     (lambda _
                       (format #t "use std::{env, fs, path::{Path, PathBuf}};~@
                              fn main() {~@
                              let out_dir =
                              PathBuf::from(env::var(\"OUT_DIR\").unwrap());~@
                              fs::copy(\"src/wasm/stdlib-symbols.txt\",~@
                              out_dir.join(\"stdlib-symbols.txt\"))
                              .unwrap();~@
                              println!(\"cargo:rustc-link-lib=tree-sitter\");~@
                              }~%")))))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:cargo-test-flags
      ''("--release" "--"
         ;; Skip tests which rely on downloading grammar fixtures.  It is
         ;; difficult to support such tests given upstream does not encode
         ;; which version of the grammars are expected.
         ;; Instead, we do run some tests for each grammar in the tree-sitter
         ;; build-system, by running `tree-sitter test'.  This isn't as
         ;; complete as running all tests from tree-sitter-cli, but it's a
         ;; good compromise compared to maintaining two different sets of
         ;; grammars (Guix packages vs test fixtures).
         "--skip=tests::corpus_test"
         "--skip=tests::github_issue_test"
         "--skip=tests::highlight_test"
         "--skip=tests::node_test"
         "--skip=tests::parser_test"
         "--skip=tests::pathological_test"
         "--skip=tests::query_test"
         "--skip=tests::tags_test"
         "--skip=tests::test_highlight_test"
         "--skip=tests::test_tags_test"
         "--skip=tests::tree_test"
         "--skip=tests::async_context_test"
         "--skip=tests::text_provider_test"
         "--skip=tests::detect_language"
         "--skip=tests::language_test"
         "--skip=tests::parser_hang_test")
      ;; We're only packaging the CLI program so we do not need to install
      ;; sources.
      #:install-source? #f
      #:cargo-inputs
      `(("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-anstyle" ,rust-anstyle-1)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-cc" ,rust-cc-1)
        ("rust-clap" ,rust-clap-4)
        ("rust-ctor" ,rust-ctor-0.2)
        ("rust-ctrlc" ,rust-ctrlc-3)
        ("rust-difference" ,rust-difference-2)
        ("rust-dirs" ,rust-dirs-5)
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-fs4" ,rust-fs4-0.8)
        ("rust-git2" ,rust-git2-0.18)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-heck" ,rust-heck-0.4)
        ("rust-html-escape" ,rust-html-escape-0.2)
        ("rust-indexmap" ,rust-indexmap-2)
        ("rust-indoc" ,rust-indoc-2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libloading" ,rust-libloading-0.8)
        ("rust-log" ,rust-log-0.4)
        ("rust-memchr" ,rust-memchr-2)
        ("rust-regex" ,rust-regex-1)
        ("rust-regex-syntax" ,rust-regex-syntax-0.8)
        ("rust-rustc-hash" ,rust-rustc-hash-1)
        ("rust-semver" ,rust-semver-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-derive" ,rust-serde-derive-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-smallbitvec" ,rust-smallbitvec-2)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tiny-http" ,rust-tiny-http-0.12)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-wasmparser" ,rust-wasmparser-0.201)
        ("rust-wasmtime" ,rust-wasmtime-18)
        ("rust-wasmtime-c-api-impl" ,rust-wasmtime-c-api-impl-18)
        ("rust-webbrowser" ,rust-webbrowser-0.8))
      #:cargo-development-inputs
      `(("rust-ctor" ,rust-ctor-0.2)
        ("rust-pretty-assertions" ,rust-pretty-assertions-1)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-unindent" ,rust-unindent-0.2))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-.cargo/config.toml
            (lambda _
              (delete-file ".cargo/config.toml")))
          (add-after 'unpack 'patch-node
            (lambda _
              (substitute* "cli/src/generate/mod.rs"
                (("js_runtime\\.unwrap_or\\(\"node\"\\)")
                 (format #f "js_runtime.unwrap_or(\"~a/bin/node\")"
                         #$(this-package-input "node"))))))
          (add-after 'unpack 'patch-dot
            (lambda _
              (substitute* "cli/src/util.rs"
                (("Command::new\\(\"dot\"\\)")
                 (format #f "Command::new(\"~a/bin/dot\")"
                         #$(this-package-input "graphviz"))))))
          (add-after 'unpack 'patch-podman
            (lambda _
              (substitute* "cli/loader/src/lib.rs"
                (("Command::new(\"podman\")")
                 (format #f "Command::new(\"~a/bin/podman\")"
                         #$(this-package-input "podman"))))))
          (add-after 'unpack 'relax-version-requirements
            (lambda _
              (substitute* "Cargo.toml"
                (("git2 = \"0.18.2\"") "git2 = \"0.18\""))
              ;; Relax rust-syn-2 version
              (substitute* "cli/src/tests/proc_macro/Cargo.toml"
                (("2\\.0\\.52") "2"))))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (install-file "target/release/tree-sitter" bin)))))))
    (inputs
     (list tree-sitter
           graphviz
           node-lts
           podman))
    (description "Tree-sitter is a parser generator tool and an incremental
parsing library.  It can build a concrete syntax tree for a source file and
efficiently update the syntax tree as the source file is edited.

Tree-sitter aims to be:

@enumerate
@item General enough to parse any programming language.
@item Fast enough to parse on every keystroke in a text editor.
@item Robust enough to provide useful results even in the presence of syntax
errors.
@item Dependency-free so that the runtime library (which is written in pure C)
can be embedded in any application.
@end enumerate

This package includes the @command{tree-sitter} command-line tool.")
    (license license:expat)))

(define (tree-sitter-delete-generated-files grammar-directories)
  #~(begin
      (use-modules (guix build utils))
      (delete-file "binding.gyp")
      (delete-file-recursively "bindings")
      (for-each
       (lambda (lang)
         (with-directory-excursion lang
           (delete-file "src/grammar.json")
           (delete-file "src/node-types.json")
           (delete-file "src/parser.c")
           (delete-file-recursively "src/tree_sitter")))
       '#$grammar-directories)))

(define* (tree-sitter-grammar
          name text hash version
          #:key
          (commit (string-append "v" version))
          (repository-url
           (format #f "https://github.com/tree-sitter/tree-sitter-~a" name))
          (grammar-directories '("."))
          (article "a")
          (inputs '())
          (get-cleanup-snippet tree-sitter-delete-generated-files)
          (license license:expat))
  "Returns a package for Tree-sitter grammar.  NAME will be used with
tree-sitter- prefix to generate package name and also for generating
REPOSITORY-URL value if it's not specified explicitly, TEXT is a string which
will be used in description and synopsis. GET-CLEANUP-SNIPPET is a function,
it recieves GRAMMAR-DIRECTORIES as an argument and should return a G-exp,
which will be used as a snippet in origin."
  (let* ((multiple? (> (length grammar-directories) 1))
         (grammar-names (string-append text " grammar" (if multiple? "s" "")))
         (synopsis (string-append "Tree-sitter " grammar-names))
         (description
          (string-append "This package provides "
                         (if multiple? "" article) (if multiple? "" " ")
                         grammar-names " for the Tree-sitter library."))
         (name (string-append "tree-sitter-" name)))
    (package
      (name name)
      (version version)
      (home-page repository-url)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url repository-url)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 (base32 hash))
                (snippet
                 (get-cleanup-snippet grammar-directories))))
      (build-system tree-sitter-build-system)
      (arguments (list #:grammar-directories grammar-directories))
      (inputs inputs)
      (synopsis synopsis)
      (description description)
      (license license))))

(define-public tree-sitter-html
  (tree-sitter-grammar
   "html" "HTML"
   "1hg7vbcy7bir6b8x11v0a4x0glvqnsqc3i2ixiarbxmycbgl3axy"
   "0.19.0"))

(define-public tree-sitter-javascript
  ;; Keep version in synchronization with tree-sitter-typescript.
  (tree-sitter-grammar
   "javascript" "JavaScript(JSX)"
   "1mvvc6cv46zyhxhdjycmj7746hbss7lxcxks61bzrh229nlrh6hy"
   "0.20.4"
   #:get-cleanup-snippet
   (lambda (grammar-directories)
     #~(begin
         (use-modules (guix build utils))
         (delete-file "tree-sitter-javascript.wasm")
         (delete-file "binding.gyp")
         (delete-file-recursively "bindings")
         (for-each
          (lambda (lang)
            (with-directory-excursion lang
              (delete-file "src/grammar.json")
              (delete-file "src/node-types.json")
              (delete-file "src/parser.c")
              (delete-file-recursively "src/tree_sitter")))
          '#$grammar-directories)))))

(define-public tree-sitter-typescript
  (tree-sitter-grammar
   "typescript" "TypeScript and TSX"
   "1lv3w5wxpzjbq629b7krnbww2hba6vk4s7y3l8p4jm4kaw9v0sxq"
   "0.20.6"
   #:inputs (list tree-sitter-javascript)
   #:grammar-directories '("typescript" "tsx")))

(define-public tree-sitter-bibtex
  (let ((commit "ccfd77db0ed799b6c22c214fe9d2937f47bc8b34")
        (revision "0"))
    (tree-sitter-grammar
     "bibtex" "Bibtex"
     "0m7f3dkqbmy8x1bhl11m8f4p6n76wfvh99rp46zrqv39355nw1y2"
     (git-version "0.1.0" revision commit)
     #:repository-url "https://github.com/latex-lsp/tree-sitter-bibtex"
     #:commit commit
     #:license license:expat)))

(define-public tree-sitter-css
  (tree-sitter-grammar
   "css" "CSS"
   "014jrlgi7zfza9g38hsr4vlbi8964i5p7iglaih6qmzaiml7bja2"
   "0.19.0"))

(define-public tree-sitter-c
  (tree-sitter-grammar
   "c" "C"
   "01w26hv024grc79wif26y655iy7w8p0zyfj5bhr6zxx8bbjbcypz"
   "0.21.0"))

(define-public tree-sitter-cpp
  (let ((base (tree-sitter-grammar
               "cpp" "C++"
               "0fjxjm3gjqvcjqgjyq6lg6sgyy0ly69dinq33rmy56806da45lq9"
               "0.20.5"
               #:inputs (list tree-sitter-c))))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
             ;; Highlight tests have not been updated yet for tree-sitter-cli@0.22
              (add-before 'check 'delete-highligh-tests
                (lambda _
                  (delete-file-recursively "test/highlight"))))))))))

(define-public tree-sitter-cmake
  (tree-sitter-grammar
   "cmake" "CMake"
   "1z49jdachwxwbzrrapskpi2kxq3ydihfj45ab9892gbamfij2zp5"
   "0.4.1"
   #:repository-url "https://github.com/uyha/tree-sitter-cmake"))

(define-public tree-sitter-elixir
  (let ((base (tree-sitter-grammar
               "elixir" "Elixir"
               "1fqsvqdjscmjj7vaq3mgs6j49m3412g5i9jrm1r61n1d8yrg3mzy"
               "0.1.1"
               #:article "an"
               #:repository-url "https://github.com/elixir-lang/tree-sitter-elixir"
               #:license (list license:asl2.0 license:expat))))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases #~%standard-phases)
         #~(modify-phases #$phases
             (add-after 'unpack 'fix-grammar-error
               (lambda _
                 (substitute* "grammar.js"
                   (("x\\{\\[0-9a-fA-F\\]\\+\\}")
                    "x\\{[0-9a-fA-F]+\\}")
                   (("u\\{\\[0-9a-fA-F\\]\\+\\}")
                    "u\\{[0-9a-fA-F]+\\}"))))
             ;; Highlight tests have not been updated yet for tree-sitter-cli@0.22
             (add-before 'check 'delete-highligh-tests
               (lambda _
                 (delete-file-recursively "test/highlight"))))))))))

(define-public tree-sitter-heex
  (tree-sitter-grammar
   "heex" "Heex"
   "00330rgg67fq0d9gk1yswj78d9mn1jvvjmmy1k7cxpvm5993p3sw"
   "0.6.0"
   #:repository-url "https://github.com/phoenixframework/tree-sitter-heex"))

(define-public tree-sitter-bash
  (tree-sitter-grammar
   "bash" "Bash"
   "01sjympivwhr037c0gdx5fqw8fvzchq4fd4m8wlr8mdw50di0ag2"
   "0.20.4"))

(define-public tree-sitter-c-sharp
  (tree-sitter-grammar
   "c-sharp" "C#"
   "0lijbi5q49g50ji00p2lb45rvd76h07sif3xjl9b31yyxwillr6l"
   "0.20.0"))

(define-public tree-sitter-dockerfile
  ;; From `git describe --tags'.
  (let ((commit "33e22c33bcdbfc33d42806ee84cfd0b1248cc392")
        (revision "29"))
    (tree-sitter-grammar
     "dockerfile" "Dockerfile"
     "1zhrg9ick72m1ywvnvab8kw4a2ncfsxl2hkrnckx0by96r6v68mq"
     (git-version "0.1.2" revision commit)
     #:repository-url "https://github.com/camdencheek/tree-sitter-dockerfile"
     #:commit commit)))

(define-public tree-sitter-elm
  (tree-sitter-grammar
   "elm" "Elm"
   "0b5jpj8bnil1ylisyc4w48j8a30dyf3zylhidj73mlrb8rf7xm2s"
   "5.6.3"
   #:article "an"
   #:repository-url "https://github.com/elm-tooling/tree-sitter-elm"))

(define-public tree-sitter-gomod
  (tree-sitter-grammar
   "gomod" "Go .mod"
   "1clw1wyjxiicdjav5g2b9m9q7vlg5k1iy1fqwmf2yc4fxrfnmyrq"
   "1.0.2"
   #:repository-url "https://github.com/camdencheek/tree-sitter-go-mod"))

(define-public tree-sitter-go
  (tree-sitter-grammar
   "go" "Go"
   "0wlhwcdlaj74japyn8wjza0fbwckqwbqv8iyyqdk0a5jf047rdqv"
   "0.20.0"))

(define-public tree-sitter-haskell
  ;; There are a lot of additions, the last tag was placed more than 4 years ago
  (let ((commit "3bdba07c7a8eec23f87fa59ce9eb2ea4823348b3")
        (revision "0"))
    (tree-sitter-grammar
     "haskell" "Haskell"
     "1hg19af1n510bndf5k5iri7dzb48xb527vispv1aapki4mvr98gx"
     (git-version "0.14.0" revision commit)
     #:commit commit)))

(define-public tree-sitter-hcl
  (tree-sitter-grammar
   "hcl" "HCL"
   "1yydi61jki7xpabi0aq6ykz4w4cya15g8rp34apb6qq9hm4lm9di"
   "1.1.0"
   #:article "an"
   #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-hcl"
   #:license license:asl2.0))

(define-public tree-sitter-java
  ;; From `git describe'. The latest tag (0.20.2) fails tests.
  (let ((commit "2aae502017d3aed587ba85e3c7e0cbc138f3e07a")
        (revision "3"))
    (tree-sitter-grammar
     "java" "Java"
     "1ajaqvm547k6m5rrjb0awh06gb1xlkx2y97di8wysvkg1c6jjcsk"
     (git-version "0.20.2" revision commit)
     #:commit commit)))

(define-public tree-sitter-json
  ;; Not tagged
  (let ((commit "5d992d9dd42d533aa25618b3a0588f4375adf9f3"))
    (tree-sitter-grammar
     "json" "JSON"
     "08kxzqyyl900al8mc0bwigxlkzsh2f14qzjyb5ki7506myxlmnql"
     "0.20.0"
     #:commit commit)))

(define-public tree-sitter-julia
  (tree-sitter-grammar
   "julia" "Julia"
   "1pbnmvhy2gq4vg1b0sjzmjm4s2gsgdjh7h01yj8qrrqbcl29c463"
   "0.19.0"))

(define-public tree-sitter-kdl
  (tree-sitter-grammar
   "kdl" "KDL"
   "1015x24ffrvzb0m0wbqdzmaqavpnjw0gvcagxi9b6vj3n1ynm0ps"
   "1.1.0"
   #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-kdl"))

(define-public tree-sitter-ocaml
  (tree-sitter-grammar
   "ocaml" "OCaml (.ml and .mli)"
   "04vscg6lkhdnzs15r1yqwwmc2lj73x4h3nf4mfpkwq6g870i04wj"
   "0.22.0"
   #:grammar-directories '("grammars/ocaml" "grammars/interface")))

(define-public tree-sitter-php
  ;; From `git describe' as some improvements have happened since 0.22.2.
  (let ((commit "29838ad107f50b1f5f51a0beefa9c9d834fce2b3")
        (revision "17"))
    (tree-sitter-grammar
     "php" "PHP"
     "1wxysbw2c5xrm99z4255x69p0phnaq08dsgxr95hmgpsgbc4rcg5"
     (git-version "0.22.2" revision commit)
     #:commit commit
     #:grammar-directories '("php" "php_only"))))

(define-public tree-sitter-python
  (tree-sitter-grammar
   "python" "Python"
   "0wv59wfbxqp8b64fy36vd5n3ifs15zlzkjdfxgb9zkg0cvs7h3v5"
   "0.21.0"))

(define-public tree-sitter-r
  ;; No tags
  (let* ((commit "c55f8b4dfaa32c80ddef6c0ac0e79b05cb0cbf57")
         (revision "1")
         (base (tree-sitter-grammar
                "r" "R"
                "0si338c05z3bapxkb7zwk30rza5w0saw0jyk0pljxi32869w8s9m"
                (git-version "0.0.1" revision commit)
                #:repository-url "https://github.com/r-lib/tree-sitter-r"
                #:commit commit)))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'fix-grammar-error
                (lambda _
                  (substitute* "grammar.js"
                    (("u\\{\\[0-9a-fA-F\\]\\+\\}")
                     "u\\{[0-9a-fA-F]+\\}")))))))))))

(define-public tree-sitter-ron
  (let ((base (tree-sitter-grammar
               "ron" "RON"
               "1la5v0nig3xp1z2v3sj36hb7wkkjch46dmxf457px7ly43x4cb83"
               "0.2.0"
               #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-ron"
               #:license (list license:asl2.0 license:expat))))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'fix-grammar-error
                (lambda _
                  (substitute* "grammar.js"
                    (("u\\{\\[0-9a-fA-F\\]\\+\\}")
                     "u\\{[0-9a-fA-F]+\\}")))))))))))

(define-public tree-sitter-ruby
  ;; This commit fixes grammar issues, the last tag doesn't contain the fix.
  ;;
  ;; Obtained from: `git describe'.
  (let* ((commit "9d86f3761bb30e8dcc81e754b81d3ce91848477e")
         (revision "2")
         (base (tree-sitter-grammar
                "ruby" "Ruby"
                "0qzwgx6hs9bx7wbgyrazsrf6k69fikcddcmqiqxlq2jnjgxyxdr1"
                (git-version "0.20.1" revision commit)
                #:commit commit)))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              ;; Highlight tests have not been updated yet for
              ;; tree-sitter-cli@0.22.
              (add-before 'check 'delete-highligh-tests
                (lambda _
                  (delete-file-recursively "test/highlight"))))))))))

(define-public tree-sitter-rust
  ;; From `git describe' as the latest tag does not build with the
  ;; tree-sitter version.
  (let ((commit "3a56481f8d13b6874a28752502a58520b9139dc7")
        (revision "25"))
    (tree-sitter-grammar
     "rust" "Rust"
     "12806974pngxqv1brj4r15yqzp2fdvid926n7941nylgmdw9f4z9"
     (git-version "0.20.4" revision commit)
     #:commit commit)))

(define-public tree-sitter-ungrammar
  ;; No releases yet.
  (let ((commit "debd26fed283d80456ebafa33a06957b0c52e451")
        (revision "0"))
    (tree-sitter-grammar
     "ungrammar" "Ungrammar"
     "09bbml1v1m6a9s9y9q1p2264ghf3fhb6kca1vj3qm19yq87xrnvy"
     (git-version "0.0.2" revision commit)
     #:commit commit
     #:repository-url "https://github.com/tree-sitter-grammars/tree-sitter-ungrammar"
     #:article "an")))

(define-public tree-sitter-clojure
  (tree-sitter-grammar
   "clojure" "Clojure"
   "0bgd9g1j4ww45g0l0aa1jac49421z95cc2rhcgqmgx7nzn94rszp"
   "0.0.11"
   #:repository-url "https://github.com/sogaiu/tree-sitter-clojure"))

(define-public tree-sitter-markdown
  ;; From `git describe --tags'.
  (let* ((commit "44017499c51cb6431635ed51d5080e1fd05c2c21")
         (revision "3")
         (base (tree-sitter-grammar
                "markdown" "Markdown (CommonMark Spec v0.31.2)"
                "1n9kf2kvqrj9s0fd5nhy31l4g8cbzzvjihsvch301rcm7dy1xbv7"
                (git-version "0.2.3" revision commit)
                #:repository-url "https://github.com/MDeiml/tree-sitter-markdown"
                #:grammar-directories '("tree-sitter-markdown"
                                        "tree-sitter-markdown-inline")
                #:commit commit)))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (add-before 'check 'delete-failing-tests
                (lambda _
                  (for-each
                    (lambda (file)
                      (delete-file (string-append
                                     "tree-sitter-markdown-inline"
                                     "/test/corpus/" file ".txt")))
                    '("extension_wikilink"
                      "spec"
                      "tags")))))))))))

(define-public tree-sitter-matlab
  (let ((commit "79d8b25f57b48f83ae1333aff6723b83c9532e37")
        (revision "0"))
    (tree-sitter-grammar
     "matlab" "Matlab"
     "04ffhfnznskkcp91fbnv8jy3wkb9cd8ifwrkrdwcw74n1b2hq80c"
     (git-version "1.0.2" revision commit)
     #:repository-url "https://github.com/acristoffers/tree-sitter-matlab"
     #:commit commit
     #:license license:expat)))

(define-public tree-sitter-meson
  (tree-sitter-grammar
   "meson" "Meson"
   "1ykyzz8rng0l4qd9jpziigxfbnvi30h1lvsgks5lv84n1w4a26pr"
   "1.2.1"
   #:repository-url "https://github.com/Decodetalkers/tree-sitter-meson"
   #:commit "1.2.1"))

(define-public tree-sitter-nix
  (tree-sitter-grammar
   "nix" "Nix"
   "0nn3ij8k6wkbf3kcvkyyp0vhfjcksi31wyyfwmsbx66maf2xgaii"
   "0.0.0"
   ;; The most recent commit at time of packaging, no tags.
   #:commit "763168fa916a333a459434f1424b5d30645f015d"
   #:repository-url "https://github.com/nix-community/tree-sitter-nix"))

(define-public tree-sitter-org
  ;; From `git describe --tags'. There are a lot of additions, the last tag
  ;; was placed a while ago.
  (let* ((commit "64cfbc213f5a83da17632c95382a5a0a2f3357c1")
         (revision "3")
         (base (tree-sitter-grammar
                "org" "Org"
                "1l62p4a3b22pa7b5mzmy497pk5b8w01hx6zinfwpbnzg2rjdwkgz"
                (git-version "1.3.1" revision commit)
                #:repository-url "https://github.com/milisims/tree-sitter-org"
                #:commit commit)))
    (package
      (inherit base)
      (source (origin
                (inherit (package-source base))
                (patches
                 (search-patches "tree-sitter-org-package-json.patch")))))))

(define-public tree-sitter-scheme
  ;; There are a lot of additions, the last tag was placed a while ago
  (let ((commit "67b90a365bebf4406af4e5a546d6336de787e135")
        (revision "0"))
    (tree-sitter-grammar
     "scheme" "Scheme (R5RS, R6RS)"
     "1pvxckza1kdfwqs78ka3lbwldrwkgymb31f5x1fq5vyawg60wxk8"
     (git-version "0.2.0" revision commit)
     #:repository-url "https://github.com/6cdh/tree-sitter-scheme"
     #:commit commit)))

(define-public tree-sitter-racket
  ;; No tags
  (let ((commit "1a5df0206b25a05cb1b35a68d2105fc7493df39b")
        (revision "0"))
    (tree-sitter-grammar
     "racket" "Racket"
     "06gwn3i7swhkvbkgxjlljdjgvx8y1afafbqmpwya70r9z635593h"
     (git-version "0.1.0" revision commit)
     #:repository-url "https://github.com/6cdh/tree-sitter-racket"
     #:commit commit)))

(define-public tree-sitter-plantuml
  ;; No tags
  (let* ((commit "c7361a1d481dc1ff6700b14ea1d5efc549b72713")
         (revision "1")
         (base (tree-sitter-grammar
                "plantuml" "PlantUML"
                "0apmv0dad58ixhxhzxkwlm9wgbphj7lxilbh427rpxy1y5hrml0f"
                (git-version "1.0.0" revision commit)
                #:repository-url "https://github.com/Decodetalkers/tree_sitter_plantuml"
                #:commit commit
                #:get-cleanup-snippet
                (lambda _
                  #~(begin
                      (use-modules (guix build utils))
                      (delete-file "binding.gyp")
                      (delete-file-recursively "bindings"))))))
    (package
      (inherit base)
      (source
       (origin
         (inherit (package-source base))
         (patches
          (search-patches "tree-sitter-plantuml-package-json.patch"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'fix-grammar-error
                (lambda _
                  (substitute* "grammar.js"
                    (("u\\{\\[0-9a-fA-F\\]\\+\\}")
                     "u\\{[0-9a-fA-F]+\\}")))))))))))

(define-public tree-sitter-latex
  (tree-sitter-grammar
   "latex" "LaTeX"
   "0lc42x604f04x3kkp88vyqa5dx90wqyisiwl7nn861lyxl6phjnf"
   "0.3.0"
   #:repository-url "https://github.com/latex-lsp/tree-sitter-latex"))

(define-public tree-sitter-lua
  (tree-sitter-grammar
   "lua" "Lua"
   "05irhg6gg11r9cnzh0h3691pnxjhd396sa1x8xrgqjz2fd09brf3"
   "0.0.19"
   #:repository-url "https://github.com/MunifTanjim/tree-sitter-lua"))

(define-public tree-sitter-scala
  (tree-sitter-grammar
   "scala" "Scala"
   "1j2ivdm21c5db54rcff00n7bqcfrfjc91jwlfl4a2cm363hbrym2"
   "0.21.0"))

(define-public tree-sitter-tlaplus
  (tree-sitter-grammar
   "tlaplus" "TLA+"
   "1k60dnzafj6m9c2d4xnwiz3d7yw3bg3iwx7c1anhwr76iyxdci3w"
   "1.0.8"
   ;; Version 1.2.1 is most recent, but requires tree-sitter >0.21.0
   #:repository-url "https://github.com/tlaplus-community/tree-sitter-tlaplus"))
