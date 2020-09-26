;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (guix scripts filesearch)
  #:use-module (guix config)            ; For %guix-version.
  #:use-module (sqlite3)
  #:use-module (guix gexp)              ; For lower-object.
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix store database)
  #:use-module (guix monads)
  #:use-module (guix grafts)
  #:use-module (guix records)
  #:use-module (guix derivations)
  #:use-module (guix utils)             ; For cache-directory
  #:use-module (gnu packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw))

;; TODO: We need to remove package duplicates.
;; Using "insert or replace ... on conflict (path) do nothing" moves database
;; generation time from 30s to 100s.
;;
;; Remove duplicates afterwards?

;; TODO: Vacuum database?  When?
;; https://sqlite.org/lang_vacuum.html

(define %db (format #f "~a/files.db" (cache-directory)))
(define %schema (search-path %load-path "guix/scripts/schema.sql"))

(define-syntax-rule (with-statement db sql stmt exp ...) ; TODO: From (guix store database)
  "Run EXP... with STMT bound to a prepared statement corresponding to the sql
string SQL for DB."
  ((@@ (guix store database) call-with-statement) db sql ; TODO: Export?
   (lambda (stmt) exp ...)))

(define* (add-files db
                    #:key
                    (name (error "Missing argument"))
                    (system (error "Missing argument"))
                    (output "out")
                    (path (error "Missing argument"))
                    (files (error "Missing argument"))
                    (version (error "Missing argument"))
                    (synopsis (error "Missing argument"))
                    (description (error "Missing argument"))
                    (guix-version (error "Missing argument")))
  "FILES is a list of path underneath PATH."
  (with-statement
      db
      (string-append "insert into Packages (name, system, output, version, path, guix)"
                     " values (:name, :system, :output, :version, :path, :guix)")
      stmt
    (sqlite-bind-arguments stmt
                           #:name name
                           #:system system
                           #:output output
                           #:path path
                           #:version version
                           #:guix guix-version)
    (map vector->list
         (sqlite-fold cons '() stmt)))
  (let ((id ((@@ (guix store database) last-insert-row-id) db))) ; TODO: Export?
    (with-statement
        db
        (string-append "insert into Info (name, synopsis, description, package)"
                       " values (:name, :synopsis, :description, :id)")
        stmt
      (sqlite-bind-arguments stmt
                             #:name name
                             #:synopsis synopsis
                             #:description description
                             #:id id)
      (map vector->list
           (sqlite-fold cons '() stmt)))
    (for-each
     (lambda (file)
       (sqlite-exec
        db
        (string-append "insert into Files (subpath, package) "
                       (format #f " values (~s, ~s)"
                               file id))))
     files)))

(define (directory-files path)
  "Return a list of all files within PATH, recursively.
Each file is returned as the path relative to PATH, starting with a '/'.
Empty directories are ignored.

It's important that the first character be the directory separator because it
gives more expressive power for search.  For instance, searching \"/bin\"
matches both \"/bin/foo\" and \"/usr/bin/foo\" but not \"barbin\"."
  (let ((file-list '()))
    (ftw path
         (lambda (filename statinfo flag)
           (when (eq? flag 'regular)
             (set! file-list (cons (string-drop filename (string-length path))
                                   file-list))) #t))
    file-list))

(define-record-type* <package-store-items> package-store-items make-package-store-items
  package-store-items?
  this-package-store-items
  (system package-store-items-system)
  (output-paths package-store-items-output-paths))

(define* (package-store-info package)
  "Return store items, even if not present locally."
  (define (lower-object/no-grafts obj system) ; From (guix scripts weather)
    (mlet* %store-monad ((previous (set-grafting #f))
                         (drv (lower-object obj system))
                         (_ (set-grafting previous)))
      (return drv)))
  (with-store store
    (run-with-store store
      (mlet %store-monad ((drv (lower-object/no-grafts package (%current-system))))
        ;; Note: we don't try building DRV like 'guix archive' does
        ;; because we don't have to since we can instead rely on
        ;; substitute meta-data.
        (return
         (package-store-items
          (system (derivation-system drv))
          (output-paths (derivation->output-paths drv))))))))

(define (persist-package-files db package)
  (let* ((info (package-store-info package))
         (system (package-store-items-system info))
         (output-path-pairs (package-store-items-output-paths info)))
    (map (match-lambda
           ((output . path)
            ;; TODO: Don't list files if entry is already in database.
            ;; TODO: Try fetching info from remote substitute server database.
            (when (file-exists? path)
              (add-files db         ; TODO: Merge this function and add-files?
                         #:name (package-name package)
                         #:system system
                         #:output output
                         #:path path ; Storing /gnu/store for all packages has no significant size cost.
                         #:version (package-version package)
                         #:synopsis (package-synopsis package)
                         #:description (package-description package)
                         #:guix-version %guix-version
                         #:files (directory-files path)))))
         output-path-pairs)))

(define (search-file-package pattern . more-patterns)
  "Return corresponding packages.
Packages or ordered by most relevant last.
Path is subject to SQLite \"full-text search\" pattern matching.
See https://www.sqlite.org/fts5.html.

Example patterns:

- \"foo bar\": Both the \"foo\" and \"bar\" full words are in the path.
- \"bar foo\": Same as above, order does not matter.
- \"foo*\": Matches any word starting with \"foo\".
- \"foo OR bar\": Either \"foo\" or \"bar\" full words are in the path."
  (with-database %db db
    (with-statement
        db
        ;; REVIEW: Is this inner join cheap?
        (string-append
         "SELECT subpath, name, version, output"
         " FROM Files INNER JOIN Packages ON Files.package = Packages.id"
         " WHERE Files.subpath MATCH :pattern ORDER BY RANK")
        stmt
      (sqlite-bind-arguments stmt #:pattern (string-concatenate
                                             (map (lambda (s)
                                                    (format #f "~s*" s))
                                                  (cons pattern more-patterns))))
      (map vector->list
           (sqlite-fold cons '() stmt)))))

(define (search-package pattern . more-patterns)
  "Return corresponding packages.
Search is performed over name, synopsis, description.
Packages or ordered by most relevant last.
Search is subject to SQLite \"full-text search\" pattern matching.
See https://www.sqlite.org/fts5.html."
  (let ((pattern (string-concatenate
                  (map (lambda (s)
                         (format #f "~s*" s))
                       (cons pattern more-patterns)))))
    (with-database %db db
      (with-statement
          db
          (string-append
           "SELECT name FROM Info WHERE Info MATCH :pattern ORDER BY RANK")
          stmt
        (sqlite-bind-arguments
         stmt
         #:pattern (format #f "name:~a OR synopsis:~a OR description:~a"
                           pattern pattern pattern))
        (map vector->list
             (sqlite-fold cons '() stmt))))))

(define (format-search search-result)
  (for-each
   (match-lambda
     ((subpath name version output)
      (format #t "~a:~a@~a~/~a~%"
              name output version subpath)))
   search-result))

(define (persist-all-local-packages)
  "Return number of persisted packages."
  (parameterize ((sql-schema %schema))
    (with-database %db db
      ;; It's important to persist all entries in a single transaction to
      ;; avoid a performance bottleneck.  See
      ;; https://www.sqlite.org/fts5.html.
      ((@@ (guix store database) call-with-transaction) ; TODO: Export?
       db
       (lambda ()
         (fold-packages
          (lambda (package count)
            (persist-package-files db package)
            (+ 1 count))
          1))))))


(define (test-missing-package)
  (package-store-info
   (@@ (gnu packages chromium) ungoogled-chromium)))

(define (test-index-git)
  (parameterize ((sql-schema %schema))
    (with-database %db db
      (persist-package-files db (@@ (gnu packages version-control) git)))))

(define (test-search)
  (test-index-git)
  (format-search (search-file-package "git perl5")))

;; TODO: Catch case we don't have a derivation.

;; TODO: Sync databases with substitute server: SQLite diffs?  Binary diff
;; with xdelta (probably not since it would send entries for Guix versions
;; that the user does not have).

;; Measures
;;
;; Context:
;; - 15005 packages
;; - 1796 store items
;; - CPU 3.5 GHz
;; - SSD
;;
;; Data with synopsis and description:
;; - Database generation time: 30 seconds.
;; - Database size: 37 MiB.
;; - Database Zstd-compressed size: 6.4 MiB.
;; - Zstd-compression time: 0.13 seconds.
;; - FTS queries: < 0.01 seconds.
;;
;; Data without synopsis and description:
;; - Database generation time: 30 seconds.
;; - Database size: 36 MiB.
;; - Database Zstd-compressed size: 6.0 MiB.
;; - Zstd-compression time: 0.13 seconds.
;; - FTS queries: < 0.01 seconds.
