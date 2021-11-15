(lang dune 2.8)
(name lamb)
(generate_opam_files true)

(package
 (name lamb)
 (synopsis "A library for terms in lambda-calculus-based languages")
 (depends
  (basis (and (>= 0.1.0) (< 0.2)))
  (context (and (>= 0.1.0) (< 0.2))))
 (license LGPL-3.0)
 (maintainers "Jordy Dickinson <jordy.dickinson@icloud.com>")
 (authors "Jordy Dickinson <jordy.dickinson@icloud.com>")
 (homepage https://github.com/jordydickinson/lamb)
 (bug_reports https://github.com/jordydickinson/lamb/issues))