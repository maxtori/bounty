(lang dune 3.24)
(generate_opam_files true)
(name bounty)
(source (github maxtori/bounty))
(license MIT)
(authors "maxtori")
(maintainers "maxtori")
(homepage https://github.com/maxtori/bounty)
(bug_reports https://github.com/maxtori/bounty/issues)
(documentation https://github.com/maxtori/bounty)

(package
 (name peerJS)
 (synopsis "PeerJS binding")
 (depends ezjs_min (ppx_deriving_jsoo :build)))

(package
 (name sync)
 (synopsis "Sync")
 (depends ezjs_min (ppx_deriving_jsoo :build))
 (depopts ezjs_idb peerJS))

(package
 (name bounty)
 (synopsis "bounty")
 (depends
  ez_api ezjs_idb peerJS sync vue-ppx
  (ppx_deriving_encoding :build)
  (ppx_deriving_jsoo :build)
  (conf-npm :build)))
