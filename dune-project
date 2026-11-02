(lang dune 3.0)
(name steiner)
(generate_opam_files true)

(package
 (name steiner)
 (synopsis "A language model's next-token choices as worldlines: a tree you grow and branch from any token")
 (depends
  (ocaml (>= 5.1))
  dune
  eio_main
  cohttp-eio
  tls-eio
  ca-certs
  mirage-crypto-rng
  yojson
  base64
  uri))
