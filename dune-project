(lang dune 3.14)
(name hotcrp)
(license LGPL-2.1-only)
(source (github smimram/ocaml-hotcrp))
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")
(generate_opam_files true)
(package
 (name hotcrp)
 (synopsis "Interface for using the HotCRP reviewing platform API")
 (description "This library provides functions to interact with a  HotCRP reviewing platform through its API. The library is compatible with lwt.")
 (depends
   cohttp-lwt-unix
   yojson
 )
)
