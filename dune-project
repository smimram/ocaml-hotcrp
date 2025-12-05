(lang dune 3.14)
(source (github smimram/ocaml-hotcrp))
(name hotcrp)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")
(generate_opam_files true)
(package
 (name hotcrp)
 (depends
   cohttp-lwt-unix
   yojson
 )
)
