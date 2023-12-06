(lang dune 3.0)

(generate_opam_files)

(source
 (github emillon/aoc2023))

(maintainers "Etienne Millon")

(package
 (name aoc2023)
 (synopsis "Advent of code 2023")
 (depends
  angstrom
  (base
   (>= v0.16))
  diet
  ppx_jane
  re
  stdio))
