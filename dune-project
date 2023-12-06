(lang dune 3.0)

(generate_opam_files)

(source
 (github emillon/aoc2023))

(maintainers "Etienne Millon")

(package
 (name aoc2023)
 (synopsis "Advent of code 2023")
 (depends
  (angstrom
   (>= 0.16))
  (base
   (>= v0.16))
  (diet
   (>= 0.4))
  ppx_jane
  re
  stdio))
