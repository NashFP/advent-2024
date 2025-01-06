# Mark Wutka's OCaml solutions for Advent of Code 2024

This project uses the Dune build system (which can
be installed using the [Opam](https://opam.ocaml.org/)
package manager for OCaml). There are some
common utils in the lib directory, and the source code for
each day's problem is in the bin directory.
I put the data files in the data directory but do not check them
in here, and I tend to hard-code the filenames in the source
files, expecting them to be run from the repo directory.

If you are running them from Emacs using the Merlin-Utop
interface, you may want to check your working directory
with `Sys.getcwd ()` and if you find that it is in the `bin`
or `lib` directory, do `Sys.chdir ".."`

To run day1, for example, you can do:

```
dune exec day1
```
