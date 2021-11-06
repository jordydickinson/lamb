(** Concrete syntax, parameterized by atoms and annotations on abstractions.

    This form of term has identifiers for local variables and indexed global
    variables. This makes it easy to construct terms relatively close to the
    source input.
  *)
type (+'atom, +'ann) t =
| Local of Ident.t
| Global of Global.indexed
| Atom of 'atom
| Abs of (Ident.t * 'ann option) list * ('atom, 'ann) t
| App of ('atom, 'ann) t * ('atom, 'ann) t list