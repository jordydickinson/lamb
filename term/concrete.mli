(** Concrete syntax, parameterized by atoms, annotations on abstractions, and
    tags on terms.

    This form of term has identifiers for local variables and indexed global
    variables. This makes it easy to construct terms relatively close to the
    source input.
  *)
type (+'atom, +'ann, +'tag) t =
| Local of 'tag option * Ident.t
| Global of 'tag option * Global.indexed
| Atom of 'tag option * 'atom
| Abs of 'tag option * (Ident.t * 'ann option) list * ('atom, 'ann, 'tag) t
| App of 'tag option * ('atom, 'ann, 'tag) t * ('atom, 'ann, 'tag) t list

(** [replace_tags tags term] replaces all tags in [term] with the contents of [tags]. *)
val replace_tags: 'tag Tags.t -> ('atom, 'ann, _) t -> ('atom, 'ann, 'tag) t
