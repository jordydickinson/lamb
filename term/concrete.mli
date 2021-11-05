(** Concrete syntax, parameterized by atoms, annotations on abstractions, and tags on terms. *)
type (+'atom, +'ann, +'tag) t =
| Local of 'tag option * Ident.t
| Global of 'tag option * Global.indexed
| Atom of 'tag option * 'atom
| Abs of 'tag option * (Ident.t * 'ann option) list * ('atom, 'ann, 'tag) t
| App of 'tag option * ('atom, 'ann, 'tag) t * ('atom, 'ann, 'tag) t list

(** Untagged concrete syntax. *)
type (+'atom, +'ann) untagged = ('atom, 'ann, void) t

(** [replace_tags tags term] replaces all tags in [term] with the contents of [tags]. *)
val replace_tags: 'tag Tags.t -> ('atom, 'ann, _) t -> ('atom, 'ann, 'tag) t

(** [untag term] is [term] with all tags replaced with [None]. *)
val untag: ('atom, 'ann, _) t -> ('atom, 'ann) untagged