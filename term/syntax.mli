(** Syntactic terms, parameterized by atoms and annotations.

    Local variables are indexed, and global variables are leveled. This makes
    it easy to move these terms around in some ambient global context so long
    as they're closed. This is also a convenient form of syntax for performing
    alpha-equivalence checks.
  *)
type (+'atom, +'ann) t =
| Local of Local.index
| Global of Global.leveled
| Atom of 'atom
| Abs of 'ann option list * ('atom, 'ann) t
| App of ('atom, 'ann) t * ('atom, 'ann) t list

(** [fix_concrete vars term] fixes all the global variables in [term] to
  produce a syntactic term. No substitutions are performed and any tags in the
  term are removed. If the term changes shape for any reason, the paths of
  tags will need updating in the returned tags structure. *)
val fix_concrete: (Ident.t, _) Vars.t -> ('atom, 'ann, 'tag) Concrete.t -> ('atom, 'ann) t * 'tag Tags.t

(** [equal equal_atom a b] tests the terms [a] and [b] for alpha equivalence,
    using [equal_atom] to compare atoms. *)
val equal: ('atom -> 'atom -> bool) -> ('atom, 'ann) t -> ('atom, 'ann) t -> bool