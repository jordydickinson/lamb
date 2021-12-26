(** Syntactic terms, parameterized by atoms, annotations on abstractions, and
    tags on terms.

    Local variables are indexed, and global variables are leveled. This makes
    it easy to move these terms around in some ambient global context so long
    as they're closed. This is also a convenient form of syntax for performing
    alpha-equivalence checks.
  *)
type (+'atom, +'ann, +'tag) t =
| Local of 'tag option * Local.index
| Global of 'tag option * Global.leveled
| Atom of 'tag option * 'atom
| Abs of 'tag option * 'ann option list * ('atom, 'ann, 'tag) t
| App of 'tag option * ('atom, 'ann, 'tag) t * ('atom, 'ann, 'tag) t list

(** [fix_concrete vars term] fixes all the global variables in [term] to
  produce a syntactic term. No substitutions are performed and any tags in the
  term are removed. If the term changes shape for any reason, the paths of
  tags will need updating in the returned tags structure. *)
val fix_concrete: (Ident.t, _) Vars.t -> ('atom, 'ann, 'tag) Concrete.t -> ('atom, 'ann, 'tag) t

val equal: ?equal_ann:('ann -> 'ann -> bool)
  -> ('atom -> 'atom -> bool)
  -> ('atom, 'ann, 'tag) t -> ('atom, 'ann, 'tag) t
  -> bool