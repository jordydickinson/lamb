include module type of struct include Syntax_intf end

(** [fix_concrete vars term] fixes all the global variables in [term] to
  produce a syntactic term. No substitutions are performed and any tags in the
  term are removed. If the term changes shape for any reason, the paths of
  tags will need updating in the returned tags structure. *)
val fix_concrete: (Ident.t, _) Vars.t -> ('atom, 'ann, 'tag) Concrete.t -> ('atom, 'ann, 'tag) t

module Make (Atom: Atom): S with type atom := Atom.t