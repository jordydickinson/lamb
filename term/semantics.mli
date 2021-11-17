type pool := Local.pool

include module type of struct include Semantics_intf end

(** [fix pool term] fixes the variables in [term] to produce a semantic term.
    Effectively, this simply uses [pool] to convert the indices of [term] to
    levels. *)
val fix: pool -> ('atom, 'ann) Syntax.t -> ('atom, 'ann) t

(** [replace vars term] performs a replacement of variables in [term] 
    according to the entries of [vars]. This function is capture-avoiding by
    virtue of the use of leveled variables, but it is a simple replacement
    operation. *)
val replace: ('atom, 'ann) vars -> ('atom, 'ann) t -> ('atom, 'ann) t

(** [spec] combines the functionality of [fix] and [replace] in one step. *)
val spec: ('atom, 'ann) ctx -> ('atom, 'ann) Syntax.t -> ('atom, 'ann) t

(** [gen] is the inverse of [fix], converting levels to indices in [term] to
    obtain a syntactic term. In addition, it performs any pending 
    substitutions due to previous calls to [replace]. *)
val gen: pool -> ('atom, 'ann) t -> ('atom, 'ann) Syntax.t

module Make (Atom: Atom): S with type atom = Atom.t