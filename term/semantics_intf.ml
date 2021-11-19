(** Semantic terms, parameterized by atoms, annotations on abstractions, and
    tags on terms.

    This form has levels for both local and global variables, except under
    closures where local variables are indexed. This makes it very easy to
    move these terms about in a context since levels require no shifting when
    the context is extended or when performing substitutions. Additionally,
    the use of closures for abstractions means that substitutions can be done
    much more efficiently than would normally be possible. So this is a good
    form of syntax for performing reductions. To perform an 
    alpha-beta-equivalence check, the recommended method would be to apply
    the reduction functions in this form, then convert back to syntax and
    perform an alpha-equivalence check.
 *)
type (+'atom, +'ann, +'tag) t =
| Local of 'tag option * Local.level
| Global of 'tag option * Global.leveled
| Atom of 'tag option * 'atom
| Abs of 'tag option * ('atom, 'ann, 'tag) clos
| App of 'tag option * ('atom, 'ann, 'tag) t * ('atom, 'ann, 'tag) t list

and (+'atom, +'ann, +'tag) ctx = (('atom, 'ann, 'tag) t, ('atom, 'ann, 'tag) t, 'ann) Ctx.t

and (+'atom, +'ann, +'tag) clos =
  { ctx: ('atom, 'ann, 'tag) ctx
  ; bound: 'ann option list
  ; body: ('atom, 'ann, 'tag) Syntax.t
  }

type (+'atom, +'ann, +'tag) vars = (('atom, 'ann, 'tag) t, ('atom, 'ann, 'tag) t) Vars.t

module type S = sig
  type pool := Local.pool

  type atom

  type nonrec (+'ann, +'tag) t = (atom, 'ann, 'tag) t
  type nonrec (+'ann, +'tag) ctx = (atom, 'ann, 'tag) ctx
  type nonrec (+'ann, +'tag) clos = (atom, 'ann, 'tag) clos
  type nonrec (+'ann, +'tag) vars = (atom, 'ann, 'tag) vars

  (** [fix pool term] fixes the variables in [term] to produce a semantic term.
      Effectively, this simply uses [pool] to convert the indices of [term] to
      levels. *)
  val fix: pool -> (atom, 'ann, 'tag) Syntax.t -> ('ann, 'tag) t

  (** [replace vars term] performs a replacement of variables in [term] 
      according to the entries of [vars]. This function is capture-avoiding by
      virtue of the use of leveled variables, but it is a simple replacement
      operation. *)
  val replace: ('ann, 'tag) vars -> ('ann, 'tag) t -> ('ann, 'tag) t

  (** [spec] combines the functionality of [fix] and [replace] in one step. *)
  val spec: ('ann, 'tag) ctx -> (atom, 'ann, 'tag) Syntax.t -> ('ann, 'tag) t

  (** [gen] is the inverse of [fix], converting levels to indices in [term] to
      obtain a syntactic term. In addition, it performs any pending 
      substitutions due to previous calls to [replace]. *)
  val gen: pool -> ('ann, 'tag) t -> (atom, 'ann, 'tag) Syntax.t

  (** [beta_app f vs] is [Some reduct] if [f] applied to the arguments [vs] 
      reduces, and [None] if it does not. Note that this need not fully reduce
      the term, it will only reduce a single outer beta redex. *)
  val beta_app: ('ann, 'tag) t -> ('ann, 'tag) t list -> ('ann, 'tag) t option

  (** [beta term] is [Some reduct] if [term] is a beta redex, and [None] 
      otherwise. *)
  val beta: ('ann, 'tag) t -> ('ann, 'tag) t option

  (** [cbn term] reduces [term] to weak head normal form using the call-by-name
      evaluation strategy.  *)
  val cbn: ('ann, 'tag) t -> ('ann, 'tag) t

  (** [cbv term] reduces [term] to weak normal form using the call-by-value
      evaluation strategy. *)
  val cbv: ('ann, 'tag) t -> ('ann, 'tag) t

  (** [hsr term] reduces [term] to head normal form using the head-spine 
      reduction evaluation strategy. *)
  val hsr: ('ann, 'tag) t -> ('ann, 'tag) t

  (** [nor term] reduces [term] to normal form using the normal order evaluation
      strategy. This strategy is normalizing; i.e., if the term has a normal 
      form, then this evaluation strategy will terminate. *)
  val nor: ('ann, 'tag) t -> ('ann, 'tag) t

  (** [hao term] reduces [term] to normal form using the hybrid applicative order
      evaluation strategy. This strategy is not normalizing; i.e., for some
      terms which have normal forms, this strategy may loop forever. An example
      is the Ω ≡ (λx. x x)(λx. x x) fixpoint combinator. However, it is much
      faster than {!val:nor}, and may be used with fixpoint combinators designed
      for a strict evaluation strategy: e.g.,
      Y ≡ λh.(λx.λa.h (x x) a)(λx.λa.h (x x) a). *)
  val hao: ('ann, 'tag) t -> ('ann, 'tag) t

  (** [hno term] reduces [term] to normal form using the hybrid normal order
      evaluation strategy. This strategy is normalizing; i.e., if the term has
      a normal form, then this evaluation strategy will terminate. *)
  val hno: ('ann, 'tag) t -> ('ann, 'tag) t

  (** [apo term] reduces [term] to normal form using the applicative order
      evaluation strategy. This strategy is not normalizing; i.e., for some
      terms which have normal forms, this strategy may loop forever. In fact,
      even fixpoints designed for strict evaluation may fail to normalize.
      However, it is very efficient, and it may be useful if you know that your
      terms do not contain such fixpoints. For example, languages which perform
      totality checking can safely use this evaluation strategy. *)
  val apo: ('ann, 'tag) t -> ('ann, 'tag) t

  val delta: ('ann, 'tag) ctx -> ('ann, 'tag) t -> ('ann, 'tag) t option
end

module type Atom = sig
  type ('atom, 'ann, 'tag) tm := ('atom, 'ann, 'tag) t
  type t

  val beta_app: (module S with type atom = t) -> t -> (t, 'ann, 'tag) tm list -> (t, 'ann, 'tag) tm option

  val delta: (module S with type atom = t) -> (t, 'ann, 'tag) ctx -> t -> (t, 'ann, 'tag) tm option
end