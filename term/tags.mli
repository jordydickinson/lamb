type path := Path.t

(** Auxiliary data associated with term locations. *)
type +'a t

(** [empty] contains no bindings. *)
val empty: 'a t

(** [is_empty tags] is [true] if [tags] is empty and [false] otherwise. *)
val is_empty: 'a t -> bool

(** [singleton path v] associates the single binding of [v] at [path]. *)
val singleton: path -> 'a -> 'a t

(** [add path v tags] adds the binding of [v] to [path] in [tags]. If [path]
    was already bound, the old binding is replaced, or @raise Invalid_arg if
    the pre-existing bindings render [path] inaccessible. *)
val add: path -> 'a -> 'a t -> 'a t

(** [remove path v tags] is some [tags'] which has all the bindings of [tags]
    except that any accessible via [path] are removed. *)
val remove: path -> 'a t -> 'a t

(** [update path f tags] is some [tags'] which contains all the bindings of
    [tags] exception that the binding to [path] in [tags'] is updated according
    to the result of applying [f] to the corresponding binding [v : 'a option]
    in [tags]. In particular, if [f v] is [None], the binding is {!val:remove}d.
    Otherwise, if [f v] is [Some v'], then the binding of [v'] to [path] is
    {!val:add}ed. If [f v] is physically equal to [v], then [tags'] will be
    physically equal to [tags]. This function will @raise Invalid_arg under the
    same circumstances as {!val:add}. *)
val update: path -> ('a option -> 'a option) -> 'a t -> 'a t

(** [replace] is like {!val:add}, but rather than raising an exception any
    incompatible bindings are {!val:remove}d before the new bindings are
    {!val:add}ed. *)
val replace: path -> 'a -> 'a t -> 'a t

(** [change] is like {!val:update}, but rather than raising an exception any
    incompatible bindings are {!val:replace}d. *)
val change: path -> ('a option -> 'a option) -> 'a t -> 'a t

(** [goto path tags] is the substructure in [tags] in which all the bindings
    accesible from [path] exist. *)
val goto: path -> 'a t -> 'a t

(** [find path data] obtains the value of the binding located at [path] or
    @raise Not_found if no such binding exists. *)
val find: path -> 'a t -> 'a

(** [find_opt] is like {!val:find} but it returns [None] rather than raising an
    exception. *)
val find_opt: path -> 'a t -> 'a option