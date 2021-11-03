type (+'atom, +'ann, +'tag) t =
| Local of 'tag option * Ident.t
| Global of 'tag option * Global.indexed
| Atom of 'tag option * 'atom
| Abs of 'tag option * (Ident.t * 'ann option) list * ('atom, 'ann, 'tag) t
| App of 'tag option * ('atom, 'ann, 'tag) t * ('atom, 'ann, 'tag) t list

type (+'atom, +'ann) untagged = ('atom, 'ann, void) t

let rec replace_tags tags = function
| Local (_, id) -> Local (Tags.find_opt Path.here tags, id)
| Global (_, x) -> Global (Tags.find_opt Path.here tags, x)
| Atom (_, a) -> Atom (Tags.find_opt Path.here tags, a)
| Abs (_, xs, body) ->
  let body = replace_tags (Tags.goto Path.(body here) tags) body in
  Abs (Tags.find_opt Path.here tags, xs, body)
| App (_, f, vs) ->
  let f = replace_tags (Tags.goto Path.(head here) tags) f in
  let vs = vs
  |> List.mapi (fun i v -> replace_tags (Tags.goto Path.(spine i here) tags) v)
  in
  App (Tags.find_opt Path.here tags, f, vs)

let rec untag : _ -> _ untagged = function
| Local (_, id) -> Local (None, id)
| Global (_, x) -> Global (None, x)
| Atom (_, a) -> Atom (None, a)
| Abs (_, xs, body) -> Abs (None, xs, untag body)
| App (_, f, vs) -> App (None, untag f, List.map untag vs)
