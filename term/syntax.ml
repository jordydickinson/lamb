module Index = Local.Index

type (+'atom, +'ann, +'tag) t =
| Local of 'tag option * Local.index
| Global of 'tag option * Global.leveled
| Atom of 'tag option * 'atom
| Abs of 'tag option * 'ann option list * ('atom, 'ann, 'tag) t
| App of 'tag option * ('atom, 'ann, 'tag) t * ('atom, 'ann, 'tag) t list

let rec fix_concrete vars : _ Concrete.t -> _ = function
| Local (tag, id) -> Local (tag, Locals.index (Ident.equal id) (Vars.locals vars))
| Global (tag, x) -> Global (tag, Globals.fix x @@ Vars.globals vars)
| Atom (tag, a) -> Atom (tag, a)
| Abs (tag, [], body) -> Abs (tag, [], fix_concrete vars body) (* Avoids an allocation *)
| Abs (tag, bound, body) ->
  let ids, bound = List.split bound in
  let body =
    vars
    |> Vars.update_locals (fun xs -> List.fold_left (Fun.flip Locals.add) xs ids)
    |> Fun.flip fix_concrete body
  in
  Abs (tag, bound, body)
| App (tag, f, vs) ->
  let f = fix_concrete vars f in
  let vs = List.map (fix_concrete vars) vs in
  App (tag, f, vs)

let rec equal ?equal_ann equal_atom a b = match a, b with
| Local (_, i), Local (_, j) -> Local.Index.equal i j
| Local _, _ -> false
| Global (_, x), Global (_, y) -> Global.equal x y
| Global _, _ -> false
| Atom (_, a), Atom (_, b) -> equal_atom a b
| Atom _, _ -> false
| Abs (_, bound, body), Abs (_, bound', body') ->
  equal ?equal_ann equal_atom body body' &&
  begin match equal_ann with
  | None -> List.compare_lengths bound bound' = 0
  | Some equal_ann -> List.equal (Option.equal equal_ann) bound bound'
  end
| Abs _, _ -> false
| App (_, f, vs), App (_, f', vs') ->
  equal ?equal_ann equal_atom f f' &&
  List.equal (equal ?equal_ann equal_atom) vs vs'
| App _, _ -> false