module Index = Local.Index

type (+'atom, +'ann) t =
| Local of Local.index
| Global of Global.leveled
| Atom of 'atom
| Abs of 'ann option list * ('atom, 'ann) t
| App of ('atom, 'ann) t * ('atom, 'ann) t list

let rec fix_concrete vars : _ Concrete.t -> _ t = function
| Local id -> Local (Locals.index (Ident.equal id) (Vars.locals vars))
| Global x -> Global (Globals.fix x @@ Vars.globals vars)
| Atom a -> Atom a
| Abs ([], body) -> Abs ([], fix_concrete vars body) (* Avoids an allocation *)
| Abs (bound, body) ->
  let ids, bound = List.split bound in
  let body =
    vars
    |> Vars.update_locals (fun xs -> List.fold_left (Fun.flip Locals.add) xs ids)
    |> Fun.flip fix_concrete body
  in
  Abs (bound, body)
| App (f, vs) -> App (fix_concrete vars f, List.map (fix_concrete vars) vs)

let rec equal equal_atom x y = match x, y with
| Local i, Local j -> Index.equal i j
| Local _, _ -> false
| Global x, Global y -> Global.equal x y
| Global _, _ -> false
| Atom a, Atom b -> equal_atom a b
| Atom _, _ -> false
| Abs (xs, body1), Abs (ys, body2) ->
  List.compare_lengths xs ys = 0 &&
  equal equal_atom body1 body2
| Abs _, _ -> false
| App (f1, vs1), App (f2, vs2) ->
  equal equal_atom f1 f2 &&
  List.equal (equal equal_atom) vs1 vs2
| App _, _ -> false