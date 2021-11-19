module Index = Local.Index

include Syntax_intf

module Common = struct
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
end
include Common

module Make (Atom: Atom) = struct
  module rec T: S with type atom = Atom.t = struct
    include Common
    
    type atom = Atom.t
    type nonrec (+'ann, +'tag) t = (atom, 'ann, 'tag) t

    let rec equal x y = match x, y with
    | Local (_, i), Local (_, j) -> Index.equal i j
    | Local _, _ -> false
    | Global (_, x), Global (_, y) -> Global.equal x y
    | Global _, _ -> false
    | Atom (_, a), Atom (_, b) -> Atom.equal (module T) a b
    | Atom _, _ -> false
    | Abs (_, xs, body1), Abs (_, ys, body2) ->
      List.compare_lengths xs ys = 0 && equal body1 body2
    | Abs _, _ -> false
    | App (_, f1, vs1), App (_, f2, vs2) ->
      equal f1 f2 &&
      List.equal equal vs1 vs2
    | App _, _ -> false
  end
  include T
end