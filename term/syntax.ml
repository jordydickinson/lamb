module Index = Local.Index

include Syntax_intf

module Common = struct
  let rec fix_concrete vars : _ Concrete.t -> _ = function
  | Local (tag, id) ->
    let term = Local (Locals.index (Ident.equal id) (Vars.locals vars)) in
    let tags = match tag with
    | None -> Tags.empty
    | Some tag -> Tags.singleton Path.here tag
    in
    term, tags
  | Global (tag, x) ->
    let term = Global (Globals.fix x @@ Vars.globals vars) in
    let tags = match tag with
    | None -> Tags.empty
    | Some tag -> Tags.singleton Path.here tag
    in
    term, tags
  | Atom (tag, a) ->
    let term = Atom a in
    let tags = match tag with
    | None -> Tags.empty
    | Some tag -> Tags.singleton Path.here tag
    in
    term, tags
  | Abs (tag, [], body) -> (* Avoids an allocation *)
    let body, body_tags = fix_concrete vars body in
    let term = Abs ([], body) in
    let tags = Tags.abs ?tag body_tags in
    term, tags
  | Abs (tag, bound, body) ->
    let ids, bound = List.split bound in
    let body, body_tags =
      vars
      |> Vars.update_locals (fun xs -> List.fold_left (Fun.flip Locals.add) xs ids)
      |> Fun.flip fix_concrete body
    in
    let term = Abs (bound, body) in
    let tags = Tags.abs ?tag body_tags in
    term, tags
  | App (tag, f, vs) ->
    let f, f_tags = fix_concrete vars f in
    let vs, vs_tags = List.map (fix_concrete vars) vs |> List.split in
    let term = App (f, vs) in
    let tags = Tags.app ?tag f_tags vs_tags in
    term, tags
end
include Common

module Make (Atom: Atom) = struct
  module rec T: S with type atom = Atom.t = struct
    include Common
    
    type atom = Atom.t
    type nonrec +'ann t = (atom, 'ann) t

    let rec equal x y = match x, y with
    | Local i, Local j -> Index.equal i j
    | Local _, _ -> false
    | Global x, Global y -> Global.equal x y
    | Global _, _ -> false
    | Atom a, Atom b -> Atom.equal (module T) a b
    | Atom _, _ -> false
    | Abs (xs, body1), Abs (ys, body2) ->
      List.compare_lengths xs ys = 0 && equal body1 body2
    | Abs _, _ -> false
    | App (f1, vs1), App (f2, vs2) ->
      equal f1 f2 &&
      List.equal equal vs1 vs2
    | App _, _ -> false
  end
  include T
end