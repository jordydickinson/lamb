module Index = Local.Index
module Level = Local.Level
module Pool = Local.Pool

include Semantics_intf

module Common = struct

  let next_neutral pool = Local (None, Pool.next_level pool)

  let add_neutral ?ann ctx =
    let v = next_neutral @@ Locals.pool @@ Ctx.locals ctx in
    Ctx.add ?data:ann v ctx

  let add_neutrals anns ctx =
    List.fold_left (fun ctx ann -> add_neutral ?ann ctx) ctx anns

  let neutral_ctx pool =
    let rec neutral_ctx' ctx =
      if Pool.equal pool (Locals.pool @@ Ctx.locals ctx)
      then ctx
      else neutral_ctx' (add_neutral ctx)
    in
    neutral_ctx' @@ Ctx.make Globals.empty

  let rec spec ctx : _ Syntax.t -> _ t = function
  | Local (_, i) -> Locals.ith i @@ Ctx.locals ctx
  | Global (_, x) -> Globals.find x @@ Ctx.globals ctx
  | Atom (tag, a) -> Atom (tag, a)
  | Abs (tag, bound, body) -> Abs (tag, { ctx; bound; body })
  | App (tag, f, vs) -> App (tag, spec ctx f, List.map (spec ctx) vs)

  let fix pool = spec @@ neutral_ctx pool

  let open_clos clos =
    let ctx = add_neutrals clos.bound clos.ctx in
    spec ctx clos.body, ctx

  let rec gen pool : _ t -> _ Syntax.t = function
  | Local (tag, l) -> Local (tag, Pool.index_of_level pool l)
  | Global (tag, x) -> Global (tag, x)
  | Atom (tag, a) -> Atom (tag, a)
  | Abs (tag, clos) ->
    let body, ctx = open_clos clos in
    Abs (tag, clos.bound, gen ctx.scope.pool body)
  | App (tag, f, vs) -> App (tag, gen pool f, List.map (gen pool) vs)

  let rec replace vars : _ t -> _ t = function
  | Local (_, l) as e ->
    begin match Locals.lth_opt l @@ Vars.locals vars with
    | None -> e
    | Some v -> v
    end
  | Global (_, x) as e ->
    begin match Globals.find_opt x @@ Vars.globals vars with
    | None -> e
    | Some v -> v
    end
  | Atom _ as e -> e
  | Abs (tag, clos) as e ->
    let ctx = Ctx.map_locals (replace vars) clos.ctx in
    if ctx.vars.locals == clos.ctx.vars.locals then e else
    Abs (tag, { clos with ctx })
  | App (tag, f, vs) as e ->
    let f' = replace vars f and vs' = List.map (replace vars) vs in
    if f == f' && vs == vs' then e else App (tag, f', vs')
end
include Common

module Make (Atom: Atom) = struct
  module rec T: S with type atom = Atom.t = struct
    include Common

    type atom = Atom.t

    type nonrec (+'ann, +'tag) t = (atom, 'ann, 'tag) t
    type nonrec (+'ann, +'tag) ctx = (atom, 'ann, 'tag) ctx
    type nonrec (+'ann, +'tag) clos = (atom, 'ann, 'tag) clos
    type nonrec (+'ann, +'tag) vars = (atom, 'ann, 'tag) vars

    let beta_app f vs = match f with
    | Abs (tag, { ctx; bound; body }) ->
      let bvs, rem = List.combine_rem bound vs in
      let ctx = List.fold_left (fun ctx (ann, v) -> Ctx.add ?data:ann v ctx) ctx bvs in
      begin match rem with
      | None -> Some (spec ctx body)
      | Some Left bound -> Some (Abs (tag, { ctx; bound; body }))
      | Some Right vs ->
        let f = spec ctx body in
        Some (App (tag, f, vs))
      end
    | Atom (_, a) -> Atom.beta_app (module T) a vs
    | _ -> None

    let beta = function
    | App (_, f, vs) -> beta_app f vs
    | _ -> None

    let under_clos f clos =
      let body, ctx = open_clos clos in
      let body = f body |> gen ctx.scope.pool in
      { clos with body }

    let rec cbn = function
    | Local _ | Global _ | Atom _ | Abs _ as e -> e
    | App (tag, f, vs) as e ->
      let f' = cbn f in
      begin match beta_app f' vs with
      | None when f == f' -> e
      | None -> App (tag, f', vs)
      | Some v -> cbn v
      end

    let rec cbv = function
    | Local _ | Global _ | Atom _ | Abs _ as e -> e
    | App (tag, f, vs) as e ->
      let f' = cbv f in
      let vs' = List.map cbv vs in
      begin match beta_app f vs with
      | None when f == f' && vs == vs' -> e
      | None -> App (tag, f', vs')
      | Some v -> cbv v
      end

    let rec hsr = function
    | Local _ | Global _ | Atom _ as e -> e
    | Abs (tag, clos) as e ->
      let clos' = under_clos hsr clos in
      if clos == clos' then e else Abs (tag, clos')
    | App (tag, f, vs) as e ->
      let f' = hsr f in
      begin match beta_app f' vs with
      | None when f == f' -> e
      | None -> App (tag, f', vs)
      | Some v -> hsr v
      end

    let rec nor = function
    | Local _ | Global _ | Atom _ as e -> e
    | Abs (tag, clos) as e ->
      let clos' = under_clos nor clos in
      if clos == clos' then e else Abs (tag, clos')
    | App (tag, f, vs) as e ->
      let f' = cbn f in
      begin match beta_app f' vs with
      | None ->
        let f' = nor f' and vs' = List.map nor vs in
        if f == f' && vs == vs' then e else
        App (tag, f', vs')
      | Some v -> nor v
      end

    let rec hao = function
    | Local _ | Global _ | Atom _ as e -> e
    | Abs (tag, clos) as e ->
      let clos' = under_clos hao clos in
      if clos == clos' then e else Abs (tag, clos')
    | App (tag, f, vs) as e ->
      let f' = cbv f and vs' = List.map hao vs in
      begin match beta_app f' vs' with
      | None ->
        let f' = hao f' in
        if f == f' && vs == vs' then e else
        App (tag, f', vs')
      | Some v -> hao v
      end

    let rec hno = function
    | Local _ | Global _ | Atom _ as e -> e
    | Abs (tag, clos) as e ->
      let clos' = under_clos hno clos in
      if clos == clos' then e else Abs (tag, clos')
    | App (tag, f, vs) as e ->
      let f' = hsr f in
      begin match beta_app f' vs with
      | None ->
        let f' = hno f' and vs' = List.map hno vs in
        if f == f' && vs == vs' then e else
        App (tag, f', vs')
      | Some v -> hno v
      end

    let rec apo = function
    | Local _ | Global _ | Atom _ as e -> e
    | Abs (tag, clos) as e ->
      let clos' = under_clos apo clos in
      if clos == clos' then e else Abs (tag, clos')
    | App (tag, f, vs) as e ->
      let f' = apo f in
      let vs' = List.map apo vs in
      begin match beta_app f' vs' with
      | None when f == f' && vs == vs' -> e
      | None -> App (tag, f', vs')
      | Some v -> apo v
      end

    let delta ctx = function
    | Global (_, x) -> Globals.find_opt x (Ctx.globals ctx)
    | Atom (_, a) -> Atom.delta (module T) ctx a
    | _ -> None
  end
  include T
end