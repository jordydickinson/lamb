module Index = Local.Index
module Level = Local.Level
module Pool = Local.Pool

include Semantics_intf

module Common = struct

  let next_neutral pool = Local (Pool.next_level pool)

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
  | Local i -> Locals.ith i @@ Ctx.locals ctx
  | Global x -> Globals.find x @@ Ctx.globals ctx
  | Atom a -> Atom a
  | Abs (bound, body) -> Abs { ctx; bound; body }
  | App (f, vs) -> App (spec ctx f, List.map (spec ctx) vs)

  let fix pool = spec @@ neutral_ctx pool

  let open_clos clos =
    let ctx = add_neutrals clos.bound clos.ctx in
    spec ctx clos.body, ctx

  let rec gen pool : _ t -> _ Syntax.t = function
  | Local l -> Local (Pool.index_of_level pool l)
  | Global x -> Global x
  | Atom a -> Atom a
  | Abs clos ->
    let body, ctx = open_clos clos in
    Abs (clos.bound, gen ctx.scope.pool body)
  | App (f, vs) -> App (gen pool f, List.map (gen pool) vs)

  let rec replace vars : _ t -> _ t = function
  | Local l as e ->
    begin match Locals.lth_opt l @@ Vars.locals vars with
    | None -> e
    | Some v -> v
    end
  | Global x as e ->
    begin match Globals.find_opt x @@ Vars.globals vars with
    | None -> e
    | Some v -> v
    end
  | Atom _ as e -> e
  | Abs clos as e ->
    let ctx = Ctx.map_locals (replace vars) clos.ctx in
    if ctx.vars.locals == clos.ctx.vars.locals then e else
    Abs { clos with ctx }
  | App (f, vs) as e ->
    let f' = replace vars f and vs' = List.map (replace vars) vs in
    if f == f' && vs == vs' then e else App (f', vs')
end
include Common

module Make (Atom: Atom) = struct
  include Common

  type atom = Atom.t

  type nonrec +'ann t = (atom, 'ann) t
  type nonrec +'ann ctx = (atom, 'ann) ctx
  type nonrec +'ann clos = (atom, 'ann) clos
  type nonrec +'ann vars = (atom, 'ann) vars

  let beta_app f vs = match f with
  | Abs { ctx; bound; body } ->
    let bvs, rem = List.combine_rem bound vs in
    let ctx = List.fold_left (fun ctx (ann, v) -> Ctx.add ?data:ann v ctx) ctx bvs in
    begin match rem with
    | None -> Some (spec ctx body)
    | Some Left bound -> Some (Abs { ctx; bound; body })
    | Some Right vs ->
      let f = spec ctx body in
      Some (App (f, vs))
    end
  | Atom a -> Atom.beta_app a vs
  | _ -> None

  let beta = function
  | App (f, vs) -> beta_app f vs
  | _ -> None

  let under_clos f clos =
    let body, ctx = open_clos clos in
    let body = f body |> gen ctx.scope.pool in
    { clos with body }

  let rec cbn = function
  | Local _ | Global _ | Atom _ | Abs _ as e -> e
  | App (f, vs) as e ->
    let f' = cbn f in
    begin match beta_app f' vs with
    | None when f == f' -> e
    | None -> App (f', vs)
    | Some v -> cbn v
    end

  let rec cbv = function
  | Local _ | Global _ | Atom _ | Abs _ as e -> e
  | App (f, vs) as e ->
    let f' = cbv f in
    let vs' = List.map cbv vs in
    begin match beta_app f vs with
    | None when f == f' && vs == vs' -> e
    | None -> App (f', vs')
    | Some v -> cbv v
    end

  let rec hsr = function
  | Local _ | Global _ | Atom _ as e -> e
  | Abs clos as e ->
    let clos' = under_clos hsr clos in
    if clos == clos' then e else Abs clos'
  | App (f, vs) as e ->
    let f' = hsr f in
    begin match beta_app f' vs with
    | None when f == f' -> e
    | None -> App (f', vs)
    | Some v -> hsr v
    end

  let rec nor = function
  | Local _ | Global _ | Atom _ as e -> e
  | Abs clos as e ->
    let clos' = under_clos nor clos in
    if clos == clos' then e else Abs clos'
  | App (f, vs) as e ->
    let f' = cbn f in
    begin match beta_app f' vs with
    | None ->
      let f' = nor f' and vs' = List.map nor vs in
      if f == f' && vs == vs' then e else
      App (f', vs')
    | Some v -> nor v
    end

  let rec hao = function
  | Local _ | Global _ | Atom _ as e -> e
  | Abs clos as e ->
    let clos' = under_clos hao clos in
    if clos == clos' then e else Abs clos'
  | App (f, vs) as e ->
    let f' = cbv f and vs' = List.map hao vs in
    begin match beta_app f' vs' with
    | None ->
      let f' = hao f' in
      if f == f' && vs == vs' then e else
      App (f', vs')
    | Some v -> hao v
    end

  let rec hno = function
  | Local _ | Global _ | Atom _ as e -> e
  | Abs clos as e ->
    let clos' = under_clos hno clos in
    if clos == clos' then e else Abs clos'
  | App (f, vs) as e ->
    let f' = hsr f in
    begin match beta_app f' vs with
    | None ->
      let f' = hno f' and vs' = List.map hno vs in
      if f == f' && vs == vs' then e else
      App (f', vs')
    | Some v -> hno v
    end

  let rec apo = function
  | Local _ | Global _ | Atom _ as e -> e
  | Abs clos as e ->
    let clos' = under_clos apo clos in
    if clos == clos' then e else Abs clos'
  | App (f, vs) as e ->
    let f' = apo f in
    let vs' = List.map apo vs in
    begin match beta_app f' vs' with
    | None when f == f' && vs == vs' -> e
    | None -> App (f', vs')
    | Some v -> apo v
    end
end