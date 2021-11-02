module Index = Local.Index
module Level = Local.Level
module Pool = Local.Pool

type index = Local.index
type level = Local.level
type pool = Local.pool

type (+'atom, +'ann) t =
| Local of Local.level
| Global of Global.leveled
| Atom of 'atom
| Abs of ('atom, 'ann) clos
| App of ('atom, 'ann) t * ('atom, 'ann) t list


and (+'atom, +'ann) ctx = (('atom, 'ann) t, ('atom, 'ann) t, 'ann) Ctx.t

and (+'atom, +'ann) clos =
  { ctx: ('atom, 'ann) ctx
  ; bound: 'ann option list
  ; body: ('atom, 'ann) Syntax.t
  }

let rec spec ctx : _ Syntax.t -> _ t = function
| Local i -> Locals.ith i @@ Ctx.locals ctx
| Global x -> Globals.find x @@ Ctx.globals ctx
| Atom a -> Atom a
| Abs (bound, body) -> Abs { ctx; bound; body }
| App (f, vs) -> App (spec ctx f, List.map (spec ctx) vs)

let next_neutral pool = Local (Pool.next_level pool)

let add_neutral ?ann ctx =
  let v = next_neutral @@ Locals.pool @@ Ctx.locals ctx in
  Ctx.add ?data:ann v ctx

let add_neutrals anns ctx =
  List.fold_left (fun ctx ann -> add_neutral ?ann ctx) ctx anns

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