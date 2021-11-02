type +'a node =
| Leaf of 'a
| App of 'a option * 'a t * 'a node Int.Map.t
| Abs of 'a option * 'a node

and +'a t = 'a node option

let empty = None

let is_empty = Option.is_none

let rec qualify_node (path: Path.t) node = match path with
| Here -> node
| Head path -> App (None, Option.some @@ qualify_node path node, Int.Map.empty)
| Spine (i, path) ->
  let spine_i = qualify_node path node in
  App (None, None, Int.Map.singleton i spine_i)
| Body path ->
  let body = qualify_node path node in
  Abs (None, body)

let singleton_node path v = qualify_node path (Leaf v)

let singleton path v = Some (singleton_node path v)

let rec goto (path: Path.t) data = match path with
| Here -> data
| Head path ->
  begin match data with
  | None | Some Leaf _ | Some Abs _ -> None
  | Some App (_, head, _) -> goto path head
  end
| Spine (i, path) ->
  begin match data with
  | None | Some Leaf _ | Some Abs _ -> None
  | Some App (_, _, spine) ->
    let v = Int.Map.find_opt i spine in
    goto path v
  end
| Body path ->
  begin match data with
  | None | Some Leaf _ | Some App _ -> None
  | Some Abs (_, body) -> goto path (Some body)
  end

let find_here_opt = function
| None -> None
| Some Leaf x -> Some x
| Some App (x, _, _) -> x
| Some Abs (x, _) -> x

let find_opt path data = goto path data |> find_here_opt

let find path data = match find_opt path data with
| None -> raise Not_found
| Some v -> v

let add_node_here v node = match node with
| Leaf v' -> if v == v' then node else Leaf v
| App (Some v', _, _) when v == v' -> node
| App (_, head, spine) -> App (Some v, head, spine)
| Abs (Some v', _) when v == v' -> node
| Abs (_, body) -> Abs (Some v, body)

let add_here v data = match data with
| None -> Some (Leaf v)
| Some node -> Some (add_node_here v node)

module type Node_setter = sig
  val set_node: Path.t -> 'a -> 'a node -> 'a node
end

module type Setter = sig
  val set: Path.t -> 'a -> 'a t -> 'a t
end

module Make_setter (M: Node_setter): Setter = struct
  let mismatch = M.set_node

  let rec set_node (path: Path.t) v node = match path, node with
  | Here, _ -> add_node_here v node
  | Head path, Leaf v' -> App (Some v', singleton path v, Int.Map.empty)
  | Head path, App (v', head, spine) -> App (v', set path v head, spine)
  | Head _, Abs _ -> mismatch path v node
  | Spine (i, path), Leaf v' ->
    let spine = Int.Map.singleton i (singleton_node path v) in
    App (Some v', None, spine)
  | Spine (i, path), App (v', head, spine) ->
    let spine = spine |> Int.Map.update i (set path v) in
    App (v', head, spine)
  | Spine _, Abs _ -> mismatch path v node
  | Body path, Leaf v' -> Abs (Some v', singleton_node path v)
  | Body path, Abs (v', body) -> Abs (v', set_node path v body)
  | Body _, App _ -> mismatch path v node

  and set path v = function
  | None -> singleton path v
  | Some node -> Some (set_node path v node)
end

module Add_setter = Make_setter (struct
  let set_node _ = invalid_arg "path is incompatible with pre-existing data"
end)

let add = Add_setter.set

module Replace_setter = Make_setter (struct
  let set_node path v _ = singleton_node path v
end)

let replace = Replace_setter.set

let remove_here = function
| None | Some Leaf _ -> None
| Some App (Some _, head, spine) -> Some (App (None, head, spine))
| Some App _ as x -> x
| Some Abs (Some _, body) -> Some (Abs (None, body))
| Some Abs _ as x -> x

let rec remove (path: Path.t) data = match path, data with
| _, None -> None
| Here, _ -> remove_here data
| Head _, Some (Leaf _ | Abs _) -> data
| Head path, Some App (v, head, spine) ->
  let head' = remove path head in
  if head == head' then data else
  Some (App (v, head', spine))
| Spine _, Some (Leaf _ | Abs _) -> data
| Spine (i, path), Some App (v, head, spine) ->
  let spine' = spine |> Int.Map.update i (remove path) in
  if spine == spine' then data else
  Some (App (v, head, spine'))
| Body _, Some (Leaf _ | App _) -> data
| Body path, Some Abs (v, body) ->
  begin match remove path (Some body), v with
  | None, None -> None
  | None, Some v -> Some (Leaf v)
  | Some body', _ ->
    if body == body' then data else
    Some (Abs (v, body'))
  end

let update_here f data = match f @@ find_here_opt data with
| None -> remove_here data
| Some v -> add_here v data

module type Modifier = sig
  val modify: Path.t -> ('a option -> 'a option) -> 'a t -> 'a t
end

module Make_modifier (M: Setter): Modifier = struct
  let set = M.set

  let rec modify (path: Path.t) f data = match path, data with
  | Here, _ -> update_here f data
  | _, None
  | _, Some Leaf _ ->
    begin match f None with
    | None -> data
    | Some v -> set path v data
    end
  | Head path, Some App (v, head, spine) ->
    let head' = modify path f head in
    if head == head' then data else
    Some (App (v, head', spine))
  | Spine (i, path), Some App (v, head, spine) ->
    let spine' = spine |> Int.Map.update i (modify path f) in
    if spine == spine' then data else Some (App (v, head, spine'))
  | Head _, Some Abs _
  | Spine _, Some Abs _ ->
    begin match f None with
    | None -> data
    | Some v -> set path v data
    end
  | Body path, Some Abs (v, body) ->
    begin match modify path f (Some body) with
    | None -> Option.map (fun v -> Leaf v) v
    | Some body' when body == body' -> data
    | Some body -> Some (Abs (v, body))
    end
  | Body _, Some App _ ->
    begin match f None with
    | None -> data
    | Some v -> set path v data
    end
end

module Update_modifier = Make_modifier (Add_setter)

let update = Update_modifier.modify

module Change_modifier = Make_modifier (Replace_setter)

let change = Change_modifier.modify
