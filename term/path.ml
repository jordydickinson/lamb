type t =
| Here
| Head of t
| Spine of int * t
| Body of t

type quotient =
  { whole: t
  ; frac: (t * t) option
  }
let here = Here

let head path = Head path

let spine i path =
  if i < 0 then invalid_arg "negative spine offset";
  Spine (i, path)

let body path = Body path

let rec add prefix path = match prefix with
| Here -> path
| Head prefix -> Head (add prefix path)
| Spine (i, prefix) -> Spine (i, add prefix path)
| Body prefix -> Body (add prefix path)

let rec div numer denom = match numer, denom with
| Here, Here -> { whole = Here; frac = None }
| Here, _ -> { whole = Here; frac = Some (numer, denom) }
| Head numer, Head denom ->
  let q = div numer denom in
  { q with whole = add (Head Here) q.whole }
| Head _, _ -> { whole = Here; frac = Some (numer, denom) }
| Spine (i, numer), Spine (i', denom) when Int.equal i i' ->
  let q = div numer denom in
  { q with whole = add (Spine (i, Here)) q.whole }
| Spine _, _ -> { whole = Here; frac = Some (numer, denom) }
| Body numer, Body denom ->
  let q = div numer denom in
  { q with whole = add (Body Here) q.whole }
| Body _, _ -> { whole = Here; frac = Some (numer, denom) }