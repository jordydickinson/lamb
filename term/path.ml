type t =
| Here
| Head of t
| Spine of int * t
| Body of t

let here = Here

let head path = Head path

let spine i path =
  if i < 0 then invalid_arg "negative spine offset";
  Spine (i, path)

let body path = Body path