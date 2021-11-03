type (+'atom, +'ann, +'tag) t =
| Local of 'tag option * Ident.t
| Global of 'tag option * Global.indexed
| Atom of 'tag option * 'atom
| Abs of 'tag option * (Ident.t * 'ann option) list * ('atom, 'ann, 'tag) t
| App of 'tag option * ('atom, 'ann, 'tag) t * ('atom, 'ann, 'tag) t list
