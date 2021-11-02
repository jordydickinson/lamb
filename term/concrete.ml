type (+'atom, +'ann) t =
| Local of Ident.t
| Global of Global.indexed
| Atom of 'atom
| Abs of (Ident.t * 'ann option) list * ('atom, 'ann) t
| App of ('atom, 'ann) t * ('atom, 'ann) t list
