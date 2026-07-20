type t =
  { absolute : int
  ; line : int
  ; column : int
  }
[@@deriving show]

type 'a located = 'a * t [@@deriving show]

let distance pos_from pos_to = pos_to.absolute - pos_from.absolute

let newline pos = { absolute = pos.absolute + 1; line = pos.line + 1; column = 0 }
let advance pos = { pos with absolute = pos.absolute + 1; column = pos.column + 1 }
