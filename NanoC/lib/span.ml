type t =
  { start : Position.t
  ; stop : Position.t
  }
[@@deriving show]

type 'a located = 'a * t [@@deriving show]
