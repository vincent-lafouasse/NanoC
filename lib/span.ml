type t =
  { start : Position.t
  ; stop : Position.t
  }
[@@deriving show]

type 'a located = 'a * t [@@deriving show]

let len span = span.stop.absolute - span.start.absolute

let slice source span = String.sub source span.start.absolute (len span)
