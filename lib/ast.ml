module BinaryOp = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | BitAnd
    | BitOr
    | BitXor
    | Lshift
    | Rshift
    | Eq
    | Neq
    | Lt
    | Le
    | Gt
    | Ge
    | LogAnd
    | LogOr
  [@@deriving show]
end

module UnaryOp = struct
  type t =
    | Negate
    | LogNot
    | BitNot
    | AddrOf
    | Deref
  [@@deriving show]
end

type literal =
  | IntLiteral of int64
  | UnsignedIntLiteral of int64
  | ByteLiteral of int64
  | PtrLiteral of int64
  | StringLiteral of string
  | CharLiteral of char
[@@deriving show]

type expr =
  | Literal of literal
  | Identifier of string
  | Binary of
      { op : BinaryOp.t
      ; lhs : expr
      ; rhs : expr
      }
  | Unary of
      { op : UnaryOp.t
      ; operand : expr
      }
  | Syscall of expr list
  | Grouping of expr
  | Call of
      { callee : expr
      ; args : expr list
      }
  | DotAccess of
      { target : expr
      ; field : string
      }
  | ArrowAccess of
      { target : expr
      ; field : string
      }
  | Index of
      { target : expr
      ; index : expr
      }
[@@deriving show]

let rec s_expr (e : expr) : string =
  let s_expr_char (c : char) : string =
    let code = Char.code c in
    if code >= 0x21 && code <= 0x7E
    then Printf.sprintf "'%c'" c
    else Printf.sprintf "'\\x%02X'" code
  in
  let bin_op_repr = function
    | BinaryOp.Add -> "+"
    | BinaryOp.Sub -> "-"
    | BinaryOp.Mul -> "*"
    | BinaryOp.Div -> "/"
    | BinaryOp.Mod -> "%"
    | BinaryOp.BitAnd -> "&"
    | BinaryOp.BitOr -> "|"
    | BinaryOp.BitXor -> "^"
    | BinaryOp.Lshift -> "<<"
    | BinaryOp.Rshift -> ">>"
    | BinaryOp.Eq -> "=="
    | BinaryOp.Neq -> "!="
    | BinaryOp.Lt -> "<"
    | BinaryOp.Le -> "<="
    | BinaryOp.Gt -> ">"
    | BinaryOp.Ge -> ">="
    | BinaryOp.LogAnd -> "&&"
    | BinaryOp.LogOr -> "||"
  in
  let unary_op_repr = function
    | UnaryOp.Negate -> "-"
    | UnaryOp.LogNot -> "!"
    | UnaryOp.BitNot -> "~"
    | UnaryOp.AddrOf -> "&"
    | UnaryOp.Deref -> "*"
  in
  match e with
  | Literal (IntLiteral value) -> Int64.to_string value ^ "i32"
  | Literal (UnsignedIntLiteral value) -> Int64.to_string value ^ "u32"
  | Literal (ByteLiteral value) -> Int64.to_string value ^ "u8"
  | Literal (PtrLiteral value) -> Int64.to_string value ^ "ptr"
  | Literal (StringLiteral s) -> "\"" ^ s ^ "\""
  | Literal (CharLiteral c) -> s_expr_char c
  | Identifier id -> id
  | Binary { op; lhs; rhs } ->
    "(" ^ bin_op_repr op ^ " " ^ s_expr lhs ^ " " ^ s_expr rhs ^ ")"
  | Unary { op; operand } -> "(" ^ unary_op_repr op ^ " " ^ s_expr operand ^ ")"
  | Syscall args -> "(syscall " ^ String.concat " " (List.map s_expr args) ^ ")"
  | Grouping expr -> "(group " ^ s_expr expr ^ ")"
  | Call { callee; args } ->
    let parts = "call" :: s_expr callee :: List.map s_expr args in
    "(" ^ String.concat " " parts ^ ")"
  | DotAccess { target; field } -> "(. " ^ s_expr target ^ " " ^ field ^ ")"
  | ArrowAccess { target; field } -> "(-> " ^ s_expr target ^ " " ^ field ^ ")"
  | Index { target; index } -> "([] " ^ s_expr target ^ " " ^ s_expr index ^ ")"
;;

type primitive_type =
  | U8
  | U32
  | I32
  | Ptr

type type_ =
  | RegisterSized of register_sized_type
  | Struct of string

and register_sized_type =
  | PrimitiveType of primitive_type
  | Pointer of type_

type field =
  { ty : type_
  ; name : string
  }

type struct_ =
  { name : string
  ; fields : field list
  }

type var_initializer =
  | Initializer of expr
  | Undefined
  | Zeroed

type var_decl =
  { ty : type_
  ; name : string
  ; init : var_initializer
  }

type function_arg =
  { ty : register_sized_type
  ; name : string
  }

type statement =
  | Expr of expr
  | Assign of
      { lvalue : expr
      ; rvalue : expr
      }
  | Block of statement list
  | If of
      { if_clause : statement
      ; then_clause : statement option
      }
  | While of
      { cond : expr
      ; body : statement list
      }
  | Labeled of
      { label : string
      ; stmt : statement
      }
  | Return of expr option

type function_ =
  { name : string
  ; args : function_arg list
  ; return_type : register_sized_type
  ; statements : statement list
  }

type top_level_item =
  | GlobalVar of var_decl
  | StructDecl of struct_
  | FunctionDef of function_

type program = top_level_item list
