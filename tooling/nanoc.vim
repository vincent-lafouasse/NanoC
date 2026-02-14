if exists("b:current_syntax")
  finish
endif

syn case match

" Keywords
syn keyword nanocKeyword var const return if else while break continue syscall

syn keyword nanocPrimitiveType u8 i32 u32 ptr

syn keyword nanocConstant true false

syn match nanocOperator '\v\+|\-|\*|/|\%|\&|\||\^|\~|\!|\<|\>|\='
syn match nanocOperator '\v\&\&|\|\||\<\<|\>\>|\<\=|\>\=|[\=]{2}|\!\=|\-\>'

syn keyword nanocKeyword goto nextgroup=nanocLabelTarget skipwhite skipnl
syn match nanocLabelTarget "\w\+" contained skipwhite skipnl
syn match nanocLabel "\v^\s*\zs\w+\ze\s*:\s*$"

" Numbers (with optional 'u' suffix)
syn match nanocNumber "\v<\d+u?>"
syn match nanocNumber "\v<0x[0-9a-fA-F]+u?>"
syn match nanocNumber "\v<0b[01]+u?>"

" literals
syn region nanocChar start="'" end="'" skip="\\." contains=nanocEscape
syn region nanocString start='"' end='"' skip='\\.' contains=nanocEscape

" escape sequences, namely \n \r \t \0 \\ \' \"
syn match nanocEscape contained "\v\\[nrt0\\'\"]"
" hex escape sequences, e.g. \x67
syn match nanocEscape contained "\v\\x[0-9a-fA-F]{2}"

syn match nanocDelimiter "\v[\(\)\{\}\[\]]"

syn region nanocComment start="//" end="$" contains=nanocTodo
syn region nanocComment start="/\*" end="\*/" contains=nanocTodo
syn keyword nanocTodo contained TODO FIXME XXX BUG NOTE

" fn keyword
syn keyword nanocKeyword fn nextgroup=nanocFunction skipwhite skipnl
" function declaration
syn match nanocFunction "\w\+" contained nextgroup=nanocFuncParams skipwhite skipnl
" function call
syn match nanocFunction "\v\w+\ze\s*\("

syn keyword nanocKeyword struct nextgroup=nanocStructName skipwhite skipnl
syn match nanocStructName "\w\+" contained

" the stuff after the :
syn match nanocTypeName "\v(:\s*)@<=\w+"

" pointer stars are operator-colored
syn match nanocPointerOperator "\v(\w+)@<=\*+" contains=NONE

" field access (struct->field or struct.field)
syn match nanocField "\v(\-\>|\.)@<=\w+"

" the stuff before the :
syn match nanocParameter "\v\w+\ze\s*:" contained

syn region nanocFuncParams start="(" end=")" contained transparent contains=nanocParameter,nanocTypeName,nanocPrimitiveType,nanocOperator,nanocPointerOperator,nanocDelimiter,nanocComment

hi def link nanocKeyword Keyword
hi def link nanocPrimitiveType Keyword
hi def link nanocConstant Constant
hi def link nanocNumber Number
hi def link nanocChar Character
hi def link nanocString String
hi def link nanocEscape SpecialChar
hi def link nanocComment Comment
hi def link nanocTodo Todo
hi def link nanocOperator Operator
hi def link nanocDelimiter Delimiter
hi def link nanocLabelTarget Label
hi def link nanocLabel Label

hi def link nanocFunction Function
hi def link nanocStructName Type
hi def link nanocTypeName Type
hi def link nanocPointerOperator Operator

let b:current_syntax = "nanoc"
