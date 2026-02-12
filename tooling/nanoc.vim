if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword nanocKeyword fn var const return if else struct while break continue goto syscall
syn keyword nanocType u8 i32 u32 ptr

" Constants
syn keyword nanocConstant true false

" Operators
syn match nanocOperator '\v\+|\-|\*|/|\%|\&|\||\^|\~|\!|\<|\>|\='
syn match nanocOperator '\v\&\&|\|\||\<\<|\>\>|\<\=|\>\=|[\=]{2}|\!\=|\-\>'

" Numbers
syn match nanocNumber "\v<\d+>"
syn match nanocNumber "\v<0x[0-9a-fA-F]+>"
syn match nanocNumber "\v<0b[01]+>"

" Character literals
syn region nanocChar start="'" end="'" skip="\\." contains=nanocEscape

" String literals
syn region nanocString start='"' end='"' skip='\\.' contains=nanocEscape

" Escape sequences
syn match nanocEscape contained "\v\\[nrt0\\'\"]"
syn match nanocEscape contained "\v\\x[0-9a-fA-F]{2}"

" Comments
syn region nanocComment start="//" end="$"
syn region nanocComment start="/\*" end="\*/"

" Function definitions
syn match nanocFunction "\v<fn\s+\zs\w+\ze\s*\("

" Highlighting links
hi def link nanocKeyword Keyword
hi def link nanocType Type
hi def link nanocConstant Constant
hi def link nanocNumber Number
hi def link nanocChar Character
hi def link nanocString String
hi def link nanocEscape SpecialChar
hi def link nanocComment Comment
hi def link nanocFunction Function
hi def link nanocOperator Operator

let b:current_syntax = "nanoc"
