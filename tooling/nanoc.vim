if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword nanocKeyword var const return if else struct while break continue syscall
syn keyword nanocKeyword fn nextgroup=nanocFunction skipwhite
syn keyword nanocKeyword goto nextgroup=nanocLabelTarget skipwhite
syn keyword nanocPrimitiveType u8 i32 u32 ptr

" Constants
syn keyword nanocConstant true false

" Operators
syn match nanocOperator '\v\+|\-|\*|/|\%|\&|\||\^|\~|\!|\<|\>|\='
syn match nanocOperator '\v\&\&|\|\||\<\<|\>\>|\<\=|\>\=|[\=]{2}|\!\=|\-\>'

syn match nanocLabel "\v^\s*\zs\w+\ze\s*:\s*$"
syn match nanocLabelTarget "\w\+" contained

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

syn match nanocDelimiter "\v[\(\)\{\}\[\]]"
hi def link nanocDelimiter Delimiter

" Comments
syn region nanocComment start="//" end="$"
syn region nanocComment start="/\*" end="\*/"

" Function definitions
syn match nanocFunction "\w\+" contained
syn match nanocFunction "\v\w+\ze\s*\("

" Highlighting links
hi def link nanocKeyword Keyword
hi def link nanocPrimitiveType Keyword
hi def link nanocConstant Constant
hi def link nanocNumber Number
hi def link nanocChar Character
hi def link nanocString String
hi def link nanocEscape SpecialChar
hi def link nanocComment Comment
hi def link nanocFunction Function
hi def link nanocOperator Operator
hi def link nanocLabel Label
hi def link nanocLabelTarget Label

let b:current_syntax = "nanoc"
