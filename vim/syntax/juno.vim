" Vim syntax file
" Language: Juno
" Maintainer: 05st
" Latest Revision: 24 October 2021

if exists("b:current_syntax")
    finish
endif

" Misc Keywords
syn keyword Keyword import extern infixl infixr infix prefix postfix

" Keywords
syn keyword Keyword fn op data class impl return sizeof
syn keyword StorageClass mut

" Conditionals
syn keyword Conditional if else match

" Loops
syn keyword Conditional while for in

" Built-in types
syn keyword Type i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 str char bool unit

" Boolean values
syn keyword Boolean true false

" Identifiers
syn match Ignore "[a-zA-Z][a-zA-Z0-9_']*"

" Integers
syn match Number '[-ox]\?\d\+'

" Floats
syn match Float '[-]\?\d\+\.\d*'

" Strings
syn match SpecialChar contained "\\."
syn region String start='"' end='"' contains=SpecialChar

" Characters
syn match Character "'.'"
syn match Special "'\\.'"

" Operators
syn match Keyword "[!#$^&*\-+=<>./?\\|~]\+"
syn match Keyword ":="

" Semicolons
syn match Ignore ";"

" Comments
syn keyword Todo contained TODO FIXME NOTE
syn match Comment "//.*$" contains=Todo
syn region Comment start="/\*" end="\*/" contains=Todo

" Function Calls
syn match Function "\w\(\w\)*("he=e-1,me=e-1
