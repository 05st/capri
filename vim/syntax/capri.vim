" Vim syntax file
" Language: Capri
" Maintainer: 05st
" Latest Revision: 6 November 2021

if exists("b:current_syntax")
    finish
endif

" Misc Keywords
syn keyword Keyword module import extern infixl infixr infix prefix postfix

" Keywords
syn keyword Keyword pub fn op type struct return sizeof
syn keyword StorageClass mut

" Conditionals
syn keyword Conditional if else match

" Loops
syn keyword Conditional while for in

" Built-in types
syn keyword Type i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char bool unit

" Boolean values
syn keyword Boolean true false

" Identifiers
syn match Ignore "[a-zA-Z][a-zA-Z0-9_']*"

" Operators
syn match Keyword "[!#$%^&*\-+=<>./?\\|~]\+"
syn match Keyword ":="

" Integers
syn match Number "\<[0-9][0-9]*"
syn match Number "\<0x[a-fA-F0-9]\+"
syn match Number "\<0o[0-7]\+"
syn match Number "\<0b[01]\+"

" Floats
syn match Float "\<[0-9]\+\.[0-9]*"
syn match Float "\<[0-9]\+[eE][-]\?[0-9]\+"
syn match Float "\<[0-9]\+\.[0-9]*[eE][-]\?[0-9]\+"

" Strings
syn match SpecialChar contained "\\."
syn region String start='"' end='"' contains=SpecialChar

" Characters
syn match Character "'.'"
syn match Special "'\\.'"

" Semicolons
syn match Ignore ";"

" Comments
syn keyword Todo contained TODO FIXME NOTE
syn match Comment "//.*$" contains=Todo
syn region Comment start="/\*" end="\*/" contains=Todo

" Function Calls
syn match Function "\w\(\w\)*("he=e-1,me=e-1

" Parameterized Types
syn match Function "\w\(\w\)*<"he=e-1,me=e-1
