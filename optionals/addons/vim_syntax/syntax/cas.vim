" Vim syntax file
" Language: Telemac Steering file
" Maintainer: Yoann Audouin
" Latest Revision: 12/02/2016

if exists("b:current_syntax")
  finish
endif

" Keyword
syn match keywordCas "^[^=:]*[=:]"me=e-1
"syn match keywordCas "[ ][^=:]*[=:]"

" Strings
syn region txtCas start="'" skip="''" end="'"

" Comments
syn match commentCas "^/.*$"

" equal
syn match sepCas "[;]" 

" Boolean
syn keyword boolCas OUI YES NON NO VRAI FAUX


let b:current_syntax = "cas"

hi def link keywordCas Type
hi def link txtCas Constant
hi def link commentCas Comment
hi def link sepCas PreProc
hi def link boolCas PreProc
