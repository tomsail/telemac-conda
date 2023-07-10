" Vim syntax file
" Language: Telemac Dictionary file
" Maintainer: Yoann Audouin
" Latest Revision: 12/02/2016

if exists("b:current_syntax")
  finish
endif

" Dictionary keywords
syn keyword dicoKeywords NOM1 NOM TYPE INDEX MNEMO TAILLE SUBMIT DEFAUT DEFAUT1 CHOIX CHOIX1 RUBRIQUE RUBRIQUE1 COMPOSE CONTROLE COMPORT NIVEAU APPARENCE AIDE AIDE1

" Type
syn keyword typeDico ENTIER REEL LOGIQUE STRING REAL INTEGER BOOLEAN CARACTERE

" Comments
syn match commentDico "^/.*$"

" Numbers
" Regular int like number with - + or nothing in front
syn match celNumber '\d\+'
syn match celNumber '[-+]\d\+'

" Floating point number with decimal no E or e (+,-)
syn match celNumber '\d\+\.\d*'
syn match celNumber '[-+]\d\+\.\d*'

" Floating point like number with E and no decimal point (+,-)
syn match celNumber '[-+]\=\d[[:digit:]]*[eE][\-+]\=\d\+'
syn match celNumber '\d[[:digit:]]*[eE][\-+]\=\d\+'

" Floating point like number with E and decimal point (+,-)
syn match celNumber '[-+]\=\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'
syn match celNumber '\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'

" Strings
syn region txtDico start="'" skip="''" end="'" contains=celNumber

" Boolean
syn keyword boolDico OUI YES NON NO VRAI FAUX

let b:current_syntax = "dico"

hi def link dicoKeywords Type
hi def link txtDico Constant
hi def link commentDico Comment
hi def link celNumber Constant
hi def link typeDico PreProc
hi def link boolDico PreProc
