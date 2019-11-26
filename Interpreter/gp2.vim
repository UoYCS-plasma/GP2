" Vim syntax file
" Language:	GP2


syn match isKeyword  /if\|then\|else\|try\|interface/
syn match isType     /:\s*\(int\|list\)/ contained
syn match isDefn   /\S*\s\+([^)]*)/ contains=isType
syn match isSimpleLoop /\S\+\!/
syn match isColour /#\s*\(grey\|red\|blue\)/ contained
syn match isOp  /+\s*\S\+\s\+\S\+\s+/ contained
syn match isLoop1 /(.\{-})!/
syn match isLoop2 /{.\{-}}!/

syn region String  start=+"+ skip=+\\"+ end=+"+
syn region Char    start="'" skip="\\'" end="'"
syn region isNode  start=+(+ end=+)+ contained contains=isColour,isOp
syn region isGraph start=+\[+ end=+]+ contains=isNode,isSqBracket

hi link isType Type
hi link isKeyword Keyword 
hi link isSimpleLoop Repeat
hi link isLoop1 Repeat
hi link isLoop2 Repeat
hi link isDefn  Function
hi link isColour Identifier
hi link isNode Constant
hi link isOp Number


"hi hasPriW ctermfg=darkgrey cterm=bold guifg=darkgrey



