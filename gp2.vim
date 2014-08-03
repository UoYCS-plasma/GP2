" Vim syntax file
" Language:	GP2


syn region isGraph start=+\[+ end=+]+ contains=isNode,isSqBracket

syn match isKeyword  /if\|then\|else\|try/
syn match isDefn   /\S*\s\+([^)]*)/ " contains=isTag
syn match isSimpleLoop /\S\+\!/
syn match isColour /#\s*\S\+/ contained
syn match isNode /([^)]*)/ contained contains=isColour


syn region String  start=+"+ skip=+\\"+ end=+"+
syn region Char    start="'" skip="\\'" end="'"
syn region isGraph start="\[" end="]"

syn match isQuote  /\S\+'/ "contains=isTag
syn match isImmed  /\S\+#/ "contains=isTag
syn match isImmed  /;/
syn match isDefn   /\S\+:/ "contains=isTag

hi link isKeyword Keyword 
hi link isSimpleLoop Repeat
hi link isDefn  Function
hi link isColour Identifier
hi link isNode Constant

hi link isHex Number
hi link isDec Number
hi link isQuote Type
hi link isImmed Identifier
hi link isTag   Macro
hi link isLeadSpace Todo

"hi hasPriW ctermfg=darkgrey cterm=bold guifg=darkgrey



