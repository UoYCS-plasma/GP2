" Vim syntax file
" Language:	Cantilever

setlocal isk=33-255

"syn spell toplevel
"syn case ignore
"syn sync linebreaks=1

"syn match ref /$\S*/

" syn match Constant /|S*:\s\+\S\+/

" syn match isWord   /\S*/
"syn match isQuote  /'\s\+\S\+/
"syn match isImmed  /#\s*\S\+/
"syn match isDefn   /:\s\+\S\+/ " contains=isTag

" syn match isSuffix /[:#']/
" syn match isTag    /['#:]/ contained
" syn match isHex   /\<0x[0-9a-f]\+\>/
syn match Number  /\<-\?[0-9]\+\>/

" syn region String  start="\"" end="\""
syn match stringEsc /\\[n0"\\]/
syn region String  start="s\" " end="\"" contains=stringEsc
syn region String  start="z\" " end="\"" contains=stringEsc
syn region Comment start="\<#\>" end="\n"
" syn region String  start="\"\n" end="\""
" syn region String  start="\"\t" end="\""

syn match isMacro  /\S*:\s\+\S\+/
syn match isDefn   /^\S*:\s\+\S\+/
syn match isDefn   /\<as\s\+\S\+/
syn match assertion /\<\(assertion\|test\): .*/
syn match isDebug  /\<?\s\+.*/ contains=assertion
syn match isElem   /\<[0-9]\+[ne]\>/
syn match isQuote  /\<'\s\+\S\+/
syn match isQuote  /\<'\s\+`\s\+\S\+/
syn match isQuote  /\<'\s\+\S\+:\s\+\S\+/
syn match isImmed  /\<#\S\+/
" syn match Number   /\<#\s\+[0-9]\+\>/
syn match Number   /\<hex:\s\+[0-9a-f]\+\>/
syn match Number   /\<oct:\s\+[0-8]\+\>/
syn match Number   /\<bin:\s\+[01]\+\>/
syn match Character /\<c:\s\+\S\+\>/
syn match isImmed  /\<\(if\|else\|endif\|;\|;;\|->\|forever\|times\|repeat\)\>/
syn match Boolean  /\<\(true\|false\|\S\+?\|[<>=]\+\)\>/
syn match usesRS   /\<\(push\|pop\|stash\|trash\|peek\)\>/
" syn match Exception /\<\(raise\|handles\)\>/
" syn match tailCall /\<tail: \S\+\>/
syn region String  start="string:" end="\n" contains=stringEsc,isDefn

syn region isImmed start="\<#\[\>" end="\<\]\>" contains=Number,Comment

hi link assertion String
hi isDebug ctermfg=brown guifg=brown
hi link nestComment Comment
hi link isHex Number
hi link isDec Number
hi link isQuote Number
hi link isImmed Macro
hi link isMacro Macro
hi link isDefn  Function
hi link isLeadSpace Todo
hi link usesRS Statement
hi link tailCall Macro
hi link isElem String

"hi hasPriW ctermfg=darkgrey cterm=bold guifg=darkgrey






