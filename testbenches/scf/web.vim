" Vim syntax file
" Language:	WEB
" Maintainer:	Andreas Scherer <andreas.scherer@pobox.com>
" Last Change:	April 30, 2001

" Details of the WEB language can be found in the article by Donald E. Knuth,
" "The WEB System of Structured Documentation", included as "webman.tex" in
" the standard WEB distribution, available for anonymous ftp at
" ftp://labrea.stanford.edu/pub/tex/web/.

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

"syn match   webDecorator     "@<" display nextgroup=webFunction skipwhite

" Although WEB is the ur-language for the "Literate Programming" paradigm,
" we base this syntax file on the modern superset, CWEB.  Note: This shortcut
" may introduce some illegal constructs, e.g., CWEB's "@c" does _not_ start a
" code section in WEB.  Anyway, I'm not a WEB programmer.

" Replace C/C++ syntax by Pascal syntax.
"syntax include @webIncludedC <sfile>:p:h/fortran.vim

"if version < 600
"  source <sfile>:p:h/tex.vim
"else
"  runtime! syntax/tex.vim
"  unlet b:current_syntax
"endif
syn region  webString  start='@<' end='@>' 
syn region  webComment start="/\*" end="\*/" contains=@Spell extend
syn region  webText    start='@\*' end='@a' 
syn region  webText    start='"'   end='"'

syn keyword webStatement integer double precision implicit character real complex rewind cycle
syn keyword webStatement while do end end do enddo if endif end if select case go to goto save exit
syn keyword webStatement format write read open return data parameter equivalence continue else then
syn keyword webFunction  call
syn keyword webInclude   STOP subroutine function program
syn keyword webStatement INTEGER DOUBLE PRECISION IMPLICIT CHARACTER REAL COMPLEX REWIND CYCLE
syn keyword webStatement WHILE DO END END DO ENDDO IF ENDIF END IF SELECT CASE GO TO GOTO SAVE EXIT
syn keyword webStatement FORMAT WRITE READ OPEN RETURN DATA PARAMETER EQUIVALENCE CONTINUE ELSE THEN
syn keyword webFunction  CALL
syn keyword webInclude   STOP SUBROUTINE FUNCTION PROGRAM

syn match   webOperator     "\.\s*\(eq\|ne\)\s*\."
syn match   webOperator     "\.\s*\(and\|or\)\s*\."
syn match   webOperator     "\.\s*\(gt\|ge\)\s*\."
syn match   webOperator     "\.\s*\(lt\|le\)\s*\."
syn match   webOperator     "\.\s*\(EQ\|NE\)\s*\."
syn match   webOperator     "\.\s*\(AND\|OR\)\s*\."
syn match   webOperator     "\.\s*\(GT\|GE\)\s*\."
syn match   webOperator     "\.\s*\(LT\|LE\)\s*\."
syn match   webHighlight    "@n"
syn match   webHighlight    "@r"
syn match   webHighlight    "@c"
syn match   webHighlight    "@c++"




" Double-@ means single-@, anywhere in the WEB source (as in CWEB).
" Don't misinterpret "@'" as the start of a Pascal string.
syntax match webIgnoredStuff "@[@']"

let b:current_syntax = "web"

hi def link webString               Function
hi def link webStringI              Function
hi def link webComment              Comment
hi def link webText                 String
hi def link webStatement            Statement
hi def link webFunction             Function
hi def link webInclude              Include
hi def link webOperator             Operator
hi def link webHighlight            Special
" vim: ts=8


