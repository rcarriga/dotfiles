; extends

((expression_statement
   (string (
      (string_content) @injection.content)
      ; (#strip_indent! @injection.content)
     )
      (#set! injection.language "markdown_inline") 
   )
 )

; ((comment) @injection.content
;  (#lua-match? @injection.content "^#: ")
;  (#offset! @injection.content 0 3 0 0)
;  (#set! injection.language "rst"))


