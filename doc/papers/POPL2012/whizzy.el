(whizzy-add-configuration
 ".*\.\\(tex\\|sty\\)"
 '((whizzy-master . "main.tex"))
)
(whizzy-add-configuration
 "main\.tex" 
 '((whizzy . "section -advi \"advi -geometry 2048x1024 -fullwidth \" -dvicopy dvicopy" ))
)
