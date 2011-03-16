###########################
## Package configuration ##
###########################

VERSION  = 0.1
TARGET   = gasp
PACKAGE  = gasp-$(VERSION)

include Makefile.generic
include Makefile.check

.PHONY: spl
spl: 
	rm -f spl
	$(OCAMLBUILD) spltc.byte && cp spltc.byte spl