###########################
## Package configuration ##
###########################

VERSION  = 0.1
TARGET   = gasp
PACKAGE  = gasp-$(VERSION)

include Makefile.generic

%.elf.out: %.elf $(TARGET)
	@echo " * Test" $<.; \
	LOG=`./$(TARGET) $<` && \
	echo $$LOG > $@

check: $(addsuffix .out,$(wildcard tests/*.elf))
