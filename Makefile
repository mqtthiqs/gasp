###########################
## Package configuration ##
###########################

VERSION  = 0.1
TARGET   = gasp
PACKAGE  = gasp-$(VERSION)

include Makefile.generic

%.elf.out: %.elf $(TARGET)
	@echo " * Test" $<.; \
	LOG=`./$(TARGET) init $<` && \
	echo $$LOG > $@

check: $(addsuffix .out,$(wildcard tests/*.elf))
