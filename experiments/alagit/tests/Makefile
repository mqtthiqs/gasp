GASP=../gasp

.PHONY: check

check: $(addsuffix .out,$(wildcard *.ga))
clean:
	rm *.out

%.ga.out: %.ga
	@echo -n " * Test" $<... ""; \
	LOG=`$(GASP) $<` && \
	echo $$LOG > $<.out && echo OK
