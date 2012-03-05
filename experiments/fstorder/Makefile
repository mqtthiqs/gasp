TARGET=gasp

.PHONY: all clean

OCAMLBUILD_OPT=-use-ocamlfind -classic-display -j 5
OCAMLBUILD=ocamlbuild ${OCAMLBUILD_OPT}

all: ${TARGET}

${TARGET}:
	@${OCAMLBUILD} ${TARGET}.otarget

clean:
	@${OCAMLBUILD} -clean

%:
	@$(OCAMLBUILD) $@

test: ${TARGET} tests.otarget
	for test in `cat tests.itarget`; do \
		echo "* TEST: $$test"; \
		_build/$$test; \
		STATUS=$$?; \
		if [ $$STATUS != 0 ]; then \
			echo "* FAIL $$test"; \
			exit $$STATUS; \
		fi; \
		echo "* SUCCESS: $$test"; \
	done