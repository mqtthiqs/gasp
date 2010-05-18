
check:
	@for i in *.ga; do            \
	  echo -n " * Test" $$i... ""; \
	  ../gasp $$i;                  \
	  if [ $$? != 0 ]; then          \
	    exit 1;                       \
	  fi;                              \
	done
