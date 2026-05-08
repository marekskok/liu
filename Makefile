PKG_NAME = liu
C_SRCS = src/apiR_tree.c src/build_int_tree.c src/functions_int_tree.c src/build_double_tree.c src/functions_double_tree.c
HEADERS = src/declarations.h
R_SRCS = R/liu_interface.R

all: install

doc: $(R_SRCS) $(C_SRCS) DESCRIPTION
	touch doc

install: doc
	R CMD INSTALL .

check: doc
	R CMD check .
	R CMD check --as-cran .

clean:
	rm -f src/*.o src/*.so src/*.dll
	rm -rf man/
	rm -f doc

.PHONY: all install doc check clean