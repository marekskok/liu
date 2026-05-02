# Nazwa pakietu (musi być zgodna z plikiem DESCRIPTION)
PKG_NAME = liu

# Pliki źródłowe C
C_SRCS = src/apiR_tree.c src/build_int_tree.c src/functions_int_tree.c src/build_double_tree.c src/functions_double_tree.c
HEADERS = src/declarations.h

# Pliki R
R_SRCS = R/liu_interface.R

# Główna reguła: Dokumentuje i instaluje pakiet
all: install

# Generowanie dokumentacji (NAMESPACE, man/*.Rd) za pomocą roxygen2
# Zostanie wywołane tylko, gdy zmienią się pliki R lub C
doc: $(R_SRCS) $(C_SRCS) DESCRIPTION
	Rscript -e 'roxygen2::roxygenise(".")'
	touch doc

# Instalacja pakietu
# R CMD INSTALL zajmie się kompilacją plików w folderze src/
install: doc
	R CMD INSTALL .

# Sprawdzanie pakietu (opcjonalnie, warto robić przed publikacją)
check: doc
	R CMD check .
	R CMD check --as-cran .

# Czyszczenie: usuwa skompilowane obiekty i dokumentację
clean:
	rm -f src/*.o src/*.so src/*.dll
	rm -rf man/
	rm -f doc

.PHONY: all install doc check clean