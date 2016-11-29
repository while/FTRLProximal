all: doc test

doc: 
	R --vanilla -q -e "devtools::document()"

test: 
	R --vanilla -q -e "devtools::test()"

install: 
	R --vanilla -q -e "devtools::install()"

check: build
	cd build/; R CMD check *.tar.gz

build: doc
	mkdir -p build; cd build/; R CMD build ..

clean:
	rm -r build/

cover:
	Rscript --vanilla -e "covr::package_coverage()"
