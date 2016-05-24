all:
	Rscript -e "devtools::document()"

test:
	Rscript -e "devtools::test()"

install:
	Rscript -e "devtools::install()"

check:
	Rscript -e "devtools::check()"
