all:
	Rscript -e "devtools::document()"


test:
	Rscript -e "devtools::test()"
