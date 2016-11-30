#!usr/bin/make -f
# All commands are run as R functions rather than shell commands so that it will work easily on any Windows machine, even if the Windows machine isn't properly set up with all the right tools

all: README.md

clean:
	Rscript -e 'suppressWarnings(file.remove("README.md", "vignettes/colourpicker.md"))'

.PHONY: all clean
.DELETE_ON_ERROR:
.SECONDARY:

README.md : vignettes/colourpicker.Rmd
#	echo "Rendering the colourpicker vignette"
	Rscript -e 'rmarkdown::render("vignettes/colourpicker.Rmd", output_format = "md_document")'
#	echo "Correcting image paths"
#	sed -i -- 's,../inst,inst,g' vignettes/colourpicker.md
	Rscript -e 'file <- gsub("\\.\\./inst", "inst", readLines("vignettes/colourpicker.md")); writeLines(file, "vignettes/colourpicker.md")'
#	echo "Copying output to README.md"
#	cp vignettes/colourpicker.md README.md
	Rscript -e 'file.copy("vignettes/colourpicker.md", "README.md", overwrite = TRUE)'
	Rscript -e 'suppressWarnings(file.remove("vignettes/colourpicker.md"))'
