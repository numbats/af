SLIDES := $(wildcard slides/*.qmd)
SLIDE_PDFS := $(SLIDES:.qmd=.pdf)

default: slides preview

all: slides build

preview:
	quarto preview

build:
	quarto render

slides: $(SLIDE_PDFS)

slides/%.pdf: slides/%.qmd
	@echo "$< -> $@"
	quarto render '$<'

clean:
	rm -rf _site
	rm -rf _freeze
	rm $(SLIDE_PDFS)
	rm -rf slides/*_cache
	rm -rf slides/*_files
