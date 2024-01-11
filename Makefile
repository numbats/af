default: preview

all: build

preview:
	quarto preview

build:
	quarto render

clean:
	rm -rf _site
	rm -rf _freeze
