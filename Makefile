default: preview

all: build

preview:
	quarto preview

build:
	quarto render

deploy:
	quarto publish gh-pages

clean:
	rm -rf _site
	rm -rf _freeze
