default: preview

all: build

preview:
	quarto preview

build:
	quarto render

deploy:
	quarto publish gh-pages --no-prompt --no-browser

clean:
	rm -rf _site
	rm -rf _freeze
