default: preview

all: build

preview:
	quarto preview

build:
	quarto render

deploy:
	git push

clean:
	rm -rf _site
	rm -rf _freeze
