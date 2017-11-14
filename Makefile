OUTDIR = html
SRC = src

SOURCES = $(wilcard $(SRC)/**/*.purs)

OUTPUT = $(OUTDIR)/index.js

all: $(OUTPUT)

build:
	pulp build

test:
	pulp test

node_modules/:
	npm install

bower_components/:
	bower install

html/index.js html/%.js %.js: $(SOURCES) | node_modules/ bower_components/
	pulp browserify --to $@

clean:
	rm -rf $(OUTPUT) output/

.PHONY: all $(OUTPUT) build clean test
