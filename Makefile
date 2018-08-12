SERVER_STACK_YAML := server/stack.yaml
SERVER_SOURCES := $(shell find server -name '*.hs' -print)
SERVER_STACK := stack --stack-yaml $(SERVER_STACK_YAML)
SERVER_ROOTDIR := $(shell $(SERVER_STACK) path --local-install-root)
SERVER_TARGET := $(SERVER_ROOTDIR)/bin/server

UI_STACK_YAML := ui/stack.yaml
UI_SOURCES := $(shell find ui -name '*.hs' -print)
UI_STACK := stack --stack-yaml $(UI_STACK_YAML)
UI_ROOTDIR := $(shell $(UI_STACK) path --local-install-root)
UI_BINDIR := $(UI_ROOTDIR)/bin/ui.jsexe
UI_TARGET := $(UI_BINDIR)/all.js

SHARED_SOURCES := $(shell find shared -name '*.hs' -print)

all: $(SERVER_TARGET)

clean:

distclean:
	$(SERVER_STACK) clean --full
	rm -rf dist
	$(UI_STACK) clean --full

$(UI_TARGET): $(UI_SOURCES) $(SHARED_SOURCES)
	mkdir dist || true
	touch dist/_.js
	$(UI_STACK) build
	rm dist/_.js

dist/index.js: $(UI_TARGET) webpack/webpack.config.js
	cd webpack && yarn run webpack --env.SRCDIR=$(UI_BINDIR)

$(SERVER_TARGET): dist/index.js $(SERVER_SOURCES) $(SHARED_SOURCES)
	$(SERVER_STACK) build
