.PHONY: build system


# variables
BIN=./node_modules/.bin
BUILD_DIR=./build


# tasks
all: build


build: clean system elm css


build-production: build
	# TODO: Minify, etc.


clean:
	@echo "> Cleaning Build Directory"
	@rm -rf $(BUILD_DIR)


css:
	@echo "> Compiling CSS"
	@$(BIN)/postcss \
		-u postcss-import \
		-u postcss-mixins \
		-u postcss-custom-units \
		-u postcss-remify --postcss-remify.base=16 \
		-u postcss-simple-vars \
		-u postcss-cssnext --no-postcss-cssnext.features.rem \
		-o $(BUILD_DIR)/application.css \
		./src/Css/index.css


elm:
	@echo "> Compiling Elm code"

	$(eval ELM_MAKE_ARGS = src/App/Main.elm --output $(BUILD_DIR)/application.js --yes)

	@if [ -d ./libsysconfcpus/bin ]; then \
		./libsysconfcpus/bin/sysconfcpus -n 1 elm-make $(ELM_MAKE_ARGS); \
	else \
		elm-make $(ELM_MAKE_ARGS); \
	fi


server:
	@echo "> Booting up web server"
	@stack build && stack exec server


system:
	@echo "> Compiling System"
	@stack build && stack exec build


systemWithProfiles:
	@echo "> Compiling System (with stack-traces / profiles)"
	@stack build --force-dirty --executable-profiling --library-profiling && stack exec build


watch: build
	@stack exec fswatcher -- \
		--path ./ \
		--include "icidasset-template|src|system" \
		--exclude "build|node_modules|.DS_Store|.stack-work" \
		make
