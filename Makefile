.PHONY: build system


# Variables
# ---------

BIN=./node_modules/.bin
BUILD_DIR=build
SRC_DIR=src
TEMPLATE_DIR=icidasset-template



# Default task
# ------------

all: build
	@make -j watch server



# Tasks
# -----

build: clean system elm css
	@echo "> Build completed ⚡"


build-production: clean system elm-prod css-prod


clean:
	@echo "> Cleaning build directory"
	@rm -rf $(BUILD_DIR)


css:
	@echo "> Compiling Css"
	@$(BIN)/postcss \
		"${SRC_DIR}/Css/Main.css" \
		--output "${BUILD_DIR}/stylesheet.css" \
		--config "${TEMPLATE_DIR}/Css/"


css-prod:
	@echo "> Compiling Css (optimized)"
	@$(BIN)/postcss \
		"${SRC_DIR}/Css/Main.css" \
		--output "${BUILD_DIR}/stylesheet.css" \
		--config "${TEMPLATE_DIR}/Css/"
		--env production


elm:
	@echo "> Compiling Elm application"
	@elm make src/App/Main.elm --output $(BUILD_DIR)/application.js


elm-prod:
	@echo "> Compiling Elm application (optimized)"
	@elm make src/App/Main.elm --output $(BUILD_DIR)/application.js --optimize

	@$(BIN)/terser $(BUILD_DIR)/application.js \
		--output $(BUILD_DIR)/application.tmp.js \
		--compress --mangle

	@rm $(BUILD_DIR)/application.js
	@mv $(BUILD_DIR)/application.tmp.js $(BUILD_DIR)/application.js


server:
	@echo "> Booting up web server on port 8000"
	@devd --port 8000 --all --crossdomain --quiet --notfound=200.html $(BUILD_DIR)


system:
	@echo "> Compiling System"
	@stack build --fast && stack exec build


watch:
	@echo "> Watching"
	@make -j watch-css watch-elm watch-system


watch-css:
	@watchexec -p -i "${BUILD_DIR}/*" --exts "css" -- make css


watch-elm:
	@watchexec -p -i "${BUILD_DIR}/*" --exts "elm" -- make elm


watch-system:
	@watchexec -p -i "${BUILD_DIR}/*" --exts "md,hs,yaml" -- make system
