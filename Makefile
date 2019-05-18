PROJECT_NAME = $(shell basename "$$PWD" )
DIR = $(shell cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
BUILD_DIR = $(DIR)/dist
BUILD_DIR_APP = $(BUILD_DIR)/app
BUILD_DIR_GHC = $(BUILD_DIR)/ghc

ATTR=ghc
PACKAGE=Server

GHCID_ = $(SHELL_) --run 'ghcid --command "ghci" -W --test $(PACKAGE).main --output=$(DIR)/ghcid.txt'
SHELL_ = nix-shell .nix/default.nix \
							--add-root $(BUILD_DIR_GHC)/gc-roots/gc-root \
							--indirect \
							--attr shells.$(ATTR) \
							--pure $(ARGS)

.SILENT:

shell:
	@echo "Entering shell.."
	$(SHELL_)

shell-js: ATTR=ghcjs
shell-js:
	@echo "Entering GHCJS shell.."
	$(SHELL_)

ghcid-client: PACKAGE=Client
ghcid-client:
	@echo "Starting Ghcid.."
	$(GHCID_)

ghcid-server: PACKAGE=Server
ghcid-server:
	@echo "Starting Ghcid.."
	$(GHCID_)
