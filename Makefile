# Makefile for ringsConverter

APPNAME = ringsConverter
LPI = converter.lpr
LAZBUILD = $(HOME)/fpcupdeluxe/lazarus/lazbuild
FPC = fpc
BUILD_DIR = build
OUTPUT = $(BUILD_DIR)/$(APPNAME)

# Default target
all: create_build_dir build

create_build_dir:
	@mkdir -p $(BUILD_DIR)

# Lazarus build
build:
	$(LAZBUILD) --build-mode=Release $(LPI)
	@cp $(APPNAME) $(OUTPUT)

# Clean object files and builds
clean:
	@rm -rf lib
	@rm -rf $(BUILD_DIR)
	@rm -f $(APPNAME)
	@echo "Cleaned build files."

# Rebuild everything
rebuild: clean all

# macOS app bundle
macos-bundle:
	@echo "Creating macOS .app bundle..."
	@mkdir -p $(APPNAME).app/Contents/MacOS
	@cp $(APPNAME) $(APPNAME).app/Contents/MacOS/
	@echo "Done: $(APPNAME).app"

# Install binary to /usr/local/bin
install:
	@install -m 755 $(APPNAME) /usr/local/bin/$(APPNAME)
	@echo "Installed to /usr/local/bin/$(APPNAME)"

.PHONY: all build clean rebuild macos-bundle install

