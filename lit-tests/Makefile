# Paths
VENV_DIR  := tests/.venv
VENV_BIN  := $(VENV_DIR)/bin
LIT       := $(VENV_BIN)/lit -j1
PIP       := $(VENV_BIN)/pip

# Phony targets
.PHONY: test clean

# Main test target
test: $(LIT)
	$(LIT) -v tests/

# Rule to create the virtual environment and install lit
$(LIT):
	@echo "Setting up virtual environment..."
	python3 -m venv $(VENV_DIR)
	$(PIP) install --upgrade pip
	$(PIP) install lit
	@echo "Setup complete."

# Utility to remove the venv
clean:
	rm -rf $(VENV_DIR)
