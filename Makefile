INPUT_DIR = test/input
DOT_DIR = test/dots
PNG_DIR = test/pngs
OUTPUT_DIR = test/output

PYTHON = python3
SCRIPT = run_tests.py

RED=\033[0;31m
GREEN=\033[0;32m
NC=\033[0m

all: build run-tests generate-pngs

build:
	@echo "${RED}Building the project...${NC}"
	stack build
	@echo "${GREEN}Build finished.${NC}"

run-tests: build
	@echo "${RED}Running tests and generating DOT files...${NC}"
	$(PYTHON) $(SCRIPT) run-tests
	@echo "${GREEN}Tests finished and DOT files generated.${NC}"

generate-pngs:
	@echo "${RED}Generating PNGs from DOT files...${NC}"
	$(PYTHON) $(SCRIPT) generate-pngs
	@echo "${GREEN}PNGs generated from DOT files.${NC}"

clean:
	@echo "${RED}Cleaning the project...${NC}"
	stack clean
	rm -rf $(DOT_DIR)/*
	rm -rf $(PNG_DIR)/*
	rm -rf $(OUTPUT_DIR)/*
	@echo "${GREEN}Project cleaned.${NC}"

.PHONY: all build run-tests generate-pngs clean
