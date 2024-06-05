INPUT_DIR = test/input
DOT_DIR = test/dots
PNG_DIR = test/pngs
EXECUTABLE = stack exec type-checker-exe

TESTS = $(shell find $(INPUT_DIR) -type f -name '*.json')
DOTS = $(shell find $(DOT_DIR) -type f -name '*.dot')

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
	@for test in $(TESTS); do \
		dirname=$$(dirname $$test | sed 's|$(INPUT_DIR)|$(DOT_DIR)|'); \
		base=$$(basename $$test .json); \
		index=$$(echo $$base | sed 's|in|out|'); \
		mkdir -p $$dirname; \
		$(EXECUTABLE) $$test > $$dirname/$$index.dot; \
	done
	@echo "${GREEN}Tests finished and DOT files generated.${NC}"

generate-pngs:
	@echo "${RED}Generating PNGs from DOT files...${NC}"
	@for dotfile in $(DOTS); do \
		dirname=$$(dirname $$dotfile | sed 's|$(DOT_DIR)|$(PNG_DIR)|'); \
		base=$$(basename $$dotfile .dot); \
		mkdir -p $$dirname; \
		dot -Tpng $$dotfile -o $$dirname/$$base.png; \
	done
	@echo "${GREEN}PNGs generated from DOT files.${NC}"

clean:
	@echo "${RED}Cleaning the project...${NC}"
	stack clean
	rm -rf $(DOT_DIR)/*
	rm -rf $(PNG_DIR)/*
	@echo "${GREEN}Project cleaned.${NC}"

.PHONY: all build run-tests generate-pngs clean
