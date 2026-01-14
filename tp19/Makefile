FILE := tests.ml main.ml
BUILD := ./build
EXE := $(BUILD)/prog.exe
OPTS := -color always -error-style contextual -g

all: compile

compile: $(BUILD)
	@echo "compiling $(FILE) to $(EXE)"
	@ocamlopt $(OPTS) $(FILE) -o $(EXE)

$(BUILD):
	mkdir -p $(BUILD)

run: compile
	@echo "---------------"
	@echo ""
	@$(EXE)
	@echo ""
	@echo "---------------"

