FILE := main.ml
BUILD := ./build
EXE := $(BUILD)/prog.exe

all: compile

compile: $(BUILD)
	ocamlopt $(FILE) -o $(EXE)
	

$(BUILD):
	mkdir -p $(BUILD)

run: compile
	@echo "---------------\n"
	$(EXE)
	@echo "\n---------------"

