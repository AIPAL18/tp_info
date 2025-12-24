FILE := main.ml

BUILD := ./build
EXE := $(BUILD)/prog.exe

all: compile

compile: $(BUILD)
	ocamlc $(FILE) -o $(EXE)

$(BUILD):
	mkdir -p $(BUILD)

run: compile
	@echo "---------------\n"
	@ocamlrun $(EXE)
	@echo "\n---------------"

