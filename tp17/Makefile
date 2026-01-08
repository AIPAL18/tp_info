mlFILE := main.ml
cFILE := main.c

BUILD := ./build
mlEXE := $(BUILD)/prog_ml.exe
cEXE := $(BUILD)/prog_c.exe

all: compile_ml compile_c

compile_ml: $(BUILD)
	ocamlopt $(mlFILE) -o $(mlEXE)

compile_c: $(BUILD)
	gcc -std=c23 $(cFILE) -o $(cEXE)

$(BUILD):
	mkdir -p $(BUILD)

clean:


run: compile_ml compile_c
	@echo "---------------OCaml\n"
	@$(mlEXE)
	@echo "\n---------------C\n"
	@$(cEXE)
	@echo "\n---------------"

