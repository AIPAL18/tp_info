FILE := ex1.c

BUILD := ./build
EXE := $(BUILD)/prog.exe

FLAGS := -O0 -Wall -Wextra -Wvla -Wno-unused -fsanitize=address,undefined
VERSION := c23

all: compile

compile: $(BUILD)
	gcc -std=$(VERSION) -ggdb $(FLAGS) $(LIBS) -o $(EXE) $(FILE)

$(BUILD):
	mkdir -p $(BUILD)

run: compile
	@echo "---------------\n"
	$(EXE)
	@echo "\n---------------"

