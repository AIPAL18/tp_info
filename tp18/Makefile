FILE := main.c

BUILD := ./build
EXE := $(BUILD)/prog.exe

all: compile

compile: $(BUILD)
	gcc -std=c23 -O3 -DNDEBUG -Wall -Wextra -Werror -Wno-vla -Wno-unused -fsanitize=address,undefined $(FILE) -o $(EXE)

$(BUILD):
	mkdir -p $(BUILD)

run: compile
	@echo "---------------\n"
	@$(EXE)
	@echo "\n---------------"

