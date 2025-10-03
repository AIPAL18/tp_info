FILES := squelette.c

BUILD := ./build
EXE := $(BUILD)/prog.exe

FLAGS := -O0 -Wall -Wextra -Wvla -Wno-unused -fsanitize=address,undefined
VERSION := c23
LIBS := -lm -ldl

all: compile

compile: $(BUILD)
	gcc -std=$(VERSION) -ggdb $(FLAGS) $(LIBS) -o $(EXE) $(FILES)

$(BUILD):
	mkdir -p $(BUILD)

run: compile
	@echo "---------------\n"
	$(EXE) ./images/bird.png bird.png
	@echo "\n---------------"

