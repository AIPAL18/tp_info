SOURCES := nob.c
PROJECT := nob

BUILD := ./build
EXE := $(BUILD)/$(PROJECT).exe

COMP := gcc

all: compile

compile: $(BUILD)
	$(COMP) $(SOURCES) -o $(EXE)

$(BUILD):
	mkdir -p $(BUILD)

clean:
	rm -rf $(BUILD)

run: compile
	@echo "---------------"
	@echo
	@$(EXE)
	@echo
	@echo "---------------"
