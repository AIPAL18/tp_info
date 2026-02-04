LANG := c

ifeq ($(strip $(LANG)),c)
SOURCES := main.c
else
SOURCES := main.ml
endif

BUILD := ./build
EXE := $(BUILD)/$(SOURCES).exe


O_COMP := ocamlopt
C_COMP := gcc
C_VERSION := c23
C_OPTI := -O0
C_FLAGS :=  -pedantic \
			-fsanitize=address,undefined
C_WFLAGS := -Wall \
			-Wextra \
			-Werror \
			-Wconversion
DEBUG = -DNDEBUG


all: compile

ifeq ($(strip $(LANG)),c)
compile: $(BUILD)
	@echo "$(C_COMP) $(SOURCES) -o $(EXE)"
	@$(C_COMP) $(SOURCES) -o $(EXE) \
		-std=$(C_VERSION) \
		$(C_OPTI) \
		$(C_FLAGS) \
		$(C_WFLAGS) \
		$(DEBUG)
else
compile: $(BUILD)
	@echo "$(O_COMP) $(SOURCES) -o $(EXE)"
	@$(O_COMP) $(SOURCES) -o $(EXE)
endif

$(BUILD):
	mkdir -p $(BUILD)

run: compile
	@echo "---------------"
	@echo
	@$(EXE)
	@echo
	@echo "---------------"


# pedantic: Wattributes
# 			Wdeclaration-after-statement
#			Wimplicit-int
#			Wimplicit-function-declaration
#			Wincompatible-pointer-types
#			Wint-conversion
#			Wlong-long
#			Wmain
#			Wpointer-arith
#			Wpointer-sign
#			Wincompatible-pointer-types
#			Wvla
#------------------------------------------------------------------------------
# all:	Waddress
#		Warray-bounds=1 (only with -O2)
#		Warray-compare
#		Warray-parameter=2
#		Wbool-compare
#		Wbool-operation
#		Wchar-subscripts
#		Wcomment
#		Wdangling-else
#		Wdangling-pointer=2
#		Wduplicate-decl-specifier
#		Wenum-compare
#		Wenum-int-mismatch
#		Wformat=1
#		Wformat-contains-nul
#		Wformat-diag
#		Wformat-extra-args
#		Wformat-overflow=1
#		Wformat-truncation=1
#		Wformat-zero-length
#		Wframe-address
#		Wimplicit
#		Wimplicit-function-declaration
#		Wimplicit-int
#		Winfinite-recursion
#		Wint-in-bool-context
#		Wlogical-not-parentheses
#		Wmain
#		Wmaybe-uninitialized
#		Wmemset-elt-size
#		Wmemset-transposed-args
#		Wmisleading-indentation
#		Wmismatched-dealloc
#		Wmissing-attributes
#		Wmissing-braces
#		Wmultistatement-macros
#		Wnonnull
#		Wnonnull-compare
#		Wopenmp-simd
#		Wpacked-not-aligned
#		Wparentheses
#		Wpointer-sign
#		Wrestrict
#		Wreturn-type
#		Wsequence-point
#		Wsizeof-array-div
#		Wsizeof-pointer-div
#		Wsizeof-pointer-memaccess
#		Wstrict-aliasing
#		Wstrict-overflow=1
#		Wswitch
#		Wtautological-compare
#		Wtrigraphs
#		Wuninitialized
#		Wunknown-pragmas
#		Wunused
#		Wunused-but-set-variable
#		Wunused-const-variable=1
#		Wunused-function
#		Wunused-label
#		Wunused-local-typedefs
#		Wunused-value
#		Wunused-variable
#		Wuse-after-free=2
#		Wvla-parameter
#		Wvolatile-register-var
#		Wzero-length-bounds
#------------------------------------------------------------------------------
# extra: 	-Wabsolute-value
#			-Walloc-size
#			-Wcalloc-transposed-args
#			-Wcast-function-type
#			-Wclobbered
#			-Wempty-body
#			-Wenum-conversion
#			-Wexpansion-to-defined
#			-Wignored-qualifiers
#			-Wimplicit-fallthrough=3
#			-Wmaybe-uninitialized
#			-Wmissing-field-initializers
#			-Wmissing-parameter-name
#			-Wmissing-parameter-type
#			-Wold-style-declaration
#			-Wmultiple-parameter-fwd-decl-lists
#			-Woverride-init
#			-Wshift-negative-value
#			-Wstring-compare
#			-Wtype-limits
#			-Wuninitialized
#			-Wunterminated-string-initialization
#			-Wunused-parameter (only with -Wunused or -Wall)
#			-Wunused-but-set-parameter (only with -Wunused or -Wall)
