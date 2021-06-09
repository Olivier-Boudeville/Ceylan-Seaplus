# Here are gathered explicit (immediate, static) rules for Seaplus.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: SEAPLUS_TOP must be set.


.PHONY: default-seaplus-rule clean clean-obj \
		clean-ast-debug clean-ast-debug-seaplus


# The first non-generic, therefore default, rule:
default-seaplus-rule: all


info-compile-seaplus: info-c-compile
	@echo "ERL_BASE_FIRST_CANDIDATE = $(ERL_BASE_FIRST_CANDIDATE)"
	@echo "ERL_BASE_SECOND_CANDIDATE = $(ERL_BASE_SECOND_CANDIDATE)"
	@echo "ERL_BASE = $(ERL_BASE)"
	@echo "ERL_COMPILER  = $(ERL_COMPILER)"
	@echo "ERL_INTERFACE = $(ERL_INTERFACE)"
	@echo "ERL_INTERFACE_LIB = $(ERL_INTERFACE_LIB)"


info-c-compile:
	@echo "C_COMPILER = $(C_COMPILER)"
	@echo "C_LINKER = $(C_LINKER)"
	@echo "C_INC = $(C_INC)"
	@echo "C_LIB = $(C_LIB)"
	@echo "C_COMPILER_OPT = $(C_COMPILER_OPT)"
	@echo "C_COMPILER_OPT_FOR_OBJ = $(C_COMPILER_OPT_FOR_OBJ)"
	@echo "C_LINKER_OPT = $(C_LINKER_OPT)"
	@echo "C_LINKER_LIBS = $(C_LINKER_LIBS)"


clean: clean-binaries

clean-binaries: clean-obj clean-lib clean-test-exec


clean-obj:
	-@/bin/rm -f obj/*.o


clean-lib:
	-@/bin/rm -f lib/*.so

clean-test-exec:
	-@/bin/rm -f bin/*_test


# Removes the debug files created about ASTs:
clean-ast-debug: clean-ast-debug-seaplus


clean-ast-debug-seaplus:
	@find . -name 'Seaplus-*put-AST*.txt' -exec /bin/rm -f '{}' ';'
