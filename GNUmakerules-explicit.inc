# Here are gathered explicit (immediate, static) rules for Seaplus.
#
# See GNUmakerules-automatic.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: SEAPLUS_TOP must be set.


.PHONY: default-seaplus-rule clean clean-obj \
		clean-ast-debug clean-ast-debug-seaplus


# The first non-generic, therefore default, rule:
default-seaplus-rule: all


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
	@find . -name 'Seaplus-*put-AST*.txt' -exec /bin/rm '{}' ';'
