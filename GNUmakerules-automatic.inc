# Here are gathered automatic (generic, pattern-based) rules for Seaplus.
#
# See GNUmakerules-explicit.inc for the explicit (immediate, static)
# counterparts.


# Prerequisite: SEAPLUS_TOP must be set.


# Add all Seaplus-specific rules below.


# Outside prerequisites: the modules from Ceylan-Myriad.


# Allows any upper layer to override as a whole the Seaplus rules used to compile
# classes and other modules:
#
ifndef OVERRIDE_SEAPLUS_BUILD_RULES

%.beam: %.erl %.hrl
	@echo "		xxCompiling module with header $<"
	@#echo $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<


# The main, most usual rule:
%.beam: %.erl
	@echo "		xxCompiling module $<"
	@#echo $(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<
	@$(ERLANG_COMPILER) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $@ $<


endif
