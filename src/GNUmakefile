SEAPLUS_TOP = ..


.PHONY: all build-seaplus clean clean-local clean-foobar info info-bin


# Note: libtool is not used anymore (not worth the trouble).


# List of the object files comprised in the library:
#SEAPLUS_OBJ := seaplus.lo
SEAPLUS_OBJ := seaplus.o

SEAPLUS_TEST_OBJ := seaplus_test.o



# Must be included prior to specifying variable-based targets:
include $(SEAPLUS_TOP)/GNUmakesettings.inc


%.o: %.c %.h
	@echo "   Compiling $< with header in $@ for inclusion in Seaplus library"
	@$(C_COMPILER) $(C_COMPILER_OPT_FOR_OBJ) $(SEAPLUS_INC) -c $<


%_test.o: %_test.c
	@echo "   Compiling $< in $@ for test of the Seaplus library"
	@$(C_COMPILER) $(C_COMPILER_OPT) $(SEAPLUS_INC) -c $<


%.o: %.c
	@echo "   Compiling $< in $@ for inclusion in Seaplus library"
	@$(C_COMPILER) $(C_COMPILER_OPT_FOR_OBJ) $(SEAPLUS_INC) -c $<


#@$(LIBTOOL) $(LIBTOOL_C_OPT) --mode=compile $(C_COMPILER) $(C_COMPILER_OPT_FOR_OBJ) $(SEAPLUS_INC) -c $<



all: all-local

all-recurse: all-local


all-local: $(SEAPLUS_LIB) seaplus_test

build-seaplus: build-lib build-test

build-lib: $(SEAPLUS_LIB)

$(SEAPLUS_LIB): $(SEAPLUS_OBJ)
	@echo "   Linking, in Seaplus library $@, $^"
	@$(C_LINKER) $^ $(C_LINKER_OPT) -shared -o $@ $(C_LINKER_LIBS)

#$(LIBTOOL) --mode=link $(LIBTOOL_C_OPT) $(C_LINKER) -shared $(C_LINKER_OPT) -o $(SEAPLUS_LIB) $^ $(C_LINKER_LIBS)


build-test: seaplus_test $(SEAPLUS_LIB)

seaplus_test: $(SEAPLUS_TEST_OBJ) $(SEAPLUS_LIB)
	@echo "   Linking Seaplus test from $^"
	@$(C_LINKER) $(C_LINKER_OPT) -o $@ $< $(SEAPLUS_LIB_OPT)

#$(LIBTOOL) --mode=link $(LIBTOOL_C_OPT) $(C_LINKER) -o $@ $^ $(SEAPLUS_LIB_OPT) $(C_LINKER_OPT)


test: run-test

run-test: build-test
	@LD_LIBRARY_PATH=.:${LD_LIBRARY_PATH} ./seaplus_test


clean: clean-local


# Apparently libtool (not used anymore) systematically compiled a .lo and a .o:
clean-local: clean-foobar
	-@/bin/rm -rf .libs
	-@/bin/rm -f *.lo $(SEAPLUS_OBJ) $(SEAPLUS_LIB) seaplus_test \
		$(SEAPLUS_TEST_OBJ) seaplus_test.log

clean-foobar:
	@echo "Cleaning files generated for foobar"


info: info-bin

info-bin:
	@echo "SEAPLUS_OBJ = $(SEAPLUS_OBJ)"
	@echo "SEAPLUS_LIB = $(SEAPLUS_LIB)"
