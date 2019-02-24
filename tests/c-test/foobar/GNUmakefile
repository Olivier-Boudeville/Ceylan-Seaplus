SEAPLUS_TOP = ../../..


# Makefile in charge of the vanilla foobar fake service.


.PHONY: all build build-foobar compile-foobar link-foobar-lib \
		build-foobar-test test run-foobar-test                \
		info info-binaries info-link info-target


obj/%.o: src/%.c inc/%.h
	@echo "   Compiling $< in $@ for inclusion in foobar library"
	@mkdir -p obj
	@$(C_COMPILER) $(C_COMPILER_OPT_FOR_OBJ) $(FOOBAR_INC_OPT) -o $@ -c $<



TARGET_SERVICE := foobar

TARGET_SERVICE_OBJ := obj/foobar.o

TARGET_LIBRARY = lib/libfoobar.so

all: build

build: build-foobar build-foobar-test

build-foobar: compile-foobar link-foobar-lib


compile-foobar: $(TARGET_SERVICE_OBJ)


link-foobar-lib: $(TARGET_LIBRARY)


$(TARGET_LIBRARY): $(TARGET_SERVICE_OBJ)
	@echo "   Linking service library $@ from $^"
	@mkdir -p lib
	@$(C_LINKER) $^ $(C_LINKER_OPT) -o $@


TARGET_SERVICE_TEST := foobar_test
TARGET_SERVICE_TEST_EXEC := bin/$(TARGET_SERVICE_TEST)
TARGET_SERVICE_TEST_SRC := test/$(TARGET_SERVICE_TEST).c


build-foobar-test: $(TARGET_SERVICE_TEST_EXEC)


$(TARGET_SERVICE_TEST_EXEC): $(TARGET_SERVICE_TEST_SRC) $(TARGET_SERVICE_LIBRARY)
	@echo "   Building executable $@"
	@mkdir -p bin
	@$(C_COMPILER) $(C_COMPILER_OPT) $< -o $(TARGET_SERVICE_TEST_EXEC) $(FOOBAR_INC_OPT) -Llib -l$(TARGET_SERVICE)


test: run-foobar-test

run-foobar-test: $(TARGET_SERVICE_TEST_EXEC)
	@echo "   Running test $<"
	@LD_LIBRARY_PATH=$(FOOBAR_LIB_DIR):$(LD_LIBRARY_PATH) $(TARGET_SERVICE_TEST_EXEC)


info: info-binaries info-link info-target


info-binaries:
	@echo "TARGET_SERVICE_OBJ = $(TARGET_SERVICE_OBJ)"
	@echo "TARGET_SERVICE_LIBRARY = $(TARGET_SERVICE_LIBRARY)"


info-link:
	@echo "C_LINKER_OPT = $(C_LINKER_OPT)"


info-target:
	@echo "TARGET_SERVICE_LIBRARY = $(TARGET_SERVICE_LIBRARY)"
	@echo "TARGET_SERVICE_TEST_EXEC = $(TARGET_SERVICE_TEST_EXEC)"


include $(SEAPLUS_TOP)/GNUmakesettings.inc
