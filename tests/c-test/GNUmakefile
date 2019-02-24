SEAPLUS_TOP = ../..

.PHONY: all compile-erl-generic-support compile-service-driver          \
		compile-service-to-wrap run-driver erlang-test integration-test \
		clean clean-local clean-driver clean-beams                      \
		info info-compile info-erlang-local info-target


MODULES_DIRS = foobar

TARGET_DRIVER := foobar_seaplus_driver

TARGET_DRIVER_SRC := $(TARGET_DRIVER).c
TARGET_DRIVER_OBJ := $(TARGET_DRIVER).o
TARGET_DRIVER_EXEC := $(TARGET_DRIVER)



%.o: %.c
	@echo "   Compiling $<"
	$(C_COMPILER) $(C_COMPILER_OPT_FOR_OBJ) $(INC) $(FOOBAR_INC_OPT) -o $@ -c $<


all: $(TARGET_DRIVER_EXEC) erlang-test


compile-service-driver: $(TARGET_DRIVER_OBJ)


link-service-library: $(TARGET_SERVICE_LIBRARY)


$(TARGET_SERVICE_LIBRARY): $(TARGET_SERVICE_OBJ)
	@echo "   Linking $(TARGET_SERVICE_LIBRARY)"
	$(C_LINKER) $+ $(C_LINKER_OPT) -o $(TARGET_SERVICE_LIBRARY)


build-service-library-tester: $(TARGET_SERVICE_TESTER_EXEC)

$(TARGET_SERVICE_TESTER_EXEC): $(TARGET_SERVICE_TESTER_SRC) $(TARGET_SERVICE_LIBRARY)
	@echo "   Building executable $@"
	$(C_COMPILER) $(C_COMPILER_OPT) $< -o $(TARGET_SERVICE_TESTER_EXEC) -L. -l$(TARGET_SERVICE)


compile-service-to-wrap: $(TARGET_SERVICE_OBJ)


$(TARGET_DRIVER_EXEC): $(TARGET_DRIVER_OBJ) $(SEAPLUS_LIB)
	@echo "   Linking driver $(TARGET_DRIVER)"
	$(C_LINKER) -o $(TARGET_DRIVER_EXEC) $(TARGET_DRIVER_OBJ) \
	 $(SEAPLUS_LIB_OPT) $(C_LINKER_OPT)


#FOOBAR_LIBRARY = $(FOOBAR_LIB_DIR)/libfoobar.so

# One day I will understand why the $(FOOBAR_LIBRARY) prerequisite is not taken
# into account, as if that variable was not defined:
#
run-driver: $(TARGET_DRIVER_EXEC) $(FOOBAR_LIBRARY)
	@cd foobar && $(MAKE) -s link-foobar-lib
	@echo "   Running foobar driver, which will use $(FOOBAR_LIBRARY)"
	LD_LIBRARY_PATH=$(FOOBAR_LIB_DIR):$(SEAPLUS_LIB):$(LD_LIBRARY_PATH) ./$(TARGET_DRIVER_EXEC)


erlang-test: foobar.beam $(TEST).beam


integration-test: erlang-test $(TARGET_DRIVER_EXEC)
	@echo "   Running test $(TEST)"
	@$(ERL_INTERPRETER) -s $(TEST) run -s init stop -noshell


clean: clean-local


clean-local:
	-@/bin/rm -f $(TARGET_SERVICE_TESTER_EXEC) $(TARGET_SERVICE_LIBRARY) \
$(TARGET_DRIVER) $(TARGET_DRIVER_OBJ)


info: info-erlang info-target

info-erlang: info-erlang-local


info-erlang-local:
	@echo "ERL_BASE = $(ERL_BASE)"
	@echo "ERL_INTERFACE = $(ERL_INTERFACE)"


info-target:
	@echo "TARGET_SERVICE_SRC = $(TARGET_SERVICE_SRC)"
	@echo "TARGET_SERVICE_LIBRARY = $(TARGET_SERVICE_LIBRARY)"
	@echo "TARGET_SERVICE_TESTER_EXEC = $(TARGET_SERVICE_TESTER_EXEC)"
	@echo "TARGET_DRIVER_SRC = $(TARGET_DRIVER_SRC)"
	@echo "TARGET_DRIVER = $(TARGET_DRIVER)"
	@echo "FOOBAR_LIBRARY = $(FOOBAR_LIBRARY)"


include $(SEAPLUS_TOP)/GNUmakesettings.inc
