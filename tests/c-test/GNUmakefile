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

TEST := foobar_test


# Must be included prior to specifying variable-based targets:
include $(SEAPLUS_TOP)/GNUmakesettings.inc


%.o: %.c
	@echo "   Compiling $<"
	$(C_COMPILER) $(C_COMPILER_OPT_FOR_OBJ) $(INC) $(FOOBAR_INC_OPT) -o $@ -c $<


all: build-foobar-lib $(TARGET_DRIVER_EXEC) erlang-test


build-foobar-lib:
	@cd foobar && $(MAKE) -s build-foobar-lib


compile-service-driver: $(TARGET_DRIVER_OBJ)


link-service-library: $(TARGET_SERVICE_LIBRARY)


$(TARGET_SERVICE_LIBRARY): $(TARGET_SERVICE_OBJ)
	@echo "   Linking $(TARGET_SERVICE_LIBRARY)"
	@$(C_LINKER) $+ $(C_LINKER_OPT) -o $(TARGET_SERVICE_LIBRARY)


build-service-library-tester: $(TARGET_SERVICE_TESTER_EXEC)

$(TARGET_SERVICE_TESTER_EXEC): $(TARGET_SERVICE_TESTER_SRC) $(TARGET_SERVICE_LIBRARY)
	@echo "   Building executable $@"
	@$(C_COMPILER) $(C_COMPILER_OPT) $< -o $(TARGET_SERVICE_TESTER_EXEC) -L. -l$(TARGET_SERVICE)


compile-service-to-wrap: $(TARGET_SERVICE_OBJ)


$(TARGET_DRIVER_EXEC): $(TARGET_DRIVER_OBJ) $(SEAPLUS_LIB_PATH)
	@echo "   Linking driver $(TARGET_DRIVER)"
	@$(C_LINKER) -o $(TARGET_DRIVER_EXEC) $(TARGET_DRIVER_OBJ) \
	 $(SEAPLUS_LIB_OPT) $(C_LINKER_OPT) -L$(FOOBAR_LIB_DIR) -lfoobar


#FOOBAR_LIBRARY = $(FOOBAR_LIB_DIR)/libfoobar.so

run-driver: $(TARGET_DRIVER_EXEC) $(FOOBAR_LIB_PATH)
	@cd foobar && $(MAKE) -s link-foobar-lib
	@echo "   Running foobar driver, which will use $(FOOBAR_LIBRARY)"
	@LD_LIBRARY_PATH=$(FOOBAR_LIB_DIR):$(SEAPLUS_LIB_DIR):$(LD_LIBRARY_PATH) ./$(TARGET_DRIVER_EXEC)


erlang-test: foobar.beam $(TEST).beam


integration-test: clean-log erlang-test $(TARGET_DRIVER_EXEC)
	@$(MAKE) -s foobar_run LD_LIBRARY_PATH=$(SEAPLUS_TOP)/src:foobar/lib:$(LD_LIBRARY_PATH)


clean: clean-local


clean-local: clean-log
	-@/bin/rm -f $(TARGET_SERVICE_TESTER_EXEC) $(TARGET_SERVICE_LIBRARY) \
$(TARGET_DRIVER) $(TARGET_DRIVER_OBJ)


clean-log:
	-@/bin/rm -f seaplus.log


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
