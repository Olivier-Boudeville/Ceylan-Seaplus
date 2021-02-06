SEAPLUS_TOP = ../..

.PHONY: all test test-local compile-erl-generic-support compile-service-driver \
		compile-service-to-wrap run-driver erlang-test integration-test        \
		clean clean-local clean-driver clean-beams                             \
		info info-compile info-erlang-local info-target info-compile


# Explicit recursion (done by build-foobar-lib) preferred, to avoid parallel
# build potentially resulting in libfoobar.so not being available when the
# foobar driver is linked):
#
#MODULES_DIRS = foobar

TARGET_DRIVER := foobar_seaplus_driver

TARGET_DRIVER_OBJ := $(TARGET_DRIVER).o
TARGET_DRIVER_EXEC := $(TARGET_DRIVER)

TEST := foobar_test


# Must be included prior to specifying variable-based targets:
include $(SEAPLUS_TOP)/GNUmakesettings.inc


%.o: %.c
	@echo "   Compiling $<"
	@$(C_COMPILER) $(C_COMPILER_OPT_FOR_OBJ) $(FOOBAR_INC_OPT) -o $@ -c $<


all: build-foobar-lib $(TARGET_DRIVER_EXEC) erlang-test


build-foobar-lib:
	@cd foobar && $(MAKE) -s build-foobar-lib


compile-service-driver: $(TARGET_DRIVER_OBJ)


link-service-library: $(TARGET_SERVICE_LIBRARY)


$(TARGET_SERVICE_LIBRARY): $(TARGET_SERVICE_OBJ)
	@echo "   Linking $@"
	@$(C_LINKER) $+ $(C_LINKER_OPT) -o $(TARGET_SERVICE_LIBRARY)


build-service-library-tester: $(TARGET_SERVICE_TESTER_EXEC)

$(TARGET_SERVICE_TESTER_EXEC): $(TARGET_SERVICE_TESTER_SRC) $(TARGET_SERVICE_LIBRARY)
	@echo "   Building executable $@"
	@$(C_COMPILER) $(C_COMPILER_OPT) $< -o $(TARGET_SERVICE_TESTER_EXEC) -L. -l$(TARGET_SERVICE)


compile-service-to-wrap: $(TARGET_SERVICE_OBJ)

$(TARGET_DRIVER_OBJ): foobar.beam


# For some reason, $(TARGET_DRIVER_OBJ) has to be specified twice if relying on
# the auto-generated templated version of it, otherwise foobar_seaplus_driver.o
# is not found:
#
$(TARGET_DRIVER_EXEC): $(TARGET_DRIVER_OBJ) $(SEAPLUS_LIB_PATH)
	@$(MAKE) -s $(TARGET_DRIVER_OBJ)
	@echo "   Linking driver $(TARGET_DRIVER)"
	@ls -l $(FOOBAR_LIB_DIR)/libfoobar.so
	@file $(FOOBAR_LIB_DIR)/libfoobar.so
	@nm -D $(FOOBAR_LIB_DIR)/libfoobar.so
	$(C_LINKER) -o $(TARGET_DRIVER_EXEC) $(TARGET_DRIVER_OBJ) \
	 $(SEAPLUS_LIB_OPT) $(C_LINKER_OPT) -L$(FOOBAR_LIB_DIR) -lfoobar


# Main interest: ensuring that the driver executable can be run at all.
run-driver: $(TARGET_DRIVER_EXEC) $(FOOBAR_LIB_PATH)
	@cd foobar && $(MAKE) -s link-foobar-lib
	@echo "   Running foobar driver, which will use $(FOOBAR_LIBRARY)"
	@LD_LIBRARY_PATH=$(FOOBAR_LIB_DIR):$(SEAPLUS_LIB_DIR):$(LD_LIBRARY_PATH) ./$(TARGET_DRIVER_EXEC)


erlang-test: foobar.beam $(TEST).beam


# The actual target to execute for testing:
# (deactivated now that environment is set from the test case itself)
#integration-test: clean-log erlang-test $(TARGET_DRIVER_EXEC)
#	@$(MAKE) -s foobar_run LD_LIBRARY_PATH=$(SEAPLUS_TOP)/src:foobar/lib:$(LD_LIBRARY_PATH) || cat seaplus-driver.*.log


clean: clean-local


clean-local: clean-log
	-@/bin/rm -f $(TARGET_SERVICE_TESTER_EXEC) $(TARGET_SERVICE_LIBRARY) $(TARGET_DRIVER) $(TARGET_DRIVER_OBJ) *_seaplus_api_mapping.h
	@cd foobar && $(MAKE) -s clean


clean-log:
	-@/bin/rm -f seaplus-driver.*.log


info: info-erlang info-target

info-erlang: info-erlang-local


info-erlang-local:
	@echo "ERL_BASE = $(ERL_BASE)"
	@echo "ERL_INTERFACE = $(ERL_INTERFACE)"


info-target:
	@echo "TARGET_SERVICE_SRC = $(TARGET_SERVICE_SRC)"
	@echo "TARGET_SERVICE_LIBRARY = $(TARGET_SERVICE_LIBRARY)"
	@echo "TARGET_SERVICE_TESTER_EXEC = $(TARGET_SERVICE_TESTER_EXEC)"
	@echo "TARGET_DRIVER = $(TARGET_DRIVER)"
	@echo "FOOBAR_LIBRARY = $(FOOBAR_LIBRARY)"


info-compile:
	@echo "INC = $(INC)"
	@echo "C_INC = $(C_INC)"
	@echo "C_COMPILER_OPT_FOR_OBJ = $(C_COMPILER_OPT_FOR_OBJ)"
	@echo "FOOBAR_INC_OPT = $(FOOBAR_INC_OPT)"
