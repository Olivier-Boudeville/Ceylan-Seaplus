SEAPLUS_TOP = ..


.PHONY: all build-seaplus clean clean-local info info-bin


SEAPLUS_OBJ := seaplus.o


SEAPLUS_LIB = libseaplus-$(SEAPLUS_VERSION).so


%.o: %.c %.h
	@echo "   Compiling $< in $@ for inclusion in Seaplus library"
	@$(C_COMPILER) $(C_COMPILER_OPT_FOR_OBJ) $(SEAPLUS_INC) -o $@ -c $<



all: build-seaplus

build-seaplus: $(SEAPLUS_LIB)

$(SEAPLUS_LIB): $(SEAPLUS_OBJ)
	@echo "   Linking, in Seaplus library $(SEAPLUS_LIB), $^"
	@$(C_LINKER) $^ $(C_LINKER_OPT) -o $(SEAPLUS_LIB)



clean: clean-local

clean-local:
	-@/bin/rm -f $(SEAPLUS_OBJ) $(SEAPLUS_LIB)


info: info-bin

info-bin:
	@echo "SEAPLUS_OBJ = $(SEAPLUS_OBJ)"
	@echo "SEAPLUS_LIB = $(SEAPLUS_LIB)"


include $(SEAPLUS_TOP)/GNUmakesettings.inc
