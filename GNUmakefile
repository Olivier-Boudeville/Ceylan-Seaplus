SEAPLUS_TOP = .


.PHONY: help help-intro help-seaplus all all-check                       \
		register-version-in-header register-seaplus list-beam-dirs       \
		add-prerequisite-plts link-plt                                   \
		send-release release release-zip release-bz2 release-xz          \
		prepare-release clean-release clean-archive stats                \
		info-erlang-for-c info-paths info-compile info-erlang-compile    \
		info-c-compile info-parse-transform info-context info-versions


MODULES_DIRS = src doc conf test priv


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


SEAPLUS_RELEASES = $(SEAPLUS_RELEASE_ARCHIVE_BZ2) \
				   $(SEAPLUS_RELEASE_ARCHIVE_ZIP) \
				   $(SEAPLUS_RELEASE_ARCHIVE_XZ)


# First target for default:
help: help-intro help-seaplus


help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-seaplus:
	@cd $(MYRIAD_TOP) && $(MAKE) -s help-myriad


all: all-check



all-check:
	@if [ ! -d "$(ERL_BASE_CONTENT)" ]; then echo "Error, the base Erlang installation (needed to locate the Erl_Interface headers) could not be determined (searched for its '$(ERL_BASE_CONTENT)' subdirectory). Please update the ERL_BASE make variable, in GNUmakevars.inc." 1>&2; exit 5; fi


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 51 ; else \
	$(MAKE) register-seaplus ; fi


register-seaplus:
	@echo "-define( seaplus_version, \"$(SEAPLUS_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(SEAPLUS_BEAM_DIRS) ; do echo $$(readlink -f $$d) ; done


add-prerequisite-plts: link-plt


# As upper layers may rely on the 'seaplus' naming:
link-plt:
	@if [ ! "$(PLT_FILE)" = "$(SEAPLUS_PLT_FILE)" ]; then /bin/ln -s --force $(PLT_FILE) $(SEAPLUS_PLT_FILE) ; fi


# Note: the source archives are not produced in this directory, but in its
# parent, so that everything related to Seaplus (including these rules) remains
# self-contained.

release: release-zip release-bz2 release-xz
	@$(MAKE) clean-release


release-zip: prepare-release
	@echo "     Creating Seaplus release archive $(SEAPLUS_RELEASE_ARCHIVE_ZIP)"
	@cd .. && zip -r $(SEAPLUS_RELEASE_ARCHIVE_ZIP) $(SEAPLUS_RELEASE_BASE) \
	&& echo "     Archive $(SEAPLUS_RELEASE_ARCHIVE_ZIP) ready in "`pwd`


release-bz2: prepare-release
	@echo "     Creating Seaplus release archive $(SEAPLUS_RELEASE_ARCHIVE_BZ2)"
	@cd .. && tar chvjf $(SEAPLUS_RELEASE_ARCHIVE_BZ2) $(SEAPLUS_RELEASE_BASE) \
	&& echo "     Archive $(SEAPLUS_RELEASE_ARCHIVE_BZ2) ready in "`pwd`


release-xz: prepare-release
	@echo "     Creating Seaplus release archive $(SEAPLUS_RELEASE_ARCHIVE_XZ)"
	@cd .. && tar chvjf $(SEAPLUS_RELEASE_ARCHIVE_XZ) $(SEAPLUS_RELEASE_BASE) \
	&& echo "     Archive $(SEAPLUS_RELEASE_ARCHIVE_XZ) ready in "`pwd`


# The '-L' option with cp is used so that symbolic links are replaced by their
# actual target file, otherwise tar would include dead links in releases.
#
prepare-release: clean clean-release
	@echo "     Preparing release archive for Seaplus $(SEAPLUS_VERSION)"
	@cd .. && mkdir -p $(SEAPLUS_RELEASE_BASE) && /bin/cp -L -r myriad seaplus $(SEAPLUS_RELEASE_BASE)
	@cd ../$(SEAPLUS_RELEASE_BASE) && mv seaplus/top-GNUmakefile-for-releases GNUmakefile
	-@cd .. && find $(SEAPLUS_RELEASE_BASE) -type d -a -name '.git' -exec /bin/rm -rf '{}' ';' 2>/dev/null
	-@cd .. && find $(SEAPLUS_RELEASE_BASE) -type f -a -name '*.beam' -exec /bin/rm -f '{}' ';' 2>/dev/null


clean: clean-release clean-archive


clean-release:
	@echo "   Cleaning release archive for Seaplus"
	-@cd .. && /bin/rm -rf $(SEAPLUS_RELEASE_BASE)


clean-archive:
	-@cd .. && /bin/rm -f $(SEAPLUS_RELEASES)


stats:
	@$(MAKE_CODE_STATS) $(MYRIAD_TOP)


info-erlang-for-c:
	@echo "ERL_BASE = $(ERL_BASE)"
	@echo "ERL_INTERFACE = $(ERL_INTERFACE)"


info-paths:
	@echo "BEAM_PATH_OPT = $(BEAM_PATH_OPT)"


info-compile: info-erlang-compile info-c-compile


info-erlang-compile:
	@echo "ERLANG_COMPILER_BASE_OPT = $(ERLANG_COMPILER_BASE_OPT)"
	@echo "BEAM_DIRS = $(BEAM_DIRS)"
	@echo "INC = $(INC)"
	@echo "ERLANG_COMPILER_EXEC_TARGET_OPT = $(ERLANG_COMPILER_EXEC_TARGET_OPT)"
	@echo "ERLANG_COMPILER_DEBUG_OPT = $(ERLANG_COMPILER_DEBUG_OPT)"
	@echo "ERLANG_COMPILER_NATIVE_COMPILATION_OPT = $(ERLANG_COMPILER_NATIVE_COMPILATION_OPT)"
	@echo "ERLANG_COMPILER_WARNING_OPT = $(ERLANG_COMPILER_WARNING_OPT)"
	@echo "ERLANG_COMPILER_OPT_BASE = $(ERLANG_COMPILER_OPT_BASE)"
	@echo "OVERALL_PZ_OPT = $(OVERALL_PZ_OPT)"
	@echo "ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES = $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES)"


info-c-compile:
	@echo "ERL_BASE_FIRST_CANDIDATE = $(ERL_BASE_FIRST_CANDIDATE)"
	@echo "ERL_BASE_SECOND_CANDIDATE = $(ERL_BASE_SECOND_CANDIDATE)"
	@echo "ERL_BASE = $(ERL_BASE)"
	@echo "ERL_INTERFACE = $(ERL_INTERFACE)"
	@echo "ERL_INTERFACE_LIB = $(ERL_INTERFACE_LIB)"
	@echo "C_COMPILER = $(C_COMPILER)"
	@echo "C_LINKER = $(C_LINKER)"
	@echo "C_INC = $(C_INC)"
	@echo "C_LIB = $(C_LIB)"
	@echo "C_COMPILER_OPT = $(C_COMPILER_OPT)"
	@echo "C_LINKER_OPT = $(C_LINKER_OPT)"
	@echo "ERL_COMPILER  = $(ERL_COMPILER)"
	@echo "C_LINKER_LIBS = $(C_LINKER_LIBS)"


info-parse-transform:
	@echo "BOOTSTRAP_MODULES = $(BOOTSTRAP_MODULES)"
	@echo "ERLANG_COMPILER_OPT_FOR_PT = $(ERLANG_COMPILER_OPT_FOR_PT)"
	@echo "META_BEAM_FILES = $(META_BEAM_FILES)"
	@echo "ERLANG_COMPILER_PARSE_TRANSFORM_OPT = $(ERLANG_COMPILER_PARSE_TRANSFORM_OPT)"


# Typically useful to know the software context for continuous integration:
info-context: info-platform info-versions info-source-layout


info-versions:
	@echo "MYRIAD_VERSION = $(MYRIAD_VERSION)"


include $(SEAPLUS_TOP)/GNUmakesettings.inc
