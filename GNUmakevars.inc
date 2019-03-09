# Prerequisite: SEAPLUS_TOP must be set.


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Seaplus
endif


SEAPLUS_VERSION := 1.0

# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = $(SEAPLUS_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = Ceylan-Seaplus
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(SEAPLUS_TOP)
endif


SEAPLUS_RELEASE_BASE := seaplus-$(PROJECT_VERSION)

SEAPLUS_RELEASE_ARCHIVE_ZIP := $(SEAPLUS_RELEASE_BASE).zip
SEAPLUS_RELEASE_ARCHIVE_BZ2 := $(SEAPLUS_RELEASE_BASE).tar.bz2
SEAPLUS_RELEASE_ARCHIVE_XZ  := $(SEAPLUS_RELEASE_BASE).tar.xz


# Source section.
SEAPLUS_SRC := $(SEAPLUS_TOP)/src

# BEAM path section.
SEAPLUS_BEAM := $(SEAPLUS_SRC)

# Include path section.
SEAPLUS_INC = -I$(SEAPLUS_SRC)

# Library referencing section.

SEAPLUS_LIB = libseaplus-$(SEAPLUS_VERSION).so
SEAPLUS_LIB_DIR = $(SEAPLUS_TOP)/src
SEAPLUS_LIB_PATH = $(SEAPLUS_LIB_DIR)/$(SEAPLUS_LIB)
SEAPLUS_LIB_OPT = -L$(SEAPLUS_LIB_DIR) -lseaplus-$(SEAPLUS_VERSION)


DOC_ROOT       = $(SEAPLUS_TOP)/doc
SEAPLUS_DOC_DIR = $(DOC_ROOT)/web/main/documentation/seaplus


INC += $(SEAPLUS_INC)



ifndef VM_TEST_NAME

VM_NAME := seaplus_debug

endif


ifndef VM_TEST_NAME

VM_TEST_NAME := seaplus_test

endif




# So that they can be fetched from outside
# (see the 'list-beam-dirs' target)
#
SEAPLUS_BEAM_DIRS = $(SEAPLUS_TOP)/src      \
					$(SEAPLUS_TOP)/examples


# We rely on Myriad as well:
BEAM_DIRS += $(SEAPLUS_BEAM_DIRS)


# Overall settings section.


#EXECUTION_TARGET=production

ifndef EXECUTION_TARGET

  # Other possible value: production
  EXECUTION_TARGET=development

endif


ifeq ($(EXECUTION_TARGET),development)

  #$(info Execution target is development)

  ENABLE_DEBUG=true

else ifeq ($(EXECUTION_TARGET),production)

  #$(info Execution target is production)

  ENABLE_DEBUG=false

else

  $(error Invalid execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif



#ENABLE_DEBUG=false

# By default, this debug mode is enabled:
ifndef ENABLE_DEBUG

	ENABLE_DEBUG = true

endif



# Tells whether the debug mode will be activated for the next Seaplus classes to
# be built (maximum performance versus extended checkings).
#
ifeq ($(ENABLE_DEBUG),true)

	SEAPLUS_DEBUG_OPT = -DDEBUG_SEAPLUS -DSEAPLUS_ENABLE_LOG=1

else

	SEAPLUS_DEBUG_OPT = -DSEAPLUS_ENABLE_LOG=0

endif



# Build section.


LIBTOOL = $(shell which libtool)

# This is a C-based (not C++) example:
LIBTOOL_C_OPT += --tag=CC --verbose


# Compiler section.

SEAPLUS_BASE_NAME := seaplus



# C subsection.


ERL_INTERFACE_INC = -I$(ERL_INTERFACE)/include
ERL_INTERFACE_LIB = -L$(ERL_INTERFACE)/lib

C_COMPILER := $(shell which gcc)


C_INC = $(ERL_INTERFACE_INC) -I.
C_LIB = $(ERL_INTERFACE_LIB)

C_COMPILER_OPT = $(C_INC) $(SEAPLUS_DEBUG_OPT) -Wall -Werror -g

C_COMPILER_OPT_FOR_OBJ = $(C_COMPILER_OPT) -fPIC




# Erlang subsection.

ERL_BASE := $(HOME)/Software/Erlang/Erlang-current-install

ERL_INTERFACE = $(shell /bin/ls -d $(ERL_BASE)/lib/erlang/lib/erl_interface-*)

# Modules needed for the bootstrapping for others (hence to be built first, and
# not parse-transformed; typically *used* by parse-transforms, or being a
# parse-transform themselves)
#
# (see the 'Bootstrap section' in GNUmakerules-explicit.inc for their
# special-cased build)
#
ifndef BOOTSTRAP_MODULES

	# We want to bootstrap all relevant Seaplus modules (Myriad expected to be
	# already fully built)


	# We list here all the SEAPLUS-level prerequisites of the SEAPLUS
	# parse-transform:
	#
	# - seaplus_info: to manage class-level information

	#BOOTSTRAP_MODULES = $(SEAPLUS_TOP)/src/seaplus_info.beam

endif


ifndef COMPILER_OPT_FOR_SEAPLUS_CLASSES

	   COMPILER_OPT_FOR_SEAPLUS_CLASSES =                      \
			$(ERLANG_COMPILER_OPT_BASE)                        \
			$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_SEAPLUS)

endif


#ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES

	   # Standard (non-class) modules in Seaplus shall not be compiled with the
	   #  Seaplus parse-transform, but with the Myriad one:
	   #
	   ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES = \
				$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_SEAPLUS)

#endif


# Conditionally defined so that upper layer may update these settings (ex:
# adding pz directories):
#
ifndef ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES

	ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES =                          \
			$(ERLANG_COMPILER_OPT_BASE)                                 \
			$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES) \
			$(SEAPLUS_DEBUG_OPT)

endif


## Parse-transform related section.


# List here all the directories expected to contain parse transforms:
#
# (to be overridden by each upper layer)
#
ifndef PARSE_TRANSFORM_DIRS

	   PARSE_TRANSFORM_DIRS = $(SEAPLUS_TOP)/src

endif


# So that the (compilation of the) Seaplus parse transform can benefit from the
# Myriad parse transform:
#
ERLANG_COMPILER_OPT_FOR_PT = $(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MYRIAD)


# Variable defined to be re-usable by upper layers, so that compiling their own
# parse transforms can take advantage of the Seaplus one:
#
ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_SEAPLUS =                \
					$(OVERALL_PZ_OPT)                            \
					-pz $(SEAPLUS_TOP)/src                       \
					'+{parse_transform,seaplus_parse_transform}'


# We conditionally defined this option, as upper layers may want to trigger
# their own parse transforms *instead* (probably that will call directly, by
# themselves, at the Erlang level, this 'SEAPLUS' parse transform):
#
ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT

	   ERLANG_COMPILER_PARSE_TRANSFORM_OPT = $(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_SEAPLUS)

endif


# Variable defined to be re-usable by upper layers, so that compiling their own
# parse transforms can take advantage of the Seaplus one:
#
COMPILER_PARSE_TRANSFORM_FOR_SEAPLUS_CLASSES_OPT = \
					'+{parse_transform,seaplus_parse_transform}'


# Linking section.

C_LINKER := $(C_COMPILER)

# -shared probably unwelcome with libtool:
C_LINKER_OPT := -g

C_LINKER_LIBS := $(ERL_INTERFACE_LIB) -lerl_interface -lei -lpthread



# Checking section.

# For later reuse in the next layer of the software stack:
SEAPLUS_PLT_FILE := $(SEAPLUS_TOP)/seaplus.plt


# This is the merged PLT of the level just below in the software stack.
# For 'SEAPLUS', it is the PLT of 'Myriad':
ifndef PREDECESSOR_PLT
	   PREDECESSOR_PLT = $(MYRIAD_PLT_FILE)
endif

# To use a generic rule to produce it (unless already defined):
ifndef PLT_FILE
	   PLT_FILE = $(PACKAGE_TOP)/$(PACKAGE_NAME).plt
endif




# Foobar-related section.

FOOBAR_ROOT = $(SEAPLUS_TOP)/tests/c-test/foobar

FOOBAR_INC = $(FOOBAR_ROOT)/inc

FOOBAR_INC_OPT = -I$(FOOBAR_INC)

FOOBAR_LIB_DIR = $(FOOBAR_ROOT)/lib

FOOBAR_LIBRARY = libfoobar.so

FOOBAR_LIB_PATH = $(FOOBAR_LIB_DIR)/$(FOOBAR_LIBRARY)




# We define the path to the Myriad layer:
#
# (it may be a symbolic link pointing to the actual Myriad package to be used,
# which itself may be either a 'Ceylan-Myriad' directory or a 'myriad' one)
#
ifneq ($(wildcard $(SEAPLUS_TOP)/../myriad),)

	MYRIAD_TOP = $(SEAPLUS_TOP)/../myriad

else

	# Default:
	MYRIAD_TOP = $(SEAPLUS_TOP)/../Ceylan-Myriad

endif
