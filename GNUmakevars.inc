# Prerequisite: SEAPLUS_TOP must be set.


# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)

	ENABLE_DEBUG = true

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)

	ENABLE_DEBUG = false

else

	$(error Invalid Seaplus execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif


# One may define here:
# EXECUTION_TARGET := production / development [the default]
# ENABLE_DEBUG := false / true [the default]


# Tells whether the debug mode will be activated for the next Seaplus modules to
# be built (maximum performance versus extended checkings).
#
ifeq ($(ENABLE_DEBUG),true)
	SEAPLUS_DEBUG_OPT := -DDEBUG_SEAPLUS -DSEAPLUS_ENABLE_LOG=1
	ENABLE_SEAPLUS_DEBUG_OPT = -Dseaplus_debug_mode
else
	SEAPLUS_DEBUG_OPT := -DSEAPLUS_ENABLE_LOG=0
	ENABLE_SEAPLUS_DEBUG_OPT =
endif



# Defining (optional) layer-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management):

# Topics covered by flags:
#  - general: how Seaplus behaves in general
#  - port: how the service port is managed
#  - driver: information about the Seaplus-generated driver

# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	#SEAPLUS_DEBUG_FLAGS += -Dseaplus_debug_general
	#SEAPLUS_DEBUG_FLAGS += -Dseaplus_debug_port
	#SEAPLUS_DEBUG_FLAGS += -Dseaplus_debug_driver
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	#SEAPLUS_CHECK_FLAGS += -Dseaplus_check_driver
endif


ERLANG_COMPILER_OPT_BASE += $(SEAPLUS_DEBUG_FLAGS) \
							$(SEAPLUS_CHECK_FLAGS)



# Useful to target for example the root of the current layer (ex: to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(SEAPLUS_TOP)
endif


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := Ceylan-Seaplus
endif


# PROJECT_SHORT_NAME should be defined on a per-project basis.
ifndef PROJECT_SHORT_NAME
	PROJECT_SHORT_NAME := seaplus
endif


# PROJECT_NAME_FOR_REBAR should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_REBAR
	PROJECT_NAME_FOR_REBAR := $(PROJECT_SHORT_NAME)
endif


# The uniquely-defined version of this layer, for all uses (including rebar and
# hex):
#
SEAPLUS_VERSION := 1.1.6


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(SEAPLUS_VERSION)
endif



ifndef PACKAGE_TOP
	PACKAGE_TOP = $(SEAPLUS_TOP)
endif


# For any quick, local, non release-packaged update thereof:
MYRIAD_CHECKOUT_BUILD = $(LAYER_TOP)/_checkouts/myriad
MYRIAD_SIBLING_BUILD = $(LAYER_TOP)/../myriad


# The OTP tree of the Myriad application within a local OTP _build tree:
MYRIAD_LOCAL_APP := $(LAYER_TOP)/_build/default/lib/myriad

# Where Myriad BEAMs are to lie when used as an OTP application:
MYRIAD_LOCAL_EBIN := $(MYRIAD_LOCAL_APP)/ebin



SEAPLUS_RELEASE_BASE := seaplus-$(PROJECT_VERSION)

SEAPLUS_RELEASE_ARCHIVE_ZIP := $(SEAPLUS_RELEASE_BASE).zip
SEAPLUS_RELEASE_ARCHIVE_BZ2 := $(SEAPLUS_RELEASE_BASE).tar.bz2
SEAPLUS_RELEASE_ARCHIVE_XZ  := $(SEAPLUS_RELEASE_BASE).tar.xz


# Source section.
SEAPLUS_SRC := $(SEAPLUS_TOP)/src

# BEAM path section.
SEAPLUS_BEAM := $(SEAPLUS_SRC)

# Include path section.
SEAPLUS_INC = -I$(SEAPLUS_TOP)/include


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
SEAPLUS_BEAM_DIRS := $(SEAPLUS_BEAM) $(SEAPLUS_TOP)/..


# For OTP releases and all:

SEAPLUS_REBAR_BUILD_BASE = $(REBAR_BUILD_DIR)/lib/seaplus


# When building Seaplus in an OTP context from its usual (GIT) root (ex: with
# 'make rebar3-application'), the BEAMs of Myriad are to be found in the OTP
# build tree - rather than in a supposedly fully-built usual root for Myriad:
#
# (these information are useful only at compilation-time, in order to locate the
# needed parse transforms and their dependencies; at runtime, the OTP rules
# ensure that the relevant ebin directories are in the code path)
#
# Note that this implies that the BEAM files in all ebin directories should be
# up to date with the ones in the build trees, otherwise hard-to-debug
# discrepancies may happen (these OTP/rebar-related directories act as default
# catch-alls, should no prior directory correspond in the BEAM paths).
#
MYRIAD_OTP_BEAM_DIR_FROM_USUAL = $(LAYER_TOP)/$(MYRIAD_REBAR_BUILD_BASE)/ebin


# In an OTP/rebar3-style application layout (not speaking of sibling base
# directories here), at compilation time as well, modules compiled (directly or
# not) by the Myriad parse transform will expect its modules to be located in:
#
# (a third place could be considered, in the _build tree of Myriad itself, yet
# it would not provide any additional chance)
#
MYRIAD_OTP_BEAM_DIR_FROM_OTP = $(LAYER_TOP)/../myriad/ebin/


# To locate Myriad modules from all OTP contexts:
MYRIAD_PARSE_TRANSFORMS_PZ_OPT = -pz $(MYRIAD_OTP_BEAM_DIR_FROM_USUAL) \
								 -pz $(MYRIAD_OTP_BEAM_DIR_FROM_OTP)


# Library referencing section.

SEAPLUS_LIB = libseaplus-$(SEAPLUS_VERSION).so
SEAPLUS_LIB_DIR = $(SEAPLUS_TOP)/src
SEAPLUS_LIB_PATH = $(SEAPLUS_LIB_DIR)/$(SEAPLUS_LIB)
SEAPLUS_LIB_OPT = -L$(SEAPLUS_LIB_DIR) -lseaplus-$(SEAPLUS_VERSION)


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
SEAPLUS_BEAM_DIRS = $(SEAPLUS_TOP)/src


# We rely on Myriad as well:
BEAM_DIRS += $(SEAPLUS_BEAM_DIRS)


ifndef VM_TEST_NAME
	VM_NAME := seaplus_debug
endif

ifndef VM_TEST_NAME
	VM_TEST_NAME := seaplus_test
endif




# Build section.


# libtool not used anymore, was making the build degenerate in a mess:

#LIBTOOL = $(shell which libtool 2>/dev/null)

# This is a C-based (not C++) example:
#LIBTOOL_C_OPT += --tag=CC --verbose



# Compiler section.

SEAPLUS_BASE_NAME := seaplus



# C subsection.


ERL_INTERFACE_INC = -I$(ERL_INTERFACE)/include
ERL_INTERFACE_LIB = -L$(ERL_INTERFACE)/lib

C_COMPILER := $(shell which gcc 2>/dev/null)


C_INC = $(ERL_INTERFACE_INC) -I. $(SEAPLUS_INC)
C_LIB = $(ERL_INTERFACE_LIB)

ifndef C_COMPILER_OPT
	C_COMPILER_OPT = $(C_INC) $(SEAPLUS_DEBUG_OPT) -Wall -Werror -g
endif


ifndef C_COMPILER_OPT_FOR_OBJ
	C_COMPILER_OPT_FOR_OBJ = $(C_COMPILER_OPT) -fPIC
endif



# Erlang subsection.


# Base Erlang installation either hardcoded or deduced from selected
# interpreter:
#
#ERL_BASE := $(HOME)/Software/Erlang/Erlang-current-install
#ERL_BASE = $(shell dirname $(ERLANG_INTERPRETER) 2>/dev/null)/..


# As installation structures may vary:

# Typically if installed thanks to our install-erlang.sh script:
ERL_BASE_FIRST_CANDIDATE = $(shell dirname $(ERLANG_INTERPRETER) 2>/dev/null)/..

# Typically if using Github's continuous integration:
ERL_BASE_SECOND_CANDIDATE = $(shell dirname $(ERLANG_INTERPRETER) 2>/dev/null)/../../../..

ERL_BASE = $(shell if [ -d $(ERL_BASE_FIRST_CANDIDATE)/lib/erlang/lib/erl_interface-* ]; then echo "$(ERL_BASE_FIRST_CANDIDATE)" ; else echo "$(ERL_BASE_SECOND_CANDIDATE)" ; fi)


# As version numbers are in the way:
ERL_INTERFACE = $(shell /bin/ls -d $(ERL_BASE)/lib/erlang/lib/erl_interface-* 2>/dev/null)


# Allows to ensure that we have a proper base installation (and not for example
# an empty directory due to a failed filtering):
#
ERL_BASE_CONTENT = $(ERL_BASE)/lib/erlang


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

	# We list here all the Seaplus-level prerequisites of the Seaplus
	# parse-transform:
	#
	# - seaplus_info: to manage layer-level information

	#BOOTSTRAP_MODULES = $(SEAPLUS_TOP)/src/seaplus_info.beam

	# Needed to be defined and set to empty, otherwise deemed undefined and
	# inheriting thus the Myriad ones:
	#
	BOOTSTRAP_MODULES = $(empty)

endif

ifndef COMPILER_OPT_FOR_SEAPLUS_MODULES

	COMPILER_OPT_FOR_SEAPLUS_MODULES =                     \
		$(ERLANG_COMPILER_OPT_BASE)                        \
		$(ENABLE_SEAPLUS_DEBUG_OPT)                        \
		$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_SEAPLUS)

endif


# With Seaplus, *all* modules (being Seaplus stubs or not) shall be compiled
# with the Seaplus parse-transform, which operates only if needed, and triggers
# the Myriad one appropriately:
#
ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES

	# All modules (being Seaplus stubs or not) shall be compiled with the
	# Seaplus parse-transform, which operates only if needed, driving the Myriad
	# one appropriately:
	#
	ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES = \
		$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_SEAPLUS)     \
		$(MYRIAD_PARSE_TRANSFORMS_PZ_OPT)
endif


# Conditionally defined so that upper layer may update these settings (ex:
# by adding pz directories):
#
ifndef ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES

	# Same rules for all modules:
	ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES =                          \
			$(ERLANG_COMPILER_OPT_BASE)                                 \
			$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES) \
			$(ENABLE_SEAPLUS_DEBUG_OPT)

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
ERLANG_COMPILER_OPT_FOR_PT = $(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MYRIAD) \
							 $(ENABLE_SEAPLUS_DEBUG_OPT)


# Variable defined to be re-usable by upper layers, so that compiling their own
# parse transforms can take advantage of the Seaplus one:
#
# (to find seaplus_parse_transform.beam, both the Seaplus 'src' and 'ebin'
# directories are specified: the former for our native build, the latter for a
# rebar3-based one)
#
ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_SEAPLUS = \
	$(OVERALL_PZ_OPT)                             \
	-Dseaplus_root=\"$(SEAPLUS_TOP)\"             \
	-pz $(SEAPLUS_TOP)/ebin                       \
	-pz $(SEAPLUS_TOP)/src                        \
	$(MYRIAD_PARSE_TRANSFORMS_PZ_OPT)             \
	'+{parse_transform,seaplus_parse_transform}'


# We conditionally defined this option, as upper layers may want to trigger
# their own parse transforms *instead* (probably that they will call directly,
# by themselves, at the Erlang level, this 'Seaplus' parse transform):
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

# Not used anymore since Erl_interface is deprecated: -lerl_interface
C_LINKER_LIBS = $(ERL_INTERFACE_LIB) -lei -lpthread



# Checking section.

# For later reuse in the next layer of the software stack:
SEAPLUS_PLT_FILE := $(SEAPLUS_TOP)/Ceylan-Seaplus.plt


# This is the merged PLT of the level just below in the software stack.
# For 'Seaplus', it is the PLT of 'Myriad':
#
ifndef PREDECESSOR_PLT
	PREDECESSOR_PLT = $(MYRIAD_PLT_FILE)
endif



# Foobar-related section.

FOOBAR_ROOT = $(SEAPLUS_TOP)/test/c-test/foobar

FOOBAR_INC = $(FOOBAR_ROOT)/inc

FOOBAR_INC_OPT = -I$(FOOBAR_INC)

FOOBAR_LIB_DIR = $(FOOBAR_ROOT)/lib

FOOBAR_LIBRARY = libfoobar.so

FOOBAR_LIB_PATH = $(FOOBAR_LIB_DIR)/$(FOOBAR_LIBRARY)



# Documentation section.


ifndef EDOC_OPTS
	EDOC_OPTS = '[{dir,"$(API_DOC_ROOT)"}, {def,{vsn,"$(PROJECT_VERSION)"}}, {preprocess,true}, {includes,["$(LAYER_TOP)/include", "$(MYRIAD_TOP)/.."]}, {overview, "$(LAYER_TOP)/overview.edoc"}]'
endif



# We define the path to the Myriad layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, Ceylan dependencies will be
# searched (in that order, the one enforced in otp_utils) in a local _checkouts
# child directory (if checkout directories are used and if the top-level package
# of interest is the current one), otherwise in sibling directories (i.e. whose
# build root is at the same level as this one).
#
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (ex: myriad) as opposed to
# project/repository names (ex: Ceylan-Myriad):
#
# First priority is an existing checkout directory:
ifneq ($(wildcard $(SEAPLUS_TOP)/_checkouts/myriad/.),)
	MYRIAD_TOP = $(SEAPLUS_TOP)/_checkouts/myriad
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(SEAPLUS_TOP)/../myriad/.),)
		MYRIAD_TOP = $(SEAPLUS_TOP)/../myriad
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(SEAPLUS_TOP)/_build/default/lib/myriad/.),)
			MYRIAD_TOP = $(SEAPLUS_TOP)/_build/default/lib/myriad
		else
			FORCE_EVAL := $(error Unable to find the build root of Ceylan-Myriad from '$(realpath .)'.)
		endif
	endif
endif
