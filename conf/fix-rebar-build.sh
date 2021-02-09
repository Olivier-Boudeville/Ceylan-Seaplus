#!/bin/sh

# Script defined for convenience and reliability.

# Recreates a proper rebar3 landscape, based on our build, so that rebar3 will
# not attempt (and fail) to recreate BEAM files that are already correct as they
# are.
#

# Even if copying the relevant header/source/BEAM files with relevant
# timestamps, for some reason rebar will still find it relevant to rebuild some
# of them. So we have to hide the corresponding sources as well...

project_name="$1"

if [ -z "${project_name}" ]; then

	echo "  Error, project name not set." 1>&2

	exit 5

fi

echo "Fixing rebar build for ${project_name}: building all first, from $(pwd)."
#tree

make -s all 2>/dev/null


# Element locations may vary depending on whether this project is the main
# target or a dependency, or a checkout.

target_base_dir="./_build/default/lib/${project_name}"

if [ ! -d "${target_base_dir}" ]; then

	echo "(project based directory is not '${target_base_dir}')"

	target_base_dir="../${project_name}"

	if [ ! -d "${target_base_dir}" ]; then

		echo "(project based directory is not '${target_base_dir}')"

		echo "  Error, target directory not found for ${project_name}." 1>&2

		exit 10

	fi

fi

echo "Copying built-related elements in the '${target_base_dir}' target tree."

# Transforming a potentially nested hierarchy (tree) into a flat directory:
# (operation order allows proper timestamp ordering)
#
# Note that 'ebin' is bound to be an actual directory yet (probably if dev_mode
# has been set to true in rebar.config), 'include', 'priv' and 'src' may be
# symlinks to their original versions.
#
# As they could be nested (a tree rather than flat directories), we remove any
# pre-existing directory and replace it with a flat copy of our own).

target_hdr_dir="${target_base_dir}/include"

if [ -L "${target_hdr_dir}" ]; then

	echo "Replacing the ${target_hdr_dir} symlink by an actual directory."
	/bin/rm -f "${target_hdr_dir}"
	mkdir "${target_hdr_dir}"

elif [ ! -d "${target_hdr_dir}" ]; then

	echo "Creating the non-existing ${target_hdr_dir} directory."
	mkdir "${target_hdr_dir}"

else

	echo "(${target_hdr_dir} directory already existing)"
	#echo "Unexpected target ${target_hdr_dir}: $(ls -l ${target_hdr_dir})" 1>&2
	#exit 5

fi


all_hdrs=$(find src test include -name '*.hrl' 2>/dev/null)

echo "  Copying all headers to ${target_hdr_dir}: ${all_hdrs}"

for f in ${all_hdrs}; do
	/bin/cp -f $f ${target_hdr_dir}
done



target_src_dir="${target_base_dir}/src"

if [ -L "${target_src_dir}" ]; then

	echo "Replacing the ${target_src_dir} symlink by an actual directory."
	/bin/rm -f "${target_src_dir}"
	mkdir "${target_src_dir}"

elif [ ! -d "${target_src_dir}" ]; then

	echo "Creating the non-existing ${target_src_dir} directory."
	mkdir "${target_src_dir}"

fi


all_srcs=$(find src test -name '*.erl' 2>/dev/null)

echo "  Copying all sources to ${target_src_dir} then hiding the original ones: ${all_srcs}"

for f in ${all_srcs}; do
	/bin/cp -f $f ${target_src_dir}
	/bin/mv -f $f $f-hidden
done



target_beam_dir="${target_base_dir}/ebin"

if [ -L "${target_beam_dir}" ]; then

	echo "Replacing the ${target_beam_dir} symlink by an actual directory."
	/bin/rm -f "${target_beam_dir}"
	mkdir "${target_beam_dir}"

elif [ ! -d "${target_beam_dir}" ]; then

	echo "Creating the non-existing ${target_beam_dir} directory."
	mkdir "${target_beam_dir}"

fi


all_beams=$(find src test -name '*.beam' 2>/dev/null)

echo "  Copying all BEAM files to ${target_beam_dir}: ${all_beams}"

for f in ${all_beams}; do
	/bin/cp -f $f ${target_beam_dir}
done

tree ${target_base_dir_build}/${project_name}

echo "Rebar build fixed for ${project_name}."
