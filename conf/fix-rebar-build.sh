#!/bin/sh

# Script defined for convenience.

echo "Fixing rebar build from Seaplus hook"

# May not be here if Seaplus is a checkout:
parse_src="src/seaplus_parse_transform.erl"

if [ -f "${parse_src}" ]; then
	/bin/mv -f ${parse_src} ${parse_src}-hidden
fi

target_dir="./_build/default/lib/seaplus/ebin/"

if [ ! -d "${target_dir}" ]; then
	target_dir="../seaplus/ebin/"
fi

echo "rebar fix for hook: selected target directory for Seaplus parse transform is '${target_dir}'."

parse_beam="src/seaplus_parse_transform.beam"

if [ -f "${parse_beam}" ]; then

	/bin/cp -f ${parse_beam} ${target_dir}

else

	echo "Warning: no ${parse_beam} found from $(pwd); content: $(tree)"

fi

echo "rebar build fixed from Seaplus hook"
