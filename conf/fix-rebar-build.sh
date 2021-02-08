#!/bin/sh

# Script defined for convenience.

/bin/mv -f src/seaplus_parse_transform.erl src/seaplus_parse_transform.erl-hidden

target_dir="./_build/default/lib/seaplus/ebin/"

if [ ! -d "${target_dir}" ]; then
	target_dir="../seaplus/ebin"
fi

/bin/cp -f src/seaplus_parse_transform.beam "${target_dir}"
