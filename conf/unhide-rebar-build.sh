#!/bin/sh

# Removes the side effects in source tree of conf/fix-rebar-build.sh.

to_rename=$(find src test -name '*.erl-hidden')

# Applies only if needed:
echo "Renaming back ${to_rename}"
for f in ${to_rename}; do

	corrected_f=$(echo $f|sed 's|\.erl-hidden$|.erl|1')
	/bin/mv -f $f ${corrected_f}

done
