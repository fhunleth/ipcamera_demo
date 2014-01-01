#!/bin/sh

# Check the environment for whether we're cross-compiling or not

if [ -z "$NERVES_SDK_SYSROOT" ]; then
	echo "Generating a host release (Did you forget to load the nerves environment?)"
	relx
else
	echo "Generating a cross-compiled release"
	relx --system_libs $NERVES_SDK_SYSROOT/usr/lib/erlang/lib
fi

# Trim up the generated release
find _rel -name "*~" -type f -print0 | xargs -0 rm -f
find _rel -name "src" -type d -print0 | xargs -0 rm -fr
find _rel -name "c_src" -type d -print0 | xargs -0 rm -fr
find _rel -name "include" -type d -print0 | xargs -0 rm -fr
find _rel -name "obj" -type d -print0 | xargs -0 rm -fr

