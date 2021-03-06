#!/bin/bash
# a 2017 version of the ExaHyPE build infos
#
# This script is called by the Makefile with tons of parameters, all of the form K=V.
# That is, a call looks like
#  ./generate-buildinfos.sh COMPILER=gcc MODE=Release "PEANO_PATH=/my/freaking path/"
# This script then goes and dumps these parameters as well as extracting
# something from them.

echo "/* This file is autogenerated by generate-buildinfo.sh */"
echo
echo "#ifndef _EXAHYPE_BUILD_INFO_H_"
echo "#define _EXAHYPE_BUILD_INFO_H_"
echo
echo "#define EXAHYPE_BUILDINFO_AVAILABLE"
echo "#define EXAHYPE_BUILD_DATE          "\"$(LC_ALL=en_US.utf8 date)\"
echo "#define EXAHYPE_BUILD_HOST          "\"$(hostname)\"
echo
echo "/* Strings passed by the Makefile */"
echo "#define EXAHYPE_BUILD_INFO \\"
for assignment in "$@"; do
echo "    \"$assignment\\n\" \\";
done
echo "    \"\""
echo

# extract information about the git,svn repositories and dump them into the code

# we expect the paths to be absolute or relative to the current working directory
oldpwd="$(pwd)"
exahype_path=$(for as in "$@"; do echo $as | grep "EXAHYPE_PATH" | sed 's/.*=\s*//'; done)
peano_path=$(for as in "$@"; do echo $as | grep "PEANO_KERNEL_PEANO_PATH" | sed 's/.*=\s*//'; done)

# this is to support out of tree setups where there is no direct access to the
# repository (was not copied for speed reasons) but the oot setup code was nice
# and prepared a "static" version of the buildinfo where we can extract all the
# repository information from.
static_repo_info="buildinfo-out-of-tree.h"
getstatic_repo_info() {
	# expects argument: the key what variable to get
	argumentKey="$1"
	if STATIC_LINES=$(grep -i "#define $argumentKey" "$static_repo_info"); then
		echo "/* Information extracted from $static_repo_info */"
		echo "/* This is used when doing an out-of-tree build, ie. there is no reachable git/svn repository */"
		# this makes use of C string joining: "a" "b" => "ab"
		echo "$STATIC_LINES \" [static out-of-tree]\""
	else

		echo "#define $argumentKey \"Malformed static repository info.\""
	fi
}

echo
echo "/*"
echo " * ExaHyPE Git Repository information extraction"
echo " * Extracted from git repository in $exahype_path"
echo " */"
echo

cd "$exahype_path"
if which git &>/dev/null && git rev-parse --git-dir >/dev/null 2>&1; then
	echo "/* Information collected with $(git --version | head -n1) */"

	BRANCH="$(git rev-parse --abbrev-ref HEAD)"
	SHORTREF="$(git rev-parse --short HEAD)"
	DATE="$(git log -1 --format=%cd --date=local)"

	echo "#define EXAHYPE_GIT_INFO \"$BRANCH  $SHORTREF $DATE\""
elif [[ -e "$static_repo_info" ]]; then
	getstatic_repo_info "EXAHYPE_GIT_INFO"
else
	echo "/* No git repository found or git binary not available */"
	echo "#define EXAHYPE_GIT_INFO \"No git/repository available\""
fi

cd "$oldpwd"

echo
echo "/*"
echo " * Peano Git Repository information extraction"
echo " * Extracted from git repository in $peano_path"
echo " */"
echo

cd "$peano_path"
if which git &>/dev/null && git rev-parse --git-dir >/dev/null 2>&1; then
	echo "/* Information collected with $(git --version | head -n1) */"
	
	BRANCH="$(git rev-parse --abbrev-ref HEAD)"
	SHORTREF="$(git rev-parse --short HEAD)"
	DATE="$(git log -1 --format=%cd --date=local)"

	echo "#define PEANO_GIT_INFO \"$BRANCH  $SHORTREF $DATE\""
	
elif [[ -e "$static_repo_info" ]]; then
	getstatic_repo_info "PEANO_GIT_INFO"
else
	echo "/* No git repository found or git binary not available */"
	echo "#define PEANO_GIT_INFO \"No git/repository available\""
	
fi

echo "#define PEANO_SVN_INFO  PEANO_GIT_INFO /* transition time */"
echo
echo "/*"
echo "    Peano version check"
echo 
echo "    FIXME TODO This is the worst place ever to hook in version"
echo "               requirements. Please move the following to somewhere"
echo "               suitable, such as the Peano startup phase."
echo
echo "*/"
echo
echo "#include \"peano/version.h\""
echo 
echo "#endif /* EXAHYPE_BUILD_INFO_H */"
echo "#if PEANO_VERSION<2509 "
echo "#error Old Peano version. Version 2509 required. Please update your Peano installation."
echo "#endif"


### Add any other repository dependency here
### if you like.
