#!/bin/bash
#
# Peano is nowadays managed as a git submodule. That makes it easy
# to keep in sync. However, you also need access to the Peano
# repository (you find the URL in the file ../.gitmodules).
#
# If you don't have (read) access to the repository, you can use
# the script ./fetchSnapshot.sh  to download a public available
# tarball instead.
#

#CONFIGURATION VALUE
pathToExaHyPETopLevelFromHere=".."

# local var to resolve relative path correctly
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")") #Should be the Submodules directory
currentLocation=$(pwd)
pathToTopLevel="$scriptDir"/"$pathToExaHyPETopLevelFromHere"


# Move to the Submodules directory
cd "$scriptDir"

# By default don't rebuild LIBXSMM is nothing changed
REBUILD_LIBXSMM=false

update_Peano() {
	#Peano
	if [ ! -d Peano ]; then
		mkdir Peano
	fi
	if [ ! -f Peano/.git ]; then
		echo "Initialize Peano submodule"
		cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
		git submodule update --init Submodules/Peano
		cd "$scriptDir" #move back
	else
		echo "Update Peano submodule"
		if [ "$toNewest" = true ]; then
			cd Peano
			git pull origin master
			cd ..
		else
			cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
			git submodule update Submodules/Peano
			cd "$scriptDir" #move back
		fi
	fi
}

update_others() {
	#Jinja2
	if [ ! -d jinja ]; then
		mkdir jinja
	fi
	if [ ! -f jinja/.git ]; then
		echo "Initialize jinja submodule"
		cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
		git submodule update --init Submodules/jinja
		cd "$scriptDir" #move back
	else
		echo "Update jinja submodule"
		if [ "$toNewest" = true ]; then
			cd jinja
			git pull origin master
			cd ..
		else
			cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
			git submodule update Submodules/jinja
			cd "$scriptDir" #move back
		fi
	fi
	#Markupsafe
	if [ ! -d markupsafe ]; then
		mkdir markupsafe
	fi
	rm -r markupsafe # there seems to be a dependency issue with jinja otherwise
	if [ ! -f markupsafe/.git ]; then
		echo "Initialize markupsafe submodule"
		cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
                git submodule update --init Submodules/markupsafe
		cd "$scriptDir" #move back
	else
		echo "Update markupsafe submodule"
		if [ "$toNewest" = true ]; then
			cd markupsafe
			git pull origin master
			cd ..
		else
			cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
			git submodule update Submodules/markupsafe
			cd "$scriptDir" #move back
		fi
	fi
	#attrs
	if [ ! -d attrs ]; then
		mkdir attrs
	fi
	if [ ! -f attrs/.git ]; then
		echo "Initialize attrs submodule"
		cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
		git submodule update --init Submodules/attrs
		cd "$scriptDir" #move back
	else
		echo "Update attrs submodule"
		if [ "$toNewest" = true ]; then
			cd attrs
			git pull origin master
			cd ..
		else
			cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
			git submodule update Submodules/attrs
			cd "$scriptDir" #move back
		fi
	fi
	#pyrsistent
	if [ ! -d pyrsistent ]; then
		mkdir pyrsistent
	fi
	if [ ! -f pyrsistent/.git ]; then
		echo "Initialize pyrsistent submodule"
		cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
		git submodule update --init Submodules/pyrsistent
		cd "$scriptDir" #move back
	else
		echo "Update pyrsistent submodule"
		if [ "$toNewest" = true ]; then
			cd pyrsistent
			git pull origin master
			cd ..
		else
			cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
			git submodule update Submodules/pyrsistent
			cd "$scriptDir" #move back
		fi
	fi
	#jsonschema
	if [ ! -d jsonschema ]; then
		mkdir jsonschema
	fi
	if [ ! -f jsonschema/.git ]; then
		echo "Initialize jsonschema submodule"
		cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
		git submodule update --init Submodules/jsonschema
		cd "$scriptDir" #move back
	else
		echo "Update jsonschema submodule"
		if [ "$toNewest" = true ]; then
			cd jsonschema
			git checkout -- * #undo modifications
			git pull origin master
			cd ..
		else
			cd jsonschema
			git checkout -- * #undo modifications
			cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
			git submodule update Submodules/jsonschema
			cd "$scriptDir" #move back
		fi
	fi
	# comment last two lines of module init file (hot fix)
	cd jsonschema
	sed -i -e "s,^from pkg_resources import get_distribution,#from pkg_resources import get_distribution,g" jsonschema/__init__.py
	sed -i -e "s,^__version__ = get_distribution(__name__).version,#__version__ = get_distribution(__name__).version,g" jsonschema/__init__.py
	cd ..
	#six
	if [ ! -d six ]; then
		mkdir six
	fi
	if [ ! -f six/.git ]; then
		echo "Initialize six submodule"
		cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
		git submodule update --init Submodules/six
		cd "$scriptDir" #move back
	else
		echo "Update six submodule"
		if [ "$toNewest" = true ]; then
			cd six
			git pull origin master
			cd ..
		else
			cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
			git submodule update Submodules/six
			cd "$scriptDir" #move back
		fi
	fi
	#Libxsmm
	if [ ! -d libxsmm ]; then
		mkdir libxsmm
	fi
	if [ ! -f libxsmm/.git ]; then
		echo "Initialize libxsmm submodule"
		cd "$pathToTopLevel" # move to the top level (required for git version below 1.8.4)
		git submodule update --init Submodules/libxsmm
		cd "$scriptDir" #move back
		#Clean documentation to save space
		cd libxsmm
		rm -rf samples/
		rm -rf documentation/
		cd ..
	else
		echo "Update libxsmm submodule"
		cd libxsmm
		LOCAL=$(git rev-parse master)
		REMOTE=$(git rev-parse origin/master)
		if [ $LOCAL = $REMOTE ]; then
			echo "Up-to-date"
		else
			REBUILD_LIBXSMM=true
			git stash -q     #silently stash the changes (deleted directories)
			git pull origin master
			git stash pop -q #silently unstash the changes (deleted directories)
			rm -rf samples/       #delete potential new stuff
			rm -rf documentation/ #delete potential new stuff
		fi
		cd ..
	fi
	
	# build libxsmm
	if [ ! -d libxsmm/bin ] || [ ! -e libxsmm/bin/libxsmm_gemm_generator ]; then
		REBUILD_LIBXSMM=true
	fi
	if [ "$REBUILD_LIBXSMM" = true ]; then
		echo "Build libxsmm gemm generator"
		cd libxsmm
		make realclean
		make generator
		cd ..
	fi
}

toNewest=false #Only do submodule update not git pull
schedulePeano=false
scheduleOthers=false
if [ $# -eq 0 ]; then
	schedulePeano=true
	scheduleOthers=true
else
	while getopts hntswpo opt; do
	case $opt in
		h)  echo "-h prints this message"
			echo "-s set submodules url to ssh"
			echo "-t use ssh tunnel (port: 12345) and git protocol (works on SuperMUC)"
			echo "-w set submodules url to https"
			echo "-p only update the Peano submodule"
			echo "-o only update submodules other than Peano"
			echo "-n update dependeices to the newest version instead of the git submodule version"
			exit -1;;
		n)  echo "Will update to newest version instead of submodule version"
			toNewest=true
			schedulePeano=true
			scheduleOthers=true;;
		t)  git config submodule.Submodules/Peano.url    git://localhost:12345/gi26det/Peano.git
			git config submodule.Submodules/jinja.url       git://localhost:12345/pallets/jinja.git
			git config submodule.Submodules/markupsafe.url  git://localhost:12345/pallets/markupsafe.git
			git config submodule.Submodules/attrs.url       git://localhost:12345/python-attrs/attrs.git
			git config submodule.Submodules/pyrsistent.url  git://localhost:12345/tobgu/pyrsistent.git
			git config submodule.Submodules/jsonschema.url  git://localhost:12345/Julian/jsonschema.git
			git config submodule.Submodules/six.url         git://localhost:12345/benjaminp/six.git
			git config submodule.Submodules/libxsmm.url     git://localhost:12345/hfp/libxsmm.git ;;
		s)  git config submodule.Submodules/Peano.url       git@gitlab.lrz.de:gi26det/Peano.git
			git config submodule.Submodules/jinja.url       git@github.com:pallets/jinja.git
			git config submodule.Submodules/markupsafe.url  git@github.com:pallets/markupsafe.git
			git config submodule.Submodules/attrs.url       git@github.com:python-attrs/attrs.git
			git config submodule.Submodules/pyrsistent.url  git@github.com:tobgu/pyrsistent.git
			git config submodule.Submodules/jsonschema.url  git@github.com:Julian/jsonschema.git
			git config submodule.Submodules/six.url         git@github.com:benjaminp/six.git
			git config submodule.Submodules/libxsmm.url     git@github.com:hfp/libxsmm.git ;;
		w)  git config submodule.Submodules/Peano.url       https://gitlab.lrz.de/gi26det/Peano.git
			git config submodule.Submodules/jinja.url       https://github.com/pallets/jinja.git
			git config submodule.Submodules/markupsafe.url  https://github.com/pallets/markupsafe.git
			git config submodule.Submodules/attrs.url       https://github.com/python-attrs/attrs.git
			git config submodule.Submodules/pyrsistent.url  https://github.com/tobgu/pyrsistent.git
			git config submodule.Submodules/jsonschema.url  https://github.com/Julian/jsonschema.git
			git config submodule.Submodules/six.url         https://github.com/benjaminp/six.git
			git config submodule.Submodules/libxsmm.url     https://github.com/hfp/libxsmm.git ;;
		p)  echo "only update Peano"
			schedulePeano=true
			scheduleOthers=false ;;
		o)  echo "only update submodules other than Peano"
			schedulePeano=false
			scheduleOthers=true ;;
	esac
	done
fi
if [ "$scheduleOthers" = true ] ; then
	update_others
fi

# move back to where the script was called
cd "$currentLocation"




