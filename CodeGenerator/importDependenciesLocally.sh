#!/bin/bash
#
# Use this script to import the CodeGenerator dependencies locally

# -s

#************************************
#***** Configuration variables ******
#************************************

# By default don't rebuild LIBXSMM is nothing changed
REBUILD_LIBXSMM=false

# Change this if Jinja2 is available with your python3.
JINJA2_ALREADY_AVAILABLE=false #false (default) => import Jinja2 and MarkupSafe locally, true => skip it.

# Delete libxsmm doc+sample directories to save space
DELETE_UNUSED_LIBXSMM_SAMPLE_AND_DOC=true #true (default) => delete, false => keep it. Sample + Documentation is ~200MB.

# SuperMUC has a very restrictive firewall
# Use the github URL below after you logged in to SuperMUC via
# ssh -R 12345:github.com:9418 <Your Login>@<SuperMUC Login Node> (port 12345 is arbitrarily chosen)
#GITHUB_URL="git://localhost:12345"
if [ $# -eq 0 ]; then
    GITHUB_URL="https://github.com/"
else
    while getopts "hsr" opt; do
    case $opt in
        h) echo "Help"
           echo \
'''
Use this script to import the CodeGenerator dependencies locally.

This script will import:
	* jinja2       the CodeGenerator'\''s template engine
	* markupsafe   jinja2'\''s dependency
	* libxsmm      generates assembly gemms for matmul operations
into CodeGenerator/dependencies

Options:
	-h Show this help
	-s Import over ssh (git@github.com) instead of https (https://github.com/)
	-r Force libxsmm to be rebuild
'''
        exit;;
        s) echo "Clone over ssh"
           GITHUB_URL="git@github.com:";;
        r) echo "Force rebuild of Libxsmm"
           REBUILD_LIBXSMM=true;;
    esac
    done
fi


# Git paths
JINJA_GIT_URL=$GITHUB_URL"pallets/jinja.git"
MARKUPSAFE_GIT_URL=$GITHUB_URL"pallets/markupsafe.git"
LIBXSMM_GIT_URL=$GITHUB_URL"hfp/libxsmm.git"
# Local import paths
JINJA_LOCAL_DIR="dependencies/jinja"
MARKUPSAFE_LOCAL_DIR="dependencies/markupsafe"
LIBXSMM_LOCAL_DIR="dependencies/libxsmm"


#************************************
#********** Import steps ************
#************************************

# local var to resolve relative path correctly
scriptDir=$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")
currentLocation=$(pwd)

# move to the CodeGenerator directory
cd "$scriptDir"

if [ "$JINJA2_ALREADY_AVAILABLE" = false ] ; then
  echo "Importing Jinja2 and MarkupSafe"
  # import or update jinja2 to CodeGenerator/dependencies/jinja
  if [ -d "$JINJA_LOCAL_DIR" ] && [ -e "$JINJA_LOCAL_DIR"/.git ]; then
    echo "Jinja2 already imported."
    cd "$JINJA_LOCAL_DIR"
    #Compare master branch
    LOCAL=$(git rev-parse master)
    REMOTE=$(git rev-parse origin/master)
    if [ $LOCAL = $REMOTE ]; then
      echo "Up-to-date"
    else
      echo "Updating..."
      git pull
    fi
    cd "$scriptDir"
  else 
    echo "Cloning Jinja2 from $JINJA_GIT_URL"
    git clone "$JINJA_GIT_URL" "$JINJA_LOCAL_DIR"
  fi

  # import or update markupsafe to CodeGenerator/dependencies/markupsafe
  if [ -d "$MARKUPSAFE_LOCAL_DIR" ] && [ -e "$MARKUPSAFE_LOCAL_DIR"/.git ]; then
    echo "MarkupSafe already imported."
    cd "$MARKUPSAFE_LOCAL_DIR"
    #Compare master branch
    LOCAL=$(git rev-parse master)
    REMOTE=$(git rev-parse origin/master)
    if [ $LOCAL = $REMOTE ]; then
      echo "Up-to-date"
    else
      echo "Updating..."
      git pull
    fi
    cd "$scriptDir"
  else 
    echo "Cloning MarkupSafe from $MARKUPSAFE_GIT_URL"
    git clone "$MARKUPSAFE_GIT_URL" "$MARKUPSAFE_LOCAL_DIR"
  fi
else
  echo "Jinja2 is already available with python3"
fi

# import or update libxsmm to CodeGenerator/dependencies/libxsmm
if [ -d "$LIBXSMM_LOCAL_DIR" ] && [ -e "$LIBXSMM_LOCAL_DIR"/.git ]; then
  echo "LIBXSMM already imported."
  cd "$LIBXSMM_LOCAL_DIR"
  # Compare release branch
  LOCAL=$(git rev-parse release)
  REMOTE=$(git rev-parse origin/release)
  if [ $LOCAL = $REMOTE ]; then
    echo "Up-to-date"
  else
    echo "Updating..."
    REBUILD_LIBXSMM=true
    if [ "$DELETE_UNUSED_LIBXSMM_SAMPLE_AND_DOC" = true ] ; then
      git stash -q     #silently stash the changes (deleted directories)
      git pull
      git stash pop -q #silently unstash the changes (deleted directories)
      rm -rf samples/       #delete potentially new stuff
      rm -rf documentation/ #delete potentially new stuff
    else
      git pull
    fi
  fi
  cd "$scriptDir"
else 
  echo "Cloning LIBXSMM from $LIBXSMM_GIT_URL"
  git clone -b release --single-branch "$LIBXSMM_GIT_URL" "$LIBXSMM_LOCAL_DIR" #only clone the release branch
  if [ "$DELETE_UNUSED_LIBXSMM_SAMPLE_AND_DOC" = true ] ; then
    cd "$LIBXSMM_LOCAL_DIR"
    rm -rf samples/
    rm -rf documentation/
    cd "$scriptDir"
  fi
fi

# build libxsmm
if [ ! -d "$LIBXSMM_LOCAL_DIR"/bin ] || [ ! -e "$LIBXSMM_LOCAL_DIR"/bin/libxsmm_gemm_generator ]; then
  REBUILD_LIBXSMM=true
fi
if [ "$REBUILD_LIBXSMM" = true ]; then
  echo "Build libxsmm gemm generator"
  cd "$LIBXSMM_LOCAL_DIR"
  make realclean
  make generator
  cd "$scriptDir"
fi

# move back to where the script was called
cd "$currentLocation"
