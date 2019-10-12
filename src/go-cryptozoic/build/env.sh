#!/bin/sh

set -e

if [ ! -f "build/env.sh" ]; then
    echo "$0 must be run from the root of the repository."
    exit 2
fi

# Create fake Go workspace if it doesn't exist yet.
workspace="$PWD/build/.workspace"
root="$PWD"
gvcdir="$workspace/src"
if [ ! -L "$gvcdir/go-cryptozoic" ]; then
    mkdir -p "$gvcdir"
    cd "$gvcdir"
    ln -s ../../../. go-cryptozoic
    cd "$root"
fi

# Set up the environment to use the workspace.
GOPATH="$workspace"
export GOPATH

# Run the command inside the workspace.
cd "$gvcdir/go-cryptozoic"
PWD="$gvcdir/go-cryptozoic"

# Launch the arguments with the configured environment.
exec "$@"
