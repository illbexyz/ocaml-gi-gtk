#!/bin/bash

install=false
full=false

while getopts ":if" opt; do
  case ${opt} in
    i ) # process option a
      install=true
      ;;
    f )
      full=true
      ;;
    \? ) echo "Usage: generate.sh [-i] (install), [-f] (full)"
      ;;
  esac
done


if $full; then
    libs="pango gdk gtk"
else
    libs="gtk"
fi

# From: https://stackoverflow.com/questions/59895/how-to-get-the-source-directory-of-a-bash-script-from-within-the-script-itself
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

export BASE_OCAML_C=$DIR/base-ocaml/c
export GDK_INCLUDES=$DIR/bindings/Gdk/include

generate () {
    stack build && stack exec ocaml-gi-gtk-exe $libs
}

build () {
    local lib_name=$1
    pushd bindings/$lib_name > /dev/null
        dune build
        if $install; then
            dune install
        fi
    popd > /dev/null
}

generate

if $full; then
    build Gdk
    build Pango
fi

build Gtk