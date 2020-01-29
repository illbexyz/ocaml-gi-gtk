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
    libs="gobject glib gio atk pango gdk gdkpixbuf gtk"
else
    libs="gtk"
fi

# From: https://stackoverflow.com/questions/59895/how-to-get-the-source-directory-of-a-bash-script-from-within-the-script-itself
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

export BASE_OCAML_C=$DIR/base-ocaml/c
export GI_INCLUDES="-I$DIR/bindings/GObject/include -I$DIR/bindings/GLib/include -I$DIR/bindings/Gio/include -I$DIR/bindings/Atk/include -I$DIR/bindings/GdkPixbuf/include -I$DIR/bindings/Gdk/include -I$DIR/bindings/Pango/include"

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
    build GObject
    build GLib
    build Gio
    build Atk
    build GdkPixbuf
    build Gdk
    build Pango
fi

build Gtk