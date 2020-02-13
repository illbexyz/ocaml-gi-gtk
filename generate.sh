#!/bin/bash

install=false
full=false

# The order is important because some libs depends on the previous ones
# and they will throw errors if they are not installed in the right order
avail_libs="glib gobject gio atk cairo pango gdkpixbuf gdk gtk"

lib_name_to_dir () {
  local lib_name=$1
  case "$lib_name" in
    glib      ) echo "GLib";;
    gobject   ) echo "GObject";;
    gio       ) echo "Gio";;
    atk       ) echo "Atk";;
    cairo     ) echo "cairo";;
    pango     ) echo "Pango";;
    gdkpixbuf ) echo "GdkPixbuf";;
    gdk       ) echo "Gdk";;
    gtk       ) echo "Gtk";;
    *         )
      echo "Error: unknown lib name, exiting"
      exit 1
      ;;
  esac
}

usage () {
  echo "Usage: generate.sh [OPTION] LIB [LIB...]"
  echo ""
  echo "Available libs are: $avail_libs"
  echo ""
  echo "Options:"
  echo "  -h, (help)     Display this help and exit"
  echo "  -f, (full)     Generate every available library. The LIB arguments will be ignored."
  echo "  -i, (install)  After the library generation, it will install it using 'dune install'"
  exit 0
}

while getopts ":ifh" opt; do
  case ${opt} in
    i )
      install=true
      ;;
    f )
      full=true
      ;;
    h ) usage
      ;;
  esac
done

shift $((OPTIND - 1))
lib=$1

if $full; then
    libs=$avail_libs
else
    libs=$lib
fi

# When no lib nor the full argument are specified
if [ -z "$libs" ]; then
  usage
fi

# From: https://stackoverflow.com/questions/59895/how-to-get-the-source-directory-of-a-bash-script-from-within-the-script-itself
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
export BASE_OCAML_C=$DIR/base-ocaml/c

include_dirs () {
  for avail_lib in $avail_libs; do
    local lib_dir=$(lib_name_to_dir $avail_lib)
    echo "-I$DIR/bindings/$lib_dir/include"
  done
}

export GI_INCLUDES=$(include_dirs)

install () {
  local lib_name=$1
  local lib_dir=$(lib_name_to_dir $lib_name)
  dune install --root bindings/$lib_dir
}

for lib in $libs; do
  stack build && stack exec ocaml-gi-gtk-exe $lib
  if $install; then
    install $lib
  fi
done