# ocaml-gi-gtk

Automatic generation of GTK bindings for OCaml using GObject Introspection. Currently this project is in an highly experimental status.

This is a fork of [haskell-gi](https://github.com/haskell-gi/haskell-gi).

The design of the generated APIs is heavily inspired by [lablgtk](https://github.com/garrigue/lablgtk).

Table of Contents
=================

  * [Build](#build)
  * [Generate the bindings](#generate-the-bindings)
  * [Running the examples](#running-the-examples)
  * [Running the project manually](#running-the-project-manually)
  * [Adding a new library](#adding-a-new-library)

## Build

Running:

```
$ stack build
```

will download the needed dependencies and build the project.

## Installing the core library
Enter the base-ocaml/gilablgtk3 directory, compile and install the library:

```
dune install
```

## Generate the bindings
The easiest way is to use the `generate.sh` script. The bindings will be generated inside the `bindings` folder.

```
Usage: generate.sh [OPTION] LIB [LIB...]

Available libs are: glib gobject gio atk cairo pango gdkpixbuf gdk gtk

Options:
  -h, (help)     Display this help and exit
  -f, (full)     Generate every available library. The LIB arguments will be ignored.
  -i, (install)  After the library generation, it will install it using 'dune install'
```

Generally, you want to generate the bindings using:
```
$ ./generate.sh -f -i
```

This will generate and install every available library in the correct order. The order is important because some libraries depends on the previous ones.

## Running the examples

Inside the `examples` directory there is a dune project containing some example programs you can run.

Assuming you have installed the `GIGtk` library, you can run them by:

```
$ cd examples
$ dune exec ./example-name.exe
``` 

Alternatively if you wish to run them without changing directory you can do:

```
$ dune exec --root examples ./example-name.exe
``` 

## Running the project manually

First you have to define some environment variables:

- `BASE_OCAML_C`: The generated libraries need to link against some C header files which are part of this library. By default they are inside `base-ocaml/c`. The env variable must point to this directory.
- `GI_INCLUDES`: Some of the generated libraries depend on C header files of other (previously generated) libraries. This env variable is needed to tell dune where to find them. These headers are located inside `bindings/LIB_NAME/include`. So, for every dependency of the library you want to generate, `GI_INCLUDES` must contain ``-I`pwd`/bindings/LIB_NAME/include``.

Example: If you wish to generate `gio`, which depends on `glib` and `gobject`, then ``GI_INCLUDES=-I`pwd`/bindings/GLib/include -I`pwd`/bindings/GObject/include``

Once these env variables are exported, you can run the generator directly from stack using: 

```
$ stack build && stack exec ocaml-gi-gtk-exe -- lib-name
```

The usage is:

```
Usage: ocaml-gi-gtk [-h] [-v] LIB
Available LIBs are: glib, gobject, gio, atk, cairo, pango, gdkpixbuf, gdk, gtk, gtksource
```

## Adding a new library

The available libraries are listed inside `app/Main.hs`, so a new library must be defined there.

The `name` and `version` fields of Library are needed to correctly resolve the corresponding GIR file. The `overrides` file is an optional file used to change some (possibly incorrect) GIR attributes.

These attributes can be found in the [haskell-gi's bindings directory ](https://github.com/haskell-gi/haskell-gi/tree/master/bindings). Inside every subdirectory there is a `pkg.info` file containing the needed informations (`girName`, `girVersion` and `girOverrides`). The override file has to be copied from the haskell-gi's repository to the `overrides` directory.
