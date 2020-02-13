module Main where

import           Control.Monad                  ( forM_ )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           System.Environment
import           System.Exit

import           GI                             ( genBindings
                                                , Library(..)
                                                )

glibLib :: Library
glibLib = Library { name          = "GLib"
                  , version       = "2.0"
                  , overridesFile = Just "overrides/GLib.overrides"
                  }

gobjectLib :: Library
gobjectLib = Library { name          = "GObject"
                     , version       = "2.0"
                     , overridesFile = Just "overrides/GObject.overrides"
                     }

gioLib :: Library
gioLib = Library { name          = "Gio"
                 , version       = "2.0"
                 , overridesFile = Just "overrides/Gio.overrides"
                 }

atkLib :: Library
atkLib = Library { name          = "Atk"
                 , version       = "1.0"
                 , overridesFile = Just "overrides/Atk.overrides"
                 }

cairoLib :: Library
cairoLib = Library { name          = "cairo"
                   , version       = "1.0"
                   , overridesFile = Just "overrides/cairo.overrides"
                   }

pangoLib :: Library
pangoLib = Library { name          = "Pango"
                   , version       = "1.0"
                   , overridesFile = Just "overrides/Pango.overrides"
                   }

gdkPixbufLib :: Library
gdkPixbufLib = Library { name          = "GdkPixbuf"
                       , version       = "2.0"
                       , overridesFile = Just "overrides/GdkPixbuf.overrides"
                       }

gdkLib :: Library
gdkLib = Library { name          = "Gdk"
                 , version       = "3.0"
                 , overridesFile = Just "overrides/Gdk.overrides"
                 }

gtkLib :: Library
gtkLib = Library { name          = "Gtk"
                 , version       = "3.0"
                 , overridesFile = Just "overrides/Gtk.overrides"
                 }

gtkSourceLib :: Library
gtkSourceLib = Library { name          = "GtkSource"
                       , version       = "3.0"
                       , overridesFile = Just "overrides/GtkSource.overrides"
                       }

parseArg :: String -> IO Library
parseArg "glib"      = return glibLib
parseArg "gobject"   = return gobjectLib
parseArg "gio"       = return gioLib
parseArg "atk"       = return atkLib
parseArg "cairo"     = return cairoLib
parseArg "pango"     = return pangoLib
parseArg "gdkpixbuf" = return gdkPixbufLib
parseArg "gdk"       = return gdkLib
parseArg "gtk"       = return gtkLib
parseArg "gtksource" = return gtkSourceLib
parseArg "-h"        = printUsage >> exitSuccess
parseArg "-v"        = printVersion >> exitSuccess
parseArg _           = printUsage >> exitSuccess

main :: IO ()
main = do
  let verbose = False
  args <- getArgs
  case args of
    [] -> printUsage >> exitSuccess
    _  -> do
      libs <- mapM parseArg args
      genBindings verbose (last libs)

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: ocaml-gi-gtk [-h] [-v] LIB"
  putStrLn
    "Available LIBs are: glib, gobject, gio, atk, cairo, pango, gdkpixbuf, gdk, gtk, gtksource"

printVersion :: IO ()
printVersion = putStrLn "ocaml-gi-gtk 0.1"
