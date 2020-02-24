module Files
    ( excludeFiles
    , noCheckMacro
    , noCType
    , buggedIfaces
    )
where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S

import           API                            ( Name(..) )

noCType :: Set Name
noCType = S.fromList [Name "Gtk" "HeaderBarAccessible"] -- Its type isn't included in <gtk/gtk-a11y.h>

-- These files doesn't have a macro to check the cast
noCheckMacro :: Set Name
noCheckMacro = S.fromList [Name "Pango" "Coverage"]

buggedIfaces :: Set Name
buggedIfaces = S.fromList
    [ Name "Gtk" "TreeModel"                  -- Strange type error
    , Name "Gtk" "CellLayout"                 -- Dependency cycle
    , Name "Gtk" "StyleProvider"              -- Dependency cycle
    ]

excludeFiles :: Set Name
excludeFiles = S.fromList
    [ Name "Gtk"       "HeaderBarAccessible"  -- Its type isn't included in <gtk/gtk-a11y.h>
    , Name "Gtk"       "EntryIconAccessible"  -- Bug: The GIR Parser doesn't return its CType (bug in the parser)
    -- , Name "Gtk"       "TreeModelFilter"      -- Need to generate interface
    , Name "Gtk"       "CellAccessibleParent" -- The get_cell_extents method uses an integer pointer which is parsed as an int
    , Name "Gtk"       "TreeViewAccessible"   -- Depends on CellAccessibleParent
    , Name "Gtk"       "Widget"               -- We use the lablgtk one
    , Name "Gtk"       "FileChooserButton"    -- Using wrong conversion macro for gtk_file_chooser_button_new_with_dialog
    , Name "Pango"     "Engine"
    , Name "Pango"     "EngineShape"
    , Name "Pango"     "EngineLang"
    , Name "Pango"     "FontsetSimple"
    , Name "GtkSource" "Gutter"               -- gtk_source_gutter_get_padding uses an integer pointer which is parsed as an int
    , Name "GtkSource" "View"                 -- gtk_source_view_get_mark_attributes uses an integer pointer which is parsed as an int
    , Name "GtkSource" "Map"                  -- Depends on View
    , Name "GtkSource" "Mark"
    -- The following ones have the most obscure #bug: for some reason calling
    -- (moduleCode minfo) in Code.hs, while writing the files crashes the 
    -- program
    , Name "GtkSource" "LanguageManager"
    , Name "GtkSource" "StyleSchemeManager"
    , Name "Gio"       "ApplicationCommandLine"
    , Name "Gio"       "ListStore"
    -- For the next two we need to write the C functions without using the macro 
    -- because of pointers in the CType
    , Name "Gio"       "MenuLinkIter"
    , Name "Gio"       "SimpleProxyResolver"
    -- Bug: Unlike every other C in/out functions seen before, the ones in these
    -- three files doesn't pass every in argument before the out arguments
    -- Need to generate the C function without a macro
    , Name "Atk"       "Component"
    , Name "Atk"       "Image"
    , Name "Atk"       "Text"
    --
    , Name "Atk"       "EditableText" -- ‘atk_editable_text_insert_text’ has an int pointer
    , Name "Atk"       "Table"        -- Like above for ‘atk_table_get_selected_columns’
    , Name "Gdk"       "Window"       -- gtk_window_destroy_notify is found inside the GIR
                                      -- but it doesn't exist
    , Name "Gdk"       "Device"       -- Methods returning an object inside a tuple aren't handled well
    , Name "Gdk"       "Display"      -- Same as above
    ]
