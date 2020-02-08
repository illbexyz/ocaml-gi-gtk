module Files
    ( excludeFiles
    , genFiles
    )
where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S

import           API                            ( Name(..) )

excludeFiles :: Set Name
excludeFiles = S.fromList
    [ Name "Gtk"   "HeaderBarAccessible"  -- Its type isn't included in <gtk/gtk-a11y.h>
    , Name "Gtk"   "EntryIconAccessible"  -- Bug: The GIR Parser doesn't return its CType (bug in the parser)
    , Name "Gtk"   "TreeModelFilter"      -- Need to generate interface
    --
    -- , Name "Gtk"   "StyleContext"
    -- Pango
    , Name "Pango" "Engine"
    , Name "Pango" "EngineShape"
    , Name "Pango" "EngineLang"
    , Name "Pango" "FontsetSimple"
    ]

genFiles :: Set Name
genFiles = S.fromList
    [ Name "Gtk" "Bin"
    , Name "Gtk" "Button"
    , Name "Gtk" "CheckButton"
    , Name "Gtk" "ToggleButton"
    , Name "Gtk" "RadioButton"
    , Name "Gtk" "Misc"
    , Name "Gtk" "Label"
    , Name "Gtk" "Entry"
    , Name "Gtk" "ActionBar"
    , Name "Gtk" "MenuShell"
    , Name "Gtk" "RadioMenuItem"
    , Name "Gtk" "CheckMenuItem"
    , Name "Gtk" "Menu"
    , Name "Gtk" "MenuItem"
    , Name "Gtk" "Adjustment"
    , Name "Gtk" "Alignment"
    , Name "Gtk" "AppChooserButton"
    , Name "Gtk" "ComboBox"
    , Name "Gtk" "Notebook"
    , Name "Gtk" "Action"
    , Name "Gtk" "IconView"
    , Name "Gtk" "Clipboard"
    , Name "Gtk" "RcStyle"
    , Name "Gtk" "ThemingEngine"
    , Name "Gtk" "MountOperation"
    , Name "Gtk" "Application"
    , Name "Gtk" "Window"
    , Name "Gtk" "WindowGroup"
    , Name "Gtk" "NumerableIcon"
    , Name "Gtk" "AccelMap"
    , Name "Gtk" "IconInfo"
    , Name "Gtk" "IconTheme"
    , Name "Gtk" "Settings"
    , Name "Gtk" "RecentManager"
    -- , Name "Gtk" "Tooltip"        -- Gtk-CRITICAL **: 21:32:35.797: _gtk_style_provider_private_get_settings: assertion 'GTK_IS_STYLE_PROVIDER_PRIVATE (provider)' failed
    , Name "Gtk" "TreeModel"
    , Name "Gtk" "HSV"
    , Name "Gtk" "Image"
    , Name "Gtk" "Range"
    , Name "Gtk" "Layout"
    , Name "Gtk" "ToolItem"
    , Name "Gtk" "SizeGroup"
    , Name "Gtk" "ComboBoxText"
    , Name "Gtk" "TextTag"
    , Name "Gtk" "ScrolledWindow"
    , Name "Gtk" "PlacesSidebar"
    , Name "Gtk" "CssProvider"
    , Name "Gtk" "EntryIconAccessible"
    , Name "Gtk" "Box"
    , Name "Gtk" "TreeModel"
    , Name "Gtk" "ToolItemGroup"
    , Name "Gtk" "PrintOperation"
    , Name "Gtk" "CellRenderer"
    -- , Name "Gtk" "CellRendererSpinner"
    -- , Name "Gtk" "CellRendererText"
    , Name "Gtk" "ToolPalette"
    , Name "Gtk" "TreeModelFilter"
    -- , Name "Gtk" "StyleContext"
    --  , Name "Gtk" "Container"
    ]
