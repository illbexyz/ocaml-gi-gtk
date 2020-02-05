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
    [ Name "Gtk"   "HeaderBarAccessible"
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
    , Name "Gtk" "Tooltip"
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
    -- , Name "Gtk" "TreeModelFilter"
    -- , Name "Gtk" "StyleContext"
    --  , Name "Gtk" "Container"
    ]
