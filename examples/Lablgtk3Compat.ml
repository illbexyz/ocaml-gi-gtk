open GIGtk

type expand_type = [`NONE | `X | `Y | `BOTH]

let has_x : expand_type -> bool =
  function `X|`BOTH -> true | `Y|`NONE -> false
let has_y : expand_type -> bool =
  function `Y|`BOTH -> true | `X|`NONE -> false

let attach (tbl : #TableG.table) ~left ~top ?(right=left+1) ?(bottom=top+1)
    ?(expand=`NONE) ?(fill=`BOTH) ?(shrink=`NONE)
    ?(xpadding=0) ?(ypadding=0) w =
    let xoptions = if has_x shrink then [`SHRINK] else [] in
    let xoptions = if has_x fill then `FILL::xoptions else xoptions in
    let xoptions = if has_x expand then `EXPAND::xoptions else xoptions in
    let yoptions = if has_y shrink then [`SHRINK] else [] in
    let yoptions = if has_y fill then `FILL::yoptions else yoptions in
    let yoptions = if has_y expand then `EXPAND::yoptions else yoptions in
  tbl#attach w left right top bottom xoptions yoptions xpadding ypadding

let pack (box : #BoxG.box) ?(expand=false) ?(fill=true) ?(padding=0) w =
 box#pack_start w expand fill padding

let append_page (assistant : #AssistantG.assistant) ?page_type ?title ?header_image ?side_image ?complete w =
  let n = assistant#append_page w in
  Gaux.may (assistant#set_page_type w) page_type;
  Gaux.may (assistant#set_page_title w) title;
  Gaux.may (assistant#set_page_header_image w) header_image;
  Gaux.may (assistant#set_page_side_image w) side_image;
  Gaux.may (assistant#set_page_complete w) complete;
  n
