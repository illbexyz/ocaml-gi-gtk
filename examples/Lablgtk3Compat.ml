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
