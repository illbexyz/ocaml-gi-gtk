/* align : conversion table */
const lookup_info ml_table_align[] = {
  { 0, 5 },
  { MLTAG_BASELINE, GTK_ALIGN_BASELINE },
  { MLTAG_END, GTK_ALIGN_END },
  { MLTAG_START, GTK_ALIGN_START },
  { MLTAG_FILL, GTK_ALIGN_FILL },
  { MLTAG_CENTER, GTK_ALIGN_CENTER },
};

/* arrow_type : conversion table */
const lookup_info ml_table_arrow_type[] = {
  { 0, 5 },
  { MLTAG_RIGHT, GTK_ARROW_RIGHT },
  { MLTAG_UP, GTK_ARROW_UP },
  { MLTAG_DOWN, GTK_ARROW_DOWN },
  { MLTAG_LEFT, GTK_ARROW_LEFT },
  { MLTAG_NONE, GTK_ARROW_NONE },
};

/* attach_options : conversion table */
const lookup_info ml_table_attach_options[] = {
  { 0, 3 },
  { MLTAG_SHRINK, GTK_SHRINK },
  { MLTAG_EXPAND, GTK_EXPAND },
  { MLTAG_FILL, GTK_FILL },
};

/* baseline_position : conversion table */
const lookup_info ml_table_baseline_position[] = {
  { 0, 3 },
  { MLTAG_TOP, GTK_BASELINE_POSITION_TOP },
  { MLTAG_BOTTOM, GTK_BASELINE_POSITION_BOTTOM },
  { MLTAG_CENTER, GTK_BASELINE_POSITION_CENTER },
};

/* delete_type : conversion table */
const lookup_info ml_table_delete_type[] = {
  { 0, 8 },
  { MLTAG_CHARS, GTK_DELETE_CHARS },
  { MLTAG_WORDS, GTK_DELETE_WORDS },
  { MLTAG_PARAGRAPHS, GTK_DELETE_PARAGRAPHS },
  { MLTAG_DISPLAY_LINE_ENDS, GTK_DELETE_DISPLAY_LINE_ENDS },
  { MLTAG_PARAGRAPH_ENDS, GTK_DELETE_PARAGRAPH_ENDS },
  { MLTAG_DISPLAY_LINES, GTK_DELETE_DISPLAY_LINES },
  { MLTAG_WHITESPACE, GTK_DELETE_WHITESPACE },
  { MLTAG_WORD_ENDS, GTK_DELETE_WORD_ENDS },
};

/* direction_type : conversion table */
const lookup_info ml_table_direction_type[] = {
  { 0, 6 },
  { MLTAG_RIGHT, GTK_DIR_RIGHT },
  { MLTAG_UP, GTK_DIR_UP },
  { MLTAG_TAB_FORWARD, GTK_DIR_TAB_FORWARD },
  { MLTAG_TAB_BACKWARD, GTK_DIR_TAB_BACKWARD },
  { MLTAG_DOWN, GTK_DIR_DOWN },
  { MLTAG_LEFT, GTK_DIR_LEFT },
};

/* icon_size : conversion table */
const lookup_info ml_table_icon_size[] = {
  { 0, 7 },
  { MLTAG_DIALOG, GTK_ICON_SIZE_DIALOG },
  { MLTAG_SMALL_TOOLBAR, GTK_ICON_SIZE_SMALL_TOOLBAR },
  { MLTAG_DND, GTK_ICON_SIZE_DND },
  { MLTAG_LARGE_TOOLBAR, GTK_ICON_SIZE_LARGE_TOOLBAR },
  { MLTAG_BUTTON, GTK_ICON_SIZE_BUTTON },
  { MLTAG_MENU, GTK_ICON_SIZE_MENU },
  { MLTAG_INVALID, GTK_ICON_SIZE_INVALID },
};

/* sensitivity_type : conversion table */
const lookup_info ml_table_sensitivity_type[] = {
  { 0, 3 },
  { MLTAG_ON, GTK_SENSITIVITY_ON },
  { MLTAG_OFF, GTK_SENSITIVITY_OFF },
  { MLTAG_AUTO, GTK_SENSITIVITY_AUTO },
};

/* text_direction : conversion table */
const lookup_info ml_table_text_direction[] = {
  { 0, 3 },
  { MLTAG_LTR, GTK_TEXT_DIR_LTR },
  { MLTAG_RTL, GTK_TEXT_DIR_RTL },
  { MLTAG_NONE, GTK_TEXT_DIR_NONE },
};

/* justification : conversion table */
const lookup_info ml_table_justification[] = {
  { 0, 4 },
  { MLTAG_RIGHT, GTK_JUSTIFY_RIGHT },
  { MLTAG_FILL, GTK_JUSTIFY_FILL },
  { MLTAG_LEFT, GTK_JUSTIFY_LEFT },
  { MLTAG_CENTER, GTK_JUSTIFY_CENTER },
};

/* menu_direction_type : conversion table */
const lookup_info ml_table_menu_direction_type[] = {
  { 0, 4 },
  { MLTAG_CHILD, GTK_MENU_DIR_CHILD },
  { MLTAG_PARENT, GTK_MENU_DIR_PARENT },
  { MLTAG_NEXT, GTK_MENU_DIR_NEXT },
  { MLTAG_PREV, GTK_MENU_DIR_PREV },
};

/* message_type : conversion table */
const lookup_info ml_table_message_type[] = {
  { 0, 5 },
  { MLTAG_ERROR, GTK_MESSAGE_ERROR },
  { MLTAG_QUESTION, GTK_MESSAGE_QUESTION },
  { MLTAG_WARNING, GTK_MESSAGE_WARNING },
  { MLTAG_INFO, GTK_MESSAGE_INFO },
  { MLTAG_OTHER, GTK_MESSAGE_OTHER },
};

/* movement_step : conversion table */
const lookup_info ml_table_movement_step[] = {
  { 0, 10 },
  { MLTAG_BUFFER_ENDS, GTK_MOVEMENT_BUFFER_ENDS },
  { MLTAG_VISUAL_POSITIONS, GTK_MOVEMENT_VISUAL_POSITIONS },
  { MLTAG_WORDS, GTK_MOVEMENT_WORDS },
  { MLTAG_PARAGRAPHS, GTK_MOVEMENT_PARAGRAPHS },
  { MLTAG_DISPLAY_LINE_ENDS, GTK_MOVEMENT_DISPLAY_LINE_ENDS },
  { MLTAG_PARAGRAPH_ENDS, GTK_MOVEMENT_PARAGRAPH_ENDS },
  { MLTAG_HORIZONTAL_PAGES, GTK_MOVEMENT_HORIZONTAL_PAGES },
  { MLTAG_LOGICAL_POSITIONS, GTK_MOVEMENT_LOGICAL_POSITIONS },
  { MLTAG_DISPLAY_LINES, GTK_MOVEMENT_DISPLAY_LINES },
  { MLTAG_PAGES, GTK_MOVEMENT_PAGES },
};

/* orientation : conversion table */
const lookup_info ml_table_orientation[] = {
  { 0, 2 },
  { MLTAG_VERTICAL, GTK_ORIENTATION_VERTICAL },
  { MLTAG_HORIZONTAL, GTK_ORIENTATION_HORIZONTAL },
};

/* pack_type : conversion table */
const lookup_info ml_table_pack_type[] = {
  { 0, 2 },
  { MLTAG_END, GTK_PACK_END },
  { MLTAG_START, GTK_PACK_START },
};

/* position_type : conversion table */
const lookup_info ml_table_position_type[] = {
  { 0, 4 },
  { MLTAG_RIGHT, GTK_POS_RIGHT },
  { MLTAG_TOP, GTK_POS_TOP },
  { MLTAG_BOTTOM, GTK_POS_BOTTOM },
  { MLTAG_LEFT, GTK_POS_LEFT },
};

/* relief_style : conversion table */
const lookup_info ml_table_relief_style[] = {
  { 0, 3 },
  { MLTAG_NORMAL, GTK_RELIEF_NORMAL },
  { MLTAG_HALF, GTK_RELIEF_HALF },
  { MLTAG_NONE, GTK_RELIEF_NONE },
};

/* scroll_step : conversion table */
const lookup_info ml_table_scroll_step[] = {
  { 0, 6 },
  { MLTAG_HORIZONTAL_STEPS, GTK_SCROLL_HORIZONTAL_STEPS },
  { MLTAG_END, GTK_SCROLL_END },
  { MLTAG_STEPS, GTK_SCROLL_STEPS },
  { MLTAG_HORIZONTAL_PAGES, GTK_SCROLL_HORIZONTAL_PAGES },
  { MLTAG_HORIZONTAL_ENDS, GTK_SCROLL_HORIZONTAL_ENDS },
  { MLTAG_PAGES, GTK_SCROLL_PAGES },
};

/* scroll_type : conversion table */
const lookup_info ml_table_scroll_type[] = {
  { 0, 16 },
  { MLTAG_STEP_LEFT, GTK_SCROLL_STEP_LEFT },
  { MLTAG_STEP_BACKWARD, GTK_SCROLL_STEP_BACKWARD },
  { MLTAG_PAGE_BACKWARD, GTK_SCROLL_PAGE_BACKWARD },
  { MLTAG_STEP_RIGHT, GTK_SCROLL_STEP_RIGHT },
  { MLTAG_PAGE_DOWN, GTK_SCROLL_PAGE_DOWN },
  { MLTAG_PAGE_LEFT, GTK_SCROLL_PAGE_LEFT },
  { MLTAG_STEP_FORWARD, GTK_SCROLL_STEP_FORWARD },
  { MLTAG_END, GTK_SCROLL_END },
  { MLTAG_STEP_UP, GTK_SCROLL_STEP_UP },
  { MLTAG_START, GTK_SCROLL_START },
  { MLTAG_PAGE_UP, GTK_SCROLL_PAGE_UP },
  { MLTAG_PAGE_FORWARD, GTK_SCROLL_PAGE_FORWARD },
  { MLTAG_PAGE_RIGHT, GTK_SCROLL_PAGE_RIGHT },
  { MLTAG_JUMP, GTK_SCROLL_JUMP },
  { MLTAG_NONE, GTK_SCROLL_NONE },
  { MLTAG_STEP_DOWN, GTK_SCROLL_STEP_DOWN },
};

/* selection_mode : conversion table */
const lookup_info ml_table_selection_mode[] = {
  { 0, 4 },
  { MLTAG_BROWSE, GTK_SELECTION_BROWSE },
  { MLTAG_SINGLE, GTK_SELECTION_SINGLE },
  { MLTAG_MULTIPLE, GTK_SELECTION_MULTIPLE },
  { MLTAG_NONE, GTK_SELECTION_NONE },
};

/* shadow_type : conversion table */
const lookup_info ml_table_shadow_type[] = {
  { 0, 5 },
  { MLTAG_IN, GTK_SHADOW_IN },
  { MLTAG_OUT, GTK_SHADOW_OUT },
  { MLTAG_ETCHED_OUT, GTK_SHADOW_ETCHED_OUT },
  { MLTAG_ETCHED_IN, GTK_SHADOW_ETCHED_IN },
  { MLTAG_NONE, GTK_SHADOW_NONE },
};

/* state_type : conversion table */
const lookup_info ml_table_state_type[] = {
  { 0, 7 },
  { MLTAG_INSENSITIVE, GTK_STATE_INSENSITIVE },
  { MLTAG_FOCUSED, GTK_STATE_FOCUSED },
  { MLTAG_ACTIVE, GTK_STATE_ACTIVE },
  { MLTAG_NORMAL, GTK_STATE_NORMAL },
  { MLTAG_INCONSISTENT, GTK_STATE_INCONSISTENT },
  { MLTAG_SELECTED, GTK_STATE_SELECTED },
  { MLTAG_PRELIGHT, GTK_STATE_PRELIGHT },
};

/* toolbar_style : conversion table */
const lookup_info ml_table_toolbar_style[] = {
  { 0, 4 },
  { MLTAG_BOTH_HORIZ, GTK_TOOLBAR_BOTH_HORIZ },
  { MLTAG_BOTH, GTK_TOOLBAR_BOTH },
  { MLTAG_ICONS, GTK_TOOLBAR_ICONS },
  { MLTAG_TEXT, GTK_TOOLBAR_TEXT },
};

/* wrap_mode : conversion table */
const lookup_info ml_table_wrap_mode[] = {
  { 0, 4 },
  { MLTAG_CHAR, GTK_WRAP_CHAR },
  { MLTAG_NONE, GTK_WRAP_NONE },
  { MLTAG_WORD_CHAR, GTK_WRAP_WORD_CHAR },
  { MLTAG_WORD, GTK_WRAP_WORD },
};

/* sort_type : conversion table */
const lookup_info ml_table_sort_type[] = {
  { 0, 2 },
  { MLTAG_DESCENDING, GTK_SORT_DESCENDING },
  { MLTAG_ASCENDING, GTK_SORT_ASCENDING },
};

/* pack_direction : conversion table */
const lookup_info ml_table_pack_direction[] = {
  { 0, 4 },
  { MLTAG_BTT, GTK_PACK_DIRECTION_BTT },
  { MLTAG_LTR, GTK_PACK_DIRECTION_LTR },
  { MLTAG_RTL, GTK_PACK_DIRECTION_RTL },
  { MLTAG_TTB, GTK_PACK_DIRECTION_TTB },
};

/* print_pages : conversion table */
const lookup_info ml_table_print_pages[] = {
  { 0, 4 },
  { MLTAG_CURRENT, GTK_PRINT_PAGES_CURRENT },
  { MLTAG_RANGES, GTK_PRINT_PAGES_RANGES },
  { MLTAG_ALL, GTK_PRINT_PAGES_ALL },
  { MLTAG_SELECTION, GTK_PRINT_PAGES_SELECTION },
};

/* page_set : conversion table */
const lookup_info ml_table_page_set[] = {
  { 0, 3 },
  { MLTAG_ALL, GTK_PAGE_SET_ALL },
  { MLTAG_ODD, GTK_PAGE_SET_ODD },
  { MLTAG_EVEN, GTK_PAGE_SET_EVEN },
};

/* number_up_layout : conversion table */
const lookup_info ml_table_number_up_layout[] = {
  { 0, 8 },
  { MLTAG_TOP_TO_BOTTOM_RIGHT_TO_LEFT, GTK_NUMBER_UP_LAYOUT_TOP_TO_BOTTOM_RIGHT_TO_LEFT },
  { MLTAG_TOP_TO_BOTTOM_LEFT_TO_RIGHT, GTK_NUMBER_UP_LAYOUT_TOP_TO_BOTTOM_LEFT_TO_RIGHT },
  { MLTAG_BOTTOM_TO_TOP_RIGHT_TO_LEFT, GTK_NUMBER_UP_LAYOUT_BOTTOM_TO_TOP_RIGHT_TO_LEFT },
  { MLTAG_BOTTOM_TO_TOP_LEFT_TO_RIGHT, GTK_NUMBER_UP_LAYOUT_BOTTOM_TO_TOP_LEFT_TO_RIGHT },
  { MLTAG_RIGHT_TO_LEFT_TOP_TO_BOTTOM, GTK_NUMBER_UP_LAYOUT_RIGHT_TO_LEFT_TOP_TO_BOTTOM },
  { MLTAG_RIGHT_TO_LEFT_BOTTOM_TO_TOP, GTK_NUMBER_UP_LAYOUT_RIGHT_TO_LEFT_BOTTOM_TO_TOP },
  { MLTAG_LEFT_TO_RIGHT_TOP_TO_BOTTOM, GTK_NUMBER_UP_LAYOUT_LEFT_TO_RIGHT_TOP_TO_BOTTOM },
  { MLTAG_LEFT_TO_RIGHT_BOTTOM_TO_TOP, GTK_NUMBER_UP_LAYOUT_LEFT_TO_RIGHT_BOTTOM_TO_TOP },
};

/* page_orientation : conversion table */
const lookup_info ml_table_page_orientation[] = {
  { 0, 4 },
  { MLTAG_REVERSE_LANDSCAPE, GTK_PAGE_ORIENTATION_REVERSE_LANDSCAPE },
  { MLTAG_LANDSCAPE, GTK_PAGE_ORIENTATION_LANDSCAPE },
  { MLTAG_PORTRAIT, GTK_PAGE_ORIENTATION_PORTRAIT },
  { MLTAG_REVERSE_PORTRAIT, GTK_PAGE_ORIENTATION_REVERSE_PORTRAIT },
};

/* print_quality : conversion table */
const lookup_info ml_table_print_quality[] = {
  { 0, 4 },
  { MLTAG_DRAFT, GTK_PRINT_QUALITY_DRAFT },
  { MLTAG_NORMAL, GTK_PRINT_QUALITY_NORMAL },
  { MLTAG_LOW, GTK_PRINT_QUALITY_LOW },
  { MLTAG_HIGH, GTK_PRINT_QUALITY_HIGH },
};

/* print_duplex : conversion table */
const lookup_info ml_table_print_duplex[] = {
  { 0, 3 },
  { MLTAG_VERTICAL, GTK_PRINT_DUPLEX_VERTICAL },
  { MLTAG_HORIZONTAL, GTK_PRINT_DUPLEX_HORIZONTAL },
  { MLTAG_SIMPLEX, GTK_PRINT_DUPLEX_SIMPLEX },
};

/* gtk_unit : conversion table */
const lookup_info ml_table_gtk_unit[] = {
  { 0, 5 },
  { MLTAG_PIXEL, GTK_UNIT_PIXEL },
  { MLTAG_MM, GTK_UNIT_MM },
  { MLTAG_POINTS, GTK_UNIT_POINTS },
  { MLTAG_INCH, GTK_UNIT_INCH },
  { MLTAG_NONE, GTK_UNIT_NONE },
};

/* tree_view_grid_lines : conversion table */
const lookup_info ml_table_tree_view_grid_lines[] = {
  { 0, 4 },
  { MLTAG_VERTICAL, GTK_TREE_VIEW_GRID_LINES_VERTICAL },
  { MLTAG_HORIZONTAL, GTK_TREE_VIEW_GRID_LINES_HORIZONTAL },
  { MLTAG_BOTH, GTK_TREE_VIEW_GRID_LINES_BOTH },
  { MLTAG_NONE, GTK_TREE_VIEW_GRID_LINES_NONE },
};

/* drag_result : conversion table */
const lookup_info ml_table_drag_result[] = {
  { 0, 6 },
  { MLTAG_TIMEOUT_EXPIRED, GTK_DRAG_RESULT_TIMEOUT_EXPIRED },
  { MLTAG_GRAB_BROKEN, GTK_DRAG_RESULT_GRAB_BROKEN },
  { MLTAG_ERROR, GTK_DRAG_RESULT_ERROR },
  { MLTAG_NO_TARGET, GTK_DRAG_RESULT_NO_TARGET },
  { MLTAG_SUCCESS, GTK_DRAG_RESULT_SUCCESS },
  { MLTAG_USER_CANCELLED, GTK_DRAG_RESULT_USER_CANCELLED },
};

/* size_group_mode : conversion table */
const lookup_info ml_table_size_group_mode[] = {
  { 0, 4 },
  { MLTAG_VERTICAL, GTK_SIZE_GROUP_VERTICAL },
  { MLTAG_HORIZONTAL, GTK_SIZE_GROUP_HORIZONTAL },
  { MLTAG_BOTH, GTK_SIZE_GROUP_BOTH },
  { MLTAG_NONE, GTK_SIZE_GROUP_NONE },
};

/* size_request_mode : conversion table */
const lookup_info ml_table_size_request_mode[] = {
  { 0, 3 },
  { MLTAG_CONSTANT_SIZE, GTK_SIZE_REQUEST_CONSTANT_SIZE },
  { MLTAG_HEIGHT_FOR_WIDTH, GTK_SIZE_REQUEST_HEIGHT_FOR_WIDTH },
  { MLTAG_WIDTH_FOR_HEIGHT, GTK_SIZE_REQUEST_WIDTH_FOR_HEIGHT },
};

/* scrollable_policy : conversion table */
const lookup_info ml_table_scrollable_policy[] = {
  { 0, 2 },
  { MLTAG_MINIMUM, GTK_SCROLL_MINIMUM },
  { MLTAG_NATURAL, GTK_SCROLL_NATURAL },
};

/* state_flag : conversion table */
const lookup_info ml_table_state_flag[] = {
  { 0, 13 },
  { MLTAG_INSENSITIVE, GTK_STATE_FLAG_INSENSITIVE },
  { MLTAG_FOCUSED, GTK_STATE_FLAG_FOCUSED },
  { MLTAG_ACTIVE, GTK_STATE_FLAG_ACTIVE },
  { MLTAG_NORMAL, GTK_STATE_FLAG_NORMAL },
  { MLTAG_BACKDROP, GTK_STATE_FLAG_BACKDROP },
  { MLTAG_INCONSISTENT, GTK_STATE_FLAG_INCONSISTENT },
  { MLTAG_SELECTED, GTK_STATE_FLAG_SELECTED },
  { MLTAG_CHECKED, GTK_STATE_FLAG_CHECKED },
  { MLTAG_VISITED, GTK_STATE_FLAG_VISITED },
  { MLTAG_DIR_LTR, GTK_STATE_FLAG_DIR_LTR },
  { MLTAG_DIR_RTL, GTK_STATE_FLAG_DIR_RTL },
  { MLTAG_LINK, GTK_STATE_FLAG_LINK },
  { MLTAG_PRELIGHT, GTK_STATE_FLAG_PRELIGHT },
};

/* region_flag : conversion table */
const lookup_info ml_table_region_flag[] = {
  { 0, 6 },
  { MLTAG_SORTED, GTK_REGION_SORTED },
  { MLTAG_FIRST, GTK_REGION_FIRST },
  { MLTAG_ODD, GTK_REGION_ODD },
  { MLTAG_EVEN, GTK_REGION_EVEN },
  { MLTAG_LAST, GTK_REGION_LAST },
  { MLTAG_ONLY, GTK_REGION_ONLY },
};

/* junction_sides : conversion table */
const lookup_info ml_table_junction_sides[] = {
  { 0, 9 },
  { MLTAG_RIGHT, GTK_JUNCTION_RIGHT },
  { MLTAG_CORNER_TOPLEFT, GTK_JUNCTION_CORNER_TOPLEFT },
  { MLTAG_TOP, GTK_JUNCTION_TOP },
  { MLTAG_CORNER_TOPRIGHT, GTK_JUNCTION_CORNER_TOPRIGHT },
  { MLTAG_BOTTOM, GTK_JUNCTION_BOTTOM },
  { MLTAG_CORNER_BOTTOMLEFT, GTK_JUNCTION_CORNER_BOTTOMLEFT },
  { MLTAG_LEFT, GTK_JUNCTION_LEFT },
  { MLTAG_NONE, GTK_JUNCTION_NONE },
  { MLTAG_CORNER_BOTTOMRIGHT, GTK_JUNCTION_CORNER_BOTTOMRIGHT },
};

/* border_style : conversion table */
const lookup_info ml_table_border_style[] = {
  { 0, 10 },
  { MLTAG_GROOVE, GTK_BORDER_STYLE_GROOVE },
  { MLTAG_DASHED, GTK_BORDER_STYLE_DASHED },
  { MLTAG_DOTTED, GTK_BORDER_STYLE_DOTTED },
  { MLTAG_DOUBLE, GTK_BORDER_STYLE_DOUBLE },
  { MLTAG_RIDGE, GTK_BORDER_STYLE_RIDGE },
  { MLTAG_SOLID, GTK_BORDER_STYLE_SOLID },
  { MLTAG_HIDDEN, GTK_BORDER_STYLE_HIDDEN },
  { MLTAG_NONE, GTK_BORDER_STYLE_NONE },
  { MLTAG_INSET, GTK_BORDER_STYLE_INSET },
  { MLTAG_OUTSET, GTK_BORDER_STYLE_OUTSET },
};

/* level_bar_mode : conversion table */
const lookup_info ml_table_level_bar_mode[] = {
  { 0, 2 },
  { MLTAG_CONTINUOUS, GTK_LEVEL_BAR_MODE_CONTINUOUS },
  { MLTAG_DISCRETE, GTK_LEVEL_BAR_MODE_DISCRETE },
};

/* input_purpose : conversion table */
const lookup_info ml_table_input_purpose[] = {
  { 0, 10 },
  { MLTAG_NUMBER, GTK_INPUT_PURPOSE_NUMBER },
  { MLTAG_DIGITS, GTK_INPUT_PURPOSE_DIGITS },
  { MLTAG_EMAIL, GTK_INPUT_PURPOSE_EMAIL },
  { MLTAG_PIN, GTK_INPUT_PURPOSE_PIN },
  { MLTAG_URL, GTK_INPUT_PURPOSE_URL },
  { MLTAG_ALPHA, GTK_INPUT_PURPOSE_ALPHA },
  { MLTAG_PASSWORD, GTK_INPUT_PURPOSE_PASSWORD },
  { MLTAG_FREE_FORM, GTK_INPUT_PURPOSE_FREE_FORM },
  { MLTAG_NAME, GTK_INPUT_PURPOSE_NAME },
  { MLTAG_PHONE, GTK_INPUT_PURPOSE_PHONE },
};

/* input_hints : conversion table */
const lookup_info ml_table_input_hints[] = {
  { 0, 9 },
  { MLTAG_WORD_COMPLETION, GTK_INPUT_HINT_WORD_COMPLETION },
  { MLTAG_UPPERCASE_SENTENCES, GTK_INPUT_HINT_UPPERCASE_SENTENCES },
  { MLTAG_LOWERCASE, GTK_INPUT_HINT_LOWERCASE },
  { MLTAG_UPPERCASE_CHARS, GTK_INPUT_HINT_UPPERCASE_CHARS },
  { MLTAG_INHIBIT_OSK, GTK_INPUT_HINT_INHIBIT_OSK },
  { MLTAG_UPPERCASE_WORDS, GTK_INPUT_HINT_UPPERCASE_WORDS },
  { MLTAG_NO_SPELLCHECK, GTK_INPUT_HINT_NO_SPELLCHECK },
  { MLTAG_SPELLCHECK, GTK_INPUT_HINT_SPELLCHECK },
  { MLTAG_NONE, GTK_INPUT_HINT_NONE },
};

/* propagation_phase : conversion table */
const lookup_info ml_table_propagation_phase[] = {
  { 0, 4 },
  { MLTAG_BUBBLE, GTK_PHASE_BUBBLE },
  { MLTAG_CAPTURE, GTK_PHASE_CAPTURE },
  { MLTAG_NONE, GTK_PHASE_NONE },
  { MLTAG_TARGET, GTK_PHASE_TARGET },
};

/* event_sequence_state : conversion table */
const lookup_info ml_table_event_sequence_state[] = {
  { 0, 3 },
  { MLTAG_CLAIMED, GTK_EVENT_SEQUENCE_CLAIMED },
  { MLTAG_DENIED, GTK_EVENT_SEQUENCE_DENIED },
  { MLTAG_NONE, GTK_EVENT_SEQUENCE_NONE },
};

/* pan_direction : conversion table */
const lookup_info ml_table_pan_direction[] = {
  { 0, 4 },
  { MLTAG_RIGHT, GTK_PAN_DIRECTION_RIGHT },
  { MLTAG_UP, GTK_PAN_DIRECTION_UP },
  { MLTAG_DOWN, GTK_PAN_DIRECTION_DOWN },
  { MLTAG_LEFT, GTK_PAN_DIRECTION_LEFT },
};

/* text_window_type : conversion table */
const lookup_info ml_table_text_window_type[] = {
  { 0, 7 },
  { MLTAG_RIGHT, GTK_TEXT_WINDOW_RIGHT },
  { MLTAG_WIDGET, GTK_TEXT_WINDOW_WIDGET },
  { MLTAG_TOP, GTK_TEXT_WINDOW_TOP },
  { MLTAG_PRIVATE, GTK_TEXT_WINDOW_PRIVATE },
  { MLTAG_BOTTOM, GTK_TEXT_WINDOW_BOTTOM },
  { MLTAG_LEFT, GTK_TEXT_WINDOW_LEFT },
  { MLTAG_TEXT, GTK_TEXT_WINDOW_TEXT },
};

/* text_view_layer : conversion table */
const lookup_info ml_table_text_view_layer[] = {
  { 0, 2 },
  { MLTAG_ABOVE, GTK_TEXT_VIEW_LAYER_ABOVE },
  { MLTAG_BELOW, GTK_TEXT_VIEW_LAYER_BELOW },
};

/* text_extend_selection : conversion table */
const lookup_info ml_table_text_extend_selection[] = {
  { 0, 2 },
  { MLTAG_LINE, GTK_TEXT_EXTEND_SELECTION_LINE },
  { MLTAG_WORD, GTK_TEXT_EXTEND_SELECTION_WORD },
};

/* text_search_flag : conversion table */
const lookup_info ml_table_text_search_flag[] = {
  { 0, 3 },
  { MLTAG_VISIBLE_ONLY, GTK_TEXT_SEARCH_VISIBLE_ONLY },
  { MLTAG_TEXT_ONLY, GTK_TEXT_SEARCH_TEXT_ONLY },
  { MLTAG_CASE_INSENSITIVE, GTK_TEXT_SEARCH_CASE_INSENSITIVE },
};

/* toolbar_space_style : conversion table */
const lookup_info ml_table_toolbar_space_style[] = {
  { 0, 2 },
  { MLTAG_EMPTY, GTK_TOOLBAR_SPACE_EMPTY },
  { MLTAG_LINE, GTK_TOOLBAR_SPACE_LINE },
};

/* spin_button_update_policy : conversion table */
const lookup_info ml_table_spin_button_update_policy[] = {
  { 0, 2 },
  { MLTAG_IF_VALID, GTK_UPDATE_IF_VALID },
  { MLTAG_ALWAYS, GTK_UPDATE_ALWAYS },
};

/* spin_type : conversion table */
const lookup_info ml_table_spin_type[] = {
  { 0, 7 },
  { MLTAG_STEP_BACKWARD, GTK_SPIN_STEP_BACKWARD },
  { MLTAG_PAGE_BACKWARD, GTK_SPIN_PAGE_BACKWARD },
  { MLTAG_USER_DEFINED, GTK_SPIN_USER_DEFINED },
  { MLTAG_STEP_FORWARD, GTK_SPIN_STEP_FORWARD },
  { MLTAG_END, GTK_SPIN_END },
  { MLTAG_PAGE_FORWARD, GTK_SPIN_PAGE_FORWARD },
  { MLTAG_HOME, GTK_SPIN_HOME },
};

/* accel_flag : conversion table */
const lookup_info ml_table_accel_flag[] = {
  { 0, 2 },
  { MLTAG_LOCKED, GTK_ACCEL_LOCKED },
  { MLTAG_VISIBLE, GTK_ACCEL_VISIBLE },
};

/* button_box_style : conversion table */
const lookup_info ml_table_button_box_style[] = {
  { 0, 6 },
  { MLTAG_SPREAD, GTK_BUTTONBOX_SPREAD },
  { MLTAG_EXPAND, GTK_BUTTONBOX_EXPAND },
  { MLTAG_END, GTK_BUTTONBOX_END },
  { MLTAG_START, GTK_BUTTONBOX_START },
  { MLTAG_EDGE, GTK_BUTTONBOX_EDGE },
  { MLTAG_CENTER, GTK_BUTTONBOX_CENTER },
};

/* calendar_display_options : conversion table */
const lookup_info ml_table_calendar_display_options[] = {
  { 0, 5 },
  { MLTAG_SHOW_DAY_NAMES, GTK_CALENDAR_SHOW_DAY_NAMES },
  { MLTAG_SHOW_DETAILS, GTK_CALENDAR_SHOW_DETAILS },
  { MLTAG_SHOW_WEEK_NUMBERS, GTK_CALENDAR_SHOW_WEEK_NUMBERS },
  { MLTAG_NO_MONTH_CHANGE, GTK_CALENDAR_NO_MONTH_CHANGE },
  { MLTAG_SHOW_HEADING, GTK_CALENDAR_SHOW_HEADING },
};

/* resize_mode : conversion table */
const lookup_info ml_table_resize_mode[] = {
  { 0, 3 },
  { MLTAG_IMMEDIATE, GTK_RESIZE_IMMEDIATE },
  { MLTAG_QUEUE, GTK_RESIZE_QUEUE },
  { MLTAG_PARENT, GTK_RESIZE_PARENT },
};

/* dest_defaults : conversion table */
const lookup_info ml_table_dest_defaults[] = {
  { 0, 4 },
  { MLTAG_HIGHLIGHT, GTK_DEST_DEFAULT_HIGHLIGHT },
  { MLTAG_MOTION, GTK_DEST_DEFAULT_MOTION },
  { MLTAG_ALL, GTK_DEST_DEFAULT_ALL },
  { MLTAG_DROP, GTK_DEST_DEFAULT_DROP },
};

/* target_flags : conversion table */
const lookup_info ml_table_target_flags[] = {
  { 0, 4 },
  { MLTAG_OTHER_WIDGET, GTK_TARGET_OTHER_WIDGET },
  { MLTAG_SAME_APP, GTK_TARGET_SAME_APP },
  { MLTAG_SAME_WIDGET, GTK_TARGET_SAME_WIDGET },
  { MLTAG_OTHER_APP, GTK_TARGET_OTHER_APP },
};

/* corner_type : conversion table */
const lookup_info ml_table_corner_type[] = {
  { 0, 4 },
  { MLTAG_TOP_LEFT, GTK_CORNER_TOP_LEFT },
  { MLTAG_BOTTOM_LEFT, GTK_CORNER_BOTTOM_LEFT },
  { MLTAG_BOTTOM_RIGHT, GTK_CORNER_BOTTOM_RIGHT },
  { MLTAG_TOP_RIGHT, GTK_CORNER_TOP_RIGHT },
};

/* policy_type : conversion table */
const lookup_info ml_table_policy_type[] = {
  { 0, 4 },
  { MLTAG_AUTOMATIC, GTK_POLICY_AUTOMATIC },
  { MLTAG_ALWAYS, GTK_POLICY_ALWAYS },
  { MLTAG_NEVER, GTK_POLICY_NEVER },
  { MLTAG_EXTERNAL, GTK_POLICY_EXTERNAL },
};

/* tree_model_flags : conversion table */
const lookup_info ml_table_tree_model_flags[] = {
  { 0, 2 },
  { MLTAG_LIST_ONLY, GTK_TREE_MODEL_LIST_ONLY },
  { MLTAG_ITERS_PERSIST, GTK_TREE_MODEL_ITERS_PERSIST },
};

/* tree_view_drop_position : conversion table */
const lookup_info ml_table_tree_view_drop_position[] = {
  { 0, 4 },
  { MLTAG_BEFORE, GTK_TREE_VIEW_DROP_BEFORE },
  { MLTAG_INTO_OR_AFTER, GTK_TREE_VIEW_DROP_INTO_OR_AFTER },
  { MLTAG_AFTER, GTK_TREE_VIEW_DROP_AFTER },
  { MLTAG_INTO_OR_BEFORE, GTK_TREE_VIEW_DROP_INTO_OR_BEFORE },
};

/* tree_view_column_sizing : conversion table */
const lookup_info ml_table_tree_view_column_sizing[] = {
  { 0, 3 },
  { MLTAG_GROW_ONLY, GTK_TREE_VIEW_COLUMN_GROW_ONLY },
  { MLTAG_FIXED, GTK_TREE_VIEW_COLUMN_FIXED },
  { MLTAG_AUTOSIZE, GTK_TREE_VIEW_COLUMN_AUTOSIZE },
};

/* cell_renderer_state : conversion table */
const lookup_info ml_table_cell_renderer_state[] = {
  { 0, 7 },
  { MLTAG_INSENSITIVE, GTK_CELL_RENDERER_INSENSITIVE },
  { MLTAG_FOCUSED, GTK_CELL_RENDERER_FOCUSED },
  { MLTAG_EXPANDABLE, GTK_CELL_RENDERER_EXPANDABLE },
  { MLTAG_EXPANDED, GTK_CELL_RENDERER_EXPANDED },
  { MLTAG_PRELIT, GTK_CELL_RENDERER_PRELIT },
  { MLTAG_SORTED, GTK_CELL_RENDERER_SORTED },
  { MLTAG_SELECTED, GTK_CELL_RENDERER_SELECTED },
};

/* cell_renderer_mode : conversion table */
const lookup_info ml_table_cell_renderer_mode[] = {
  { 0, 3 },
  { MLTAG_ACTIVATABLE, GTK_CELL_RENDERER_MODE_ACTIVATABLE },
  { MLTAG_EDITABLE, GTK_CELL_RENDERER_MODE_EDITABLE },
  { MLTAG_INERT, GTK_CELL_RENDERER_MODE_INERT },
};

/* cell_renderer_accel_mode : conversion table */
const lookup_info ml_table_cell_renderer_accel_mode[] = {
  { 0, 2 },
  { MLTAG_GTK, GTK_CELL_RENDERER_ACCEL_MODE_GTK },
  { MLTAG_OTHER, GTK_CELL_RENDERER_ACCEL_MODE_OTHER },
};

/* buttons_type : conversion table */
const lookup_info ml_table_buttons_type[] = {
  { 0, 6 },
  { MLTAG_CLOSE, GTK_BUTTONS_CLOSE },
  { MLTAG_CANCEL, GTK_BUTTONS_CANCEL },
  { MLTAG_OK, GTK_BUTTONS_OK },
  { MLTAG_YES_NO, GTK_BUTTONS_YES_NO },
  { MLTAG_OK_CANCEL, GTK_BUTTONS_OK_CANCEL },
  { MLTAG_NONE, GTK_BUTTONS_NONE },
};

/* dialog_flag : conversion table */
const lookup_info ml_table_dialog_flag[] = {
  { 0, 3 },
  { MLTAG_USE_HEADER_BAR, GTK_DIALOG_USE_HEADER_BAR },
  { MLTAG_MODAL, GTK_DIALOG_MODAL },
  { MLTAG_DESTROY_WITH_PARENT, GTK_DIALOG_DESTROY_WITH_PARENT },
};

/* response : conversion table */
const lookup_info ml_table_response[] = {
  { 0, 11 },
  { MLTAG_CLOSE, GTK_RESPONSE_CLOSE },
  { MLTAG_CANCEL, GTK_RESPONSE_CANCEL },
  { MLTAG_NO, GTK_RESPONSE_NO },
  { MLTAG_OK, GTK_RESPONSE_OK },
  { MLTAG_YES, GTK_RESPONSE_YES },
  { MLTAG_DELETE_EVENT, GTK_RESPONSE_DELETE_EVENT },
  { MLTAG_APPLY, GTK_RESPONSE_APPLY },
  { MLTAG_HELP, GTK_RESPONSE_HELP },
  { MLTAG_NONE, GTK_RESPONSE_NONE },
  { MLTAG_REJECT, GTK_RESPONSE_REJECT },
  { MLTAG_ACCEPT, GTK_RESPONSE_ACCEPT },
};

/* widget_help_type : conversion table */
const lookup_info ml_table_widget_help_type[] = {
  { 0, 2 },
  { MLTAG_TOOLTIP, GTK_WIDGET_HELP_TOOLTIP },
  { MLTAG_WHATS_THIS, GTK_WIDGET_HELP_WHATS_THIS },
};

/* window_position : conversion table */
const lookup_info ml_table_window_position[] = {
  { 0, 5 },
  { MLTAG_CENTER_ALWAYS, GTK_WIN_POS_CENTER_ALWAYS },
  { MLTAG_CENTER_ON_PARENT, GTK_WIN_POS_CENTER_ON_PARENT },
  { MLTAG_MOUSE, GTK_WIN_POS_MOUSE },
  { MLTAG_NONE, GTK_WIN_POS_NONE },
  { MLTAG_CENTER, GTK_WIN_POS_CENTER },
};

/* window_type : conversion table */
const lookup_info ml_table_window_type[] = {
  { 0, 2 },
  { MLTAG_POPUP, GTK_WINDOW_POPUP },
  { MLTAG_TOPLEVEL, GTK_WINDOW_TOPLEVEL },
};

/* image_type : conversion table */
const lookup_info ml_table_image_type[] = {
  { 0, 8 },
  { MLTAG_ANIMATION, GTK_IMAGE_ANIMATION },
  { MLTAG_SURFACE, GTK_IMAGE_SURFACE },
  { MLTAG_EMPTY, GTK_IMAGE_EMPTY },
  { MLTAG_ICON_SET, GTK_IMAGE_ICON_SET },
  { MLTAG_STOCK, GTK_IMAGE_STOCK },
  { MLTAG_ICON_NAME, GTK_IMAGE_ICON_NAME },
  { MLTAG_GICON, GTK_IMAGE_GICON },
  { MLTAG_PIXBUF, GTK_IMAGE_PIXBUF },
};

/* file_chooser_action : conversion table */
const lookup_info ml_table_file_chooser_action[] = {
  { 0, 4 },
  { MLTAG_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER },
  { MLTAG_CREATE_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER },
  { MLTAG_OPEN, GTK_FILE_CHOOSER_ACTION_OPEN },
  { MLTAG_SAVE, GTK_FILE_CHOOSER_ACTION_SAVE },
};

/* file_chooser_confirmation : conversion table */
const lookup_info ml_table_file_chooser_confirmation[] = {
  { 0, 3 },
  { MLTAG_CONFIRM, GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM },
  { MLTAG_SELECT_AGAIN, GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN },
  { MLTAG_ACCEPT_FILENAME, GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME },
};

/* file_chooser_errot : conversion table */
const lookup_info ml_table_file_chooser_errot[] = {
  { 0, 4 },
  { MLTAG_NONEXISTENT, GTK_FILE_CHOOSER_ERROR_NONEXISTENT },
  { MLTAG_BAD_FILENAME, GTK_FILE_CHOOSER_ERROR_BAD_FILENAME },
  { MLTAG_INCOMPLETE_HOSTNAME, GTK_FILE_CHOOSER_ERROR_INCOMPLETE_HOSTNAME },
  { MLTAG_ALREADY_EXISTS, GTK_FILE_CHOOSER_ERROR_ALREADY_EXISTS },
};

/* file_filter_flags : conversion table */
const lookup_info ml_table_file_filter_flags[] = {
  { 0, 4 },
  { MLTAG_MIME_TYPE, GTK_FILE_FILTER_MIME_TYPE },
  { MLTAG_FILENAME, GTK_FILE_FILTER_FILENAME },
  { MLTAG_URI, GTK_FILE_FILTER_URI },
  { MLTAG_DISPLAY_NAME, GTK_FILE_FILTER_DISPLAY_NAME },
};

/* ui_manager_item_type : conversion table */
const lookup_info ml_table_ui_manager_item_type[] = {
#ifdef HASGTK24
  { 0, 11 },
  { MLTAG_POPUP, GTK_UI_MANAGER_POPUP },
  { MLTAG_POPUP_WITH_ACCELS, GTK_UI_MANAGER_POPUP_WITH_ACCELS },
  { MLTAG_TOOLBAR, GTK_UI_MANAGER_TOOLBAR },
  { MLTAG_ACCELERATOR, GTK_UI_MANAGER_ACCELERATOR },
  { MLTAG_PLACEHOLDER, GTK_UI_MANAGER_PLACEHOLDER },
  { MLTAG_TOOLITEM, GTK_UI_MANAGER_TOOLITEM },
  { MLTAG_AUTO, GTK_UI_MANAGER_AUTO },
  { MLTAG_SEPARATOR, GTK_UI_MANAGER_SEPARATOR },
  { MLTAG_MENU, GTK_UI_MANAGER_MENU },
  { MLTAG_MENUBAR, GTK_UI_MANAGER_MENUBAR },
  { MLTAG_MENUITEM, GTK_UI_MANAGER_MENUITEM },
#else
  {0, 0 }
#endif /* HASGTK24 */
};

/* assistant_page_type : conversion table */
const lookup_info ml_table_assistant_page_type[] = {
#ifdef HASGTK210
  { 0, 6 },
  { MLTAG_PROGRESS, GTK_ASSISTANT_PAGE_PROGRESS },
  { MLTAG_CUSTOM, GTK_ASSISTANT_PAGE_CUSTOM },
  { MLTAG_SUMMARY, GTK_ASSISTANT_PAGE_SUMMARY },
  { MLTAG_CONFIRM, GTK_ASSISTANT_PAGE_CONFIRM },
  { MLTAG_CONTENT, GTK_ASSISTANT_PAGE_CONTENT },
  { MLTAG_INTRO, GTK_ASSISTANT_PAGE_INTRO },
#else
  {0, 0 }
#endif /* HASGTK210 */
};

/* entry_icon_position : conversion table */
const lookup_info ml_table_entry_icon_position[] = {
  { 0, 2 },
  { MLTAG_SECONDARY, GTK_ENTRY_ICON_SECONDARY },
  { MLTAG_PRIMARY, GTK_ENTRY_ICON_PRIMARY },
};

CAMLprim value ml_gtk_get_tables ()
{
  static const lookup_info *ml_lookup_tables[] = {
    ml_table_align,
    ml_table_arrow_type,
    ml_table_attach_options,
    ml_table_baseline_position,
    ml_table_delete_type,
    ml_table_direction_type,
    ml_table_icon_size,
    ml_table_sensitivity_type,
    ml_table_text_direction,
    ml_table_justification,
    ml_table_menu_direction_type,
    ml_table_message_type,
    ml_table_movement_step,
    ml_table_orientation,
    ml_table_pack_type,
    ml_table_position_type,
    ml_table_relief_style,
    ml_table_scroll_step,
    ml_table_scroll_type,
    ml_table_selection_mode,
    ml_table_shadow_type,
    ml_table_state_type,
    ml_table_toolbar_style,
    ml_table_wrap_mode,
    ml_table_sort_type,
    ml_table_pack_direction,
    ml_table_print_pages,
    ml_table_page_set,
    ml_table_number_up_layout,
    ml_table_page_orientation,
    ml_table_print_quality,
    ml_table_print_duplex,
    ml_table_gtk_unit,
    ml_table_tree_view_grid_lines,
    ml_table_drag_result,
    ml_table_size_group_mode,
    ml_table_size_request_mode,
    ml_table_scrollable_policy,
    ml_table_state_flag,
    ml_table_region_flag,
    ml_table_junction_sides,
    ml_table_border_style,
    ml_table_level_bar_mode,
    ml_table_input_purpose,
    ml_table_input_hints,
    ml_table_propagation_phase,
    ml_table_event_sequence_state,
    ml_table_pan_direction,
    ml_table_text_window_type,
    ml_table_text_view_layer,
    ml_table_text_extend_selection,
    ml_table_text_search_flag,
    ml_table_toolbar_space_style,
    ml_table_spin_button_update_policy,
    ml_table_spin_type,
    ml_table_accel_flag,
    ml_table_button_box_style,
    ml_table_calendar_display_options,
    ml_table_resize_mode,
    ml_table_dest_defaults,
    ml_table_target_flags,
    ml_table_corner_type,
    ml_table_policy_type,
    ml_table_tree_model_flags,
    ml_table_tree_view_drop_position,
    ml_table_tree_view_column_sizing,
    ml_table_cell_renderer_state,
    ml_table_cell_renderer_mode,
    ml_table_cell_renderer_accel_mode,
    ml_table_buttons_type,
    ml_table_dialog_flag,
    ml_table_response,
    ml_table_widget_help_type,
    ml_table_window_position,
    ml_table_window_type,
    ml_table_image_type,
    ml_table_file_chooser_action,
    ml_table_file_chooser_confirmation,
    ml_table_file_chooser_errot,
    ml_table_file_filter_flags,
    ml_table_ui_manager_item_type,
    ml_table_assistant_page_type,
    ml_table_entry_icon_position,
  };
  return (value)ml_lookup_tables;}
