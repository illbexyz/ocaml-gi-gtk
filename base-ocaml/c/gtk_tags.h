/* align : tags and macros */
#define MLTAG_FILL	((value)(779916931*2+1))
#define MLTAG_START	((value)(33139778*2+1))
#define MLTAG_END	((value)(3448763*2+1))
#define MLTAG_CENTER	((value)(945672661*2+1))
#define MLTAG_BASELINE	((value)(-357733243*2+1))

extern const lookup_info ml_table_align[];
#define Val_align(data) ml_lookup_from_c (ml_table_align, data)
#define Align_val(key) ml_lookup_to_c (ml_table_align, key)

/* arrow_type : tags and macros */
#define MLTAG_UP	((value)(19035*2+1))
#define MLTAG_DOWN	((value)(758038626*2+1))
#define MLTAG_LEFT	((value)(846254087*2+1))
#define MLTAG_RIGHT	((value)(-414039108*2+1))
#define MLTAG_NONE	((value)(868932280*2+1))

extern const lookup_info ml_table_arrow_type[];
#define Val_arrow_type(data) ml_lookup_from_c (ml_table_arrow_type, data)
#define Arrow_type_val(key) ml_lookup_to_c (ml_table_arrow_type, key)

/* attach_options : tags and macros */
#define MLTAG_EXPAND	((value)(-151676326*2+1))
#define MLTAG_SHRINK	((value)(-622600503*2+1))

extern const lookup_info ml_table_attach_options[];
#define Val_attach_options(data) ml_lookup_from_c (ml_table_attach_options, data)
#define Attach_options_val(key) ml_lookup_to_c (ml_table_attach_options, key)

/* baseline_position : tags and macros */
#define MLTAG_TOP	((value)(4194933*2+1))
#define MLTAG_BOTTOM	((value)(402363115*2+1))

extern const lookup_info ml_table_baseline_position[];
#define Val_baseline_position(data) ml_lookup_from_c (ml_table_baseline_position, data)
#define Baseline_position_val(key) ml_lookup_to_c (ml_table_baseline_position, key)

/* delete_type : tags and macros */
#define MLTAG_CHARS	((value)(-1012804419*2+1))
#define MLTAG_WORD_ENDS	((value)(967358989*2+1))
#define MLTAG_WORDS	((value)(-866990263*2+1))
#define MLTAG_DISPLAY_LINES	((value)(794927298*2+1))
#define MLTAG_DISPLAY_LINE_ENDS	((value)(-619428858*2+1))
#define MLTAG_PARAGRAPH_ENDS	((value)(-387090935*2+1))
#define MLTAG_PARAGRAPHS	((value)(-854403771*2+1))
#define MLTAG_WHITESPACE	((value)(932606333*2+1))

extern const lookup_info ml_table_delete_type[];
#define Val_delete_type(data) ml_lookup_from_c (ml_table_delete_type, data)
#define Delete_type_val(key) ml_lookup_to_c (ml_table_delete_type, key)

/* direction_type : tags and macros */
#define MLTAG_TAB_FORWARD	((value)(494387803*2+1))
#define MLTAG_TAB_BACKWARD	((value)(649100909*2+1))

extern const lookup_info ml_table_direction_type[];
#define Val_direction_type(data) ml_lookup_from_c (ml_table_direction_type, data)
#define Direction_type_val(key) ml_lookup_to_c (ml_table_direction_type, key)

/* icon_size : tags and macros */
#define MLTAG_INVALID	((value)(991669975*2+1))
#define MLTAG_MENU	((value)(857345439*2+1))
#define MLTAG_SMALL_TOOLBAR	((value)(-163065213*2+1))
#define MLTAG_LARGE_TOOLBAR	((value)(29501815*2+1))
#define MLTAG_BUTTON	((value)(207818226*2+1))
#define MLTAG_DND	((value)(3399034*2+1))
#define MLTAG_DIALOG	((value)(-474631992*2+1))

extern const lookup_info ml_table_icon_size[];
#define Val_icon_size(data) ml_lookup_from_c (ml_table_icon_size, data)
#define Icon_size_val(key) ml_lookup_to_c (ml_table_icon_size, key)

/* sensitivity_type : tags and macros */
#define MLTAG_AUTO	((value)(725067631*2+1))
#define MLTAG_ON	((value)(17695*2+1))
#define MLTAG_OFF	((value)(3944271*2+1))

extern const lookup_info ml_table_sensitivity_type[];
#define Val_sensitivity_type(data) ml_lookup_from_c (ml_table_sensitivity_type, data)
#define Sensitivity_type_val(key) ml_lookup_to_c (ml_table_sensitivity_type, key)

/* text_direction : tags and macros */
#define MLTAG_LTR	((value)(3798218*2+1))
#define MLTAG_RTL	((value)(4096586*2+1))

extern const lookup_info ml_table_text_direction[];
#define Val_text_direction(data) ml_lookup_from_c (ml_table_text_direction, data)
#define Text_direction_val(key) ml_lookup_to_c (ml_table_text_direction, key)

extern const lookup_info ml_table_justification[];
#define Val_justification(data) ml_lookup_from_c (ml_table_justification, data)
#define Justification_val(key) ml_lookup_to_c (ml_table_justification, key)

/* menu_direction_type : tags and macros */
#define MLTAG_PARENT	((value)(536916266*2+1))
#define MLTAG_CHILD	((value)(-1012407940*2+1))
#define MLTAG_NEXT	((value)(868437235*2+1))
#define MLTAG_PREV	((value)(891258611*2+1))

extern const lookup_info ml_table_menu_direction_type[];
#define Val_menu_direction_type(data) ml_lookup_from_c (ml_table_menu_direction_type, data)
#define Menu_direction_type_val(key) ml_lookup_to_c (ml_table_menu_direction_type, key)

/* message_type : tags and macros */
#define MLTAG_INFO	((value)(813432942*2+1))
#define MLTAG_WARNING	((value)(161459772*2+1))
#define MLTAG_QUESTION	((value)(3549990*2+1))
#define MLTAG_ERROR	((value)(-250084440*2+1))
#define MLTAG_OTHER	((value)(879009456*2+1))

extern const lookup_info ml_table_message_type[];
#define Val_message_type(data) ml_lookup_from_c (ml_table_message_type, data)
#define Message_type_val(key) ml_lookup_to_c (ml_table_message_type, key)

/* movement_step : tags and macros */
#define MLTAG_LOGICAL_POSITIONS	((value)(163938868*2+1))
#define MLTAG_VISUAL_POSITIONS	((value)(-939942389*2+1))
#define MLTAG_PAGES	((value)(993747748*2+1))
#define MLTAG_BUFFER_ENDS	((value)(-1017891049*2+1))
#define MLTAG_HORIZONTAL_PAGES	((value)(63037001*2+1))

extern const lookup_info ml_table_movement_step[];
#define Val_movement_step(data) ml_lookup_from_c (ml_table_movement_step, data)
#define Movement_step_val(key) ml_lookup_to_c (ml_table_movement_step, key)

/* orientation : tags and macros */
#define MLTAG_HORIZONTAL	((value)(130904292*2+1))
#define MLTAG_VERTICAL	((value)(-1013232522*2+1))

extern const lookup_info ml_table_orientation[];
#define Val_orientation(data) ml_lookup_from_c (ml_table_orientation, data)
#define Orientation_val(key) ml_lookup_to_c (ml_table_orientation, key)

extern const lookup_info ml_table_pack_type[];
#define Val_pack_type(data) ml_lookup_from_c (ml_table_pack_type, data)
#define Pack_type_val(key) ml_lookup_to_c (ml_table_pack_type, key)

extern const lookup_info ml_table_position_type[];
#define Val_position_type(data) ml_lookup_from_c (ml_table_position_type, data)
#define Position_type_val(key) ml_lookup_to_c (ml_table_position_type, key)

/* relief_style : tags and macros */
#define MLTAG_NORMAL	((value)(-487842265*2+1))
#define MLTAG_HALF	((value)(801698227*2+1))

extern const lookup_info ml_table_relief_style[];
#define Val_relief_style(data) ml_lookup_from_c (ml_table_relief_style, data)
#define Relief_style_val(key) ml_lookup_to_c (ml_table_relief_style, key)

/* scroll_step : tags and macros */
#define MLTAG_STEPS	((value)(33338247*2+1))
#define MLTAG_HORIZONTAL_STEPS	((value)(-897372500*2+1))
#define MLTAG_HORIZONTAL_ENDS	((value)(331551923*2+1))

extern const lookup_info ml_table_scroll_step[];
#define Val_scroll_step(data) ml_lookup_from_c (ml_table_scroll_step, data)
#define Scroll_step_val(key) ml_lookup_to_c (ml_table_scroll_step, key)

/* scroll_type : tags and macros */
#define MLTAG_JUMP	((value)(824872174*2+1))
#define MLTAG_STEP_FORWARD	((value)(-119149966*2+1))
#define MLTAG_STEP_BACKWARD	((value)(-878351754*2+1))
#define MLTAG_PAGE_BACKWARD	((value)(-675774701*2+1))
#define MLTAG_PAGE_FORWARD	((value)(324737141*2+1))
#define MLTAG_STEP_UP	((value)(20924590*2+1))
#define MLTAG_STEP_DOWN	((value)(988297589*2+1))
#define MLTAG_PAGE_UP	((value)(188668299*2+1))
#define MLTAG_PAGE_DOWN	((value)(-258770030*2+1))
#define MLTAG_STEP_LEFT	((value)(-1070970598*2+1))
#define MLTAG_STEP_RIGHT	((value)(-605897911*2+1))
#define MLTAG_PAGE_LEFT	((value)(-170554569*2+1))
#define MLTAG_PAGE_RIGHT	((value)(470897292*2+1))

extern const lookup_info ml_table_scroll_type[];
#define Val_scroll_type(data) ml_lookup_from_c (ml_table_scroll_type, data)
#define Scroll_type_val(key) ml_lookup_to_c (ml_table_scroll_type, key)

/* selection_mode : tags and macros */
#define MLTAG_SINGLE	((value)(-341568888*2+1))
#define MLTAG_BROWSE	((value)(-823948918*2+1))
#define MLTAG_MULTIPLE	((value)(-200117744*2+1))

extern const lookup_info ml_table_selection_mode[];
#define Val_selection_mode(data) ml_lookup_from_c (ml_table_selection_mode, data)
#define Selection_mode_val(key) ml_lookup_to_c (ml_table_selection_mode, key)

/* shadow_type : tags and macros */
#define MLTAG_IN	((value)(16357*2+1))
#define MLTAG_OUT	((value)(3947630*2+1))
#define MLTAG_ETCHED_IN	((value)(860859825*2+1))
#define MLTAG_ETCHED_OUT	((value)(845996322*2+1))

extern const lookup_info ml_table_shadow_type[];
#define Val_shadow_type(data) ml_lookup_from_c (ml_table_shadow_type, data)
#define Shadow_type_val(key) ml_lookup_to_c (ml_table_shadow_type, key)

/* state_type : tags and macros */
#define MLTAG_ACTIVE	((value)(-926356026*2+1))
#define MLTAG_PRELIGHT	((value)(992895443*2+1))
#define MLTAG_SELECTED	((value)(183679579*2+1))
#define MLTAG_INSENSITIVE	((value)(-1071513743*2+1))
#define MLTAG_INCONSISTENT	((value)(171312567*2+1))
#define MLTAG_FOCUSED	((value)(-1051250153*2+1))

extern const lookup_info ml_table_state_type[];
#define Val_state_type(data) ml_lookup_from_c (ml_table_state_type, data)
#define State_type_val(key) ml_lookup_to_c (ml_table_state_type, key)

/* toolbar_style : tags and macros */
#define MLTAG_ICONS	((value)(885381818*2+1))
#define MLTAG_TEXT	((value)(934974637*2+1))
#define MLTAG_BOTH	((value)(735858817*2+1))
#define MLTAG_BOTH_HORIZ	((value)(-947878242*2+1))

extern const lookup_info ml_table_toolbar_style[];
#define Val_toolbar_style(data) ml_lookup_from_c (ml_table_toolbar_style, data)
#define Toolbar_style_val(key) ml_lookup_to_c (ml_table_toolbar_style, key)

/* wrap_mode : tags and macros */
#define MLTAG_CHAR	((value)(746596054*2+1))
#define MLTAG_WORD	((value)(968739274*2+1))
#define MLTAG_WORD_CHAR	((value)(944880811*2+1))

extern const lookup_info ml_table_wrap_mode[];
#define Val_wrap_mode(data) ml_lookup_from_c (ml_table_wrap_mode, data)
#define Wrap_mode_val(key) ml_lookup_to_c (ml_table_wrap_mode, key)

/* sort_type : tags and macros */
#define MLTAG_ASCENDING	((value)(701500856*2+1))
#define MLTAG_DESCENDING	((value)(157124856*2+1))

extern const lookup_info ml_table_sort_type[];
#define Val_sort_type(data) ml_lookup_from_c (ml_table_sort_type, data)
#define Sort_type_val(key) ml_lookup_to_c (ml_table_sort_type, key)

/* pack_direction : tags and macros */
#define MLTAG_TTB	((value)(4196034*2+1))
#define MLTAG_BTT	((value)(3300930*2+1))

extern const lookup_info ml_table_pack_direction[];
#define Val_pack_direction(data) ml_lookup_from_c (ml_table_pack_direction, data)
#define Pack_direction_val(key) ml_lookup_to_c (ml_table_pack_direction, key)

/* print_pages : tags and macros */
#define MLTAG_ALL	((value)(3249409*2+1))
#define MLTAG_CURRENT	((value)(-877170663*2+1))
#define MLTAG_RANGES	((value)(-367784938*2+1))
#define MLTAG_SELECTION	((value)(158558252*2+1))

extern const lookup_info ml_table_print_pages[];
#define Val_print_pages(data) ml_lookup_from_c (ml_table_print_pages, data)
#define Print_pages_val(key) ml_lookup_to_c (ml_table_print_pages, key)

/* page_set : tags and macros */
#define MLTAG_EVEN	((value)(769472282*2+1))
#define MLTAG_ODD	((value)(3943823*2+1))

extern const lookup_info ml_table_page_set[];
#define Val_page_set(data) ml_lookup_from_c (ml_table_page_set, data)
#define Page_set_val(key) ml_lookup_to_c (ml_table_page_set, key)

/* number_up_layout : tags and macros */
#define MLTAG_LEFT_TO_RIGHT_TOP_TO_BOTTOM	((value)(510724182*2+1))
#define MLTAG_LEFT_TO_RIGHT_BOTTOM_TO_TOP	((value)(532980214*2+1))
#define MLTAG_RIGHT_TO_LEFT_TOP_TO_BOTTOM	((value)(250015470*2+1))
#define MLTAG_RIGHT_TO_LEFT_BOTTOM_TO_TOP	((value)(272271502*2+1))
#define MLTAG_TOP_TO_BOTTOM_LEFT_TO_RIGHT	((value)(-265766122*2+1))
#define MLTAG_TOP_TO_BOTTOM_RIGHT_TO_LEFT	((value)(-465909842*2+1))
#define MLTAG_BOTTOM_TO_TOP_LEFT_TO_RIGHT	((value)(-59118410*2+1))
#define MLTAG_BOTTOM_TO_TOP_RIGHT_TO_LEFT	((value)(-259262130*2+1))

extern const lookup_info ml_table_number_up_layout[];
#define Val_number_up_layout(data) ml_lookup_from_c (ml_table_number_up_layout, data)
#define Number_up_layout_val(key) ml_lookup_to_c (ml_table_number_up_layout, key)

/* page_orientation : tags and macros */
#define MLTAG_PORTRAIT	((value)(305443163*2+1))
#define MLTAG_LANDSCAPE	((value)(-133830629*2+1))
#define MLTAG_REVERSE_PORTRAIT	((value)(765848440*2+1))
#define MLTAG_REVERSE_LANDSCAPE	((value)(-542668962*2+1))

extern const lookup_info ml_table_page_orientation[];
#define Val_page_orientation(data) ml_lookup_from_c (ml_table_page_orientation, data)
#define Page_orientation_val(key) ml_lookup_to_c (ml_table_page_orientation, key)

/* print_quality : tags and macros */
#define MLTAG_LOW	((value)(3797108*2+1))
#define MLTAG_HIGH	((value)(802094946*2+1))
#define MLTAG_DRAFT	((value)(-576421631*2+1))

extern const lookup_info ml_table_print_quality[];
#define Val_print_quality(data) ml_lookup_from_c (ml_table_print_quality, data)
#define Print_quality_val(key) ml_lookup_to_c (ml_table_print_quality, key)

/* print_duplex : tags and macros */
#define MLTAG_SIMPLEX	((value)(913865702*2+1))

extern const lookup_info ml_table_print_duplex[];
#define Val_print_duplex(data) ml_lookup_from_c (ml_table_print_duplex, data)
#define Print_duplex_val(key) ml_lookup_to_c (ml_table_print_duplex, key)

/* gtk_unit : tags and macros */
#define MLTAG_POINTS	((value)(699448867*2+1))
#define MLTAG_INCH	((value)(813432266*2+1))
#define MLTAG_MM	((value)(17248*2+1))
#define MLTAG_PIXEL	((value)(-1064173978*2+1))

extern const lookup_info ml_table_gtk_unit[];
#define Val_gtk_unit(data) ml_lookup_from_c (ml_table_gtk_unit, data)
#define Gtk_unit_val(key) ml_lookup_to_c (ml_table_gtk_unit, key)

extern const lookup_info ml_table_tree_view_grid_lines[];
#define Val_tree_view_grid_lines(data) ml_lookup_from_c (ml_table_tree_view_grid_lines, data)
#define Tree_view_grid_lines_val(key) ml_lookup_to_c (ml_table_tree_view_grid_lines, key)

/* drag_result : tags and macros */
#define MLTAG_SUCCESS	((value)(941750691*2+1))
#define MLTAG_NO_TARGET	((value)(335931087*2+1))
#define MLTAG_USER_CANCELLED	((value)(972798013*2+1))
#define MLTAG_TIMEOUT_EXPIRED	((value)(-855284601*2+1))
#define MLTAG_GRAB_BROKEN	((value)(-517427800*2+1))

extern const lookup_info ml_table_drag_result[];
#define Val_drag_result(data) ml_lookup_from_c (ml_table_drag_result, data)
#define Drag_result_val(key) ml_lookup_to_c (ml_table_drag_result, key)

extern const lookup_info ml_table_size_group_mode[];
#define Val_size_group_mode(data) ml_lookup_from_c (ml_table_size_group_mode, data)
#define Size_group_mode_val(key) ml_lookup_to_c (ml_table_size_group_mode, key)

/* size_request_mode : tags and macros */
#define MLTAG_HEIGHT_FOR_WIDTH	((value)(374203192*2+1))
#define MLTAG_WIDTH_FOR_HEIGHT	((value)(956104374*2+1))
#define MLTAG_CONSTANT_SIZE	((value)(269265084*2+1))

extern const lookup_info ml_table_size_request_mode[];
#define Val_size_request_mode(data) ml_lookup_from_c (ml_table_size_request_mode, data)
#define Size_request_mode_val(key) ml_lookup_to_c (ml_table_size_request_mode, key)

/* scrollable_policy : tags and macros */
#define MLTAG_MINIMUM	((value)(-876701266*2+1))
#define MLTAG_NATURAL	((value)(1054011849*2+1))

extern const lookup_info ml_table_scrollable_policy[];
#define Val_scrollable_policy(data) ml_lookup_from_c (ml_table_scrollable_policy, data)
#define Scrollable_policy_val(key) ml_lookup_to_c (ml_table_scrollable_policy, key)

/* state_flag : tags and macros */
#define MLTAG_BACKDROP	((value)(-199474730*2+1))
#define MLTAG_DIR_LTR	((value)(832679288*2+1))
#define MLTAG_DIR_RTL	((value)(832977656*2+1))
#define MLTAG_LINK	((value)(846454778*2+1))
#define MLTAG_VISITED	((value)(587591370*2+1))
#define MLTAG_CHECKED	((value)(318277383*2+1))

extern const lookup_info ml_table_state_flag[];
#define Val_state_flag(data) ml_lookup_from_c (ml_table_state_flag, data)
#define State_flag_val(key) ml_lookup_to_c (ml_table_state_flag, key)

/* region_flag : tags and macros */
#define MLTAG_FIRST	((value)(-24399856*2+1))
#define MLTAG_LAST	((value)(846058070*2+1))
#define MLTAG_ONLY	((value)(879971692*2+1))
#define MLTAG_SORTED	((value)(-491110595*2+1))

extern const lookup_info ml_table_region_flag[];
#define Val_region_flag(data) ml_lookup_from_c (ml_table_region_flag, data)
#define Region_flag_val(key) ml_lookup_to_c (ml_table_region_flag, key)

/* junction_sides : tags and macros */
#define MLTAG_CORNER_TOPLEFT	((value)(-374123694*2+1))
#define MLTAG_CORNER_TOPRIGHT	((value)(172139025*2+1))
#define MLTAG_CORNER_BOTTOMLEFT	((value)(697936860*2+1))
#define MLTAG_CORNER_BOTTOMRIGHT	((value)(870957639*2+1))

extern const lookup_info ml_table_junction_sides[];
#define Val_junction_sides(data) ml_lookup_from_c (ml_table_junction_sides, data)
#define Junction_sides_val(key) ml_lookup_to_c (ml_table_junction_sides, key)

/* border_style : tags and macros */
#define MLTAG_SOLID	((value)(-21763061*2+1))
#define MLTAG_INSET	((value)(1007563965*2+1))
#define MLTAG_OUTSET	((value)(1057354708*2+1))
#define MLTAG_HIDDEN	((value)(-15160470*2+1))
#define MLTAG_DOTTED	((value)(-458079510*2+1))
#define MLTAG_DASHED	((value)(-731655631*2+1))
#define MLTAG_DOUBLE	((value)(-447883503*2+1))
#define MLTAG_GROOVE	((value)(-827963398*2+1))
#define MLTAG_RIDGE	((value)(-414188533*2+1))

extern const lookup_info ml_table_border_style[];
#define Val_border_style(data) ml_lookup_from_c (ml_table_border_style, data)
#define Border_style_val(key) ml_lookup_to_c (ml_table_border_style, key)

/* level_bar_mode : tags and macros */
#define MLTAG_CONTINUOUS	((value)(-803178225*2+1))
#define MLTAG_DISCRETE	((value)(115679673*2+1))

extern const lookup_info ml_table_level_bar_mode[];
#define Val_level_bar_mode(data) ml_lookup_from_c (ml_table_level_bar_mode, data)
#define Level_bar_mode_val(key) ml_lookup_to_c (ml_table_level_bar_mode, key)

/* input_purpose : tags and macros */
#define MLTAG_FREE_FORM	((value)(765882615*2+1))
#define MLTAG_ALPHA	((value)(528801598*2+1))
#define MLTAG_DIGITS	((value)(-408242650*2+1))
#define MLTAG_NUMBER	((value)(-738381111*2+1))
#define MLTAG_PHONE	((value)(1071774542*2+1))
#define MLTAG_URL	((value)(4245327*2+1))
#define MLTAG_EMAIL	((value)(-306379012*2+1))
#define MLTAG_NAME	((value)(868235851*2+1))
#define MLTAG_PASSWORD	((value)(576041755*2+1))
#define MLTAG_PIN	((value)(3994677*2+1))

extern const lookup_info ml_table_input_purpose[];
#define Val_input_purpose(data) ml_lookup_from_c (ml_table_input_purpose, data)
#define Input_purpose_val(key) ml_lookup_to_c (ml_table_input_purpose, key)

/* input_hints : tags and macros */
#define MLTAG_SPELLCHECK	((value)(760344096*2+1))
#define MLTAG_NO_SPELLCHECK	((value)(177523870*2+1))
#define MLTAG_WORD_COMPLETION	((value)(-898866511*2+1))
#define MLTAG_LOWERCASE	((value)(-503440111*2+1))
#define MLTAG_UPPERCASE_CHARS	((value)(-55943568*2+1))
#define MLTAG_UPPERCASE_WORDS	((value)(89870588*2+1))
#define MLTAG_UPPERCASE_SENTENCES	((value)(-771430013*2+1))
#define MLTAG_INHIBIT_OSK	((value)(-22323857*2+1))

extern const lookup_info ml_table_input_hints[];
#define Val_input_hints(data) ml_lookup_from_c (ml_table_input_hints, data)
#define Input_hints_val(key) ml_lookup_to_c (ml_table_input_hints, key)

/* propagation_phase : tags and macros */
#define MLTAG_CAPTURE	((value)(656778342*2+1))
#define MLTAG_BUBBLE	((value)(7310220*2+1))
#define MLTAG_TARGET	((value)(963616593*2+1))

extern const lookup_info ml_table_propagation_phase[];
#define Val_propagation_phase(data) ml_lookup_from_c (ml_table_propagation_phase, data)
#define Propagation_phase_val(key) ml_lookup_to_c (ml_table_propagation_phase, key)

/* event_sequence_state : tags and macros */
#define MLTAG_CLAIMED	((value)(-490442053*2+1))
#define MLTAG_DENIED	((value)(514905435*2+1))

extern const lookup_info ml_table_event_sequence_state[];
#define Val_event_sequence_state(data) ml_lookup_from_c (ml_table_event_sequence_state, data)
#define Event_sequence_state_val(key) ml_lookup_to_c (ml_table_event_sequence_state, key)

extern const lookup_info ml_table_pan_direction[];
#define Val_pan_direction(data) ml_lookup_from_c (ml_table_pan_direction, data)
#define Pan_direction_val(key) ml_lookup_to_c (ml_table_pan_direction, key)

/* text_window_type : tags and macros */
#define MLTAG_PRIVATE	((value)(155386083*2+1))
#define MLTAG_WIDGET	((value)(-25863228*2+1))

extern const lookup_info ml_table_text_window_type[];
#define Val_text_window_type(data) ml_lookup_from_c (ml_table_text_window_type, data)
#define Text_window_type_val(key) ml_lookup_to_c (ml_table_text_window_type, key)

/* text_view_layer : tags and macros */
#define MLTAG_BELOW	((value)(776467089*2+1))
#define MLTAG_ABOVE	((value)(417859325*2+1))

extern const lookup_info ml_table_text_view_layer[];
#define Val_text_view_layer(data) ml_lookup_from_c (ml_table_text_view_layer, data)
#define Text_view_layer_val(key) ml_lookup_to_c (ml_table_text_view_layer, key)

/* text_extend_selection : tags and macros */
#define MLTAG_LINE	((value)(846454772*2+1))

extern const lookup_info ml_table_text_extend_selection[];
#define Val_text_extend_selection(data) ml_lookup_from_c (ml_table_text_extend_selection, data)
#define Text_extend_selection_val(key) ml_lookup_to_c (ml_table_text_extend_selection, key)

/* text_search_flag : tags and macros */
#define MLTAG_VISIBLE_ONLY	((value)(-365153351*2+1))
#define MLTAG_TEXT_ONLY	((value)(-357645954*2+1))
#define MLTAG_CASE_INSENSITIVE	((value)(113105954*2+1))

extern const lookup_info ml_table_text_search_flag[];
#define Val_text_search_flag(data) ml_lookup_from_c (ml_table_text_search_flag, data)
#define Text_search_flag_val(key) ml_lookup_to_c (ml_table_text_search_flag, key)

/* toolbar_space_style : tags and macros */
#define MLTAG_EMPTY	((value)(-305630611*2+1))

extern const lookup_info ml_table_toolbar_space_style[];
#define Val_toolbar_space_style(data) ml_lookup_from_c (ml_table_toolbar_space_style, data)
#define Toolbar_space_style_val(key) ml_lookup_to_c (ml_table_toolbar_space_style, key)

/* spin_button_update_policy : tags and macros */
#define MLTAG_ALWAYS	((value)(-111559985*2+1))
#define MLTAG_IF_VALID	((value)(-640747590*2+1))

extern const lookup_info ml_table_spin_button_update_policy[];
#define Val_spin_button_update_policy(data) ml_lookup_from_c (ml_table_spin_button_update_policy, data)
#define Spin_button_update_policy_val(key) ml_lookup_to_c (ml_table_spin_button_update_policy, key)

/* spin_type : tags and macros */
#define MLTAG_HOME	((value)(802394655*2+1))
#define MLTAG_USER_DEFINED	((value)(-593067403*2+1))

extern const lookup_info ml_table_spin_type[];
#define Val_spin_type(data) ml_lookup_from_c (ml_table_spin_type, data)
#define Spin_type_val(key) ml_lookup_to_c (ml_table_spin_type, key)

/* accel_flag : tags and macros */
#define MLTAG_VISIBLE	((value)(586697810*2+1))
#define MLTAG_LOCKED	((value)(206156042*2+1))

extern const lookup_info ml_table_accel_flag[];
#define Val_accel_flag(data) ml_lookup_from_c (ml_table_accel_flag, data)
#define Accel_flag_val(key) ml_lookup_to_c (ml_table_accel_flag, key)

/* button_box_style : tags and macros */
#define MLTAG_SPREAD	((value)(-166367629*2+1))
#define MLTAG_EDGE	((value)(768577597*2+1))

extern const lookup_info ml_table_button_box_style[];
#define Val_button_box_style(data) ml_lookup_from_c (ml_table_button_box_style, data)
#define Button_box_style_val(key) ml_lookup_to_c (ml_table_button_box_style, key)

/* calendar_display_options : tags and macros */
#define MLTAG_SHOW_HEADING	((value)(859563808*2+1))
#define MLTAG_SHOW_DAY_NAMES	((value)(-1059042013*2+1))
#define MLTAG_NO_MONTH_CHANGE	((value)(575696365*2+1))
#define MLTAG_SHOW_WEEK_NUMBERS	((value)(396996257*2+1))
#define MLTAG_SHOW_DETAILS	((value)(-75011040*2+1))

extern const lookup_info ml_table_calendar_display_options[];
#define Val_calendar_display_options(data) ml_lookup_from_c (ml_table_calendar_display_options, data)
#define Calendar_display_options_val(key) ml_lookup_to_c (ml_table_calendar_display_options, key)

/* resize_mode : tags and macros */
#define MLTAG_QUEUE	((value)(-606550671*2+1))
#define MLTAG_IMMEDIATE	((value)(-884374575*2+1))

extern const lookup_info ml_table_resize_mode[];
#define Val_resize_mode(data) ml_lookup_from_c (ml_table_resize_mode, data)
#define Resize_mode_val(key) ml_lookup_to_c (ml_table_resize_mode, key)

/* dest_defaults : tags and macros */
#define MLTAG_MOTION	((value)(-35638730*2+1))
#define MLTAG_HIGHLIGHT	((value)(-396835308*2+1))
#define MLTAG_DROP	((value)(758186031*2+1))

extern const lookup_info ml_table_dest_defaults[];
#define Val_dest_defaults(data) ml_lookup_from_c (ml_table_dest_defaults, data)
#define Dest_defaults_val(key) ml_lookup_to_c (ml_table_dest_defaults, key)

/* target_flags : tags and macros */
#define MLTAG_SAME_APP	((value)(-580135960*2+1))
#define MLTAG_SAME_WIDGET	((value)(-450430275*2+1))
#define MLTAG_OTHER_APP	((value)(462328882*2+1))
#define MLTAG_OTHER_WIDGET	((value)(-1037695821*2+1))

extern const lookup_info ml_table_target_flags[];
#define Val_target_flags(data) ml_lookup_from_c (ml_table_target_flags, data)
#define Target_flags_val(key) ml_lookup_to_c (ml_table_target_flags, key)

/* corner_type : tags and macros */
#define MLTAG_TOP_LEFT	((value)(-71573167*2+1))
#define MLTAG_BOTTOM_LEFT	((value)(271770907*2+1))
#define MLTAG_TOP_RIGHT	((value)(1068913458*2+1))
#define MLTAG_BOTTOM_RIGHT	((value)(325230632*2+1))

extern const lookup_info ml_table_corner_type[];
#define Val_corner_type(data) ml_lookup_from_c (ml_table_corner_type, data)
#define Corner_type_val(key) ml_lookup_to_c (ml_table_corner_type, key)

/* policy_type : tags and macros */
#define MLTAG_AUTOMATIC	((value)(-916878197*2+1))
#define MLTAG_NEVER	((value)(387872364*2+1))
#define MLTAG_EXTERNAL	((value)(1023988299*2+1))

extern const lookup_info ml_table_policy_type[];
#define Val_policy_type(data) ml_lookup_from_c (ml_table_policy_type, data)
#define Policy_type_val(key) ml_lookup_to_c (ml_table_policy_type, key)

/* tree_model_flags : tags and macros */
#define MLTAG_ITERS_PERSIST	((value)(-339514704*2+1))
#define MLTAG_LIST_ONLY	((value)(-402526163*2+1))

extern const lookup_info ml_table_tree_model_flags[];
#define Val_tree_model_flags(data) ml_lookup_from_c (ml_table_tree_model_flags, data)
#define Tree_model_flags_val(key) ml_lookup_to_c (ml_table_tree_model_flags, key)

/* tree_view_drop_position : tags and macros */
#define MLTAG_BEFORE	((value)(-860553089*2+1))
#define MLTAG_AFTER	((value)(462462460*2+1))
#define MLTAG_INTO_OR_BEFORE	((value)(486315964*2+1))
#define MLTAG_INTO_OR_AFTER	((value)(-475234977*2+1))

extern const lookup_info ml_table_tree_view_drop_position[];
#define Val_tree_view_drop_position(data) ml_lookup_from_c (ml_table_tree_view_drop_position, data)
#define Tree_view_drop_position_val(key) ml_lookup_to_c (ml_table_tree_view_drop_position, key)

/* tree_view_column_sizing : tags and macros */
#define MLTAG_GROW_ONLY	((value)(-657645480*2+1))
#define MLTAG_AUTOSIZE	((value)(505803696*2+1))
#define MLTAG_FIXED	((value)(-24104620*2+1))

extern const lookup_info ml_table_tree_view_column_sizing[];
#define Val_tree_view_column_sizing(data) ml_lookup_from_c (ml_table_tree_view_column_sizing, data)
#define Tree_view_column_sizing_val(key) ml_lookup_to_c (ml_table_tree_view_column_sizing, key)

/* cell_renderer_state : tags and macros */
#define MLTAG_PRELIT	((value)(-516025580*2+1))
#define MLTAG_EXPANDABLE	((value)(-939260684*2+1))
#define MLTAG_EXPANDED	((value)(-749428423*2+1))

extern const lookup_info ml_table_cell_renderer_state[];
#define Val_cell_renderer_state(data) ml_lookup_from_c (ml_table_cell_renderer_state, data)
#define Cell_renderer_state_val(key) ml_lookup_to_c (ml_table_cell_renderer_state, key)

/* cell_renderer_mode : tags and macros */
#define MLTAG_INERT	((value)(1006870658*2+1))
#define MLTAG_ACTIVATABLE	((value)(281666284*2+1))
#define MLTAG_EDITABLE	((value)(791385252*2+1))

extern const lookup_info ml_table_cell_renderer_mode[];
#define Val_cell_renderer_mode(data) ml_lookup_from_c (ml_table_cell_renderer_mode, data)
#define Cell_renderer_mode_val(key) ml_lookup_to_c (ml_table_cell_renderer_mode, key)

/* cell_renderer_accel_mode : tags and macros */
#define MLTAG_GTK	((value)(3549566*2+1))

extern const lookup_info ml_table_cell_renderer_accel_mode[];
#define Val_cell_renderer_accel_mode(data) ml_lookup_from_c (ml_table_cell_renderer_accel_mode, data)
#define Cell_renderer_accel_mode_val(key) ml_lookup_to_c (ml_table_cell_renderer_accel_mode, key)

/* buttons_type : tags and macros */
#define MLTAG_OK	((value)(17692*2+1))
#define MLTAG_CLOSE	((value)(-967749736*2+1))
#define MLTAG_CANCEL	((value)(-357131910*2+1))
#define MLTAG_YES_NO	((value)(126759865*2+1))
#define MLTAG_OK_CANCEL	((value)(863273341*2+1))

extern const lookup_info ml_table_buttons_type[];
#define Val_buttons_type(data) ml_lookup_from_c (ml_table_buttons_type, data)
#define Buttons_type_val(key) ml_lookup_to_c (ml_table_buttons_type, key)

/* dialog_flag : tags and macros */
#define MLTAG_MODAL	((value)(172382221*2+1))
#define MLTAG_DESTROY_WITH_PARENT	((value)(201293662*2+1))
#define MLTAG_USE_HEADER_BAR	((value)(-636073191*2+1))

extern const lookup_info ml_table_dialog_flag[];
#define Val_dialog_flag(data) ml_lookup_from_c (ml_table_dialog_flag, data)
#define Dialog_flag_val(key) ml_lookup_to_c (ml_table_dialog_flag, key)

/* response : tags and macros */
#define MLTAG_REJECT	((value)(889716063*2+1))
#define MLTAG_ACCEPT	((value)(1032404744*2+1))
#define MLTAG_DELETE_EVENT	((value)(225027494*2+1))
#define MLTAG_YES	((value)(4441351*2+1))
#define MLTAG_NO	((value)(17473*2+1))
#define MLTAG_APPLY	((value)(573160782*2+1))
#define MLTAG_HELP	((value)(801897153*2+1))

extern const lookup_info ml_table_response[];
#define Val_response(data) ml_lookup_from_c (ml_table_response, data)
#define Response_val(key) ml_lookup_to_c (ml_table_response, key)

/* widget_help_type : tags and macros */
#define MLTAG_TOOLTIP	((value)(-362774301*2+1))
#define MLTAG_WHATS_THIS	((value)(-226815154*2+1))

extern const lookup_info ml_table_widget_help_type[];
#define Val_widget_help_type(data) ml_lookup_from_c (ml_table_widget_help_type, data)
#define Widget_help_type_val(key) ml_lookup_to_c (ml_table_widget_help_type, key)

/* window_position : tags and macros */
#define MLTAG_MOUSE	((value)(173231621*2+1))
#define MLTAG_CENTER_ALWAYS	((value)(-348647623*2+1))
#define MLTAG_CENTER_ON_PARENT	((value)(-315311776*2+1))

extern const lookup_info ml_table_window_position[];
#define Val_window_position(data) ml_lookup_from_c (ml_table_window_position, data)
#define Window_position_val(key) ml_lookup_to_c (ml_table_window_position, key)

/* window_type : tags and macros */
#define MLTAG_TOPLEVEL	((value)(109789903*2+1))
#define MLTAG_POPUP	((value)(-998030836*2+1))

extern const lookup_info ml_table_window_type[];
#define Val_window_type(data) ml_lookup_from_c (ml_table_window_type, data)
#define Window_type_val(key) ml_lookup_to_c (ml_table_window_type, key)

/* image_type : tags and macros */
#define MLTAG_PIXBUF	((value)(1059740724*2+1))
#define MLTAG_STOCK	((value)(33832630*2+1))
#define MLTAG_ICON_SET	((value)(-268575620*2+1))
#define MLTAG_ANIMATION	((value)(-963813660*2+1))
#define MLTAG_ICON_NAME	((value)(181530641*2+1))
#define MLTAG_GICON	((value)(300343104*2+1))
#define MLTAG_SURFACE	((value)(-585287155*2+1))

extern const lookup_info ml_table_image_type[];
#define Val_image_type(data) ml_lookup_from_c (ml_table_image_type, data)
#define Image_type_val(key) ml_lookup_to_c (ml_table_image_type, key)

/* file_chooser_action : tags and macros */
#define MLTAG_OPEN	((value)(880069578*2+1))
#define MLTAG_SAVE	((value)(923685693*2+1))
#define MLTAG_SELECT_FOLDER	((value)(6390993*2+1))
#define MLTAG_CREATE_FOLDER	((value)(468105425*2+1))

extern const lookup_info ml_table_file_chooser_action[];
#define Val_file_chooser_action(data) ml_lookup_from_c (ml_table_file_chooser_action, data)
#define File_chooser_action_val(key) ml_lookup_to_c (ml_table_file_chooser_action, key)

/* file_chooser_confirmation : tags and macros */
#define MLTAG_CONFIRM	((value)(269316320*2+1))
#define MLTAG_ACCEPT_FILENAME	((value)(934303262*2+1))
#define MLTAG_SELECT_AGAIN	((value)(864149629*2+1))

extern const lookup_info ml_table_file_chooser_confirmation[];
#define Val_file_chooser_confirmation(data) ml_lookup_from_c (ml_table_file_chooser_confirmation, data)
#define File_chooser_confirmation_val(key) ml_lookup_to_c (ml_table_file_chooser_confirmation, key)

/* file_chooser_errot : tags and macros */
#define MLTAG_NONEXISTENT	((value)(-934197375*2+1))
#define MLTAG_BAD_FILENAME	((value)(-668922911*2+1))
#define MLTAG_ALREADY_EXISTS	((value)(215437059*2+1))
#define MLTAG_INCOMPLETE_HOSTNAME	((value)(-311673932*2+1))

extern const lookup_info ml_table_file_chooser_errot[];
#define Val_file_chooser_errot(data) ml_lookup_from_c (ml_table_file_chooser_errot, data)
#define File_chooser_errot_val(key) ml_lookup_to_c (ml_table_file_chooser_errot, key)

/* file_filter_flags : tags and macros */
#define MLTAG_FILENAME	((value)(-789594425*2+1))
#define MLTAG_URI	((value)(4245324*2+1))
#define MLTAG_DISPLAY_NAME	((value)(6085832*2+1))
#define MLTAG_MIME_TYPE	((value)(-982608795*2+1))

extern const lookup_info ml_table_file_filter_flags[];
#define Val_file_filter_flags(data) ml_lookup_from_c (ml_table_file_filter_flags, data)
#define File_filter_flags_val(key) ml_lookup_to_c (ml_table_file_filter_flags, key)

/* ui_manager_item_type : tags and macros */
#define MLTAG_MENUBAR	((value)(976702836*2+1))
#define MLTAG_TOOLBAR	((value)(-363671205*2+1))
#define MLTAG_PLACEHOLDER	((value)(288325459*2+1))
#define MLTAG_MENUITEM	((value)(987452978*2+1))
#define MLTAG_TOOLITEM	((value)(584268907*2+1))
#define MLTAG_SEPARATOR	((value)(752341061*2+1))
#define MLTAG_ACCELERATOR	((value)(-53513845*2+1))
#define MLTAG_POPUP_WITH_ACCELS	((value)(-492352015*2+1))

extern const lookup_info ml_table_ui_manager_item_type[];
#define Val_ui_manager_item_type(data) ml_lookup_from_c (ml_table_ui_manager_item_type, data)
#define Ui_manager_item_type_val(key) ml_lookup_to_c (ml_table_ui_manager_item_type, key)

/* assistant_page_type : tags and macros */
#define MLTAG_CONTENT	((value)(424370457*2+1))
#define MLTAG_INTRO	((value)(1007616588*2+1))
#define MLTAG_SUMMARY	((value)(12377862*2+1))
#define MLTAG_PROGRESS	((value)(-542079059*2+1))
#define MLTAG_CUSTOM	((value)(-233491535*2+1))

extern const lookup_info ml_table_assistant_page_type[];
#define Val_assistant_page_type(data) ml_lookup_from_c (ml_table_assistant_page_type, data)
#define Assistant_page_type_val(key) ml_lookup_to_c (ml_table_assistant_page_type, key)

/* entry_icon_position : tags and macros */
#define MLTAG_PRIMARY	((value)(55579554*2+1))
#define MLTAG_SECONDARY	((value)(-577585324*2+1))

extern const lookup_info ml_table_entry_icon_position[];
#define Val_entry_icon_position(data) ml_lookup_from_c (ml_table_entry_icon_position, data)
#define Entry_icon_position_val(key) ml_lookup_to_c (ml_table_entry_icon_position, key)

