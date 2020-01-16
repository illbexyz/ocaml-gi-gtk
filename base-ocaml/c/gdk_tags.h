/* platform : tags and macros */
#define MLTAG_X11	((value)(4387128*2+1))
#define MLTAG_WIN32	((value)(-933730405*2+1))
#define MLTAG_QUARTZ	((value)(-13833829*2+1))
/* event_type : tags and macros */
#define MLTAG_NOTHING	((value)(-818712595*2+1))
#define MLTAG_DELETE	((value)(492530731*2+1))
#define MLTAG_DESTROY	((value)(609878234*2+1))
#define MLTAG_EXPOSE	((value)(-150979004*2+1))
#define MLTAG_MOTION_NOTIFY	((value)(1002486002*2+1))
#define MLTAG_BUTTON_PRESS	((value)(559079958*2+1))
#define MLTAG_TWO_BUTTON_PRESS	((value)(344913929*2+1))
#define MLTAG_THREE_BUTTON_PRESS	((value)(-820667913*2+1))
#define MLTAG_BUTTON_RELEASE	((value)(-463011046*2+1))
#define MLTAG_KEY_PRESS	((value)(-291069597*2+1))
#define MLTAG_KEY_RELEASE	((value)(-39653465*2+1))
#define MLTAG_ENTER_NOTIFY	((value)(347669104*2+1))
#define MLTAG_LEAVE_NOTIFY	((value)(773096561*2+1))
#define MLTAG_FOCUS_CHANGE	((value)(6790743*2+1))
#define MLTAG_CONFIGURE	((value)(1001679302*2+1))
#define MLTAG_MAP	((value)(3843708*2+1))
#define MLTAG_UNMAP	((value)(618174915*2+1))
#define MLTAG_PROPERTY_NOTIFY	((value)(1017095347*2+1))
#define MLTAG_SELECTION_CLEAR	((value)(-1043152422*2+1))
#define MLTAG_SELECTION_REQUEST	((value)(111837404*2+1))
#define MLTAG_SELECTION_NOTIFY	((value)(10997180*2+1))
#define MLTAG_PROXIMITY_IN	((value)(-953066939*2+1))
#define MLTAG_PROXIMITY_OUT	((value)(67253774*2+1))
#define MLTAG_DRAG_ENTER	((value)(498749229*2+1))
#define MLTAG_DRAG_LEAVE	((value)(528946956*2+1))
#define MLTAG_DRAG_MOTION	((value)(730822241*2+1))
#define MLTAG_DRAG_STATUS	((value)(-433203363*2+1))
#define MLTAG_DROP_START	((value)(-935149326*2+1))
#define MLTAG_DROP_FINISHED	((value)(679812290*2+1))
#define MLTAG_CLIENT_EVENT	((value)(695404486*2+1))
#define MLTAG_VISIBILITY_NOTIFY	((value)(-627886890*2+1))
#define MLTAG_SCROLL	((value)(-102267891*2+1))
#define MLTAG_WINDOW_STATE	((value)(-58144478*2+1))
#define MLTAG_SETTING	((value)(662418800*2+1))
#define MLTAG_OWNER_CHANGE	((value)(838858588*2+1))
#define MLTAG_GRAB_BROKEN	((value)(-517427800*2+1))
#define MLTAG_DAMAGE	((value)(-798540689*2+1))
#define MLTAG_TOUCH_BEGIN	((value)(-24724951*2+1))
#define MLTAG_TOUCH_UPDATE	((value)(596293097*2+1))
#define MLTAG_TOUCH_END	((value)(707456987*2+1))
#define MLTAG_TOUCH_CANCEL	((value)(-726065830*2+1))
#define MLTAG_TOUCHPAD_SWIPE	((value)(131285391*2+1))
#define MLTAG_TOUCHPAD_PINCH	((value)(-1000192177*2+1))

extern const lookup_info ml_table_event_type[];
#define Val_event_type(data) ml_lookup_from_c (ml_table_event_type, data)
#define Event_type_val(key) ml_lookup_to_c (ml_table_event_type, key)

/* gdkVisibilityState : tags and macros */
#define MLTAG_UNOBSCURED	((value)(-112785096*2+1))
#define MLTAG_PARTIAL	((value)(-360666271*2+1))
#define MLTAG_FULLY_OBSCURED	((value)(665041940*2+1))

extern const lookup_info ml_table_gdkVisibilityState[];
#define Val_gdkVisibilityState(data) ml_lookup_from_c (ml_table_gdkVisibilityState, data)
#define GdkVisibilityState_val(key) ml_lookup_to_c (ml_table_gdkVisibilityState, key)

/* GdkTouchpadGesturePhase : tags and macros */
#define MLTAG_BEGIN	((value)(776217097*2+1))
#define MLTAG_UPDATE	((value)(965227017*2+1))
#define MLTAG_END	((value)(3448763*2+1))
#define MLTAG_CANCEL	((value)(-357131910*2+1))

extern const lookup_info ml_table_GdkTouchpadGesturePhase[];
#define Val_GdkTouchpadGesturePhase(data) ml_lookup_from_c (ml_table_GdkTouchpadGesturePhase, data)
#define GdkTouchpadGesturePhase_val(key) ml_lookup_to_c (ml_table_GdkTouchpadGesturePhase, key)

/* gdkScrollDirection : tags and macros */
#define MLTAG_UP	((value)(19035*2+1))
#define MLTAG_DOWN	((value)(758038626*2+1))
#define MLTAG_LEFT	((value)(846254087*2+1))
#define MLTAG_RIGHT	((value)(-414039108*2+1))
#define MLTAG_SMOOTH	((value)(971879470*2+1))

extern const lookup_info ml_table_gdkScrollDirection[];
#define Val_gdkScrollDirection(data) ml_lookup_from_c (ml_table_gdkScrollDirection, data)
#define GdkScrollDirection_val(key) ml_lookup_to_c (ml_table_gdkScrollDirection, key)

/* gdkCrossingMode : tags and macros */
#define MLTAG_NORMAL	((value)(-487842265*2+1))
#define MLTAG_GRAB	((value)(791451596*2+1))
#define MLTAG_UNGRAB	((value)(348357285*2+1))
#define MLTAG_GTK_GRAB	((value)(-519413395*2+1))
#define MLTAG_GTK_UNGRAB	((value)(-790645114*2+1))
#define MLTAG_STATE_CHANGED	((value)(-494181050*2+1))
#define MLTAG_DEVICE_SWITCH	((value)(-437756483*2+1))

extern const lookup_info ml_table_gdkCrossingMode[];
#define Val_gdkCrossingMode(data) ml_lookup_from_c (ml_table_gdkCrossingMode, data)
#define GdkCrossingMode_val(key) ml_lookup_to_c (ml_table_gdkCrossingMode, key)

/* gdkNotifyType : tags and macros */
#define MLTAG_ANCESTOR	((value)(-318434221*2+1))
#define MLTAG_VIRTUAL	((value)(384135659*2+1))
#define MLTAG_INFERIOR	((value)(-386323714*2+1))
#define MLTAG_NONLINEAR	((value)(919364242*2+1))
#define MLTAG_NONLINEAR_VIRTUAL	((value)(-347467778*2+1))
#define MLTAG_UNKNOWN	((value)(-514918550*2+1))

extern const lookup_info ml_table_gdkNotifyType[];
#define Val_gdkNotifyType(data) ml_lookup_from_c (ml_table_gdkNotifyType, data)
#define GdkNotifyType_val(key) ml_lookup_to_c (ml_table_gdkNotifyType, key)

/* gdkSettingAction : tags and macros */
#define MLTAG_NEW	((value)(3894336*2+1))
#define MLTAG_CHANGED	((value)(-861895468*2+1))
#define MLTAG_DELETED	((value)(312687033*2+1))

extern const lookup_info ml_table_gdkSettingAction[];
#define Val_gdkSettingAction(data) ml_lookup_from_c (ml_table_gdkSettingAction, data)
#define GdkSettingAction_val(key) ml_lookup_to_c (ml_table_gdkSettingAction, key)

/* GdkOwnerChange : tags and macros */
#define MLTAG_NEW_OWNER	((value)(71185300*2+1))
#define MLTAG_CLOSE	((value)(-967749736*2+1))

extern const lookup_info ml_table_GdkOwnerChange[];
#define Val_GdkOwnerChange(data) ml_lookup_from_c (ml_table_GdkOwnerChange, data)
#define GdkOwnerChange_val(key) ml_lookup_to_c (ml_table_GdkOwnerChange, key)

/* gdkWindowState : tags and macros */
#define MLTAG_WITHDRAWN	((value)(-875643900*2+1))
#define MLTAG_ICONIFIED	((value)(-625114350*2+1))
#define MLTAG_MAXIMIZED	((value)(-96895496*2+1))
#define MLTAG_STICKY	((value)(1035688233*2+1))
#define MLTAG_FULLSCREEN	((value)(-339890629*2+1))
#define MLTAG_ABOVE	((value)(417859325*2+1))
#define MLTAG_BELOW	((value)(776467089*2+1))
#define MLTAG_FOCUSED	((value)(-1051250153*2+1))
#define MLTAG_TILED	((value)(237188438*2+1))

extern const lookup_info ml_table_gdkWindowState[];
#define Val_gdkWindowState(data) ml_lookup_from_c (ml_table_gdkWindowState, data)
#define GdkWindowState_val(key) ml_lookup_to_c (ml_table_gdkWindowState, key)

/* gdkInputSource : tags and macros */
#define MLTAG_MOUSE	((value)(173231621*2+1))
#define MLTAG_PEN	((value)(3993785*2+1))
#define MLTAG_ERASER	((value)(-122581812*2+1))
#define MLTAG_CURSOR	((value)(-244630826*2+1))
#define MLTAG_KEYBOARD	((value)(-1045476185*2+1))
#define MLTAG_TOUCHSCREEN	((value)(-834979765*2+1))
#define MLTAG_TOUCHPAD	((value)(-642782476*2+1))

extern const lookup_info ml_table_gdkInputSource[];
#define Val_gdkInputSource(data) ml_lookup_from_c (ml_table_gdkInputSource, data)
#define GdkInputSource_val(key) ml_lookup_to_c (ml_table_gdkInputSource, key)

/* gdkInputMode : tags and macros */
#define MLTAG_DISABLED	((value)(-712388228*2+1))
#define MLTAG_SCREEN	((value)(-102766740*2+1))
#define MLTAG_WINDOW	((value)(84885488*2+1))

extern const lookup_info ml_table_gdkInputMode[];
#define Val_gdkInputMode(data) ml_lookup_from_c (ml_table_gdkInputMode, data)
#define GdkInputMode_val(key) ml_lookup_to_c (ml_table_gdkInputMode, key)

/* gdkDeviceType : tags and macros */
#define MLTAG_MASTER	((value)(-308073310*2+1))
#define MLTAG_SLAVE	((value)(-55575881*2+1))
#define MLTAG_FLOATING	((value)(925188294*2+1))

extern const lookup_info ml_table_gdkDeviceType[];
#define Val_gdkDeviceType(data) ml_lookup_from_c (ml_table_gdkDeviceType, data)
#define GdkDeviceType_val(key) ml_lookup_to_c (ml_table_gdkDeviceType, key)

/* gdkVisualType : tags and macros */
#define MLTAG_STATIC_GRAY	((value)(1009448020*2+1))
#define MLTAG_GRAYSCALE	((value)(-292554841*2+1))
#define MLTAG_STATIC_COLOR	((value)(433926066*2+1))
#define MLTAG_PSEUDO_COLOR	((value)(-52356478*2+1))
#define MLTAG_TRUE_COLOR	((value)(553504338*2+1))
#define MLTAG_DIRECT_COLOR	((value)(178400365*2+1))

extern const lookup_info ml_table_gdkVisualType[];
#define Val_gdkVisualType(data) ml_lookup_from_c (ml_table_gdkVisualType, data)
#define GdkVisualType_val(key) ml_lookup_to_c (ml_table_gdkVisualType, key)

/* gdkDragAction : tags and macros */
#define MLTAG_DEFAULT	((value)(462924961*2+1))
#define MLTAG_COPY	((value)(746947509*2+1))
#define MLTAG_MOVE	((value)(857844497*2+1))
#define MLTAG_LINK	((value)(846454778*2+1))
#define MLTAG_PRIVATE	((value)(155386083*2+1))
#define MLTAG_ASK	((value)(3250969*2+1))

extern const lookup_info ml_table_gdkDragAction[];
#define Val_gdkDragAction(data) ml_lookup_from_c (ml_table_gdkDragAction, data)
#define GdkDragAction_val(key) ml_lookup_to_c (ml_table_gdkDragAction, key)

/* gdkDragProtocol : tags and macros */
#define MLTAG_NONE	((value)(868932280*2+1))
#define MLTAG_MOTIF	((value)(173179663*2+1))
#define MLTAG_XDND	((value)(979280930*2+1))
#define MLTAG_ROOTWIN	((value)(478252826*2+1))
#define MLTAG_WIN32_DROPFILES	((value)(810065636*2+1))
#define MLTAG_OLE2	((value)(879870634*2+1))
#define MLTAG_LOCAL	((value)(-153157301*2+1))
#define MLTAG_WAYLAND	((value)(269827706*2+1))

extern const lookup_info ml_table_gdkDragProtocol[];
#define Val_gdkDragProtocol(data) ml_lookup_from_c (ml_table_gdkDragProtocol, data)
#define GdkDragProtocol_val(key) ml_lookup_to_c (ml_table_gdkDragProtocol, key)

/* xdata : tags and macros */
#define MLTAG_BYTES	((value)(998654027*2+1))
#define MLTAG_SHORTS	((value)(-655420297*2+1))
#define MLTAG_INT32S	((value)(-788831899*2+1))

extern const lookup_info ml_table_xdata[];
#define Val_xdata(data) ml_lookup_from_c (ml_table_xdata, data)
#define Xdata_val(key) ml_lookup_to_c (ml_table_xdata, key)

/* property_state : tags and macros */
#define MLTAG_NEW_VALUE	((value)(-41936174*2+1))

extern const lookup_info ml_table_property_state[];
#define Val_property_state(data) ml_lookup_from_c (ml_table_property_state, data)
#define Property_state_val(key) ml_lookup_to_c (ml_table_property_state, key)

/* property_mode : tags and macros */
#define MLTAG_REPLACE	((value)(721165332*2+1))
#define MLTAG_PREPEND	((value)(934570734*2+1))
#define MLTAG_APPEND	((value)(-1034514982*2+1))

extern const lookup_info ml_table_property_mode[];
#define Val_property_mode(data) ml_lookup_from_c (ml_table_property_mode, data)
#define Property_mode_val(key) ml_lookup_to_c (ml_table_property_mode, key)

/* GdkWindowWindowClass : tags and macros */
#define MLTAG_INPUT_OUTPUT	((value)(382763510*2+1))
#define MLTAG_INPUT_ONLY	((value)(-961741535*2+1))

extern const lookup_info ml_table_GdkWindowWindowClass[];
#define Val_GdkWindowWindowClass(data) ml_lookup_from_c (ml_table_GdkWindowWindowClass, data)
#define GdkWindowWindowClass_val(key) ml_lookup_to_c (ml_table_GdkWindowWindowClass, key)

/* GdkWindowType : tags and macros */
#define MLTAG_ROOT	((value)(913290786*2+1))
#define MLTAG_TOPLEVEL	((value)(109789903*2+1))
#define MLTAG_CHILD	((value)(-1012407940*2+1))
#define MLTAG_TEMP	((value)(934972180*2+1))
#define MLTAG_FOREIGN	((value)(-641800460*2+1))
#define MLTAG_OFFSCREEN	((value)(352910331*2+1))
#define MLTAG_SUBSURFACE	((value)(-26517331*2+1))

extern const lookup_info ml_table_GdkWindowType[];
#define Val_GdkWindowType(data) ml_lookup_from_c (ml_table_GdkWindowType, data)
#define GdkWindowType_val(key) ml_lookup_to_c (ml_table_GdkWindowType, key)

/* window_attributes_type : tags and macros */
#define MLTAG_TITLE	((value)(237587832*2+1))
#define MLTAG_X	((value)(88*2+1))
#define MLTAG_Y	((value)(89*2+1))
#define MLTAG_VISUAL	((value)(571395776*2+1))
#define MLTAG_WMCLASS	((value)(831312962*2+1))
#define MLTAG_NOREDIR	((value)(644273017*2+1))
#define MLTAG_TYPE_HINT	((value)(301286508*2+1))

extern const lookup_info ml_table_window_attributes_type[];
#define Val_window_attributes_type(data) ml_lookup_from_c (ml_table_window_attributes_type, data)
#define Window_attributes_type_val(key) ml_lookup_to_c (ml_table_window_attributes_type, key)

/* window_hints : tags and macros */
#define MLTAG_POS	((value)(3996020*2+1))
#define MLTAG_MIN_SIZE	((value)(700897774*2+1))
#define MLTAG_MAX_SIZE	((value)(-592658532*2+1))
#define MLTAG_BASE_SIZE	((value)(-465111313*2+1))
#define MLTAG_ASPECT	((value)(-58048040*2+1))
#define MLTAG_RESIZE_INC	((value)(-810746445*2+1))
#define MLTAG_WIN_GRAVITY	((value)(401357739*2+1))
#define MLTAG_USER_POS	((value)(96016960*2+1))
#define MLTAG_USER_SIZE	((value)(-30082443*2+1))

extern const lookup_info ml_table_window_hints[];
#define Val_window_hints(data) ml_lookup_from_c (ml_table_window_hints, data)
#define Window_hints_val(key) ml_lookup_to_c (ml_table_window_hints, key)

/* GdkWMdecoration : tags and macros */
#define MLTAG_ALL	((value)(3249409*2+1))
#define MLTAG_BORDER	((value)(379386092*2+1))
#define MLTAG_RESIZEH	((value)(-481873964*2+1))
#define MLTAG_MENU	((value)(857345439*2+1))
#define MLTAG_MINIMIZE	((value)(-83964130*2+1))
#define MLTAG_MAXIMIZE	((value)(769963212*2+1))

extern const lookup_info ml_table_GdkWMdecoration[];
#define Val_GdkWMdecoration(data) ml_lookup_from_c (ml_table_GdkWMdecoration, data)
#define GdkWMdecoration_val(key) ml_lookup_to_c (ml_table_GdkWMdecoration, key)

/* GdkWMFunction : tags and macros */
#define MLTAG_RESIZE	((value)(989726196*2+1))

extern const lookup_info ml_table_GdkWMFunction[];
#define Val_GdkWMFunction(data) ml_lookup_from_c (ml_table_GdkWMFunction, data)
#define GdkWMFunction_val(key) ml_lookup_to_c (ml_table_GdkWMFunction, key)

/* gravity : tags and macros */
#define MLTAG_NORTH_WEST	((value)(-589513399*2+1))
#define MLTAG_NORTH	((value)(498572453*2+1))
#define MLTAG_NORTH_EAST	((value)(-789324521*2+1))
#define MLTAG_WEST	((value)(968242223*2+1))
#define MLTAG_CENTER	((value)(945672661*2+1))
#define MLTAG_EAST	((value)(768431101*2+1))
#define MLTAG_SOUTH_WEST	((value)(-907515135*2+1))
#define MLTAG_SOUTH	((value)(-21313043*2+1))
#define MLTAG_SOUTH_EAST	((value)(1040157391*2+1))
#define MLTAG_STATIC	((value)(947816622*2+1))

extern const lookup_info ml_table_gravity[];
#define Val_gravity(data) ml_lookup_from_c (ml_table_gravity, data)
#define Gravity_val(key) ml_lookup_to_c (ml_table_gravity, key)

extern const lookup_info ml_table_window_edge[];
#define Val_window_edge(data) ml_lookup_from_c (ml_table_window_edge, data)
#define Window_edge_val(key) ml_lookup_to_c (ml_table_window_edge, key)

/* fullscreen_mode : tags and macros */
#define MLTAG_ON_CURRENT_MONITOR	((value)(-69529932*2+1))
#define MLTAG_ON_ALL_MONITORS	((value)(-144892617*2+1))

extern const lookup_info ml_table_fullscreen_mode[];
#define Val_fullscreen_mode(data) ml_lookup_from_c (ml_table_fullscreen_mode, data)
#define Fullscreen_mode_val(key) ml_lookup_to_c (ml_table_fullscreen_mode, key)

/* gdkModifier : tags and macros */
#define MLTAG_SHIFT	((value)(-99539870*2+1))
#define MLTAG_LOCK	((value)(846750699*2+1))
#define MLTAG_CONTROL	((value)(425017149*2+1))
#define MLTAG_MOD1	((value)(857840463*2+1))
#define MLTAG_MOD2	((value)(857840464*2+1))
#define MLTAG_MOD3	((value)(857840465*2+1))
#define MLTAG_MOD4	((value)(857840466*2+1))
#define MLTAG_MOD5	((value)(857840467*2+1))
#define MLTAG_BUTTON1	((value)(-901175809*2+1))
#define MLTAG_BUTTON2	((value)(-901175808*2+1))
#define MLTAG_BUTTON3	((value)(-901175807*2+1))
#define MLTAG_BUTTON4	((value)(-901175806*2+1))
#define MLTAG_BUTTON5	((value)(-901175805*2+1))
#define MLTAG_SUPER	((value)(44972379*2+1))
#define MLTAG_HYPER	((value)(803910220*2+1))
#define MLTAG_META	((value)(857346757*2+1))
#define MLTAG_RELEASE	((value)(-658417241*2+1))

extern const lookup_info ml_table_gdkModifier[];
#define Val_gdkModifier(data) ml_lookup_from_c (ml_table_gdkModifier, data)
#define GdkModifier_val(key) ml_lookup_to_c (ml_table_gdkModifier, key)

/* gtkModifierIntent : tags and macros */
#define MLTAG_PRIMARY_ACCELERATOR	((value)(189341230*2+1))
#define MLTAG_CONTEXT_MENU	((value)(-104672945*2+1))
#define MLTAG_EXTEND_SELECTION	((value)(1018754631*2+1))
#define MLTAG_MODIFY_SELECTION	((value)(701612263*2+1))
#define MLTAG_NO_TEXT_INPUT	((value)(-686229994*2+1))
#define MLTAG_SHIFT_GROUP	((value)(-528102334*2+1))
#define MLTAG_DEFAULT_MOD_MASK	((value)(406246791*2+1))

extern const lookup_info ml_table_gtkModifierIntent[];
#define Val_gtkModifierIntent(data) ml_lookup_from_c (ml_table_gtkModifierIntent, data)
#define GtkModifierIntent_val(key) ml_lookup_to_c (ml_table_gtkModifierIntent, key)

/* status : tags and macros */
#define MLTAG_OK	((value)(17692*2+1))
#define MLTAG_ERROR	((value)(-250084440*2+1))
#define MLTAG_ERROR_PARAM	((value)(347943254*2+1))
#define MLTAG_ERROR_FILE	((value)(931098035*2+1))
#define MLTAG_ERROR_MEM	((value)(187491998*2+1))

extern const lookup_info ml_table_status[];
#define Val_status(data) ml_lookup_from_c (ml_table_status, data)
#define Status_val(key) ml_lookup_to_c (ml_table_status, key)

/* grab_status : tags and macros */
#define MLTAG_SUCCESS	((value)(941750691*2+1))
#define MLTAG_ALREADY_GRABBED	((value)(-166355506*2+1))
#define MLTAG_INVALID_TIME	((value)(492230069*2+1))
#define MLTAG_NOT_VIEWABLE	((value)(422744715*2+1))
#define MLTAG_FROZEN	((value)(-397199968*2+1))
#define MLTAG_FAILED	((value)(444690877*2+1))

extern const lookup_info ml_table_grab_status[];
#define Val_grab_status(data) ml_lookup_from_c (ml_table_grab_status, data)
#define Grab_status_val(key) ml_lookup_to_c (ml_table_grab_status, key)

/* grab_ownership : tags and macros */
#define MLTAG_APPLICATION	((value)(8042288*2+1))

extern const lookup_info ml_table_grab_ownership[];
#define Val_grab_ownership(data) ml_lookup_from_c (ml_table_grab_ownership, data)
#define Grab_ownership_val(key) ml_lookup_to_c (ml_table_grab_ownership, key)

/* event_mask : tags and macros */
#define MLTAG_EXPOSURE	((value)(-431242489*2+1))
#define MLTAG_POINTER_MOTION	((value)(300690648*2+1))
#define MLTAG_POINTER_MOTION_HINT	((value)(-362125938*2+1))
#define MLTAG_BUTTON_MOTION	((value)(600815651*2+1))
#define MLTAG_BUTTON1_MOTION	((value)(820800438*2+1))
#define MLTAG_BUTTON2_MOTION	((value)(-367075883*2+1))
#define MLTAG_BUTTON3_MOTION	((value)(592531444*2+1))
#define MLTAG_STRUCTURE	((value)(-1029192685*2+1))
#define MLTAG_PROPERTY_CHANGE	((value)(-1034331302*2+1))
#define MLTAG_SUBSTRUCTURE	((value)(-255536461*2+1))
#define MLTAG_TOUCH	((value)(304172959*2+1))
#define MLTAG_SMOOTH_SCROLL	((value)(895476542*2+1))
#define MLTAG_TOUCHPAD_GESTURE	((value)(865535710*2+1))
#define MLTAG_ALL_EVENTS	((value)(619809239*2+1))

extern const lookup_info ml_table_event_mask[];
#define Val_event_mask(data) ml_lookup_from_c (ml_table_event_mask, data)
#define Event_mask_val(key) ml_lookup_to_c (ml_table_event_mask, key)

/* gl_error : tags and macros */
#define MLTAG_NOT_AVAILABLE	((value)(573736989*2+1))
#define MLTAG_UNSUPPORTED_FORMAT	((value)(78864609*2+1))
#define MLTAG_UNSUPPORTED_PROFILE	((value)(744221727*2+1))

extern const lookup_info ml_table_gl_error[];
#define Val_gl_error(data) ml_lookup_from_c (ml_table_gl_error, data)
#define Gl_error_val(key) ml_lookup_to_c (ml_table_gl_error, key)

/* window_type_hint : tags and macros */
#define MLTAG_DIALOG	((value)(-474631992*2+1))
#define MLTAG_TOOLBAR	((value)(-363671205*2+1))
#define MLTAG_SPLASHSCREEN	((value)(-214066157*2+1))
#define MLTAG_UTILITY	((value)(525269836*2+1))
#define MLTAG_DOCK	((value)(758034163*2+1))
#define MLTAG_DESKTOP	((value)(510171580*2+1))
#define MLTAG_DROPDOWN_MENU	((value)(-656881715*2+1))
#define MLTAG_POPUP_MENU	((value)(-1058997262*2+1))
#define MLTAG_TOOLTIP	((value)(-362774301*2+1))
#define MLTAG_NOTIFICATION	((value)(-94810389*2+1))
#define MLTAG_COMBO	((value)(-934584274*2+1))
#define MLTAG_DND	((value)(3399034*2+1))

extern const lookup_info ml_table_window_type_hint[];
#define Val_window_type_hint(data) ml_lookup_from_c (ml_table_window_type_hint, data)
#define Window_type_hint_val(key) ml_lookup_to_c (ml_table_window_type_hint, key)

/* axis_use : tags and macros */
#define MLTAG_IGNORE	((value)(-984914670*2+1))
#define MLTAG_PRESSURE	((value)(-912049211*2+1))
#define MLTAG_XTILT	((value)(-486498411*2+1))
#define MLTAG_YTILT	((value)(-161008618*2+1))
#define MLTAG_WHEEL	((value)(-945263493*2+1))
#define MLTAG_LAST	((value)(846058070*2+1))

extern const lookup_info ml_table_axis_use[];
#define Val_axis_use(data) ml_lookup_from_c (ml_table_axis_use, data)
#define Axis_use_val(key) ml_lookup_to_c (ml_table_axis_use, key)

/* gdkCursorType : tags and macros */
#define MLTAG_X_CURSOR	((value)(154765085*2+1))
#define MLTAG_ARROW	((value)(595440041*2+1))
#define MLTAG_BASED_ARROW_DOWN	((value)(395671268*2+1))
#define MLTAG_BASED_ARROW_UP	((value)(643103837*2+1))
#define MLTAG_BOAT	((value)(735854592*2+1))
#define MLTAG_BOGOSITY	((value)(724821840*2+1))
#define MLTAG_BOTTOM_LEFT_CORNER	((value)(-840121927*2+1))
#define MLTAG_BOTTOM_RIGHT_CORNER	((value)(-544591284*2+1))
#define MLTAG_BOTTOM_SIDE	((value)(349596331*2+1))
#define MLTAG_BOTTOM_TEE	((value)(-1067310304*2+1))
#define MLTAG_BOX_SPIRAL	((value)(866974533*2+1))
#define MLTAG_CENTER_PTR	((value)(-847502556*2+1))
#define MLTAG_CIRCLE	((value)(143662608*2+1))
#define MLTAG_CLOCK	((value)(-967753298*2+1))
#define MLTAG_COFFEE_MUG	((value)(-17670868*2+1))
#define MLTAG_CROSS	((value)(-901212320*2+1))
#define MLTAG_CROSS_REVERSE	((value)(642211299*2+1))
#define MLTAG_CROSSHAIR	((value)(102807170*2+1))
#define MLTAG_DIAMOND_CROSS	((value)(760772821*2+1))
#define MLTAG_DOT	((value)(3399273*2+1))
#define MLTAG_DOTBOX	((value)(-458972382*2+1))
#define MLTAG_DOUBLE_ARROW	((value)(-971312453*2+1))
#define MLTAG_DRAFT_LARGE	((value)(346122909*2+1))
#define MLTAG_DRAFT_SMALL	((value)(609298345*2+1))
#define MLTAG_DRAPED_BOX	((value)(645695112*2+1))
#define MLTAG_EXCHANGE	((value)(535745059*2+1))
#define MLTAG_FLEUR	((value)(8222812*2+1))
#define MLTAG_GOBBLER	((value)(-890128687*2+1))
#define MLTAG_GUMBY	((value)(433912310*2+1))
#define MLTAG_HAND1	((value)(537660898*2+1))
#define MLTAG_HAND2	((value)(537660899*2+1))
#define MLTAG_HEART	((value)(581375846*2+1))
#define MLTAG_ICON	((value)(812887929*2+1))
#define MLTAG_IRON_CROSS	((value)(-19196439*2+1))
#define MLTAG_LEFT_PTR	((value)(-907074986*2+1))
#define MLTAG_LEFT_SIDE	((value)(-381540337*2+1))
#define MLTAG_LEFT_TEE	((value)(-906879428*2+1))
#define MLTAG_LEFTBUTTON	((value)(-966386375*2+1))
#define MLTAG_LL_ANGLE	((value)(-267328140*2+1))
#define MLTAG_LR_ANGLE	((value)(-376328710*2+1))
#define MLTAG_MAN	((value)(3843706*2+1))
#define MLTAG_MIDDLEBUTTON	((value)(-561104121*2+1))
#define MLTAG_PENCIL	((value)(-353067059*2+1))
#define MLTAG_PIRATE	((value)(993153369*2+1))
#define MLTAG_PLUS	((value)(890963802*2+1))
#define MLTAG_QUESTION_ARROW	((value)(-214104944*2+1))
#define MLTAG_RIGHT_PTR	((value)(-812837749*2+1))
#define MLTAG_RIGHT_SIDE	((value)(-841472966*2+1))
#define MLTAG_RIGHT_TEE	((value)(-812642191*2+1))
#define MLTAG_RIGHTBUTTON	((value)(-452147538*2+1))
#define MLTAG_RTL_LOGO	((value)(1071412416*2+1))
#define MLTAG_SAILBOAT	((value)(950785137*2+1))
#define MLTAG_SB_DOWN_ARROW	((value)(-894963780*2+1))
#define MLTAG_SB_H_DOUBLE_ARROW	((value)(35713346*2+1))
#define MLTAG_SB_LEFT_ARROW	((value)(1018480929*2+1))
#define MLTAG_SB_RIGHT_ARROW	((value)(-135609674*2+1))
#define MLTAG_SB_UP_ARROW	((value)(1052745333*2+1))
#define MLTAG_SB_V_DOUBLE_ARROW	((value)(664502388*2+1))
#define MLTAG_SHUTTLE	((value)(-302205415*2+1))
#define MLTAG_SIZING	((value)(-208394178*2+1))
#define MLTAG_SPIDER	((value)(-266222555*2+1))
#define MLTAG_SPRAYCAN	((value)(-77171517*2+1))
#define MLTAG_STAR	((value)(924625874*2+1))
#define MLTAG_TARGET	((value)(963616593*2+1))
#define MLTAG_TCROSS	((value)(-532486516*2+1))
#define MLTAG_TOP_LEFT_ARROW	((value)(520760571*2+1))
#define MLTAG_TOP_LEFT_CORNER	((value)(476010563*2+1))
#define MLTAG_TOP_RIGHT_CORNER	((value)(895177858*2+1))
#define MLTAG_TOP_SIDE	((value)(6252257*2+1))
#define MLTAG_TOP_TEE	((value)(173416362*2+1))
#define MLTAG_TREK	((value)(935616868*2+1))
#define MLTAG_UL_ANGLE	((value)(-220796789*2+1))
#define MLTAG_UMBRELLA	((value)(-940333884*2+1))
#define MLTAG_UR_ANGLE	((value)(-329797359*2+1))
#define MLTAG_WATCH	((value)(-1022144977*2+1))
#define MLTAG_XTERM	((value)(-486695996*2+1))

extern const lookup_info ml_table_gdkCursorType[];
#define Val_gdkCursorType(data) ml_lookup_from_c (ml_table_gdkCursorType, data)
#define GdkCursorType_val(key) ml_lookup_to_c (ml_table_gdkCursorType, key)

