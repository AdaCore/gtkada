"""This package contains data which must be edited by hand when adding new
   bindings.
"""

from adaformat import naming, Enum, Proxy, Record, GObject, List, Callback

# General packages that don't depend on others and must be processed first

enums = ("GtkEnums",
         "PangoEnums",
         )

# List of interfaces to bind. These are processed before the widgets themselves
# These are GIR names

interfaces = (
    "Actionable",
    "Activatable",
    # "AppChooser",
    "--Atk.ImplementorIface",
    "Buildable",
    "CellEditable",
    "CellLayout",
    "ColorChooser",
    "Editable",
    "FileChooser",
    "FontChooser",
    "Icon",
    "Orientable",
    "PrintOperationPreview",
    "RecentChooser",
    "Scrollable",
    "StyleProvider",
    "ToolShell",
    "TreeDragDest",
    "TreeDragSource",
    "TreeSortable",
    "TreeModel",

    "Action",
    "ActionGroup",
    "ActionMap",
    "ListModel",
    "--AppInfo",  # Not tested yet, from Gio
    "--AsyncInitable",  # Not tested yet, from Gio
    "--AsyncResult",  # Not tested yet, from Gio
    "--Converter",  # Not tested yet, from Gio
    "--DBusInterface",  # Not tested yet, from Gio
    "--DBusObject",  # Not tested yet, from Gio
    "--DBusObjectManager",  # Not tested yet, from Gio
    "--DesktopAppInfoLookup",  # Not tested yet, from Gio
    "--Drive",  # Not tested yet, from Gio
    "--File",  # Not tested yet, from Gio
    "--FileDescriptorBased",  # Not tested yet, from Gio
    "--Initable",  # Not tested yet, from Gio
    "--LoadableIcon",  # Not tested yet, from Gio
    "--Mount",  # Not tested yet, from Gio
    "--NetworkMonitor",  # Not tested yet, from Gio
    "--PollableInputStream",  # Not tested yet, from Gio
    "--PollableOutputStream",  # Not tested yet, from Gio
    "--Proxy",  # Not tested yet, from Gio
    "--ProxyResolver",  # Not tested yet, from Gio
    "--RemoteActionGroup",  # Not tested yet, from Gio
    "--Seekable",  # Not tested yet, from Gio
    "--SocketConnectable",  # Not tested yet, from Gio
    "--TlsBackend",  # Not tested yet, from Gio
    "--TlsClientConnection",  # Not tested yet, from Gio
    "--TlsFileDatabase",  # Not tested yet, from Gio
    "--TlsServerConnection",  # Not tested yet, from Gio
    "--Volume",  # Not tested yet, from Gio
)

# List of widgets to bind.
# Contains C type names.
# Start the name with -- for objects we do not want to bind

binding = ("--GdkAtom",   # No binding necessary, too low-level
           "GdkCursor",
           "GdkDevice",
           "GdkDeviceManager",
           "GdkDeviceTool",
           "GdkDisplay",
           "GdkDragContext",
           "GdkDrawingContext",
           "GdkEvent",
           "GdkFrameClock",
           "GdkFrameTimings",
           "GdkGLContext",
           "GdkRGBA",
           "GdkMonitor",
           "GdkScreen",
           "GdkSeat",
           "GdkWindow",

           "GApplication",
           "GApplicationCommandLine",
           "--GBytes",   # Function returning arrays
           "--GBufferedInputStream",  # Not tested yet, from Gio
           "--GBufferedOutputStream",  # Not tested yet, from Gio
           "GCancellable",
           "--GCharsetConverter",  # Not tested yet, from Gio
           "--GConverterInputStream",  # Not tested yet, from Gio
           "--GConverterOutputStream",  # Not tested yet, from Gio
           "--GCredentials",  # Not tested yet, from Giov
           "--GDBusActionGroup",  # Not tested yet, from Gio
           "--GDBusAnnotationInfo",  # Not tested yet, from Gio
           "--GDBusArgInfo",  # Not tested yet, from Gio
           "--GDBusAuthObserver",  # Not tested yet, from Gio
           "--GDBusConnection",  # Not tested yet, from Gio
           "--GDBusInterfaceInfo",  # Not tested yet, from Gio
           "--GDBusInterfaceSkeleton",  # Not tested yet, from Gio
           "--GDBusMenuModel",  # Not tested yet, from Gio
           "--GDBusMessage",  # Not tested yet, from Gio
           "--GDBusMethodInfo",  # Not tested yet, from Gio
           "--GDBusMethodInvocation",  # Not tested yet, from Gio
           "--GDBusNodeInfo",  # Not tested yet, from Gio
           "--GDBusObjectManagerClient",  # Not tested yet, from Gio
           "--GDBusObjectManagerServer",  # Not tested yet, from Gio
           "--GDBusObjectProxy",  # Not tested yet, from Gio
           "--GDBusObjectSkeleton",  # Not tested yet, from Gio
           "--GDBusPropertyInfo",  # Not tested yet, from Gio
           "--GDBusProxy",  # Not tested yet, from Gio
           "--GDBusServer",  # Not tested yet, from Gio
           "--GDBusSignalInfo",  # Not tested yet, from Gio
           "--GDataInputStream",  # Not tested yet, from Gio
           "--GDataOutputStream",  # Not tested yet, from Gio
           "--GDesktopAppInfo",  # Not tested yet, from Gio
           "--GEmblem",  # Not tested yet, from Gio
           "--GEmblemedIcon",  # Not tested yet, from Gio
           "--GFileAttributeInfoList",  # Not tested yet, from Gio
           "--GFileAttributeMatcher",  # Not tested yet, from Gio
           "--GFileEnumerator",  # Not tested yet, from Gio
           "--GFileIOStream",  # Not tested yet, from Gio
           "--GFileIcon",  # Not tested yet, from Gio
           "--GFileInfo",  # Not tested yet, from Gio
           "--GFileInputStream",  # Not tested yet, from Gio
           "--GFileMonitor",  # Not tested yet, from Gio
           "--GFileOutputStream",  # Not tested yet, from Gio
           "--GFilenameCompleter",  # Not tested yet, from Gio
           "--GFilterInputStream",  # Not tested yet, from Gio
           "--GFilterOutputStream",  # Not tested yet, from Gio
           "--GIOExtension",  # Not tested yet, from Gio
           "--GIOExtensionPoint",  # Not tested yet, from Gio
           "GIOChannel",
           "--GIOModule",  # Not tested yet, from Gio
           "--GIOModuleScope",  # Not tested yet, from Gio
           "--GIOSchedulerJob",  # Not tested yet, from Gio
           "--GIOStream",  # Not tested yet, from Gio
           "--GInetAddress",  # Not tested yet, from Gio
           "--GInetAddressMask",  # Not tested yet, from Gio
           "--GInetSocketAddress",  # Not tested yet, from Gio
           "--GInputStream",  # Not tested yet, from Gio
           "--GMemoryInputStream",  # Not tested yet, from Gio
           "--GMemoryOutputStream",  # Not tested yet, from Gio
           "GMenuModel",  # Not tested yet, from Gio
           "GMenu",  # Not tested yet, from Gio
           "GMenuAttributeIter",  # Not tested yet, from Gio
           "GMenuItem",  # Not tested yet, from Gio
           "GMenuLinkIter",  # Not tested yet, from Gio
           "--GMountOperation",  # Not tested yet, from Gio
           "--GNativeVolumeMonitor",  # Not tested yet, from Gio
           "--GNetworkAddress",  # Not tested yet, from Gio
           "--GNetworkService",  # Not tested yet, from Gio
           "GNotification",
           "--GOutputStream",  # Not tested yet, from Gio
           "GOptionContext",
           "GPoll",
           "Spawn",
           "Utils",

           "--GParamSpec",   # Bound manually
           "--GParamSpecBoolean",   # Bound manually
           "--GParamSpecBoxed",   # Bound manually
           "--GParamSpecChar",   # Bound manually
           "--GParamSpecDouble",   # Bound manually
           "--GParamSpecEnum",   # Bound manually
           "--GParamSpecFlags",   # Bound manually
           "--GParamSpecFloat",   # Bound manually
           "--GParamSpecGType",   # Bound manually
           "--GParamSpecInt",   # Bound manually
           "--GParamSpecInt64",   # Bound manually
           "--GParamSpecLong",   # Bound manually
           "--GParamSpecObject",   # Bound manually
           "--GParamSpecOverride",   # Bound manually
           "--GParamSpecParam",   # Bound manually
           "--GParamSpecPointer",   # Bound manually
           "--GParamSpecPool",   # Bound manually
           "--GParamSpecString",   # Bound manually
           "--GParamSpecUChar",   # Bound manually
           "--GParamSpecUInt",   # Bound manually
           "--GParamSpecUInt64",   # Bound manually
           "--GParamSpecULong",   # Bound manually
           "--GParamSpecUnichar",   # Bound manually
           "--GParamSpecValueArray",   # Bound manually
           "--GParamSpecVariant",   # Bound manually

           "--GPermission",  # Not tested yet, from Gio
           "--GProxyAddress",  # Not tested yet, from Gio
           "--GProxyAddressEnumerator",  # Not tested yet, from Gio
           "--GResolver",  # Not tested yet, from Gio
           "--GResource",  # Not tested yet, from Gio
           "--GSettings",  # Not tested yet, from Gio
           "--GSettingsSchema",  # Not tested yet, from Gio
           "--GSettingsSchemaSource",  # Not tested yet, from Gio
           "GSimpleAction",
           "GSimpleActionGroup",
           "--GSimpleAsyncResult",  # Not tested yet, from Gio
           "--GSimplePermission",  # Not tested yet, from Gio
           "--GSocket",  # Not tested yet, from Gio
           "--GSocketAddress",  # Not tested yet, from Gio
           "--GSocketAddressEnumerator",  # Not tested yet, from Gio
           "--GSocketClient",  # Not tested yet, from Gio
           "--GSocketConnection",  # Not tested yet, from Gio
           "--GSocketControlMessage",  # Not tested yet, from Gio
           "--GSocketListener",  # Not tested yet, from Gio
           "--GSocketService",  # Not tested yet, from Gio
           "--GSource",   # Manually bound in Glib.Main
           "--GSrvTarget",  # Not tested yet, from Gio
           "--GStaticResource",  # Not tested yet, from Gio
           "GString",
           "--GTcpConnection",  # Not tested yet, from Gio
           "--GTcpWrapperConnection",  # Not tested yet, from Gio
           "--GThemedIcon",  # Not tested yet, from Gio
           "--GThreadedSocketService",  # Not tested yet, from Gio
           "--GTlsCertificate",  # Not tested yet, from Gio
           "--GTlsConnection",  # Not tested yet, from Gio
           "--GTlsDatabase",  # Not tested yet, from Gio
           "--GTlsInteraction",  # Not tested yet, from Gio
           "--GTlsPassword",  # Not tested yet, from Gio
           "--GUnixConnection",  # Not tested yet, from Gio
           "--GUnixCredentialsMessage",  # Not tested yet, from Gio
           "--GUnixFDList",  # Not tested yet, from Gio
           "--GUnixFDMessage",  # Not tested yet, from Gio
           "--GUnixInputStream",  # Not tested yet, from Gio
           "--GUnixMountMonitor",  # Not tested yet, from Gio
           "--GUnixMountPoint",  # Not tested yet, from Gio
           "--GUnixOutputStream",  # Not tested yet, from Gio
           "--GUnixSocketAddress",  # Not tested yet, from Gio
           "GVariant",
           "GVariantIter",
           "GVariantType",
           "--GVfs",  # Not tested yet, from Gio
           "--GVolumeMonitor",  # Not tested yet, from Gio
           "--GZlibCompressor",  # Not tested yet, from Gio
           "--GZlibDecompressor",  # Not tested yet, from Gio

           "--PangoAttrIterator",
           "PangoAttrList",
           "PangoAttribute",
           "--PangoColor",
           "PangoContext",
           "PangoCoverage",
           "PangoFont",
           "PangoFontDescription",
           "PangoFontFace",
           "PangoFontFamily",
           "PangoFontMap",
           "PangoFontMetrics",
           "PangoFontset",
           "--PangoGlyphItem",
           "--PangoGlyphItemIter",
           "--PangoGlyphString",
           "--PangoItem",
           "--PangoRenderer",
           "--PangoScriptIter",
           "PangoLanguage",
           "PangoLayout",
           "PangoLayoutIter",
           "PangoLayoutLine",
           "PangoMatrix",
           "PangoTabArray",

           "GtkAboutDialog",
           "GtkAccelGroup",
           "GtkAccelLabel",
           "GtkAccelMap",
           "--GtkAccessible",  # Needs ATK
           "GtkAction",
           "GtkActionGroup",
           "GtkActionBar",
           "GtkAdjustment",
           "GtkAlignment",
           "--GtkAppChooserButton",  # Needs GFile
           "--GtkAppChooserDialog",  # Needs GFile
           "--GtkAppChooserWidget",  # Needs GFile
           "GtkApplication",
           "GtkApplicationWindow",
           "GtkArrow",
           "--GtkArrowAccessible",  # We do not support atk
           "GtkAspectFrame",
           "GtkAssistant",
           "GtkBin",
           "GtkBindingEntry",
           "GtkBindingSet",
           "GtkBorder",
           "GtkBox",
           "--GtkBooleanCellAccessible",  # We do not support atk
           "GtkBuilder",
           "GtkButton",
           "--GtkButtonAccessible",  # We do not support atk
           "GtkButtonBox",
           "GtkCalendar",
           "--GtkCellAccessible",  # We do not support atk
           "GtkCellArea",
           "GtkCellAreaBox",
           "--GtkCellAreaClass",
           "GtkCellAreaContext",
           "GtkCellRenderer",
           "GtkCellRendererAccel",
           "--GtkCellRendererClass",  # Useless in Ada
           "GtkCellRendererCombo",
           "GtkCellRendererPixbuf",
           "GtkCellRendererProgress",
           "GtkCellRendererSpin",
           "GtkCellRendererSpinner",
           "GtkCellRendererText",
           "GtkCellRendererToggle",
           "GtkCellView",
           "GtkCheckButton",
           "GtkCheckMenuItem",
           "--GtkCheckMenuItemAccessible",  # We do not support atk
           "GtkClipboard",
           "GtkColorButton",
           "GtkColorChooserDialog",
           "GtkColorChooserWidget",
           "GtkColorSelection",
           "GtkColorSelectionDialog",
           "GtkComboBox",
           "--GtkComboBoxAccessible",  # We do not support atk
           "GtkComboBoxText",
           "GtkContainer",
           "--GtkContainerAccessible",  # We do not support atk
           "--GtkContainerCellAccessible",  # We do not support atk
           "--GtkContainerClass",
           "GtkCssProvider",
           "GtkCssSection",
           "GtkDialog",
           "GtkDrawingArea",
           "GtkEntry",
           "--GtkEntryAccessible",  # We do not support atk
           "GtkEntryBuffer",
           "GtkEntryCompletion",
           "--GtkEntryIconAccessible",  # We do not support atk
           "GtkEventBox",
           "GtkEventController",
           "GtkExpander",
           "--GtkExpanderAccessible",  # We do not support atk
           "GtkFileChooserButton",
           "GtkFileChooserDialog",
           "GtkFileChooserWidget",
           "GtkFileFilter",
           "GtkFixed",
           "GtkFlowBox",
           "--GtkFlowBoxAccessible",  # We do not support atk
           "GtkFlowBoxChild",
           "--GtkFlowBoxChildAccessible",  # We do not support atk
           "GtkFontButton",
           "GtkFontChooserDialog",
           "GtkFontChooserWidget",
           "GtkFontSelection",
           "GtkFontSelectionDialog",
           "GtkFrame",
           "--GtkFrameAccessible",  # We do not support atk
           "GtkGesture",
           "GtkGestureDrag",
           "GtkGestureLongPress",
           "GtkGestureMultiPress",
           "GtkGesturePan",
           "GtkGestureRotate",
           "GtkGestureSingle",
           "GtkGestureSwipe",
           "GtkGestureZoom",
           "GtkGradient",
           "GtkGLArea",
           "GtkGrid",
           "GtkHandleBox",
           "GtkHBox",
           "GtkHButtonBox",
           "GtkHeaderBar",
           "GtkHPaned",
           "GtkHScale",
           "GtkHScrollbar",
           "GtkHSeparator",
           "GtkHSV",
           "GtkIconFactory",
           "GtkIconInfo",
           "GtkIconSet",
           "GtkIconSource",
           "GtkIconTheme",
           "GtkIconView",
           "--GtkIconViewAccessible",  # We do not support atk
           "GtkIMContext",
           "GtkIMContextSimple",
           "GtkIMMulticontext",
           "GtkImage",
           "--GtkImageAccessible",  # We do not support atk
           "--GtkImageCellAccessible",  # We do not support atk
           "GtkImageMenuItem",
           "GtkInfoBar",
           "GtkInvisible",
           "GtkLabel",
           "--GtkLabelAccessible",  # We do not support atk
           "GtkLayout",
           "GtkLevelBar",
           "--GtkLevelBarAccessible",  # We do not support atk
           "GtkLinkButton",
           "--GtkLinkButtonAccessible",  # We do not support atk
           "GtkListBox",
           "--GtkListBoxAccessible",  # We do not support atk
           "GtkListBoxRow",
           "--GtkListBoxRowAccessible",  # We do not support atk
           "GtkListStore",
           "--GtkLockButton",  # requires GPermission
           "--GtkLockButtonAccessible",  # We do not support atk
           "GtkMain",
           "GtkMisc",
           "GtkMenu",
           "--GtkMenuAccessible",  # We do not support atk
           "GtkMenuBar",
           "GtkMenuButton",
           "--GtkMenuButtonAccessible",  # We do not support atk
           "GtkMenuItem",
           "--GtkMenuItemAccessible",  # We do not support atk
           "GtkMenuShell",
           "--GtkMenuShellAccessible",  # We do not support atk
           "GtkMenuToolButton",
           "GtkMessageDialog",
           "--GtkMountOperation",  # Requires a lot of GIO
           "GtkNotebook",
           "--GtkNotebookAccessible",  # We do not support atk
           "--GtkNotebookPageAccessible",  # We do not support atk
           "--GtkNumerableIcon",   # Requires a lot of GIO
           "GtkOffscreenWindow",
           "GtkOverlay",
           "GtkPaned",
           "--GtkPanedAccessible",  # We do not support atk
           "GtkPageSetup",
           "GtkPaperSize",
           "--GtkPlacesSidebar",  # Requires GFile
           "GtkPopover",
           "--GtkPopoverAccessible",  # We do not support atk
           "GtkPrintContext",
           "GtkPrintOperation",
           "GtkPrintSettings",
           "--GtkPlug",  # X11-specific, no binding
           "GtkProgressBar",
           "--GtkProgressBarAccessible",  # We do not support atk
           "GtkRadioAction",
           "GtkRadioButton",
           "--GtkRadioButtonAccessible",  # We do not support atk
           "GtkRadioMenuItem",
           "--GtkRadioMenuItemAccessible",  # We do not support atk
           "GtkRadioToolButton",
           "GtkRange",
           "--GtkRangeAccessible",  # We do not support atk
           "--GtkRcStyle",  # manual binding for these deprecated routines
           "GtkRecentAction",
           "GtkRecentChooserDialog",
           "GtkRecentChooserMenu",
           "GtkRecentChooserWidget",
           "GtkRecentFilter",
           "GtkRecentInfo",
           "GtkRecentManager",
           "--GtkRendererCellAccessible",  # We do not support atk
           "GtkRevealer",
           "GtkScale",
           "--GtkScaleAccessible",  # We do not support atk
           "GtkScaleButton",
           "--GtkScaleButtonAccessible",  # We do not support atk
           "GtkSearchBar",
           "GtkSearchEntry",
           "GtkSelectionData",
           "GtkSeparator",
           "GtkSeparatorMenuItem",
           "GtkSeparatorToolItem",
           "GtkShortcutsWindow",
           "GtkSizeGroup",
           "GtkScrollbar",
           "GtkScrolledWindow",
           "--GtkScrolledWindowAccessible",  # We do not support atk
           "GtkSettings",
           "--GtkSocket",  # X11-specific, no binding
           "GtkSpinner",
           "--GtkSpinnerAccessible",  # We do not support atk
           "GtkSpinButton",
           "--GtkSpinButtonAccessible",  # We do not support atk
           "GtkStack",
           "GtkStackSwitcher",
           "GtkStatusbar",
           "--GtkStatusbarAccessible",  # We do not support atk
           "GtkStatusIcon",
           "GtkStockItem",
           "GtkStyle",
           "GtkStyleContext",
           "GtkStyleProperties",
           "GtkSwitch",
           "--GtkSwitchAccessible",  # We do not support atk
           "GtkSymbolicColor",
           "GtkTable",
           "GtkTargetEntry",
           "GtkTargetList",
           "GtkTearoffMenuItem",
           "GtkTextAttributes",
           "GtkTextBuffer",
           "--GtkTextCellAccessible",  # We do not support atk
           "GtkTextChildAnchor",
           "GtkTextIter",
           "GtkTextMark",
           "GtkTextTag",
           "GtkTextTagTable",
           "GtkTextView",
           "--GtkTextViewAccessible",  # We do not support atk
           "GtkThemingEngine",
           "GtkTreeIter",
           "GtkTreeModelFilter",
           "GtkTreeModelSort",
           "GtkTreePath",
           "GtkTreeRowReference",
           "GtkTreeSelection",
           "GtkTreeStore",
           "GtkTreeView",
           "--GtkTreeViewAccessible",  # We do not support atk
           "GtkTreeViewColumn",
           "GtkToggleAction",
           "GtkToggleButton",
           "--GtkToggleButtonAccessible",  # We do not support atk
           "GtkToggleToolButton",
           "GtkToolButton",
           "GtkToolbar",
           "GtkToolItem",
           "GtkToolItemGroup",
           "GtkTooltip",
           "GtkToolPalette",
           "--GtkToplevelAccessible",  # We do not support atk
           "GtkUIManager",
           "GtkVBox",
           "GtkVButtonBox",
           "GtkVPaned",
           "GtkVScale",
           "GtkVScrollbar",
           "GtkVSeparator",
           "GtkViewport",
           "GtkVolumeButton",
           "GtkWidget",
           "--GtkWidgetAccessible",  # We do not support atk
           "GtkWidgetClass",
           "GtkWindow",
           "--GtkWindowAccessible",  # We do not support atk
           "GtkWindowGroup",
           )

# Handling of functions with user data. The names below are the likely names
# for callback functions that accept user_data. The GIR file doesn't point to
# these specific parameters.

user_data_params = ["Data", "Func_Data", "User_Data", "D", "Search_Data"]
destroy_data_params = ["destroy", "func_notify", "notify"]

# Maps c methods to Ada subprograms or enumeration (for docs).
# All methods that are generated automatically will be added
# as they are processed.

naming.cname_to_adaname = {
    "gtk_show_uri":                 "gtk_show_uri()",
    "gtk_icon_factory_add_default": "Gtk.Icon_Factory.Add_Default",
    "gtk_icon_factory_add":         "Gtk.Icon_Factory.Add",
    "gdk_pixbuf_new_from_data":     "Gdk.Pixbuf.Gdk_New_From_Data",
    "gdk_pixbuf_new_from_file":     "Gdk.Pixbuf.Gdk_New_From_File",
    "gdk_pixbuf_new_from_xpm_data": "Gdk.Pixbuf.Gdk_New_From_Xpm_Data",
    "gdk_pixbuf_animation_new_from_file":
                                    "Gdk.Pixbuf.Gdk_New_From_File",
    "gdk_pixbuf_new":               "Gdk.Pixbuf.Gdk_New",
    "gdk_pixbuf_new_subpixbuf":     "Gdk.Pixbuf.Gdk_New_Subpixbuf",
    "gtk_drag_finish":              "Gtk.Dnd.Finish",

    "TRUE": "True",
    "FALSE": "False",
    "NULL": "null",

    "GDK_2BUTTON_PRESS": "Gdk_2button_Press",
    "GDK_3BUTTON_PRESS": "Gdk_3button_Press",
    "GDK_EVENT_LAST": None,

    # ??? Doesn't exist
    "gtk_activatable_get_action": "Gtk.Activatable.Get_Action",
}

# Maps GIR's "name" to a "c:type". This isn't needed for the classes
# themselves, since this is automatically read from the GIR file.
# Mostly used for properties. The values must correspond to entries in
# self.type_exceptions.

naming.girname_to_ctype = {
    "GdkPixbuf.Pixbuf":    "GdkPixbuf",
    "Pango.EllipsizeMode": "PangoEllipsizeMode",
    "Pango.FontDescription": "PangoFontDescription*",
    "Pango.AttrList":      "PangoAttrList",
    "Gio.Action":          "Action",
    "Gio.ActionGroup":     "ActionGroup",
    "Gio.ActionMap":       "ActionMap",
    "Gio.Application":     "GApplication",
    "Gio.ApplicationCommandLine": "GApplicationCommandLine",
    "ApplicationCommandLine": "GApplicationCommandLine",
    "Gio.Icon":            "GIcon*",
    "GLib.Variant":        "GVariant",
    "Gdk.Event":           "GdkEvent*",
    "Gdk.EventButton":     "GdkEventButton*",
    "Gdk.EventMotion":     "GdkEventMotion*",
    "Gdk.EventProximity":  "GdkEventProximity*",
    "Gdk.EventAny":        "GdkEventAny*",
    "Gdk.EventConfigure":  "GdkEventConfigure*",
    "Gdk.EventExpose":     "GdkEventExpose*",
    "Gdk.EventKey":        "GdkEventKey*",
    "Gdk.EventCrossing":   "GdkEventCrossing*",
    "Gdk.EventScroll":     "GdkEventScroll*",
    "Gdk.EventSequence":   "GdkEventSequence*",
    "Gdk.EventWindowState": "GdkEventWindowState*",
    "Gdk.EventVisibility":  "GdkEventVisibility*",
    "Gdk.EventSelection":   "GdkEventSelection*",
    "Gdk.EventFocus":       "GdkEventFocus*",
    "Gdk.EventProperty":    "GdkEventProperty*",
    "Gdk.EventGrabBroken":  "GdkEventGrabBroken*",
    "Gdk.EventOwnerChange": "GdkEventOwnerChange*",
    "Gdk.Pixmap":          "GdkPixmap*",
    "Gdk.Image":           "GdkImage*",
    "Gdk.GLContext":       "GdkGLContext*",
    "Gdk.DragContext":     "GdkDragContext",
    "GdkPixbuf.PixbufAnimation": "GdkPixbufAnimation*",
    "Gdk.Bitmap":          "GdkBitmap*",
    "Gdk.Color":           "GdkColor*",
    "Gdk.Screen":          "GdkScreen",
    "Gdk.RGBA":            "GdkRGBA",
    "GObject.Object":      "GObject*",
    "GObject.Closure":     "GClosure*",
    "cairo.Surface":       "cairo_surface_t*",
    "cairo.Context":       "cairo_t*",
    "cairo.RectangleInt":  "cairo_rectangle_int_t*",
    "ModifierType":        "GdkModifierType",
    "PropertyState":       "GdkPropertyState",
    "TreePath":            "GtkTreePath*",
    "TreeModel":           "GtkTreeModel*",
    "GObject.InitiallyUnowned": "GObject*",  # An alias
    "GObject.ParamSpec":   "GParamSpec",
    "Giochannel":          "GIOChannel*",
}

# Naming exceptions. In particular maps Ada keywords.

naming.exceptions = {
    "Entry": "GEntry",
    "Type":  "The_Type",
    "Range": "GRange",
    "Delay": "The_Delay",
    "Select": "Gtk_Select",
    "End":   "The_End",
    "Return": "Do_Return",
    "Function": "Func",
    "Digits": "The_Digits",
    "Reverse": "Gtk_Reverse",
    "Raise": "Gdk_Raise",
    "Use": "GUse",
    "Uri": "URI",
    "In": "Gtk_In",

    "Gtk_Imcontext": "Gtk_IM_Context",
    "Gtk_Imcontext_Simple": "Gtk_IM_Context_Simple",
    "Gtk_Immulticontext": "Gtk_IM_Multi_Context",

    "Gtk_Uimanager": "Gtk_UI_Manager",
    "Gicon": "G_Icon",
    "Gtk_Glarea": "Gtk_GLArea",
    "Gdk_Glcontext": "Gdk_GLContext",
}

# Maps C types to type descriptions.
# All standard widgets will be added automatically. Only special
# namings are needed here

naming.type_exceptions = {
    "gboolean":          Enum("Boolean",
                              "Glib.Properties.Property_Boolean"),
    "gdouble":  Proxy("Gdouble", "Glib.Properties.Property_Double"),
    "double":   Proxy("Gdouble", "Glib.Properties.Property_Double"),
    "gshort":   Proxy("Gshort",  "Glib.Properties.Property_Int"),
    "gushort":  Proxy("Gushort",  "Glib.Properties.Property_Uint"),
    "int":      Proxy("Glib.Gint",    "Glib.Properties.Property_Int"),
    "gint":     Proxy("Glib.Gint",    "Glib.Properties.Property_Int",
                      default_record_field="0"),
    "gint8":    Proxy("Gint8",   "Glib.Properties.Property_Int"),
    "gint16":   Proxy("Gint16",  "Glib.Properties.Property_Int"),
    "gint32":   Proxy("Gint32",  "Glib.Properties.Property_Int"),
    "gint64":   Proxy("Gint64",  "Glib.Properties.Property_Int"),
    "guint":    Proxy("Guint",   "Glib.Properties.Property_Uint"),
    "guint8":   Proxy("Guint8",  "Glib.Properties.Property_Uint"),
    "guint16":  Proxy("Guint16", "Glib.Properties.Property_Uint"),
    "guint32":  Proxy("Guint32", "Glib.Properties.Property_Uint"),
    "guint64":  Proxy("Guint64", "Glib.Properties.Property_Uint"),
    "gfloat":   Proxy("Gfloat",  "Glib.Properties.Property_Float"),
    "glong":    Proxy("Glong",   "Glib.Properties.Property_Long"),
    "gulong":   Proxy("Gulong"),
    "gsize":    Proxy("Gsize"),
    "gssize":   Proxy("Gssize"),
    "gunichar": Proxy("Gunichar"),
    "gchar":    Proxy("Gchar"),
    "guchar":   Proxy("Guchar"),
    "GAppInfo": Proxy("Glib.GApp_Info"),

    # These should not be necessary, but if we don't put them the gnerated
    # binding is wrong (for instance we end up passing Gdk_Event_Record as
    # parameters to functions, or Gdk_RGBA directly to C)
    "GdkRGBA":  Record("Gdk.RGBA.Gdk_RGBA",  # impose casing
                       "Gdk.RGBA.Property_RGBA"),
    "GdkRGBA*":  Proxy("Gdk.RGBA.Gdk_RGBA", "Gdk.RGBA.Property_RGBA",
                       "Gdk.RGBA.Gdk_RGBA_Or_Null"),
    "GdkEvent*": Proxy("Gdk.Event.Gdk_Event", ""),
    "GIOChannel*": Proxy("Glib.IOChannel.Giochannel", ""),

    "cairo_t*":              Proxy("Cairo.Cairo_Context"),
    "cairo_format_t":        Proxy("Cairo.Cairo_Format"),
    "cairo_rectangle_int_t*": Proxy("Cairo.Region.Cairo_Rectangle_Int"),
    "cairo_content_t":       Proxy("Cairo.Cairo_Content"),
    "cairo_pattern_t*":      Proxy("Cairo.Cairo_Pattern"),
    "cairo_surface_t*":      Proxy("Cairo.Cairo_Surface"),
    "cairo_region_t*":       Proxy("Cairo.Region.Cairo_Region"),
    "cairo_font_options_t":  Proxy("Cairo.Cairo_Font_Options"),

    "GtkWidgetClass":    Proxy("Glib.Object.GObject_Class"),
    "GtkWidgetClass*":   Proxy("Glib.Object.GObject_Class"),

    # Force mapping to a Proxy. This is also hard-coded in GITClass.__init__
    "PangoFontDescription": Proxy("Pango.Font.Pango_Font_Description",
                                  "Pango.Font.Property_Font_Description"),
    "PangoFontDescription*": Proxy("Pango.Font.Pango_Font_Description",
                                   "Pango.Font.Property_Font_Description"),
    "GtkTreeIter*":    Record("Gtk.Tree_Model.Gtk_Tree_Iter",
                              val_or_null="Iter_Or_Null"),
    "GtkTextIter*":    Record("Gtk.Text_Iter.Gtk_Text_Iter",
                              val_or_null="Iter_Or_Null"),
    "PangoAttribute": Proxy("Pango.Attributes.Pango_Attribute"),
    "PangoAttribute*": Proxy("Pango.Attributes.Pango_Attribute"),

    "GError*":           Proxy("Glib.Error.GError"),
    "GObject*":          GObject("Glib.Object.GObject"),
    "GClosure*":         Proxy("System.Address", ""),
    "GInitiallyUnowned": GObject("Glib.Object.GInitiallyUnowned"),
    "GIcon*":            Proxy("Glib.G_Icon.G_Icon"),
    "GValue":            Proxy("Glib.Values.GValue", ""),
    "GdkAtom":           Proxy("Gdk.Types.Gdk_Atom"),
    "GVariantType":      Proxy("Glib.Variant.Gvariant_Type"),
    "GVariantType*":     Proxy("Glib.Variant.Gvariant_Type"),
    "GVariantIter":      Proxy("Glib.Variant.Gvariant_Iter"),

    # Proper generation of lists
    "GdkEventSequence":  Proxy("Gdk.Event.Gdk_Event_Sequence"),

    # Specific to this binding generator (referenced from binding.xml)
    "VisualList":  List("Gdk.Visual.Gdk_Visual_List.Glist"),
    "ObjectList":  List("Glib.Object.Object_Simple_List.Glist"),
    "ObjectSList": List("Glib.Object.Object_List.GSlist"),
    "StringList":  List("Gtk.Enums.String_List.Glist"),
    "StringSList": List("Gtk.Enums.String_SList.GSlist"),
    "TreePathList": List("Gtk.Tree_Model.Gtk_Tree_Path_List.Glist"),
    "TextTagList": List("Gtk.Text_Tag.Text_Tag_List.GSlist"),
    "DeviceList": List("Gdk.Device.Device_List.Glist"),

    "gpointer":       Proxy("System.Address", "",
                            default_record_field="System.Null_Address"),
    "GDestroyNotify": Proxy("Glib.G_Destroy_Notify_Address"),
    "GQuark":        Proxy("Glib.GQuark"),
    "GObject":       Proxy("Glib.Object.GObject"),
    "GParamSpec":    Proxy("Glib.Param_Spec"),
    "GClosure":      Proxy("GClosure"),
    "GConnectFlags": Proxy("Glib.G_Connect_Flags"),
    "GSource":       Proxy("Glib.Main.G_Source"),

    "WidgetPath*":     Proxy("Gtk.Widget.Widget_Path"),

    # ??? Shouldn't we use a naming exception instead ?
    "GtkStatusbar":    GObject("Gtk.Status_Bar.Gtk_Status_Bar"),

    "GtkRcStyle":      GObject("Gtk.Rc.Gtk_Rc_Style"),

    "GtkTreeViewRowSeparatorFunc":
        Callback("Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func"),

    "GKeyFile*":           Proxy("Glib.Key_File.G_Key_File"),

    "GdkColor*": Proxy("Gdk.Color.Gdk_Color",
                       "Gdk.Color.Property_Gdk_Color",
                       "Gdk.Color.Gdk_Color_Or_Null"),
    "GdkDragContext":     GObject("Gdk.Drag_Contexts.Drag_Context"),
    "GdkFont":            Proxy("Gdk.Font.Gdk_Font"),
    "GdkVisual*":         Proxy("Gdk.Visual.Gdk_Visual"),
    "GdkPixmap*":         Proxy("Gdk.Pixmap.Gdk_Pixmap"),
    "GdkBitmap*":         Proxy("Gdk.Bitmap.Gdk_Bitmap"),
    "GdkImage*":          Proxy("Gdk.Image.Gdk_Image"),
    "GdkPixbuf":          GObject("Gdk.Pixbuf.Gdk_Pixbuf"),
    "GdkPixbufAnimation*": Proxy("Gdk.Pixbuf.Gdk_Pixbuf_Animation"),

    "GdkRectangle*":      Proxy("Gdk.Rectangle.Gdk_Rectangle"),

    # in gdkevents.h
    "GdkRectangle":      Proxy("Gdk.Rectangle.Gdk_Rectangle"),

    # ??? The above should not be needed, we should infer it from the Gir.
    # we need it to generate the "Stub" object in Gdk.Device.Get_Position
    "Gdk.ModifierType":   Proxy("Gdk.Types.Gdk_Modifier_Type"),
    "GdkModifierType":    Proxy("Gdk.Types.Gdk_Modifier_Type"),
    "GdkKeyType":         Proxy("Gdk.Types.Gdk_Key_Type"),
    "GdkWindowAttr*":     Proxy("Gdk.Gdk_Window_Attr"),

    # Override type: we do not want to show they derive from GObject
    "GdkCursor":         Proxy("Gdk.Gdk_Cursor"),
    "GdkCursor*":        Proxy("Gdk.Gdk_Cursor"),
    "GdkWindow":         Proxy("Gdk.Gdk_Window"),
    "GdkWindow*":        Proxy("Gdk.Gdk_Window"),
}
