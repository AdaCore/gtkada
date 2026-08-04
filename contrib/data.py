"""This package contains data which must be edited by hand when adding new
bindings.
"""

from adaformat import naming, Enum, Proxy, Record, GObject, Tagged, List, Callback

# General packages that don't depend on others and must be processed first

enums = (
    "GtkEnums",
    "PangoEnums",
)

# List of interfaces to bind. These are processed before the widgets themselves
# These are GIR names

interfaces = (
    "Gdk.Paintable",
    "--GdkPixbuf.Pixbuf",
    "--Gtk.Actionable",
    "--Gtk.Activatable",
    "Gtk.Accessible",
    "--Gtk.AccessibleRange",
    "Gtk.AccessibleText",
    "Gtk.AccessibleHypertext",
    # "Gtk.AppChooser",
    "--Atk.ImplementorIface",
    "Gtk.Buildable",
    "Gtk.BuilderScope",
    "Gtk.CellEditable",
    "Gtk.CellLayout",
    "--Gtk.ColorChooser",
    "Gtk.ConstraintTarget",
    "Gtk.Editable",
    "--Gtk.FileChooser",
    "--Gtk.FontChooser",
    "--Gio.Icon",
    "Gtk.Native",
    "Gtk.Orientable",
    "--Gtk.PrintOperationPreview",
    "--Gtk.RecentChooser",
    "Gtk.Root",
    "--Gtk.Scrollable",
    "Gtk.SectionModel",
    "Gtk.SelectionModel",
    "Gtk.ShortcutManager",
    "--Gtk.StyleProvider",
    "--Gtk.ToolShell",
    "Gtk.TreeDragDest",
    "Gtk.TreeDragSource",
    "Gtk.TreeSortable",
    "Gtk.TreeModel",
    "Gio.Action",
    "Gio.ActionGroup",
    "Gio.ActionMap",
    "Gio.ListModel",
    "--Gio.AppInfo",  # Not tested yet, from Gio
    "--Gio.AsyncInitable",  # Not tested yet, from Gio
    # GAsyncResult is bound as an opaque proxy in Glib.G_Async_Result.
    # We do not need its virtual methods on the Ada side: the "_finish"
    # subprogram of each async operation consumes the pointer directly.
    # Leaving it commented out keeps the generator from creating an
    # interface package with no concrete subprograms.
    "--Gio.AsyncResult",  # Bound manually as Glib.G_Async_Result
    "--Gio.Converter",  # Not tested yet, from Gio
    "--Gio.DBusInterface",  # Not tested yet, from Gio
    "--Gio.DBusObject",  # Not tested yet, from Gio
    "--Gio.DBusObjectManager",  # Not tested yet, from Gio
    "--Gio.DesktopAppInfoLookup",  # Not tested yet, from Gio
    "--Gio.Drive",  # Not tested yet, from Gio
    "--Gio.File",  # Not tested yet, from Gio
    "--Gio.FileDescriptorBased",  # Not tested yet, from Gio
    "--Gio.Initable",  # Not tested yet, from Gio
    "Gio.LoadableIcon",  # Not tested yet, from Gio
    "--Gio.Mount",  # Not tested yet, from Gio
    "--Gio.NetworkMonitor",  # Not tested yet, from Gio
    "--Gio.PollableInputStream",  # Not tested yet, from Gio
    "--Gio.PollableOutputStream",  # Not tested yet, from Gio
    "--Gio.Proxy",  # Not tested yet, from Gio
    "--Gio.ProxyResolver",  # Not tested yet, from Gio
    "--Gio.RemoteActionGroup",  # Not tested yet, from Gio
    "--Gio.Seekable",  # Not tested yet, from Gio
    "--Gio.SocketConnectable",  # Not tested yet, from Gio
    "--Gio.TlsBackend",  # Not tested yet, from Gio
    "--Gio.TlsClientConnection",  # Not tested yet, from Gio
    "--Gio.TlsFileDatabase",  # Not tested yet, from Gio
    "--Gio.TlsServerConnection",  # Not tested yet, from Gio
    "--Gio.Volume",  # Not tested yet, from Gio
)

# List of GIR widgets to bind.
# Prefer qualified GIR names for classes, records and unions found in GIR.
# Start the name with -- for objects we do not want to bind

binding = (
    "----GdkAtom",  # No binding necessary, too low-level
    "Gdk.AppLaunchContext",
    "Gdk.CicpParams",
    "Gdk.Clipboard",
    "Gdk.ColorState",
    "Gdk.ContentProvider",
    "Gdk.ContentFormats",
    "Gdk.Cursor",
    "Gdk.Device",
    "--GdkDeviceManager",
    "Gdk.DeviceTool",
    "Gdk.Display",
    "Gdk.DisplayManager",
    "Gdk.DmabufFormats",
    "Gdk.DmabufTexture",
    "Gdk.DmabufTextureBuilder",
    "Gdk.Drag",
    "--GdkDragContext",
    "Gdk.DrawContext",
    "--GdkDrawingContext",
    "Gdk.Drop",
    "--Gdk.Event",
    "Gdk.FrameClock",
    "Gdk.FrameTimings",
    "Gdk.GLContext",
    "Gdk.GLTexture",
    "Gdk.GLTextureBuilder",
    "Gdk.MemoryTexture",
    "Gdk.MemoryTextureBuilder",
    "Gdk.Monitor",
    "Gdk.RGBA",
    "Gdk.Rectangle",
    "--Gdk.Monitor",
    "--GdkScreen",
    "Gdk.Seat",
    "Gdk.Snapshot",
    "Gdk.Surface",
    "Gdk.Texture",
    "--Gdk.VulkanContext", # deprecated since 4.14
    "--GdkWindow",
    "--GAction",  # bound as an interface, see interfaces tuple
    "--GActionGroup",  # bound as an interface, see interfaces tuple
    "--GActionMap",  # bound as an interface, see interfaces tuple
    "Gio.Application",
    "Gio.AppLaunchContext",
    "Gio.ApplicationCommandLine",
    "GLib.Bytes",  # Function returning arrays
    "--Gio.BufferedInputStream",  # Not tested yet, from Gio
    "--Gio.BufferedOutputStream",  # Not tested yet, from Gio
    "Gio.Cancellable",
    "--Gio.CharsetConverter",  # Not tested yet, from Gio
    "--Gio.ConverterInputStream",  # Not tested yet, from Gio
    "--Gio.ConverterOutputStream",  # Not tested yet, from Gio
    "--Gio.Credentials",  # Not tested yet, from Giov
    "--Gio.DBusActionGroup",  # Not tested yet, from Gio
    "--Gio.DBusAnnotationInfo",  # Not tested yet, from Gio
    "--Gio.DBusArgInfo",  # Not tested yet, from Gio
    "--Gio.DBusAuthObserver",  # Not tested yet, from Gio
    "--Gio.DBusConnection",  # Not tested yet, from Gio
    "--Gio.DBusInterfaceInfo",  # Not tested yet, from Gio
    "--Gio.DBusInterfaceSkeleton",  # Not tested yet, from Gio
    "--Gio.DBusMenuModel",  # Not tested yet, from Gio
    "--Gio.DBusMessage",  # Not tested yet, from Gio
    "--Gio.DBusMethodInfo",  # Not tested yet, from Gio
    "--Gio.DBusMethodInvocation",  # Not tested yet, from Gio
    "--Gio.DBusNodeInfo",  # Not tested yet, from Gio
    "--Gio.DBusObjectManagerClient",  # Not tested yet, from Gio
    "--Gio.DBusObjectManagerServer",  # Not tested yet, from Gio
    "--Gio.DBusObjectProxy",  # Not tested yet, from Gio
    "--Gio.DBusObjectSkeleton",  # Not tested yet, from Gio
    "--Gio.DBusPropertyInfo",  # Not tested yet, from Gio
    "--Gio.DBusProxy",  # Not tested yet, from Gio
    "--Gio.DBusServer",  # Not tested yet, from Gio
    "--Gio.DBusSignalInfo",  # Not tested yet, from Gio
    "--Gio.DataInputStream",  # Not tested yet, from Gio
    "--Gio.DataOutputStream",  # Not tested yet, from Gio
    "--Gio.DesktopAppInfo",  # Not tested yet, from Gio
    "--Gio.Emblem",  # Not tested yet, from Gio
    "--Gio.EmblemedIcon",  # Not tested yet, from Gio
    "--Gio.FileAttributeInfoList",  # Not tested yet, from Gio
    "--Gio.FileAttributeMatcher",  # Not tested yet, from Gio
    "--Gio.FileEnumerator",  # Not tested yet, from Gio
    "--Gio.FileIOStream",  # Not tested yet, from Gio
    "--Gio.FileIcon",  # Not tested yet, from Gio
    "Gio.FileInfo",
    "--Gio.FileInputStream",  # Not tested yet, from Gio
    "--Gio.FileMonitor",  # Not tested yet, from Gio
    "--Gio.FileOutputStream",  # Not tested yet, from Gio
    "--Gio.FilenameCompleter",  # Not tested yet, from Gio
    "--Gio.FilterInputStream",  # Not tested yet, from Gio
    "--Gio.FilterOutputStream",  # Not tested yet, from Gio
    "--Gio.IOExtension",  # Not tested yet, from Gio
    "--Gio.IOExtensionPoint",  # Not tested yet, from Gio
    "GLib.IOChannel",
    "--Gio.IOModule",  # Not tested yet, from Gio
    "--Gio.IOModuleScope",  # Not tested yet, from Gio
    "--Gio.IOSchedulerJob",  # Not tested yet, from Gio
    "--Gio.IOStream",  # Not tested yet, from Gio
    "--Gio.InetAddress",  # Not tested yet, from Gio
    "--Gio.InetAddressMask",  # Not tested yet, from Gio
    "--Gio.InetSocketAddress",  # Not tested yet, from Gio
    "Gio.InputStream",
    "Gio.ListStore",
    "--Gio.MemoryInputStream",  # Not tested yet, from Gio
    "--Gio.MemoryOutputStream",  # Not tested yet, from Gio
    "Gio.MenuModel",  # Not tested yet, from Gio
    "Gio.Menu",  # Not tested yet, from Gio
    "Gio.MenuAttributeIter",  # Not tested yet, from Gio
    "Gio.MenuItem",  # Not tested yet, from Gio
    "Gio.MenuLinkIter",  # Not tested yet, from Gio
    "--Gio.MountOperation",  # Not tested yet, from Gio
    "--Gio.NativeVolumeMonitor",  # Not tested yet, from Gio
    "--Gio.NetworkAddress",  # Not tested yet, from Gio
    "--Gio.NetworkService",  # Not tested yet, from Gio
    "Gio.Notification",
    "Gio.OutputStream",
    "GLib.OptionContext",
    "--GPoll",  # Bound through manual_binding
    "--Spawn",  # Bound through manual_binding
    "--GTest",  # Bound through manual_binding
    "--Utils",  # Bound through manual_binding
    "--GObject.ParamSpec",  # Bound manually
    "--GObject.ParamSpecBoolean",  # Bound manually
    "--GObject.ParamSpecBoxed",  # Bound manually
    "--GObject.ParamSpecChar",  # Bound manually
    "--GObject.ParamSpecDouble",  # Bound manually
    "--GObject.ParamSpecEnum",  # Bound manually
    "--GObject.ParamSpecFlags",  # Bound manually
    "--GObject.ParamSpecFloat",  # Bound manually
    "--GObject.ParamSpecGType",  # Bound manually
    "--GObject.ParamSpecInt",  # Bound manually
    "--GObject.ParamSpecInt64",  # Bound manually
    "--GObject.ParamSpecLong",  # Bound manually
    "--GObject.ParamSpecObject",  # Bound manually
    "--GObject.ParamSpecOverride",  # Bound manually
    "--GObject.ParamSpecParam",  # Bound manually
    "--GObject.ParamSpecPointer",  # Bound manually
    "--GObject.ParamSpecPool",  # Bound manually
    "--GObject.ParamSpecString",  # Bound manually
    "--GObject.ParamSpecUChar",  # Bound manually
    "--GObject.ParamSpecUInt",  # Bound manually
    "--GObject.ParamSpecUInt64",  # Bound manually
    "--GObject.ParamSpecULong",  # Bound manually
    "--GObject.ParamSpecUnichar",  # Bound manually
    "--GObject.ParamSpecValueArray",  # Bound manually
    "--GObject.ParamSpecVariant",  # Bound manually
    "--Gio.Permission",  # Not tested yet, from Gio
    "--Gio.ProxyAddress",  # Not tested yet, from Gio
    "--Gio.ProxyAddressEnumerator",  # Not tested yet, from Gio
    "--Gio.Resolver",  # Not tested yet, from Gio
    "Gio.Resource",
    "--Gio.Settings",  # Not tested yet, from Gio
    "--Gio.SettingsSchema",  # Not tested yet, from Gio
    "--Gio.SettingsSchemaSource",  # Not tested yet, from Gio
    "Gio.SimpleAction",
    "Gio.SimpleActionGroup",
    "--Gio.SimpleAsyncResult",  # Not tested yet, from Gio
    "--Gio.SimplePermission",  # Not tested yet, from Gio
    "--Gio.Socket",  # Not tested yet, from Gio
    "--Gio.SocketAddress",  # Not tested yet, from Gio
    "--Gio.SocketAddressEnumerator",  # Not tested yet, from Gio
    "--Gio.SocketClient",  # Not tested yet, from Gio
    "--Gio.SocketConnection",  # Not tested yet, from Gio
    "--Gio.SocketControlMessage",  # Not tested yet, from Gio
    "--Gio.SocketListener",  # Not tested yet, from Gio
    "--Gio.SocketService",  # Not tested yet, from Gio
    "--GLib.Source",  # Manually bound in Glib.Main
    "--Gio.SrvTarget",  # Not tested yet, from Gio
    "--Gio.StaticResource",  # Not tested yet, from Gio
    "GLib.String",
    "--Gio.TcpConnection",  # Not tested yet, from Gio
    "--Gio.TcpWrapperConnection",  # Not tested yet, from Gio
    "--Gio.ThemedIcon",  # Not tested yet, from Gio
    "--Gio.ThreadedSocketService",  # Not tested yet, from Gio
    "--Gio.TlsCertificate",  # Not tested yet, from Gio
    "--Gio.TlsConnection",  # Not tested yet, from Gio
    "--Gio.TlsDatabase",  # Not tested yet, from Gio
    "--Gio.TlsInteraction",  # Not tested yet, from Gio
    "--Gio.TlsPassword",  # Not tested yet, from Gio
    "--Gio.UnixConnection",  # Not tested yet, from Gio
    "--Gio.UnixCredentialsMessage",  # Not tested yet, from Gio
    "--Gio.UnixFDList",  # Not tested yet, from Gio
    "--Gio.UnixFDMessage",  # Not tested yet, from Gio
    "--Gio.UnixInputStream",  # Not tested yet, from Gio
    "--Gio.UnixMountMonitor",  # Not tested yet, from Gio
    "--Gio.UnixMountPoint",  # Not tested yet, from Gio
    "--Gio.UnixOutputStream",  # Not tested yet, from Gio
    "--Gio.UnixSocketAddress",  # Not tested yet, from Gio
    "GLib.Variant",
    "GLib.VariantIter",
    "GLib.VariantType",
    "--Gio.Vfs",  # Not tested yet, from Gio
    "--Gio.VolumeMonitor",  # Not tested yet, from Gio
    "--Gio.ZlibCompressor",  # Not tested yet, from Gio
    "--Gio.ZlibDecompressor",  # Not tested yet, from Gio
    "--Pango.AttrIterator",
    "Pango.AttrList",
    "Pango.Attribute",
    "--Pango.Color",
    "Pango.Context",
    "Pango.Coverage",
    "Pango.Font",
    "Pango.FontDescription",
    "Pango.FontFace",
    "Pango.FontFamily",
    "Pango.FontMap",
    "Pango.FontMetrics",
    "Pango.Fontset",
    "--Pango.GlyphItem",
    "--Pango.GlyphItemIter",
    "--Pango.GlyphString",
    "--Pango.Item",
    "--Pango.Renderer",
    "--Pango.ScriptIter",
    "Pango.Language",
    "Pango.Layout",
    "Pango.LayoutIter",
    "Pango.LayoutLine",
    "Pango.Matrix",
    "Pango.TabArray",
    "--Gtk.AboutDialog",
    "--GtkAccelGroup",
    "--GtkAccelLabel",
    "--GtkAccelMap",
    "--GtkAccessible",  # Bound through manual_binding
    "Gtk.AccessibleHyperlink",
    "--GtkAccessibleText",  # Bound through manual_binding
    "--GtkAction",
    "--GtkActionGroup",
    "--Gtk.ActionBar",
    "Gtk.Adjustment",
    "Gtk.AlertDialog",
    "--GtkAlignment",
    "----GtkAppChooserButton",  # Needs GFile
    "----GtkAppChooserDialog",  # Needs GFile
    "----GtkAppChooserWidget",  # Needs GFile
    "Gtk.Application",
    "Gtk.ApplicationWindow",
    "--GtkArrow",
    "----GtkArrowAccessible",  # We do not support atk
    "--Gtk.AspectFrame",
    "--Gtk.Assistant",
    "--Gtk.ATContext",  # Bound manually
    "--GtkBin",
    "--GtkBindingEntry",
    "--GtkBindingSet",
    "Gtk.Bitset",
    "--Gtk.Border",
    "Gtk.Box",
    "----GtkBooleanCellAccessible",  # We do not support atk
    "Gtk.Builder",
    "Gtk.BuilderCScope",
    "Gtk.BuilderListItemFactory",
    "Gtk.Button",
    "----GtkButtonAccessible",  # We do not support atk
    "--GtkButtonBox",
    "--Gtk.Calendar",
    "----GtkCellAccessible",  # We do not support atk
    "Gtk.CellArea",
    "Gtk.CellAreaBox",
    "----GtkCellAreaClass",
    "Gtk.CellAreaContext",
    "Gtk.CellRenderer",
    "Gtk.CellRendererAccel",
    "----GtkCellRendererClass",  # Useless in Ada
    "Gtk.CellRendererCombo",
    "--Gtk.CellRendererPixbuf",
    "Gtk.CellRendererProgress",
    "Gtk.CellRendererSpin",
    "Gtk.CellRendererSpinner",
    "Gtk.CellRendererText",
    "Gtk.CellRendererToggle",
    "Gtk.CellView",
    "Gtk.CheckButton",
    "--GtkCheckMenuItem",
    "----GtkCheckMenuItemAccessible",  # We do not support atk
    "Gtk.CenterBox",
    "--GtkClipboard",
    "--Gtk.ColorButton",
    "--Gtk.ColorChooserDialog",
    "Gtk.ColorDialog",
    "Gtk.ColorDialogButton",
    "--Gtk.ColorChooserWidget",
    "--GtkColorSelection",
    "--GtkColorSelectionDialog",
    "Gtk.ColumnView",
    "Gtk.ColumnViewCell",
    "Gtk.ColumnViewColumn",
    "Gtk.ColumnViewRow",
    "--Gtk.ComboBox",
    "----GtkComboBoxAccessible",  # We do not support atk
    "--Gtk.ComboBoxText",
    "--GtkConstraintTarget",  # Bound through manual_binding
    "--GtkContainer",
    "----GtkContainerAccessible",  # We do not support atk
    "----GtkContainerCellAccessible",  # We do not support atk
    "----GtkContainerClass",
    "--Gtk.CssProvider",
    "--Gtk.CssSection",
    "--Gtk.Dialog",
    "--Gtk.DrawingArea",
    "Gtk.Entry",
    "----GtkEntryAccessible",  # We do not support atk
    "Gtk.EntryBuffer",
    "Gtk.EntryCompletion",
    "----GtkEntryIconAccessible",  # We do not support atk
    "--GtkEventBox",
    "--Gtk.EventController",
    "Gtk.Expander",
    "----GtkExpanderAccessible",  # We do not support atk
    "--GtkFileChooserButton",
    "--Gtk.FileChooserDialog",
    "--Gtk.FileChooserWidget",
    "--Gtk.FileFilter",
    "Gtk.Filter",
    "--Gtk.Fixed",
    "--Gtk.FlowBox",
    "----GtkFlowBoxAccessible",  # We do not support atk
    "--Gtk.FlowBoxChild",
    "----GtkFlowBoxChildAccessible",  # We do not support atk
    "--Gtk.FontButton",
    "--Gtk.FontChooserDialog",
    "--Gtk.FontChooserWidget",
    "Gtk.FontDialog",
    "Gtk.FontDialogButton",
    "--GtkFontSelection",
    "--GtkFontSelectionDialog",
    "Gtk.Frame",
    "----GtkFrameAccessible",  # We do not support atk
    "--Gtk.Gesture",
    "--Gtk.GestureDrag",
    "--Gtk.GestureLongPress",
    "--GtkGestureMultiPress",
    "--Gtk.GesturePan",
    "--Gtk.GestureRotate",
    "--Gtk.GestureSingle",
    "--Gtk.GestureSwipe",
    "--Gtk.GestureZoom",
    "--GtkGradient",
    "--Gtk.GLArea",
    "Gtk.Grid",
    "Gtk.GridView",
    "--GtkHandleBox",
    "--GtkHButtonBox",
    "--Gtk.HeaderBar",
    "--GtkHPaned",
    "--GtkHScale",
    "--GtkHScrollbar",
    "--GtkHSV",
    "--GtkIconFactory",
    "--GtkIconInfo",
    "--GtkIconSet",
    "--GtkIconSource",
    "--Gtk.IconTheme",
    "--Gtk.IconView",
    "----GtkIconViewAccessible",  # We do not support atk
    "--Gtk.IMContext",
    "--Gtk.IMContextSimple",
    "--Gtk.IMMulticontext",
    "--Gtk.Image",
    "----GtkImageAccessible",  # We do not support atk
    "----GtkImageCellAccessible",  # We do not support atk
    "--GtkImageMenuItem",
    "--Gtk.InfoBar",
    "--GtkInvisible",
    "Gtk.Label",
    "----GtkLabelAccessible",  # We do not support atk
    "--GtkLayout",

           # GtkLayoutManager and GtkLayoutChild are the abstract bases of
           # the layout managers. They must be listed before their
           # subclasses so the generator emits them first.
           "Gtk.LayoutManager",
           "Gtk.LayoutChild",
           "Gtk.BinLayout",
           "Gtk.BoxLayout",
           "Gtk.CenterLayout",
           "Gtk.ConstraintLayout",
           "Gtk.ConstraintLayoutChild",
           "--Gtk.CustomLayout",
           "Gtk.FixedLayout",
           "Gtk.FixedLayoutChild",
           "Gtk.GridLayout",
           "Gtk.GridLayoutChild",
           "Gtk.OverlayLayout",
           "Gtk.OverlayLayoutChild",

    "--Gtk.LevelBar",
    "----GtkLevelBarAccessible",  # We do not support atk
    "--Gtk.LinkButton",
    "----GtkLinkButtonAccessible",  # We do not support atk
    "Gtk.ListBase",
    "--Gtk.ListBox",
    "----GtkListBoxAccessible",  # We do not support atk
    "--Gtk.ListBoxRow",
    "----GtkListBoxRowAccessible",  # We do not support atk
    "Gtk.ListItem",
    "Gtk.ListItemFactory",
    "Gtk.ListStore",
    "----GtkLockButton",  # requires GPermission
    "----GtkLockButtonAccessible",  # We do not support atk
    "--GtkMain",  # Bound through manual_binding
    "--GtkMisc",
    "--GtkMenu",
    "----GtkMenuAccessible",  # We do not support atk
    "--GtkMenuBar",
    "Gtk.MenuButton",
    "----GtkMenuButtonAccessible",  # We do not support atk
    "--GtkMenuItem",
    "----GtkMenuItemAccessible",  # We do not support atk
    "--GtkMenuShell",
    "----GtkMenuShellAccessible",  # We do not support atk
    "--GtkMenuToolButton",
    "--Gtk.MessageDialog",
    "----GtkMountOperation",  # Requires a lot of GIO
    "Gtk.NativeDialog",
    "Gtk.Notebook",
    "----GtkNotebookAccessible",  # We do not support atk
    "----GtkNotebookPageAccessible",  # We do not support atk
    "----GtkNumerableIcon",  # Requires a lot of GIO
    "--GtkOffscreenWindow",
    "--Gtk.Overlay",
    "Gtk.Paned",
    "----GtkPanedAccessible",  # We do not support atk
    "--Gtk.PageSetup",
    "--Gtk.PaperSize",
    "----GtkPlacesSidebar",  # Requires GFile
    "Gtk.Popover",
    "----GtkPopoverAccessible",  # We do not support atk
    "Gtk.PopoverMenu",
    "Gtk.PopoverMenuBar",
    "--Gtk.PrintContext",
    "--Gtk.PrintOperation",
    "--Gtk.PrintSettings",
    "----GtkPlug",  # X11-specific, no binding
    "--Gtk.ProgressBar",
    "----GtkProgressBarAccessible",  # We do not support atk
    "--GtkRadioAction",
    "--GtkRadioButton",
    "----GtkRadioButtonAccessible",  # We do not support atk
    "--GtkRadioMenuItem",
    "----GtkRadioMenuItemAccessible",  # We do not support atk
    "--GtkRadioToolButton",
    "--Gtk.Range",
    "----GtkRangeAccessible",  # We do not support atk
    "----GtkRcStyle",  # manual binding for these deprecated routines
    "--GtkRecentAction",
    "--GtkRecentChooserDialog",
    "--GtkRecentChooserMenu",
    "--GtkRecentChooserWidget",
    "--GtkRecentFilter",
    "--Gtk.RecentInfo",
    "--Gtk.RecentManager",
    "----GtkRendererCellAccessible",  # We do not support atk
    "--Gtk.Revealer",
    "--Gtk.Scale",
    "----GtkScaleAccessible",  # We do not support atk
    "--Gtk.ScaleButton",
    "----GtkScaleButtonAccessible",  # We do not support atk
    "--Gtk.SearchBar",
    "--Gtk.SearchEntry",
    "--GtkSelectionData",
    "Gtk.Separator",
    "--Gtk.ShortcutsWindow",
    "--Gtk.SizeGroup",
    "--Gtk.Scrollbar",
    "Gtk.ScrolledWindow",
    "----GtkScrolledWindowAccessible",  # We do not support atk
    "Gtk.ScrollInfo",
    "--Gtk.Settings",
    "Gtk.MultiSelection",  # Implements Gtk.SelectionModel
    "Gtk.NoSelection",     # Implements Gtk.SelectionModel
    "Gtk.SingleSelection", # Implements Gtk.SelectionModel
    "Gtk.Snapshot",
    "----GtkSocket",  # X11-specific, no binding
    "Gtk.Sorter",
    "Gtk.SortListModel",
    "--Gtk.Spinner",
    "----GtkSpinnerAccessible",  # We do not support atk
    "Gtk.SpinButton",
    "----GtkSpinButtonAccessible",  # We do not support atk
    "--Gtk.Stack",
    "--Gtk.StackSwitcher",
    "--Gtk.Statusbar",
    "----GtkStatusbarAccessible",  # We do not support atk
    "--GtkStatusIcon",
    "--GtkStockItem",
    "--GtkStyle",
    "--Gtk.StyleContext",
    "--GtkStyleProperties",
    "--Gtk.Switch",
    "----GtkSwitchAccessible",  # We do not support atk
    "--GtkSymbolicColor",
    "--GtkTable",
    "--GtkTargetEntry",
    "--GtkTargetList",
    "--GtkTearoffMenuItem",
    "--GtkTextAttributes",  # Removed upstream in GTK 4
    "Gtk.TextBuffer",
    "----GtkTextCellAccessible",  # We do not support atk
    "Gtk.TextChildAnchor",
    "Gtk.TextIter",
    "Gtk.TextMark",
    "Gtk.TextTag",
    "Gtk.TextTagTable",
    "Gtk.TextView",
    "----GtkTextViewAccessible",  # We do not support atk
    "--GtkThemingEngine",
    "Gtk.TreeIter",
    "Gtk.TreeModelFilter",
    "Gtk.TreeModelSort",
    "Gtk.TreePath",
    "Gtk.TreeRowReference",
    "Gtk.TreeSelection",
    "Gtk.TreeStore",
    "Gtk.TreeView",
    "----GtkTreeViewAccessible",  # We do not support atk
    "Gtk.TreeViewColumn",
    "--GtkToggleAction",
    "Gtk.ToggleButton",
    "----GtkToggleButtonAccessible",  # We do not support atk
    "--GtkToggleToolButton",
    "--GtkToolButton",
    "--GtkToolbar",
    "--GtkToolItem",
    "--GtkToolItemGroup",
    "Gtk.Tooltip",
    "--GtkToolPalette",
    "----GtkToplevelAccessible",  # We do not support atk
    "--GtkUIManager",
    "--GtkVButtonBox",
    "--GtkVPaned",
    "--GtkVScale",
    "--GtkVScrollbar",
    "--Gtk.Viewport",
    "--Gtk.VolumeButton",
    "Gtk.Widget",
    "----GtkWidgetAccessible",  # We do not support atk
    "--Gtk.WidgetClass",
    "Gtk.Window",
    "----GtkWindowAccessible",  # We do not support atk
    "Gtk.WindowGroup",
)

# Entries that are bound through manual/TOML glue instead of GIR class lookup.
# These may still use qualified GIR names for interfaces.
manual_binding = (
    "Gio.Icon",
    "GPoll",
    "Spawn",
    "GTest",
    "Utils",
    "Gtk.Accessible",
    "Gtk.AccessibleText",
    "GtkMain",
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
    "gtk_show_uri": "gtk_show_uri()",
    "gtk_icon_factory_add_default": "Gtk.Icon_Factory.Add_Default",
    "gtk_icon_factory_add": "Gtk.Icon_Factory.Add",
    "gdk_pixbuf_new_from_data": "Gdk.Pixbuf.Gdk_New_From_Data",
    "gdk_pixbuf_new_from_file": "Gdk.Pixbuf.Gdk_New_From_File",
    "gdk_pixbuf_new_from_xpm_data": "Gdk.Pixbuf.Gdk_New_From_Xpm_Data",
    "gdk_pixbuf_animation_new_from_file": "Gdk.Pixbuf.Gdk_New_From_File",
    "gdk_pixbuf_new": "Gdk.Pixbuf.Gdk_New",
    "gdk_pixbuf_new_subpixbuf": "Gdk.Pixbuf.Gdk_New_Subpixbuf",
    "gtk_drag_finish": "Gtk.Dnd.Finish",
    "TRUE": "True",
    "FALSE": "False",
    "NULL": "null",
    "GDK_2BUTTON_PRESS": "Gdk_2button_Press",
    "GDK_3BUTTON_PRESS": "Gdk_3button_Press",
    "GDK_EVENT_LAST": None,
    # ??? Doesn't exist
    "gtk_activatable_get_action": "Gtk.Activatable.Get_Action",
    "GDK_SEAT_CAPABILITY_ALL": "All_Capabilities",
}

# Maps GIR's "name" to a "c:type". This isn't needed for the classes
# themselves, since this is automatically read from the GIR file.
# Mostly used for properties. The values must correspond to entries in
# self.type_exceptions.

naming.girname_to_ctype = {
    "GdkPixbuf.Pixbuf": "GdkPixbuf",
    "Pango.EllipsizeMode": "PangoEllipsizeMode",
    "Pango.FontDescription": "PangoFontDescription*",
    "Pango.AttrList": "PangoAttrList",
    "Gio.Action": "Action",
    "Gio.ActionGroup": "ActionGroup",
    "Gio.ActionMap": "ActionMap",
    "Gio.Application": "GApplication",
    "Gio.ApplicationCommandLine": "GApplicationCommandLine",
    "ApplicationCommandLine": "GApplicationCommandLine",
    "Gio.Icon": "GIcon*",
    "Gio.AsyncReadyCallback": "GAsyncReadyCallback",
    "Gio.AsyncResult": "GAsyncResult*",
    "Gio.Cancellable": "GCancellable*",
    "GLib.Variant": "GVariant",
    "Gdk.Clipboard": "GdkClipboard",
    "Gdk.Paintable": "GdkPaintable",
    "Gtk.Snapshot": "GtkSnapshot",
    "Gdk.Event": "GdkEvent*",
    "Gdk.EventButton": "GdkEventButton*",
    "Gdk.EventMotion": "GdkEventMotion*",
    "Gdk.EventProximity": "GdkEventProximity*",
    "Gdk.EventAny": "GdkEventAny*",
    "Gdk.EventConfigure": "GdkEventConfigure*",
    "Gdk.EventExpose": "GdkEventExpose*",
    "Gdk.EventKey": "GdkEventKey*",
    "Gdk.EventCrossing": "GdkEventCrossing*",
    "Gdk.EventScroll": "GdkEventScroll*",
    "Gdk.EventSequence": "GdkEventSequence*",
    "Gdk.EventWindowState": "GdkEventWindowState*",
    "Gdk.EventVisibility": "GdkEventVisibility*",
    "Gdk.EventSelection": "GdkEventSelection*",
    "Gdk.EventFocus": "GdkEventFocus*",
    "Gdk.EventProperty": "GdkEventProperty*",
    "Gdk.EventGrabBroken": "GdkEventGrabBroken*",
    "Gdk.EventOwnerChange": "GdkEventOwnerChange*",
    "Gdk.Pixmap": "GdkPixmap*",
    "Gdk.Image": "GdkImage*",
    "Gdk.GLContext": "GdkGLContext*",
    "Gdk.DragContext": "GdkDragContext",
    "GdkPixbuf.PixbufAnimation": "GdkPixbufAnimation*",
    "Gdk.Bitmap": "GdkBitmap*",
    "Gdk.Color": "GdkColor*",
    "Gdk.Screen": "GdkScreen",
    "Gdk.RGBA": "GdkRGBA",
    "GObject.Object": "GObject*",
    "GObject.Closure": "GClosure*",
    "cairo.Surface": "cairo_surface_t*",
    "cairo.Context": "cairo_t*",
    "cairo.RectangleInt": "cairo_rectangle_int_t*",
    "ModifierType": "GdkModifierType",
    "PropertyState": "GdkPropertyState",
    "TreePath": "GtkTreePath*",
    "TreeModel": "GtkTreeModel*",
    "GObject.InitiallyUnowned": "GObject*",  # An alias
    "GObject.ParamSpec": "GParamSpec",
    "Giochannel": "GIOChannel*",
}

# Naming exceptions. In particular maps Ada keywords.

naming.exceptions = {
    "Entry": "GEntry",
    "Type": "The_Type",
    "Range": "GRange",
    "Delay": "The_Delay",
    "Select": "Gtk_Select",
    "End": "The_End",
    "Return": "Do_Return",
    "Function": "Func",
    "Digits": "The_Digits",
    "Reverse": "Gtk_Reverse",
    "Raise": "Gdk_Raise",
    "Use": "GUse",
    "Uri": "URI",
    "In": "Gtk_In",
    # Regular renamings
    "Gtk_Imcontext": "Gtk_IM_Context",
    "Gtk_Imcontext_Simple": "Gtk_IM_Context_Simple",
    "Gtk_Immulticontext": "Gtk_IM_Multi_Context",
    "Gtk_Uimanager": "Gtk_UI_Manager",
    "Gicon": "G_Icon",
    "Gtk_Glarea": "Gtk_GLArea",
    "Gdk_Glcontext": "Gdk_GLContext",
    "Gtk_Builder_Cscope": "Gtk_Builder_C_Scope",
    "Gdk_Dndevent": "Gdk_DND_Event",
}

# Maps C types to type descriptions.
# All standard widgets will be added automatically. Only special
# namings are needed here

naming.type_exceptions = {
    "gboolean": Enum("Boolean", "Glib.Properties.Property_Boolean"),
    "gdouble": Proxy("Gdouble", "Glib.Properties.Property_Double"),
    "double": Proxy("Gdouble", "Glib.Properties.Property_Double"),
    "gshort": Proxy("Gshort", "Glib.Properties.Property_Int"),
    "gushort": Proxy("Gushort", "Glib.Properties.Property_Uint"),
    "int": Proxy("Glib.Gint", "Glib.Properties.Property_Int"),
    "gint": Proxy(
        "Glib.Gint", "Glib.Properties.Property_Int", default_record_field="0"
    ),
    "gint8": Proxy("Gint8", "Glib.Properties.Property_Int"),
    "gint16": Proxy("Gint16", "Glib.Properties.Property_Int"),
    "gint32": Proxy("Gint32", "Glib.Properties.Property_Int"),
    "gint64": Proxy("Gint64", "Glib.Properties.Property_Int"),
    "guint": Proxy("Guint", "Glib.Properties.Property_Uint"),
    "guint8": Proxy("Guint8", "Glib.Properties.Property_Uint"),
    "guint16": Proxy("Guint16", "Glib.Properties.Property_Uint"),
    "guint32": Proxy("Guint32", "Glib.Properties.Property_Uint"),
    "guint64": Proxy("Guint64", "Glib.Properties.Property_Uint"),
    "gfloat": Proxy("Gfloat", "Glib.Properties.Property_Float"),
    "float": Proxy("Interfaces.C.C_float", "Glib.Properties.Property_Float"),
    "glong": Proxy("Glong", "Glib.Properties.Property_Long"),
    "gulong": Proxy("Gulong"),
    "gsize": Proxy("Gsize"),
    "gssize": Proxy("Gssize"),
    "gunichar": Proxy("Gunichar"),
    "gchar": Proxy("Gchar"),
    "guchar": Proxy("Guchar"),
    "GAppInfo": Proxy("Glib.GApp_Info"),
    "unsigned int": Proxy("Guint"),
    # These should not be necessary, but if we don't put them the gnerated
    # binding is wrong (for instance we end up passing Gdk_Event_Record as
    # parameters to functions, or Gdk_RGBA directly to C)
    "GdkRGBA": Record("Gdk.RGBA.Gdk_RGBA", "Gdk.RGBA.Property_RGBA"),  # impose casing
    "GdkRGBA*": Proxy(
        "Gdk.RGBA.Gdk_RGBA", "Gdk.RGBA.Property_RGBA", "Gdk.RGBA.Gdk_RGBA_Or_Null"
    ),
    "GdkEvent*": Tagged("Gdk.Event.Gdk_Event", userecord=False),
    "GIOChannel*": Proxy("Glib.IOChannel.Giochannel", ""),
    "cairo_t*": Proxy("Cairo.Cairo_Context"),
    "cairo_format_t": Proxy("Cairo.Cairo_Format"),
    "cairo_rectangle_int_t*": Proxy("Cairo.Region.Cairo_Rectangle_Int"),
    "cairo_content_t": Proxy("Cairo.Cairo_Content"),
    "cairo_pattern_t*": Proxy("Cairo.Cairo_Pattern"),
    "cairo_surface_t*": Proxy("Cairo.Cairo_Surface"),
    "cairo_region_t*": Proxy("Cairo.Region.Cairo_Region"),
    "cairo_font_options_t": Proxy("Cairo.Cairo_Font_Options"),
    "GtkWidgetClass": Proxy("Glib.Object.GObject_Class"),
    "GtkWidgetClass*": Proxy("Glib.Object.GObject_Class"),
    # Force mapping to a Proxy. This is also hard-coded in GITClass.__init__
    "PangoFontDescription": Proxy(
        "Pango.Font.Pango_Font_Description", "Pango.Font.Property_Font_Description"
    ),
    "PangoFontDescription*": Proxy(
        "Pango.Font.Pango_Font_Description", "Pango.Font.Property_Font_Description"
    ),
    "GtkTreeIter*": Record("Gtk.Tree_Model.Gtk_Tree_Iter", val_or_null="Iter_Or_Null"),
    "GtkTextIter*": Record("Gtk.Text_Iter.Gtk_Text_Iter", val_or_null="Iter_Or_Null"),
    "PangoAttribute": Proxy("Pango.Attributes.Pango_Attribute"),
    "PangoAttribute*": Proxy("Pango.Attributes.Pango_Attribute"),
    "GError*": Proxy("Glib.Error.GError"),
    "GObject*": GObject("Glib.Object.GObject"),
    "GClosure*": Proxy("System.Address", ""),
    "GInitiallyUnowned": GObject("Glib.Object.GInitiallyUnowned"),
    "GIcon*": Proxy("Glib.G_Icon.G_Icon"),
    "GValue": Proxy("Glib.Values.GValue", ""),
    "GdkAtom": Proxy("Gdk.Types.Gdk_Atom"),
    "GVariantType": Proxy("Glib.Variant.Gvariant_Type"),
    "GVariantType*": Proxy("Glib.Variant.Gvariant_Type"),
    "GVariantIter": Proxy("Glib.Variant.Gvariant_Iter"),
    # Proper generation of lists
    "GdkEventSequence": Proxy("Gdk.Event.Gdk_Event_Sequence"),
    # Specific to this binding generator (referenced from binding.xml)
    "VisualList": List("Gdk.Visual.Gdk_Visual_List.Glist"),
    "ObjectList": List("Glib.Object.Object_Simple_List.Glist"),
    "ObjectSList": List("Glib.Object.Object_List.GSlist"),
    "StringList": List("Gtk.Enums.String_List.Glist"),
    "StringSList": List("Gtk.Enums.String_SList.GSlist"),
    "TreePathList": List("Gtk.Tree_Model.Gtk_Tree_Path_List.Glist"),
    "TextTagList": List("Gtk.Text_Tag.Text_Tag_List.GSlist"),
    "DeviceList": List("Gdk.Device.Device_List.Glist"),
    "DeviceToolList": List("Gdk.Device_Tool.Device_Tool_List.Glist"),
    "DisplayList": List("Gdk.Display.Display_List.Glist"),
    "SeatList": List("Gdk.Seat.Seat_List.Glist"),
    "gpointer": Proxy("System.Address", "", default_record_field="System.Null_Address"),
    "gconstpointer": Proxy("System.Address", ""),
    "GDestroyNotify": Proxy("Glib.G_Destroy_Notify_Address"),
    "GAsyncResult": Proxy("Glib.G_Async_Result"),
    "GAsyncResult*": Proxy("Glib.G_Async_Result"),
    # GAsyncReadyCallback is intentionally NOT overridden here: the
    # generator's standard callback support emits a per-package access
    # type and the matching closure-passing trampoline, which is much
    # more usable than a low-level access-to-subprogram.
    "GQuark": Proxy("Glib.GQuark"),
    "GObject": Proxy("Glib.Object.GObject"),
    "GParamSpec": Proxy("Glib.Param_Spec"),
    "GClosure": Proxy("GClosure"),
    "GConnectFlags": Proxy("Glib.G_Connect_Flags"),
    "GSource": Proxy("Glib.Main.G_Source"),
    "WidgetPath*": Proxy("Gtk.Widget.Widget_Path"),
    # ??? Shouldn't we use a naming exception instead ?
    "GtkStatusbar": GObject("Gtk.Status_Bar.Gtk_Status_Bar"),
    "GtkRcStyle": GObject("Gtk.Rc.Gtk_Rc_Style"),
    "GtkTreeViewRowSeparatorFunc": Callback(
        "Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func"
    ),
    "GKeyFile*": Proxy("Glib.Key_File.G_Key_File"),
    "GdkColor*": Proxy(
        "Gdk.Color.Gdk_Color",
        "Gdk.Color.Property_Gdk_Color",
        "Gdk.Color.Gdk_Color_Or_Null",
    ),
    "GdkDragContext": GObject("Gdk.Drag_Contexts.Drag_Context"),
    "GdkFont": Proxy("Gdk.Font.Gdk_Font"),
    "GdkVisual*": Proxy("Gdk.Visual.Gdk_Visual"),
    "GdkPixmap*": Proxy("Gdk.Pixmap.Gdk_Pixmap"),
    "GdkBitmap*": Proxy("Gdk.Bitmap.Gdk_Bitmap"),
    "GdkImage*": Proxy("Gdk.Image.Gdk_Image"),
    "GdkPixbuf": GObject("Gdk.Pixbuf.Gdk_Pixbuf"),
    "GdkPixbufAnimation*": Proxy("Gdk.Pixbuf.Gdk_Pixbuf_Animation"),
    "GdkRectangle": Record("Gdk.Rectangle.Gdk_Rectangle"),
    "GdkRectangle*": Proxy("Gdk.Rectangle.Gdk_Rectangle"),
    "Gdk.ModifierType": Proxy("Gdk.Types.Gdk_Modifier_Type"),
    "GdkModifierType": Proxy("Gdk.Types.Gdk_Modifier_Type"),
    "GdkKeyType": Proxy("Gdk.Types.Gdk_Key_Type"),
    "GdkWindowAttr*": Proxy("Gdk.Gdk_Window_Attr"),
    # Override type: we do not want to show they derive from GObject
    "GdkWindow": Proxy("Gdk.Gdk_Window"),
    "GdkWindow*": Proxy("Gdk.Gdk_Window"),
}
