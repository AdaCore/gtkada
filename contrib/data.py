"""This package contains data which must be edited by hand when adding new
   bindings.
"""

from adaformat import *

# General packages that don't depend on others and must be processed first

enums = ("GtkEnums",
         "PangoEnums",
         )

# List of interfaces to bind. These are processed before the widgets themselves.
# These are GIR names

interfaces = ("Activatable",
              #"AppChooser",
              "Buildable",
              "CellEditable",
              "CellLayout",
              "Editable",
              "FileChooser",
              "FontChooser",
              "Orientable",
              #"PrintOperationPreview",
              "RecentChooser",
              "Scrollable",
              "StyleProvider",
              "ToolShell",
              #"TreeDragDest",
              #"TreeDragSource",
              #"TreeSortable"
              #"TreeModel",
             )

# List of widgets to bind.
# Contains C type names

binding = ("GdkCursor",
           "GdkDevice",
           "GdkDragContext",
           "GdkRGBA",
           "GdkScreen",
           "GdkWindow",

           "GtkAboutDialog",
           "GtkAccelGroup",
           "GtkAction",
           # "GtkActionGroup",
           "GtkAdjustment",
           "GtkAlignment",
           #"GtkApplication",
           "GtkArrow",
           "GtkAspectFrame",
           "GtkAssistant",
           "GtkBin",
           "GtkBox",
           "GtkButton",
           "GtkButtonBox",
           "GtkCairo",
           "GtkCalendar",
           "GtkCellArea",
           "GtkCellAreaContext",
           "GtkCellRenderer",
           "GtkCheckButton",
           "GtkCheckMenuItem",
           "GtkColorButton",
           "GtkColorSelection",
           "GtkColorSelectionDialog",
           "GtkComboBox",
           "GtkComboBoxText",
           "GtkContainer",
           "GtkCssProvider",
           "GtkDialog",
           "GtkDrawingArea",
           "GtkEntry",
           "GtkEntryBuffer",
           "GtkEntryCompletion",
           "GtkEventBox",
           "GtkExpander",
           "GtkFileChooserButton",
           "GtkFileChooserDialog",
           "GtkFileChooserWidget",
           "GtkFileFilter",
           "GtkFixed",
           "GtkFontButton",
           "GtkFontSelection",
           "GtkFontSelectionDialog",
           "GtkFrame",
           "GtkHandleBox",
           "GtkHBox",
           "GtkHButtonBox",
           "GtkHPaned",
           "GtkHScale",
           "GtkHScrollbar",
           "GtkHSeparator",
           "GtkIconView",
           "GtkImage",
           "GtkImageMenuItem",
           "GtkInvisible",
           "GtkLabel",
           "GtkLayout",
           "GtkLinkButton",
           "GtkMain",
           "GtkMisc",
           "GtkMenu",
           "GtkMenuBar",
           "GtkMenuItem",
           "GtkMenuShell",
           "GtkMenuToolButton",
           "GtkNotebook",
           "GtkPaned",
           "GtkPaperSize",
           "GtkProgressBar",
           "GtkRadioAction",
           "GtkRadioButton",
           "GtkRange",
           "GtkRecentAction",
           "GtkRecentChooserDialog",
           "GtkRecentChooserMenu",
           "GtkRecentChooserWidget",
           "GtkRecentFilter",
           "GtkRecentInfo",
           "GtkRecentManager",
           "GtkScale",
           "GtkScaleButton",
           "GtkSeparator",
           "GtkSeparatorMenuItem",
           "GtkSeparatorToolItem",
           "GtkSizeGroup",
           "GtkScrollbar",
           "GtkScrolledWindow",
           "GtkSocket",
           "GtkSpinner",
           "GtkSpinButton",
           "GtkStatusbar",
           "GtkStatusIcon",
           "GtkStyle",
           "GtkStyleContext",
           "GtkStyleProperties",
           "GtkSymbolicColor",
           "GtkTable",
           "GtkTearoffMenuItem",
           "GtkTextTag",
           # "GtkTreePath",   #  into GtkTreeModel
           "GtkToggleAction",
           "GtkToggleButton",
           "GtkToggleToolButton",
           "GtkToolButton",
           "GtkToolbar",
           "GtkToolItem",
           "GtkTooltip",
           "GtkVBox",
           "GtkVButtonBox",
           "GtkVPaned",
           "GtkVScale",
           "GtkVScrollbar",
           "GtkVSeparator",
           "GtkViewport",
           "GtkVolumeButton",
           "GtkWidget",
           "GtkWindow",
           "GtkWindowGroup",
          )

# Handling of functions with user data. The names below are the likely names
# for callback functions that accept user_data. The GIR file doesn't point to
# these specific parameters.

user_data_params = ["Data", "Func_Data", "User_Data", "D"]
destroy_data_params = ["destroy", "func_notify"]

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
    "gtk_accel_map_add_entry":      "Gtk.Accel_Map.Add_Entry",
    "gtk_accel_map_change_entry":   "Gtk.Accel_Map.Change_Entry",

    "TRUE": "True",
    "FALSE": "False",
    "NULL": "null",

    # ??? Doesn't exist
    "gtk_activatable_get_action": "Gtk.Activatable.Get_Action",

    # Will be bound later
    "gtk_action_group_add_action_with_accel":
        "Gtk.Action_Group.Add_Action_With_Accel",
    "gtk_tool_item_set_expand": "Gtk.Tool_Item.Set_Expand",
    "gtk_builder_add_from_file": "Gtk.Builder.Add_From_File",
    "gtk_builder_add_from_string": "Gtk.Builder.Add_From_String",
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
    "Gio.Icon":            "GIcon*",
    "IconSet":             "GtkIconSet*",
    "Gdk.Pixmap":          "GdkPixmap*",
    "Gdk.Image":           "GdkImage*",
    "GdkPixbuf.PixbufAnimation": "GdkPixbufAnimation*",
    "Gdk.Bitmap":          "GdkBitmap*",
    "Gdk.Color":           "GdkColor*",
    "Gdk.RGBA":            "GdkRGBA",
    "GObject.Object":      "GObject*",
    "GObject.Closure":     "GClosure*",
    "Cairo.Context":       "cairo_t",
    "GObject.InitiallyUnowned": "GObject*"  # An alias
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
}

# Maps C types to type descriptions.
# All standard widgets will be added automatically. Only special
# namings are needed here

naming.type_exceptions = {
    "gboolean":          Enum("Boolean",
                              "Glib.Properties.Property_Boolean"),
    "gdouble":  Proxy("Gdouble", "Glib.Properties.Property_Double"),
    "int":      Proxy("Gint",    "Glib.Properties.Property_Int"),
    "gint":     Proxy("Gint",    "Glib.Properties.Property_Int"),
    "guint":    Proxy("Guint",   "Glib.Properties.Property_Uint"),
    "guint16":  Proxy("Guint16", "Glib.Properties.Property_Uint"),
    "gfloat":   Proxy("Gfloat",  "Glib.Properties.Property_Float"),
    "GAppInfo": Proxy("Glib.GApp_Info"),

    "GdkRGBA":  Proxy("Gdk.RGBA.Gdk_RGBA",  # impose casing
                      "Gdk.RGBA.Property_RGBA"),

    "cairo_t*":              Proxy("Cairo.Cairo_Context"),
    "cairo_content_t":       Proxy("Cairo.Cairo_Content"),
    "cairo_pattern_t*":      Proxy("Cairo.Cairo_Pattern"),
    "cairo_surface_t*":      Proxy("Cairo.Cairo_Surface"),
    "cairo_region_t*":       Proxy("Cairo.Region.Cairo_Region"),
    "cairo_font_options_t":  Proxy("Cairo.Cairo_Font_Options"),

    "PangoAttrList":     Proxy("Pango.Attributes.Pango_Attr_List", ""),
    "PangoEllipsizeMode":Enum("Pango.Layout.Pango_Ellipsize_Mode", ""),
    "PangoContext":      GObject("Pango.Context.Pango_Context"),
    "PangoLayout":       GObject("Pango.Layout.Pango_Layout"),
    "PangoLanguage":     Proxy("Pango.Font.Pango_Language"),
    "PangoFontDescription*": Proxy("Pango.Font.Pango_Font_Description",
                                   "Pango.Font.Property_Font_Description"),
    "PangoFontFace*":   GObject("Pango.Font.Pango_Font_Face"),
    "PangoFontFamily*": GObject("Pango.Font.Pango_Font_Family"),

    "GdkEvent*":         Proxy("Gdk.Event.Gdk_Event", ""),

    "GError*":           Proxy("Glib.Error.GError"),
    "GObject*":          GObject("Glib.Object.GObject"),
    "GClosure*":         Proxy("System.Address", ""),
    "GInitiallyUnowned": GObject("Glib.Object.GInitiallyUnowned"),
    "GValue":            Proxy("Glib.Values.GValue", ""),
    "GdkEventMask":      Enum("Gdk.Event.Gdk_Event_Mask"),
    "GdkAtom":           Proxy("Gdk.Types.Gdk_Atom"),

    # Specific to this binding generator (referenced from binding.xml)
    "VisualList":  List("Gdk.Visual.Gdk_Visual_List.GList"),
    "ObjectList":  List("Glib.Object.Object_Simple_List.GList"),
    "ObjectSList": List("Glib.Object.Object_List.GSlist"),
    "StringList":  List("Gtk.Enums.String_List.Glist"),
    "StringSList": List("Gtk.Enums.String_SList.GSlist"),
    "TreePathList": List("Gtk.Tree_Model.Gtk_Tree_Path_List.Glist"),

    "gpointer":       Proxy("System.Address", ""),
    "GDestroyNotify": Proxy("Glib.G_Destroy_Notify_Address"),
    "GIcon*":        Proxy("Glib.G_Icon.G_Icon"),
    "GQuark":        Proxy("Glib.GQuark"),
    "GObject":       Proxy("Glib.Object.GObject"),
    "GParamSpec":    Proxy("Glib.Param_Spec"),
    "GClosure":      Proxy("GClosure"),

    "GtkIconSet*":     Proxy("Gtk.Icon_Factory.Gtk_Icon_Set"),
    "GtkTargetList*":  Proxy ("Gtk.Selection.Target_List"),
    "GtkTreeIter*":    Proxy("Gtk.Tree_Model.Gtk_Tree_Iter"),
    "WidgetPath*":     Proxy("Gtk.Widget.Widget_Path"),

    "GtkBorder":       Proxy("Gtk.Style.Gtk_Border"),
    "GtkEntry":        GObject("Gtk.GEntry.Gtk_Entry"),
    "GtkFileFilter":   GObject("Gtk.File_Filter.Gtk_File_Filter"),
    "GtkHButtonBox":   GObject("Gtk.Hbutton_Box.Gtk_Hbutton_Box"),
    "GtkRange":        GObject("Gtk.GRange.Gtk_Range"),
    "GtkStatusbar":    GObject("Gtk.Status_Bar.Gtk_Status_Bar"),
    "GtkTreeModel":    GObject("Gtk.Tree_Model.Gtk_Tree_Model"),
    "GtkTreePath*":    Proxy("Gtk.Tree_Model.Gtk_Tree_Path"),
    "GtkVButtonBox":   GObject("Gtk.Vbutton_Box.Gtk_Vbutton_Box"),

    "GtkRcStyle":      GObject("Gtk.Rc.Gtk_Rc_Style"),

    "GtkTreeViewRowSeparatorFunc":
        Callback("Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func"),
    "GtkCellLayoutDataFunc":
        Callback("Gtk.Cell_Layout.Cell_Data_Func"),

    "GKeyFile*":           Proxy("Glib.Key_File.G_Key_File"),

    "GdkColor*": Proxy("Gdk.Color.Gdk_Color",
                       "Gdk.Color.Property_Gdk_Color",
                       "Gdk.Color.Gdk_Color_Or_Null"),
    "GdkDragContext":     GObject("Gdk.Drag_Contexts.Drag_Context"),
    "GdkEventKey*":       Proxy("Gdk.Event.Gdk_Event_Key"),
    "GdkFont":            Proxy("Gdk.Font.Gdk_Font"),
    "GdkVisual*":         Proxy("Gdk.Visual.Gdk_Visual"),
    "GdkPixmap*":         Proxy("Gdk.Pixmap.Gdk_Pixmap"),
    "GdkBitmap*":         Proxy("Gdk.Bitmap.Gdk_Bitmap"),
    "GdkImage*":          Proxy("Gdk.Image.Gdk_Image"),
    "GdkPixbuf":          GObject("Gdk.Pixbuf.Gdk_Pixbuf"),
    "GdkPixbufAnimation*": Proxy("Gdk.Pixbuf.Gdk_Pixbuf_Animation"),
    "GdkRectangle*":      Proxy("Gdk.Rectangle.Gdk_Rectangle"),
    # ??? The above should not be needed, we should infer it from the Gir.
    # we need it to generate the "Stub" object in Gdk.Device.Get_Position
    "GdkRGBA*":           Proxy("Gdk.RGBA.Gdk_RGBA", None,
                                "Gdk.RGBA.Gdk_RGBA_Or_Null"),
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
