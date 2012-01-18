"""This package contains data which must be edited by hand when adding new
   bindings.
"""

from adaformat import *

# List of interfaces to bind. These are processed before the widgets themselves.
# These are GIR names

interfaces = ("Activatable",
              #"AppChooser",
              "Buildable",
              "CellEditable",
              "CellLayout",
              "Editable",
              "FileChooser",
              #"FontChooser",
              "Orientable",
              #"PrintOperationPreview",
              #"RecentChooser",
              "Scrollable",
              #"StyleProvider",
              "ToolShell",
              #"TreeDragDest",
              #"TreeDragSource",
              #"TreeModel",
              #"TreeSortable"
             )

# List of widgets to bind.
# Contains C type names

binding = ("GdkDevice",
           "GdkDragContext",
           "GdkScreen",
           "GtkAboutDialog",
           "GtkAccelGroup",
           "GtkAdjustment",
           "GtkAlignment",
           "GtkArrow",
           "GtkAspectFrame",
           "GtkAssistant",
           "GtkBin",
           "GtkBox",
           "GtkButton",
           "GtkButtonBox",
           "GtkCalendar",
           "GtkCellArea",
           "GtkCellAreaContext",
           "GtkCellRenderer",
           "GtkCheckButton",
           "GtkColorButton",
           "GtkColorSelection",
           "GtkColorSelectionDialog",
           "GtkComboBox",
           "GtkComboBoxText",
           "GtkDialog",
           "GtkDrawingArea",
           "GtkEntry",
           "GtkEntryBuffer",
           "GtkEntryCompletion",
           "GtkEventBox",
           "GtkExpander",
           "GtkFileChooserButton",
           #"GtkFileChooserDialog",
           "GtkFileChooserWidget",
           "GtkFileFilter",
           "GtkFixed",
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
           "GtkImage",
           "GtkInvisible",
           "GtkLabel",
           "GtkLayout",
           "GtkLinkButton",
           "GtkMisc",
           "GtkMenuItem",
           "GtkNotebook",
           "GtkPaned",
           "GtkProgressBar",
           "GtkRadioAction",
           "GtkRadioButton",
           "GtkRange",
           "GtkRecentFilter",
           "GtkScale",
           "GtkSeparator",
           "GtkSeparatorMenuItem",
           "GtkSeparatorToolItem",
           "GtkSizeGroup",
           "GtkScrollbar",
           "GtkSocket",
           "GtkSpinner",
           "GtkStatusbar",
           "GtkStatusIcon",
           "GtkTable",
           "GtkToggleButton",
           "GtkToolbar",
           "GtkToolItem",
           "GtkVBox",
           "GtkVButtonBox",
           "GtkVPaned",
           "GtkVScale",
           "GtkVScrollbar",
           "GtkVSeparator",
           "GtkViewport",
           "GtkVolumeButton"
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
    "gtk_widget_get_direction":     "Gtk.Widget.Get_Direction",
    "gtk_widget_add_events":        "Gtk.Widget.Add_Event",
    "gtk_widget_set_size_request":  "Gtk.Widget.Set_Size_Request",
    "gtk_window_get_default_icon_list": "Gtk.Window.Get_Default_Icon_List",
    "gtk_window_set_default_icon":  "Gtk.Window.Set_Default_Icon",
    "gtk_widget_set_direction":     "Gtk.Window.Set_Direction",
    "gtk_widget_set_has_window":    "Gtk.Widget.Set_Has_Window",
    "gtk_show_uri":                 "gtk_show_uri()",
    "gtk_widget_show":              "Gtk.Widget.Show",
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

    # ??? Doesn't exist
    "gtk_activatable_get_action": "Gtk.Activatable.Get_Action",

    # Will be bound later
    "gtk_action_group_add_action_with_accel":
        "Gtk.Action_Group.Add_Action_With_Accel",
    "gtk_tool_item_set_expand": "Gtk.Tool_Item.Set_Expand",
    "gtk_builder_add_from_file": "Gtk.Builder.Add_From_File",
    "gtk_builder_add_from_string": "Gtk.Builder.Add_From_String",
    "gtk_container_add": "Gtk.Container.Add"
}

# Maps GIR's "name" to a "c:type". This isn't needed for the classes
# themselves, since this is automatically read from the GIR file.
# Mostly used for properties. The values must correspond to entries in
# self.type_exceptions.

naming.girname_to_ctype = {
    "GdkPixbuf.Pixbuf":    "GdkPixbuf",
    "Pango.EllipsizeMode": "PangoEllipsizeMode",
    "Pango.WrapMode":      "PangoWrapMode",
    "Pango.AttrList":      "PangoAttrList",
    "Gio.Icon":            "GIcon*",
    "IconSet":             "GtkIconSet*",
    "Gdk.Pixmap":          "GdkPixmap*",
    "Gdk.Image":           "GdkImage*",
    "GdkPixbuf.PixbufAnimation": "GdkPixbufAnimation*",
    "Gdk.Bitmap":          "GdkBitmap*",
    "GObject.Object":      "GObject*",
    "GObject.Closure":     "GClosure*",
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
    "Digits": "Number_Of_Digits",
    "Reverse": "Gtk_Reverse",
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
    "gint":     Proxy("Gint",    "Glib.Properties.Property_Int"),
    "guint":    Proxy("Guint",   "Glib.Properties.Property_Uint"),
    "guint16":  Proxy("Guint16", "Glib.Properties.Property_Uint"),
    "gfloat":   Proxy("Gfloat",  "Glib.Properties.Property_Float"),

    "cairo_t":               DirectBinding("Cairo.Cairo_Context"),
    "cairo_font_options_t":  DirectBinding("Cairo.Cairo_Font_Options"),

    "PangoAttrList":     Proxy("Pango.Attributes.Pango_Attr_List", ""),
    "PangoEllipsizeMode":Enum("Pango.Layout.Pango_Ellipsize_Mode", ""),
    "PangoWrapMode":     Enum("Pango.Layout.Pango_Wrap_Mode", ""),
    "PangoLayout":       GObject("Pango.Layout.Pango_Layout"),

    "GdkEvent*":         Proxy("Gdk.Event.Gdk_Event", ""),

    "GObject*":          GObject("Glib.Object.GObject"),
    "GClosure*":         Proxy("System.Address", ""),
    "GInitiallyUnowned": GObject("Glib.Object.GInitiallyUnowned"),
    "GValue":            Proxy("Glib.Values.GValue", ""),

    # Specific to this binding generator (referenced from binding.xml)
    "WindowList":  List("Gdk.Window.Gdk_Window_List.GList"),
    "VisualList":  List("Gdk.Visual.Gdk_Visual_List.GList"),
    "WidgetSList": List("Gtk.Widget.Widget_SList.GSList"),
    "WidgetList":  List("Gtk.Widget.Widget_List.GList"),
    "ObjectList":  List("Glib.Object.Object_Simple_List.GList"),
    "ObjectSList": List("Glib.Object.Object_List.GSList"),
    "StringList":  List("Gtk.Enums.String_List.Glist"),
    "StringSList": List("Gtk.Enums.String_SList.GSlist"),

    "gpointer":       Proxy("System.Address", ""),
    "GDestroyNotify": Proxy("Glib.G_Destroy_Notify_Address"),
    "GIcon*":        Proxy("Glib.G_Icon.G_Icon"),
    "GQuark":        Proxy("Glib.GQuark"),
    "GObject":       Proxy("Glib.Object.GObject"),
    "GClosure":      Proxy("GClosure"),

    "GtkBorder":       Proxy("Gtk.Style.Gtk_Border"),
    "GtkIconSet*":     Proxy("Gtk.Icon_Factory.Gtk_Icon_Set"),
    "GtkTargetList*":  Proxy ("Gtk.Selection.Target_List"),
    "GtkTreeIter*":    Proxy("Gtk.Tree_Model.Gtk_Tree_Iter"),

    "GtkArrowType":       Enum("Gtk.Enums.Gtk_Arrow_Type"),
    "GtkAttachOptions":   Enum("Gtk.Enums.Gtk_Attach_Options"),
    "GtkButtonBoxStyle":  Enum("Gtk.Enums.Gtk_Button_Box_Style"),
    "GtkCurveType":       Enum("Gtk.Enums.Gtk_Curve_Type"),
    "GtkDirectionType":   Enum("Gtk.Enums.Gtk_Direction_Type"),
    "GtkIconSize":        Enum("Gtk.Enums.Gtk_Icon_Size"),
    "GtkJustification":   Enum("Gtk.Enums.Gtk_Justification"),
    "GtkMetricType":      Enum("Gtk.Enums.Gtk_Metric_Type",
                               "Gtk.Enums.Property_Metric_Type"),
    "GtkOrientation":     Enum("Gtk.Enums.Gtk_Orientation"),
    "GtkPackType":        Enum("Gtk.Enums.Gtk_Pack_Type"),
    "GtkPositionType":    Enum("Gtk.Enums.Gtk_Position_Type"),
    "GtkReliefStyle":     Enum("Gtk.Enums.Gtk_Relief_Style"),
    "GtkScrollType":      Enum("Gtk.Enums.Gtk_Scroll_Type"),
    "GtkScrollablePolicy": Enum("Gtk.Enums.Gtk_Scrollable_Policy"),
    "GtkSelectionMode":   Enum("Gtk.Enums.Gtk_Selection_Mode"),
    "GtkSensitivityType": Enum("Gtk.Enums.Gtk_Sensitivity_Type"),
    "GtkSizeRequestMode": Enum("Gtk.Enums.Gtk_Size_Request_Mode"),
    "GtkShadowType":      Enum("Gtk.Enums.Gtk_Shadow_Type"),
    "GtkStateFlags":      Enum("Gtk.Enums.Gtk_State_Flags"),
    "GtkToolbarStyle":    Enum("Gtk.Enums.Gtk_Toolbar_Style"),
    "GtkUpdateType":      Enum("Gtk.Enums.Gtk_Update_Type"),

    "GtkEntry":        GObject("Gtk.GEntry.Gtk_Entry"),
    "GtkFileFilter":   GObject("Gtk.File_Filter.Gtk_File_Filter"),
    "GtkHButtonBox":   GObject("Gtk.Hbutton_Box.Gtk_Hbutton_Box"),
    "GtkRange":        GObject("Gtk.GRange.Gtk_Range"),
    "GtkStatusbar":    GObject("Gtk.Status_Bar.Gtk_Status_Bar"),
    "GtkTreeModel":    GObject("Gtk.Tree_Model.Gtk_Tree_Model"),
    "GtkVButtonBox":   GObject("Gtk.Vbutton_Box.Gtk_Vbutton_Box"),

    "GtkTreeViewRowSeparatorFunc":
        Callback("Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func"),
    "GtkCellLayoutDataFunc":
        Callback("Gtk.Cell_Layout.Cell_Data_Func"),

    "GdkColor*":          DirectBinding("Gdk.Color.Gdk_Color"),
    "GdkDragContext":     GObject("Gdk.Drag_Contexts.Drag_Context"),
    "GdkEventKey*":       Proxy("Gdk.Event.Gdk_Event_Key"),
    "GdkFont":            Proxy("Gdk.Font.Gdk_Font"),
    "GdkVisual*":         Proxy("Gdk.Visual.Gdk_Visual"),
    "GdkWindow*":         Proxy("Gdk.Window.Gdk_Window"),
    "GdkPixmap*":         Proxy("Gdk.Pixmap.Gdk_Pixmap"),
    "GdkBitmap*":         Proxy("Gdk.Bitmap.Gdk_Bitmap"),
    "GdkImage*":          Proxy("Gdk.Image.Gdk_Image"),
    "GdkPixbuf":          GObject("Gdk.Pixbuf.Gdk_Pixbuf"),
    "GdkPixbufAnimation*": Proxy("Gdk.Pixbuf.Gdk_Pixbuf_Animation"),
    "GdkRectangle*":      Proxy("Gdk.Rectangle.Gdk_Rectangle"),
    "GdkScreen":          GObject("Gdk.Screen.Gdk_Screen"),
    # ??? The above should not be needed, we should infer it from the Gir.
    # we need it to generate the "Stub" object in Gdk.Device.Get_Position
    "GdkRGBA*":           Proxy("Gdk.RGBA.Gdk_RGBA"),
    "Gdk.ModifierType":   Proxy("Gdk.Types.Gdk_Modifier_Type"),
    "GdkModifierType":    Proxy("Gdk.Types.Gdk_Modifier_Type"),
    "GdkKeyType":         Proxy("Gdk.Types.Gdk_Key_Type"),
}
