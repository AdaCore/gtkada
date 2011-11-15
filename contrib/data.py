"""This package contains data which must be edited by hand when adding new
   bindings.
"""

from adaformat import *

interfaces = ("Activatable",
              "Buildable",
              "CellEditable",
              "CellLayout",
              #"FileChooser",
              "Orientable",
              #"RecentChooser",
              #"TreeSortable"
             )

# Contains "GIR names"
binding = ("AboutDialog", "Arrow", "AspectFrame",
           "Bin", "Box", "Button", "ButtonBox",
           "EventBox",
           "Frame", "Range", "HBox", "HButtonBox", "HPaned", "HScale",
           "Label", "Layout",
           "Misc", "Paned", "Scale",
           "VBox", "VButtonBox", "Viewport", "VPaned",
           "VScale", "VolumeButton",
           # "Entry",   # partially done only
           "AccelGroup",
           "Adjustment",
           "Alignment",
           "Assistant",
           "Calendar",
           "CheckButton",
           # "ComboBox",  # Needs .gir for gtk 2.24 for backward compatibility
                          # Also needs support for interfaces
           "ColorSelection",
           "ColorSelectionDialog",
           "Dialog",
           "DrawingArea",
           "EntryCompletion",
           "Expander",
           "Fixed",
           "FontSelection",
           "FontSelectionDialog",
           "Image",
           "RadioAction",
           "RadioButton",
           "Separator", "VSeparator", "HSeparator",
           "SeparatorMenuItem",
           "SeparatorToolItem",
           "SizeGroup",
           "Spinner",
           "Statusbar",
           "ToggleButton",
           "Table"
          )


naming.cname_to_adaname = {
    # Maps c methods to Ada subprograms.
    # All methods that are generated automatically will be added
    # as they are processed.
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

naming.girname_to_ctype = {
    # Maps GIR's "name" to a "c:type". This isn't needed for the
    # classes themselves, since this is automatically read from the
    # GIR file.
    # Mostly used for properties. The values must correspond to
    # entries in self.type_exceptions.
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
    "Object":              "GtkObject",
}

naming.exceptions = {
    # Naming exceptions. In particular maps Ada keywords.
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
}

naming.type_exceptions = {
    # Maps C types to type descriptions.
    # All standard widgets will be added automatically. Only special
    # namings are needed here
    "gboolean":          Enum("Boolean",
                              "Glib.Properties.Property_Boolean"),
    "gdouble":  Proxy("Gdouble", "Glib.Properties.Property_Double"),
    "gint":     Proxy("Gint",    "Glib.Properties.Property_Int"),
    "guint":    Proxy("Guint",   "Glib.Properties.Property_Uint"),
    "gfloat":   Proxy("Gfloat",  "Glib.Properties.Property_Float"),

    "PangoAttrList":     Proxy("Pango.Attributes.Pango_Attr_List", ""),
    "PangoEllipsizeMode":Enum("Pango.Layout.Pango_Ellipsize_Mode", ""),
    "PangoWrapMode":     Enum("Pango.Layout.Pango_Wrap_Mode", ""),
    "PangoLayout":       GObject("Pango.Layout.Pango_Layout"),

    "GdkEvent*":    Proxy("Gdk.Event.Gdk_Event", ""),

    "GObject*":     GObject("Glib.Object.GObject"),
    "GClosure*":    Proxy("System.Address", ""),
    "GValue":       Proxy("Glib.Values.GValue", ""),

    # Specific to this binding generator (referenced from binding.xml)
    "WidgetSList": List("Gtk.Widget.Widget_SList.GSList"),
    "WidgetList":  List("Gtk.Widget.Widget_List.GList"),
    "ObjectList":  List("Glib.Object.Object_Simple_List.GList"),
    "ObjectSList": List("Glib.Object.Object_List.GSList"),
    "StringList":  List("Gtk.Enums.String_List.Glist"),
    "MessagesList": List("Gtk.Status_Bar.Messages_List.GSlist"),

    "gpointer":       Proxy("System.Address", ""),
    "GDestroyNotify": Proxy("Glib.G_Destroy_Notify_Address"),
    "GIcon*":        Proxy("Glib.G_Icon.G_Icon"),

    "CellLayoutDataFunc": Proxy("Gtk.Cell_Layout.Cell_Data_Func", ""),

    "GtkPositionType":    Enum("Gtk.Enums.Gtk_Position_Type"),
    "GtkReliefStyle":     Enum("Gtk.Enums.Gtk_Relief_Style"),
    "GtkShadowType":      Enum("Gtk.Enums.Gtk_Shadow_Type"),
    "GtkArrowType":       Enum("Gtk.Enums.Gtk_Arrow_Type"),
    "GtkPackType":        Enum("Gtk.Enums.Gtk_Pack_Type"),
    "GtkJustification":   Enum("Gtk.Enums.Gtk_Justification"),
    "GtkScrollType":      Enum("Gtk.Enums.Gtk_Scroll_Type"),
    "GtkSelectionMode":   Enum("Gtk.Enums.Gtk_Selection_Mode"),
    "GtkSensitivityType": Enum("Gtk.Enums.Gtk_Sensitivity_Type"),
    "GtkUpdateType":      Enum("Gtk.Enums.Gtk_Update_Type"),
    "GtkButtonBoxStyle":  Enum("Gtk.Enums.Gtk_Button_Box_Style"),
    "GtkCurveType":       Enum("Gtk.Enums.Gtk_Curve_Type"),
    "GtkMetricType":      Enum("Gtk.Enums.Gtk_Metric_Type",
                               "Gtk.Enums.Property_Metric_Type"),
    "GtkAttachOptions":   Enum("Gtk.Enums.Gtk_Attach_Options"),
    "GtkOrientation":     Enum("Gtk.Enums.Gtk_Orientation"),

    "GtkCellEditable": Interface("Gtk.Cell_Editable.Gtk_Cell_Editable"),
    "GtkCellLayout":   Interface("Gtk.Cell_Layout.Gtk_Cell_Layout"),
    "GtkFileChooser":  Interface("Gtk.File_Chooser.Gtk_File_Chooser"),
    "GtkRecentChooser":
        Interface("Gtk.Recent_Chooser.Gtk_Recent_Chooser"),
    "GtkTreeSortable": Interface("Gtk.Tree_Sortable.Gtk_Tree_Sortable"),

    "GtkObject": GObject("Glib.Object.GObject"),

    "GtkAboutDialog":  GObject("Gtk.About_Dialog.Gtk_About_Dialog"),
    "GtkAccelGroup":   GObject("Gtk.Accel_Group.Gtk_Accel_Group"),
    "GtkAspectFrame":  GObject("Gtk.Aspect_Frame.Gtk_Aspect_Frame"),
    "GtkButtonBox":    GObject("Gtk.Button_Box.Gtk_Button_Box"),
    "GtkCellRenderer":
        GObject("Gtk.Cell_Renderer.Gtk_Cell_Renderer"),
    "GtkCheckButton":  GObject("Gtk.Check_Button.Gtk_Check_Button"),
    "GtkColorSelection":
        GObject("Gtk.Color_Selection.Gtk_Color_Selection"),
    "GtkColorSelectionDialog":
        GObject("Gtk.Color_Selection_Dialog.Gtk_Color_Selection_Dialog"),
    "GtkComboBox":     GObject("Gtk.Combo_Box.Gtk_Combo_Box"),
    "GtkDrawingArea":  GObject("Gtk.Drawing_Area.Gtk_Drawing_Area"),
    "GtkEntry":        GObject("Gtk.GEntry.Gtk_Entry"),
    "GtkEntryCompletion": GObject("Gtk.Entry_Completion.Gtk_Entry_Completion"),
    "GtkEventBox":     GObject("Gtk.Event_Box.Gtk_Event_Box"),
    "GtkFileFilter":   GObject("Gtk.File_Filter.Gtk_File_Filter"),
    "GtkFontSelection": GObject("Gtk.Font_Selection.Gtk_Font_Selection"),
    "GtkFontSelectionDialog":
       GObject("Gtk.Font_Selection_Dialog.Gtk_Font_Selection_Dialog"),
    "GtkHButtonBox":   GObject("Gtk.Hbutton_Box.Gtk_Hbutton_Box"),
    "GtkMenuItem":     GObject("Gtk.Menu_Item.Gtk_Menu_Item"),
    "GtkRadioAction":  GObject("Gtk.Radio_Action.Gtk_Radio_Action"),
    "GtkRadioButton":  GObject("Gtk.Radio_Button.Gtk_Radio_Button"),
    "GtkRange":        GObject("Gtk.GRange.Gtk_Range"),
    "GtkScaleButton":  GObject("Gtk.Scale_Button.Gtk_Scale_Button"),
    "GtkSizeGroup":    GObject("Gtk.Size_Group.Gtk_Size_Group"),
    "GtkSeparatorMenuItem":
        GObject("Gtk.Separator_Menu_Item.Gtk_Separator_Menu_Item"),
    "GtkSeparatorToolItem":
        GObject("Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item"),
    "GtkStatusbar":    GObject("Gtk.Status_Bar.Gtk_Status_Bar"),
    "GtkToggleAction": GObject("Gtk.Toggle_Action.Gtk_Toggle_Action"),
    "GtkToggleButton": GObject("Gtk.Toggle_Button.Gtk_Toggle_Button"),
    "GtkToolItem":     GObject("Gtk.Tool_Item.Gtk_Tool_Item"),
    "GtkTreeIter*":     Proxy("Gtk.Tree_Model.Gtk_Tree_Iter"),
    "GtkTreeModel":    GObject("Gtk.Tree_Model.Gtk_Tree_Model"),
    "GtkVButtonBox":   GObject("Gtk.Vbutton_Box.Gtk_Vbutton_Box"),
    "GtkVolumeButton": GObject("Gtk.Volume_Button.Gtk_Volume_Button"),

    "GtkTreeViewRowSeparatorFunc":
        Callback("Gtk.Tree_View.Gtk_Tree_View_Row_Separator_Func"),
    "GtkAboutDialogActivateLinkFunc":
        Callback("Gtk.About_Dialog.Activate_Link_Func"),
    "GtkAccelGroupFindFunc":
        Callback("Gtk.Accel_Group.Gtk_Accel_Group_Find_Func"),
    "GtkColorSelectionChangePaletteWithScreenFunc":
        Callback("Gtk.Color_Selection.Gtk_Color_Selection_Change_Palette_With_Screen_Func"),
    "GtkEntryCompletionMatchFunc":
        Callback("Gtk.Entry_Completion.Gtk_Entry_Completion_Match_Func"),
    "GtkCalendarDetailFunc":
        Callback("Gtk.Calendar.Gtk_Calendar_Detail_Func"),
    "GtkCellLayoutDataFunc":
        Callback("Gtk.Cell_Layout.Cell_Data_Func"),
    "GtkAssistantPageFunc":
        Callback("Gtk.Assistant.Gtk_Assistant_Page_Func"),
    "GtkBorder":          Proxy("Gtk.Style.Gtk_Border"),
    "GtkIconSet*":        Proxy("Gtk.Icon_Factory.Gtk_Icon_Set"),

    "GdkColor":           DirectBinding("Gdk.Color.Gdk_Color"),
    "GdkFont":            Proxy("Gdk.Font.Gdk_Font"),
    "GdkWindow":          Proxy("Gdk.Window.Gdk_Window"),
    "GdkPixmap*":         Proxy("Gdk.Pixmap.Gdk_Pixmap"),
    "GdkBitmap*":         Proxy("Gdk.Bitmap.Gdk_Bitmap"),
    "GdkImage*":          Proxy("Gdk.Image.Gdk_Image"),
    "GdkPixbuf":          GObject("Gdk.Pixbuf.Gdk_Pixbuf"),
    "GdkPixbufAnimation*": Proxy("Gdk.Pixbuf.Gdk_Pixbuf_Animation"),
    "GdkRectangle":       Proxy("Gdk.Rectangle.Gdk_Rectangle"),
    "GdkModifierType":    Proxy("Gdk.Types.Gdk_Modifier_Type"),
    "GdkKeyType":         Proxy("Gdk.Types.Gdk_Key_Type"),
}
