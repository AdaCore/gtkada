package Gtk.Widget is
   function Requisition_Get_Type return Glib.GType;
   pragma Import (C, Requisition_Get_Type, "gtk_requisition_get_type");
   --  Return the internal type for a Gtk_Requisition

   procedure Destroy_Cb (Widget : access Gtk_Widget_Record'Class);
   --  This function should be used as a callback to destroy a widget.
   --  All it does is call Destroy on its argument, but its profile is
   --  compatible with the handlers found in Gtk.Handlers.

   generic
      type Widget_Type is new Gtk_Widget_Record with private;
      with procedure Realize_Proc (Widget : access Widget_Type'Class);
   package Realize_Handling is

      procedure Set_Realize (Widget : access Gtk_Widget_Record'Class);
      --  Set the realize handler at the low level.
      --  This is needed to replace the default realize in new widgets.

   private
      --  <doc_ignore>
      procedure Internal_Realize (Widget : System.Address);
      --  The wrapper passed to Gtk+.
      pragma Convention (C, Internal_Realize);
      --  </doc_ignore>
   end Realize_Handling;

   procedure Translate_Coordinates
     (Src_Widget  : Gtk_Widget;
      Dest_Widget : Gtk_Widget;
      Src_X       : Gint;
      Src_Y       : Gint;
      Dest_X      : out Gint;
      Dest_Y      : out Gint;
      Result      : out Boolean);
   --  Translate coordinates relative to Src_Widget's allocation to coordinates
   --  relative to Dest_Widget's allocations. In order to perform this
   --  operation, both widgets must be realized, and must share a common
   --  toplevel.
   --
   --  Result is set to False if either widget was not realized, or there
   --  was no common ancestor. In this case, nothing is stored in Dest_X and
   --  Dest_Y. Otherwise True.

   --------------------------
   -- Creating new widgets --
   --------------------------
   --  Although the core subprogram for creating new widgets is
   --  Glib.Gobjects.Initialize_Class_Record, it is often useful to override
   --  some internal pointers to functions.
   --  The functions below are not needed unless you are writting your own
   --  widgets, and should be reserved for advanced customization of the
   --  standard widgets.

   type Size_Allocate_Handler is access procedure
     (Widget : System.Address; Allocation : Gtk_Allocation);
   pragma Convention (C, Size_Allocate_Handler);
   --  Widget is the gtk+ C widget, that needs to be converted to Ada through
   --  a call to:
   --    declare
   --       Stub : Gtk_Widget_Record; --  or the exact type you expect
   --    begin
   --       My_Widget := Gtk_Widget (Glib.Object.Get_User_Data (Widget, Stub);
   --    end;

   procedure Set_Default_Size_Allocate_Handler
     (Klass   : Glib.Object.GObject_Class;
      Handler : Size_Allocate_Handler);
   pragma Import (C, Set_Default_Size_Allocate_Handler,
                  "ada_gtk_widget_set_default_size_allocate_handler");
   --  Override the default size_allocate handler for this class. This handler
   --  is automatically called in several cases (when a widget is dynamically
   --  resized for instance), not through a signal. Thus, if you need to
   --  override the default behavior provided by one of the standard
   --  containers, you can not simply use Gtk.Handlers.Emit_Stop_By_Name, and
   --  you must override the default handler. Note also that this handler
   --  is automatically inherited by children of this class.

end Gtk.Widget;
