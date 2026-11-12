--  Callback bodies for the Gtk.Cell_Area tests. The arguments are checked in
--  the main test through visit counts and the renderer's resulting property.

with Glib.Properties;
with Gtk.Cell_Renderer_Text;

package body Cellarea_Helpers is
   --  Plain traversal counts each renderer and never stops iteration early.
   function Each_Cell
     (Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
      return Boolean is
   begin
      Cell_Calls := Cell_Calls + 1;
      return False;
   end Each_Cell;

   --  The userdata callback adds its argument once per renderer.
   function Each_Cell_With_Data
     (Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Data     : Glib.Gint) return Boolean is
   begin
      Cell_Calls := Cell_Calls + Natural (Data);
      return False;
   end Each_Cell_With_Data;

   --  Allocation traversals use the same visit-count check.
   function Each_Cell_Alloc
     (Renderer        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Background : Gdk.Rectangle.Gdk_Rectangle) return Boolean is
   begin
      Alloc_Calls := Alloc_Calls + 1;
      return False;
   end Each_Cell_Alloc;

   --  This callback also confirms that the C closure preserves user data.
   function Each_Cell_Alloc_With_Data
     (Renderer        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
      Data            : Glib.Gint) return Boolean is
   begin
      Alloc_Calls := Alloc_Calls + Natural (Data);
      return False;
   end Each_Cell_Alloc_With_Data;

   --  The plain cell-data function writes a predictable text property.
   procedure Set_Cell_Data
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      Data_Calls := Data_Calls + 1;
      Glib.Properties.Set_Property
        (Cell, Gtk.Cell_Renderer_Text.Text_Property, "plain callback");
   end Set_Cell_Data;

   --  The generic form writes the supplied integer into the text property.
   procedure Set_Cell_Data_With_Data
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data        : Glib.Gint) is
   begin
      Data_Calls := Data_Calls + 1;
      Glib.Properties.Set_Property
        (Cell, Gtk.Cell_Renderer_Text.Text_Property, Glib.Gint'Image (Data));
   end Set_Cell_Data_With_Data;

   --  Count closure destruction when the callback is replaced or cleared.
   procedure Destroy_Data (Data : in out Glib.Gint) is
   begin
      Destroy_Calls := Destroy_Calls + 1;
   end Destroy_Data;

   --  Applying attributes emits a model row and two row-state flags.
   procedure On_Apply
     (Self        : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
      Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Is_Expander : Boolean;
      Is_Expanded : Boolean) is
   begin
      Apply_Calls := Apply_Calls + 1;
   end On_Apply;

   --  The slot form observes the same apply signal through GObject.
   procedure On_Apply_Slot
     (Self        : access Glib.Object.GObject_Record'Class;
      Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Is_Expander : Boolean;
      Is_Expanded : Boolean) is
   begin
      Apply_Slot_Calls := Apply_Slot_Calls + 1;
   end On_Apply_Slot;

   --  Focus callbacks receive the focused renderer and tree-path string.
   procedure On_Focus
     (Self     : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Path     : UTF8_String) is
   begin
      Focus_Calls := Focus_Calls + 1;
   end On_Focus;

   --  This variant covers the slot marshaller's GObject instance conversion.
   procedure On_Focus_Slot
     (Self     : access Glib.Object.GObject_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Path     : UTF8_String) is
   begin
      Focus_Slot_Calls := Focus_Slot_Calls + 1;
   end On_Focus_Slot;

   --  The add signal includes the renderer, editable interface and rectangle.
   procedure On_Add_Edit
     (Self      : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
      Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable  : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
      Path      : UTF8_String) is
   begin
      Add_Edit_Calls := Add_Edit_Calls + 1;
   end On_Add_Edit;

   --  Keep the GObject-slot signature available for editable integration.
   procedure On_Add_Edit_Slot
     (Self      : access Glib.Object.GObject_Record'Class;
      Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable  : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
      Path      : UTF8_String) is
   begin
      Add_Edit_Slot_Calls := Add_Edit_Slot_Calls + 1;
   end On_Add_Edit_Slot;

   --  Removing an editable reports its renderer and temporary widget.
   procedure On_Remove_Edit
     (Self     : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable : Gtk.Cell_Editable.Gtk_Cell_Editable) is
   begin
      Remove_Edit_Calls := Remove_Edit_Calls + 1;
   end On_Remove_Edit;

   --  This variant uses the GObject instance convention for the same signal.
   procedure On_Remove_Edit_Slot
     (Self     : access Glib.Object.GObject_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable : Gtk.Cell_Editable.Gtk_Cell_Editable) is
   begin
      Remove_Edit_Slot_Calls := Remove_Edit_Slot_Calls + 1;
   end On_Remove_Edit_Slot;
end Cellarea_Helpers;
