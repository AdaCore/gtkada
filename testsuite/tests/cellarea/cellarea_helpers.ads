--  Callback support for the Gtk.Cell_Area tests. Keeping these callbacks in
--  a package gives the C-facing adapters subprograms with no hidden state.

with Glib; use Glib;
with Gdk.Rectangle;
with Gtk.Cell_Area;
with Gtk.Cell_Area_Context;
with Gtk.Cell_Editable;
with Gtk.Cell_Layout;
with Gtk.Cell_Renderer;
with Glib.Object;
with Gtk.Tree_Model;

package Cellarea_Helpers is
   Cell_Calls        : Natural := 0;
   Alloc_Calls       : Natural := 0;
   Data_Calls        : Natural := 0;
   Destroy_Calls     : Natural := 0;
   Apply_Calls       : Natural := 0;
   Apply_Slot_Calls  : Natural := 0;
   Focus_Calls       : Natural := 0;
   Focus_Slot_Calls  : Natural := 0;
   Add_Edit_Calls    : Natural := 0;
   Add_Edit_Slot_Calls : Natural := 0;
   Remove_Edit_Calls : Natural := 0;
   Remove_Edit_Slot_Calls : Natural := 0;

   --  Each callback counts visits and returns False so traversal continues.
   function Each_Cell
     (Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
      return Boolean;
   function Each_Cell_With_Data
     (Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Data     : Glib.Gint) return Boolean;

   function Each_Cell_Alloc
     (Renderer        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Background : Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   function Each_Cell_Alloc_With_Data
     (Renderer        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
      Data            : Glib.Gint) return Boolean;

   --  Data functions set a visible renderer property; the generic form also
   --  records the user data supplied through Gtk.Cell_Area's closure.
   procedure Set_Cell_Data
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   procedure Set_Cell_Data_With_Data
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data        : Glib.Gint);
   procedure Destroy_Data (Data : in out Glib.Gint);

   --  Typed and GObject-slot forms cover all four Gtk.Cell_Area signals.
   --  Counters let the main test confirm emitted signals reached a handler.
   --  The apply signal passes a model row and two row-state flags.
   procedure On_Apply
     (Self        : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
      Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Is_Expander : Boolean;
      Is_Expanded : Boolean);
   procedure On_Apply_Slot
     (Self        : access Glib.Object.GObject_Record'Class;
      Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
      Is_Expander : Boolean;
      Is_Expanded : Boolean);
   --  Focus handlers receive the renderer and current tree-path string.
   procedure On_Focus
     (Self     : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Path     : UTF8_String);
   --  The GObject form uses the same focus arguments with a generic instance.
   procedure On_Focus_Slot
     (Self     : access Glib.Object.GObject_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Path     : UTF8_String);
   procedure On_Add_Edit
     (Self      : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
      Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable  : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
      Path      : UTF8_String);
   --  Add-editable supplies the editable interface, cell bounds and path.
   procedure On_Add_Edit_Slot
     (Self      : access Glib.Object.GObject_Record'Class;
      Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable  : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
      Path      : UTF8_String);
   --  Remove-editable reports the renderer and temporary editable widget.
   procedure On_Remove_Edit
     (Self     : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable : Gtk.Cell_Editable.Gtk_Cell_Editable);
   procedure On_Remove_Edit_Slot
     (Self     : access Glib.Object.GObject_Record'Class;
      Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Editable : Gtk.Cell_Editable.Gtk_Cell_Editable);
end Cellarea_Helpers;
