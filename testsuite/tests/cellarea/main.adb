--  Tests Gtk.Cell_Area's renderer, context, sizing, allocation, model and
--  callback APIs. The cell-view constructor case ports
--  testsuite/c_tests/cellarea.c:test_cellview_new_with_context.

pragma Warnings (Off, "*obsolescent*");

with Ada.Command_Line;

with Glib;                  use Glib;
with Glib.Object;
with Glib.Main;
with Glib.Properties;       use Glib.Properties;
with Glib.Test;             use Glib.Test;
with Glib.Values;           use Glib.Values;
with Gdk.Rectangle;         use Gdk.Rectangle;
with Gdk.Pixbuf;
with Gdk.Texture;
with Gtk.Cell_Area;         use Gtk.Cell_Area;
with Gtk.Cell_Area_Box;     use Gtk.Cell_Area_Box;
with Gtk.Cell_Area_Context; use Gtk.Cell_Area_Context;
with Gtk.Cell_Layout;       use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Cell_Editable;
with Gtk.Cell_View;         use Gtk.Cell_View;
with Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Label;             use Gtk.Label;
with Gtk.List_Store;        use Gtk.List_Store;
with Gtk.Main;
with Gtk.Orientable;
with Gtk.Snapshot;          use Gtk.Snapshot;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Widget;
with Gtk.Window;
with Cellarea_Helpers;

procedure Main is
   use type Gtk.Cell_Area.Gtk_Cell_Area;
   use type Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   use type Gtk.Cell_Editable.Gtk_Cell_Editable;
   use type Gtk.Cell_View.Gtk_Cell_View;
   use type Gtk.Cell_Layout.Gtk_Cell_Layout;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Tree_Model.Gtk_Tree_Model;
   use type Gtk.Tree_Model.Gtk_Tree_Path;

   package Cell_Callbacks is new Gtk.Cell_Area.Foreach_User_Data
     (User_Data_Type => Gint);
   package Alloc_Callbacks is new Gtk.Cell_Area.Foreach_Alloc_User_Data
     (User_Data_Type => Gint);
   package Data_Callbacks is new Gtk.Cell_Area.Set_Cell_Data_Func_User_Data
     (User_Data_Type => Gint,
      Destroy        => Cellarea_Helpers.Destroy_Data);
   package Layout_Data_Callbacks is new Gtk.Cell_Layout.Set_Cell_Data_Func_User_Data
     (User_Data_Type => Gint,
      Destroy        => Cellarea_Helpers.Destroy_Data);
   --  Instantiate the box's generic callback API as well as the parent area's:
   --  each binding has its own generated closure trampoline.
   package Box_Data_Callbacks is new Gtk.Cell_Area_Box.Set_Cell_Data_Func_User_Data
     (User_Data_Type => Gint,
      Destroy        => Cellarea_Helpers.Destroy_Data);

   procedure Test_Cell_View_Context
   with Convention => C;
   procedure Test_Cell_View_Constructors
   with Convention => C;
   procedure Test_Cell_View_Methods
   with Convention => C;
   procedure Test_Cell_View_Accessibility
   with Convention => C;
   procedure Test_Cell_Area_Methods
   with Convention => C;
   procedure Test_Cell_Area_Context
   with Convention => C;
   procedure Test_Cell_Area_Box_Methods
   with Convention => C;

   --  Get_Area takes an access to the specific context record. Materialize
   --  that parent view because Gtk_Cell_Area_Context is access-to-classwide.
   function Context_Area (Context : Gtk_Cell_Area_Context) return Gtk_Cell_Area is
      Parent_View : aliased Gtk_Cell_Area_Context_Record :=
        Gtk_Cell_Area_Context_Record (Context.all);
   begin
      return Gtk.Cell_Area.Get_Area (Parent_View'Access);
   end Context_Area;

   ----------------------------
   -- Test_Cell_View_Context --
   ----------------------------

   --  The C regression checks that a cell view keeps both the supplied area
   --  and its matching context; this also exercises both GtkAda Get_Area
   --  overloads.
   procedure Test_Cell_View_Context is
      Area    : constant Gtk_Cell_Area_Box := Gtk_Cell_Area_Box_New;
      Context : constant Gtk_Cell_Area_Context := Area.Create_Context;
      View    : constant Gtk_Cell_View := Gtk_Cell_View_New_With_Context
        (Area, Context);
   begin
      Assert_True (Context_Area (Context) = Gtk_Cell_Area (Area));
      Assert_True (Gtk.Cell_Area.Get_Area (+View) = Gtk_Cell_Area (Area));
   end Test_Cell_View_Context;

   ---------------------------------
   -- Test_Cell_View_Constructors --
   ---------------------------------

   --  Exercise both constructor styles and their already-created guards.
   --  A filled in-memory pixbuf supplies a deterministic texture without I/O.
   procedure Test_Cell_View_Constructors is
      Area       : constant Gtk_Cell_Area_Box := Gtk_Cell_Area_Box_New;
      Context    : constant Gtk_Cell_Area_Context := Area.Create_Context;
      Pixbuf     : constant Gdk.Pixbuf.Gdk_Pixbuf :=
        Gdk.Pixbuf.Gdk_Pixbuf_New
          (Gdk.Pixbuf.Gdk_Colorspace_Rgb, True, 8, 1, 1);
      Texture    : constant Gdk.Texture.Gdk_Texture :=
        Gdk.Texture.Gdk_Texture_New_For_Pixbuf (Pixbuf);
      By_Default : constant Gtk_Cell_View := Gtk_Cell_View_New;
      By_Context : constant Gtk_Cell_View := Gtk_Cell_View_New_With_Context
        (Area, Context);
      By_Markup  : constant Gtk_Cell_View := Gtk_Cell_View_New_With_Markup
        ("<b>markup</b>");
      By_Text    : constant Gtk_Cell_View := Gtk_Cell_View_New_With_Text
        ("constructor text");
      By_Texture : constant Gtk_Cell_View := Gtk_Cell_View_New_With_Texture
        (Texture);
      Via_Default : Gtk_Cell_View;
      Via_Context : Gtk_Cell_View;
      Via_Markup  : Gtk_Cell_View;
      Via_Text    : Gtk_Cell_View;
      Via_Texture : Gtk_Cell_View;
      Existing    : constant Gtk_Cell_View := Gtk_Cell_View_New;
   begin
      Pixbuf.Fill (16#3366CCFF#);
      --  Each function constructor creates a live widget; text, markup and
      --  texture variants also install their initial renderer.
      Assert_True (By_Default.Is_Created);
      Assert_True (By_Context.Is_Created);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length
           (Gtk.Cell_View.Get_Cells (By_Markup)), 1);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length
           (Gtk.Cell_View.Get_Cells (By_Text)), 1);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length
           (Gtk.Cell_View.Get_Cells (By_Texture)), 1);

      --  The Gtk_New overloads return the same initialized widget forms.
      Gtk_New (Via_Default);
      Gtk_New_With_Context (Via_Context, Area, Context);
      Gtk_New_With_Markup (Via_Markup, "<i>procedure markup</i>");
      Gtk_New_With_Text (Via_Text, "procedure text");
      Gtk_New_With_Texture (Via_Texture, Texture);
      Assert_True (Via_Default.Is_Created and Via_Context.Is_Created);
      Assert_True (Via_Markup.Is_Created and Via_Text.Is_Created);
      Assert_True (Via_Texture.Is_Created);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length
           (Gtk.Cell_View.Get_Cells (Via_Text)), 1);

      --  All Initialize* entry points must leave an existing object intact.
      Gtk.Cell_View.Initialize (Existing);
      Gtk.Cell_View.Initialize_With_Context (Existing, Area, Context);
      Gtk.Cell_View.Initialize_With_Markup (Existing, "ignored markup");
      Gtk.Cell_View.Initialize_With_Text (Existing, "ignored text");
      Gtk.Cell_View.Initialize_With_Texture (Existing, Texture);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length
           (Gtk.Cell_View.Get_Cells (Existing)), 0);
   end Test_Cell_View_Constructors;

   ----------------------------
   -- Test_Cell_View_Methods --
   ----------------------------

   --  Port the observable GtkCellView model, displayed-row, cell-layout and
   --  data-function behavior using one in-memory row and two text renderers.
   --  Cell-layout operations go through the interface package so its binding
   --  wrappers and both callback trampolines are exercised directly.
   procedure Test_Cell_View_Methods is
      View       : constant Gtk_Cell_View := Gtk_Cell_View_New;
      Layout     : constant Gtk.Cell_Layout.Gtk_Cell_Layout := +View;
      Store      : Gtk_List_Store;
      Iter       : Gtk_Tree_Iter;
      Path       : constant Gtk_Tree_Path := Gtk_Tree_Path_New_First;
      First      : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Last       : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Cells      : Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
      Minimum    : Gint;
      Natural    : Gint;
      Min_Base   : Gint;
      Nat_Base   : Gint;
      Window     : Gtk.Window.Gtk_Window;

      --  Gtk.Widget.Measure takes the specific parent access type, so copy
      --  the classwide view into its GtkWidget record view for each request.
      procedure Measure_View is
         Parent_View : aliased Gtk.Widget.Gtk_Widget_Record :=
           Gtk.Widget.Gtk_Widget_Record (View.all);
      begin
         Gtk.Widget.Measure
           (Parent_View'Access, Orientation_Horizontal, -1,
            Minimum, Natural, Min_Base, Nat_Base);
      end Measure_View;

      --  Queue a redraw through GtkWidget's specific parent access type.
      procedure Queue_View_Draw is
         Parent_View : aliased Gtk.Widget.Gtk_Widget_Record :=
           Gtk.Widget.Gtk_Widget_Record (View.all);
      begin
         Gtk.Widget.Queue_Draw (Parent_View'Access);
      end Queue_View_Draw;

      --  GtkCellView applies cell data functions while drawing its row. Process
      --  the private display's frame events until the callback is observed.
      procedure Wait_For_Data_Callback is
      begin
         for Attempt in 1 .. 100 loop
            exit when Cellarea_Helpers.Data_Calls > 0;
            declare
               Dispatched : constant Boolean :=
                 Glib.Main.Main_Context_Iteration (May_Block => False);
               pragma Unreferenced (Dispatched);
            begin
               null;
            end;
            delay 0.01;
         end loop;
      end Wait_For_Data_Callback;
   begin
      --  Exercise the Gtk.Cell_Layout identity conversion on the interface
      --  value before using it through the implementation.
      Assert_True (Gtk.Cell_Layout."+" (Layout) = Layout);

      Gtk_New (Store, (0 => GType_String));
      Store.Append (Iter);
      Store.Set (Iter, 0, "cell view row");

      --  Model and displayed-row getters round-trip the same GtkTreeModel row.
      Gtk.Cell_View.Set_Model (View, +Store);
      Assert_True (Gtk.Cell_View.Get_Model (View) = +Store);
      Gtk.Cell_View.Set_Displayed_Row (View, Path);
      Assert_Cmpstr_Eq
        (Gtk.Tree_Model.To_String (Gtk.Cell_View.Get_Displayed_Row (View)), "0");
      Gtk.Cell_View.Set_Displayed_Row
        (View, Gtk.Tree_Model.Null_Gtk_Tree_Path);
      Assert_True
        (Gtk.Cell_View.Get_Displayed_Row (View)
         = Gtk.Tree_Model.Null_Gtk_Tree_Path);
      Gtk.Cell_View.Set_Displayed_Row (View, Path);

      --  Boolean controls are checked in both directions, including reset.
      Gtk.Cell_View.Set_Draw_Sensitive (View, True);
      Assert_True (Gtk.Cell_View.Get_Draw_Sensitive (View));
      Gtk.Cell_View.Set_Draw_Sensitive (View, False);
      Assert_False (Gtk.Cell_View.Get_Draw_Sensitive (View));
      Gtk.Cell_View.Set_Fit_Model (View, True);
      Assert_True (Gtk.Cell_View.Get_Fit_Model (View));
      Gtk.Cell_View.Set_Fit_Model (View, False);
      Assert_False (Gtk.Cell_View.Get_Fit_Model (View));

      --  Packing and reordering are verified through the returned cell list.
      Gtk.Cell_Layout.Pack_Start (Layout, First, True);
      Gtk.Cell_Layout.Pack_End (Layout, Last, False);
      Cells := Gtk.Cell_Layout.Get_Cells (Layout);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length (Cells), 2);
      Gtk.Cell_Layout.Reorder (Layout, Last, 0);
      Cells := Gtk.Cell_Layout.Get_Cells (Layout);
      Assert_True
        (Gtk.Cell_Renderer.Cell_Renderer_List.Nth_Data (Cells, 0)
         = Gtk.Cell_Renderer.Gtk_Cell_Renderer (Last));

      --  Attribute mapping follows the selected row and stops after clearing.
      Gtk.Cell_Layout.Add_Attribute (Layout, First, "text", 0);
      Measure_View;
      Assert_True (Minimum >= 0 and Natural >= Minimum);
      Assert_Cmpstr_Eq
        (Get_Property (First, Gtk.Cell_Renderer_Text.Text_Property),
         "cell view row");
      Store.Set (Iter, 0, "updated row");
      Measure_View;
      Assert_Cmpstr_Eq
        (Get_Property (First, Gtk.Cell_Renderer_Text.Text_Property),
         "updated row");
      Gtk.Cell_Layout.Clear_Attributes (Layout, First);
      Store.Set (Iter, 0, "after clear");
      Measure_View;
      Assert_Cmpstr_Eq
        (Get_Property (First, Gtk.Cell_Renderer_Text.Text_Property),
         "updated row");

      --  The direct callback writes a known renderer value; null removes it.
      Cellarea_Helpers.Data_Calls := 0;
      Gtk.Cell_Layout.Set_Cell_Data_Func
        (Layout, First, Cellarea_Helpers.Set_Cell_Data'Access);
      Gtk.Window.Gtk_New (Window);
      Window.Set_Default_Size (220, 60);
      Window.Set_Child (View);
      Window.Present;
      Queue_View_Draw;
      Wait_For_Data_Callback;
      Assert_Cmpstr_Eq
        (Get_Property (First, Gtk.Cell_Renderer_Text.Text_Property),
         "plain callback");
      Assert_True (Cellarea_Helpers.Data_Calls > 0);
      Gtk.Cell_Layout.Set_Cell_Data_Func (Layout, First, null);

      --  The generic closure passes user data and invokes its destroy hook.
      Cellarea_Helpers.Data_Calls := 0;
      Cellarea_Helpers.Destroy_Calls := 0;
      Layout_Data_Callbacks.Set_Cell_Data_Func
        (Layout, First, Cellarea_Helpers.Set_Cell_Data_With_Data'Access, 37);
      Cellarea_Helpers.Data_Calls := 0;
      Queue_View_Draw;
      Wait_For_Data_Callback;
      Assert_Cmpstr_Eq
        (Get_Property (First, Gtk.Cell_Renderer_Text.Text_Property), " 37");
      Assert_True (Cellarea_Helpers.Data_Calls > 0);
      Layout_Data_Callbacks.Set_Cell_Data_Func (Layout, First, null, 0);
      Assert_Cmpuint_Eq (Guint (Cellarea_Helpers.Destroy_Calls), 1);
      --  Keep the cell-area test's closure counter independent of this case.
      Cellarea_Helpers.Destroy_Calls := 0;
      Cellarea_Helpers.Data_Calls := 0;

      --  Clear removes both renderers; clearing the model makes its getter null.
      Gtk.Cell_Layout.Clear (Layout);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length
           (Gtk.Cell_Layout.Get_Cells (Layout)), 0);
      Gtk.Cell_View.Set_Model (View, Gtk.Tree_Model.Null_Gtk_Tree_Model);
      Assert_True
        (Gtk.Cell_View.Get_Model (View) = Gtk.Tree_Model.Null_Gtk_Tree_Model);
      Window.Destroy;
   end Test_Cell_View_Methods;

   ------------------------------------
   -- Test_Cell_View_Accessibility --
   ------------------------------------

   --  An unrooted cell view has stable accessibility defaults; interface
   --  conversions also verify the five interfaces declared by the binding.
   procedure Test_Cell_View_Accessibility is
      View       : constant Gtk_Cell_View := Gtk_Cell_View_New;
      Parent     : Gtk.Accessible.Gtk_Accessible;
      Sibling    : Gtk.Accessible.Gtk_Accessible;
      Context    : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
      Accessible : constant Gtk.Accessible.Gtk_Accessible := +View;
      Buildable  : constant Gtk.Buildable.Gtk_Buildable := +View;
      Layout     : constant Gtk.Cell_Layout.Gtk_Cell_Layout := +View;
      Target     : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +View;
      Orientable : constant Gtk.Orientable.Gtk_Orientable := +View;
   begin
      Assert_True
        (Gtk.Cell_View.Get_Accessible_Role (View)
         = Gtk.Accessible.Accessible_Role_Generic);
      Assert_Cmpstr_Eq (Gtk.Cell_View.Get_Accessible_Id (View), "");
      Context := Gtk.Cell_View.Get_At_Context (View);
      Assert_True (Context = null);
      Has_Bounds := Gtk.Cell_View.Get_Bounds (View, X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Parent := Gtk.Cell_View.Get_Accessible_Parent (View);
      Assert_True (Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Sibling := Gtk.Cell_View.Get_Next_Accessible_Sibling (View);
      Assert_True (Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Gtk.Cell_View.Get_First_Accessible_Child (View)
         = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_False
        (Gtk.Cell_View.Get_Platform_State
           (View, Gtk.Accessible.Accessible_Platform_State_Focused));

      --  Detached widgets accept clearing their optional accessible relations.
      Gtk.Cell_View.Set_Accessible_Parent
        (View, Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Gtk.Cell_View.Update_Next_Accessible_Sibling
        (View, Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Gtk.Cell_View.Get_Accessible_Parent (View)
         = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Gtk.Cell_View.Get_Next_Accessible_Sibling (View)
         = Gtk.Accessible.Null_Gtk_Accessible);

      --  These accessibility operations safely reset optional state and send
      --  a low-priority announcement while no accessibility backend is mapped.
      Gtk.Cell_View.Announce
        (View, "cell view ready",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Gtk.Cell_View.Reset_Property
        (View, Gtk.Accessible.Accessible_Property_Label);
      Gtk.Cell_View.Reset_Relation
        (View, Gtk.Accessible.Accessible_Relation_Labelled_By);
      Gtk.Cell_View.Reset_State
        (View, Gtk.Accessible.Accessible_State_Busy);
      Gtk.Cell_View.Update_Platform_State
        (View, Gtk.Accessible.Accessible_Platform_State_Focused);

      --  Round trips make each generated interface conversion observable.
      Assert_True (Gtk.Cell_View."-" (Accessible) = View);
      Assert_True (Gtk.Cell_View."-" (Buildable) = View);
      Assert_True (Gtk.Cell_View."-" (Layout) = View);
      Assert_True (Gtk.Cell_View."-" (Target) = View);
      Assert_True (Gtk.Cell_View."-" (Orientable) = View);
      Gtk.Cell_View.Set_Orientation (View, Orientation_Vertical);
      Assert_True
        (Gtk.Cell_View.Get_Orientation (View) = Orientation_Vertical);
      Gtk.Cell_View.Set_Orientation (View, Orientation_Horizontal);
      Assert_True
        (Gtk.Cell_View.Get_Orientation (View) = Orientation_Horizontal);
   end Test_Cell_View_Accessibility;

   ----------------------------
   -- Test_Cell_Area_Context --
   ----------------------------

   --  Exercise the public GtkCellAreaContext size cache, allocation and reset
   --  methods directly, including their documented empty and reset values.
   procedure Test_Cell_Area_Context is
      Area      : constant Gtk_Cell_Area :=
        Gtk_Cell_Area (Gtk_Cell_Area_Box_New);
      Area_View : aliased Gtk_Cell_Area_Record :=
        Gtk_Cell_Area_Record (Area.all);
      Context   : constant Gtk_Cell_Area_Context :=
        Gtk.Cell_Area.Create_Context (Area_View'Access);
      Width, Height : Gint;
      Min_W, Nat_W  : Gint;
      Min_H, Nat_H  : Gint;
   begin
      --  The GtkCellAreaBox context initializes allocation and cross-axis
      --  sizes to zero; accumulated primary requests are zero as documented.
      Context.Get_Allocation (Width, Height);
      Assert_Cmpint_Eq (Width, 0);
      Assert_Cmpint_Eq (Height, 0);
      Context.Get_Preferred_Width (Min_W, Nat_W);
      Assert_Cmpint_Eq (Min_W, 0);
      Assert_Cmpint_Eq (Nat_W, 0);
      Context.Get_Preferred_Height (Min_H, Nat_H);
      Assert_Cmpint_Eq (Min_H, 0);
      Assert_Cmpint_Eq (Nat_H, 0);
      Context.Get_Preferred_Height_For_Width (120, Min_H, Nat_H);
      Assert_Cmpint_Eq (Min_H, 0);
      Assert_Cmpint_Eq (Nat_H, 0);
      Context.Get_Preferred_Width_For_Height (40, Min_W, Nat_W);
      Assert_Cmpint_Eq (Min_W, 0);
      Assert_Cmpint_Eq (Nat_W, 0);

      --  Pushed sizes grow each dimension independently: smaller proposals do
      --  not shrink the cache, while larger natural sizes are retained.
      Context.Push_Preferred_Width (8, 13);
      Context.Push_Preferred_Width (4, 17);
      Context.Get_Preferred_Width (Min_W, Nat_W);
      Assert_Cmpint_Eq (Min_W, 8);
      Assert_Cmpint_Eq (Nat_W, 17);
      Context.Push_Preferred_Height (6, 11);
      Context.Push_Preferred_Height (9, 10);
      Context.Get_Preferred_Height (Min_H, Nat_H);
      Assert_Cmpint_Eq (Min_H, 9);
      Assert_Cmpint_Eq (Nat_H, 11);

      --  Allocation is returned as supplied, and reset clears both the
      --  allocation and accumulated preferred dimensions. The concrete box
      --  context reports zero for its reset allocation, as on initialization.
      Context.Allocate (120, 40);
      Context.Get_Allocation (Width, Height);
      Assert_Cmpint_Eq (Width, 120);
      Assert_Cmpint_Eq (Height, 40);
      Context.Reset;
      Context.Get_Allocation (Width, Height);
      Assert_Cmpint_Eq (Width, 0);
      Assert_Cmpint_Eq (Height, 0);
      Context.Get_Preferred_Width (Min_W, Nat_W);
      Assert_Cmpint_Eq (Min_W, 0);
      Assert_Cmpint_Eq (Nat_W, 0);
      Context.Get_Preferred_Height (Min_H, Nat_H);
      Assert_Cmpint_Eq (Min_H, 0);
      Assert_Cmpint_Eq (Nat_H, 0);
   end Test_Cell_Area_Context;

   --------------------------
   -- Test_Cell_Area_Methods --
   --------------------------

   --  Exercise the concrete cell operations on a small two-renderer area.
   --  A plain label is enough as the layout widget: no window or user input
   --  is needed to request sizes and compute cell allocations.
   procedure Test_Cell_Area_Methods is
      Area       : constant Gtk_Cell_Area :=
        Gtk_Cell_Area (Gtk_Cell_Area_Box_New);
      --  The parent view keeps primitive calls statically in Gtk.Cell_Area;
      --  a call through the classwide Box access would dispatch to Box's copy.
      Area_View  : aliased Gtk_Cell_Area_Record :=
        Gtk_Cell_Area_Record (Area.all);
      Text       : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Toggle     : constant Gtk_Cell_Renderer_Toggle := Gtk_Cell_Renderer_Toggle_New;
      Other      : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Widget     : constant Gtk_Label := Gtk_Label_New ("cell area target");
      Context    : Gtk_Cell_Area_Context;
      Store      : Gtk_List_Store;
      Iter       : Gtk_Tree_Iter;
      Cell_Area  : aliased Gdk_Rectangle := (0, 0, 240, 40);
      Background : constant Gdk_Rectangle := (0, 0, 240, 40);
      Inner      : Gdk_Rectangle;
      Allocation : aliased Gdk_Rectangle;
      At_X       : Gtk_Cell_Renderer;
      Found_Area : Gdk_Rectangle;
      Min_W      : Gint;
      Nat_W      : Gint;
      Min_H      : Gint;
      Nat_H      : Gint;
      Activated  : Boolean;
      Value      : GValue;
      Readback   : GValue;
      Snapshot   : constant Gtk_Snapshot := Gtk_Snapshot_New;
   begin
      --  Pack through both CellLayout entry points and verify membership.
      Gtk.Cell_Area.Pack_Start (Area_View'Access, Text, True);
      Gtk.Cell_Area.Pack_End (Area_View'Access, Toggle, False);
      Assert_True (Gtk.Cell_Area.Has_Renderer (Area_View'Access, Text));
      Assert_True (Gtk.Cell_Area.Has_Renderer (Area_View'Access, Toggle));
      Assert_False (Gtk.Cell_Area.Has_Renderer (Area_View'Access, Other));
      Gtk.Cell_Area.Reorder (Area_View'Access, Toggle, 0);
      Gtk.Cell_Area.Add (Area_View'Access, Other);
      Assert_True (Gtk.Cell_Area.Has_Renderer (Area_View'Access, Other));
      Gtk.Cell_Area.Remove (Area_View'Access, Other);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length
           (Gtk.Cell_Area.Get_Cells (Area_View'Access)), 2);

      --  Add and remove a focus sibling and verify both relationship queries.
      Gtk.Cell_Area.Add_Focus_Sibling (Area_View'Access, Text, Toggle);
      Assert_True
        (Gtk.Cell_Area.Is_Focus_Sibling (Area_View'Access, Text, Toggle));
      Assert_True
        (Gtk.Cell_Area.Get_Focus_From_Sibling (Area_View'Access, Toggle)
         = Gtk_Cell_Renderer (Text));
      Assert_True (Glib.Object.Object_Simple_List.Length
        (Gtk.Cell_Area.Get_Focus_Siblings (Area_View'Access, Text)) = 1);
      Gtk.Cell_Area.Remove_Focus_Sibling (Area_View'Access, Text, Toggle);
      Assert_False
        (Gtk.Cell_Area.Is_Focus_Sibling (Area_View'Access, Text, Toggle));

      --  Cell properties belong to the area. Round-trip the "expand" child
      --  property to check both the GValue input and output paths.
      Init_Set_Boolean (Value, True);
      Gtk.Cell_Area.Cell_Set_Property
        (Area_View'Access, Text, "expand", Value);
      Init (Readback, GType_Boolean);
      Gtk.Cell_Area.Cell_Get_Property
        (Area_View'Access, Text, "expand", Readback);
      Assert_True (Get_Boolean (Readback));
      Unset (Value);
      Unset (Readback);

      --  Attribute mappings are reflected by the model column query, applied
      --  to a real row, and then cleared using both clearing operations.
      Gtk_New (Store, (0 => GType_String, 1 => GType_Boolean));
      Store.Append (Iter);
      Store.Set (Iter, 0, "model text");
      Store.Set (Iter, 1, True);

      --  Register typed and GObject-slot callbacks for all signals. The
      --  standalone area emits apply-attributes and focus-changed below; edit
      --  signals need an owning interactive layout to be emitted safely.
      Gtk.Cell_Area.On_Apply_Attributes
        (Area_View'Access, Cellarea_Helpers.On_Apply'Access);
      Gtk.Cell_Area.On_Apply_Attributes
        (Area_View'Access, Cellarea_Helpers.On_Apply_Slot'Access,
         Area_View'Access);
      Gtk.Cell_Area.On_Focus_Changed
        (Area_View'Access, Cellarea_Helpers.On_Focus'Access);
      Gtk.Cell_Area.On_Focus_Changed
        (Area_View'Access, Cellarea_Helpers.On_Focus_Slot'Access,
         Area_View'Access);
      Gtk.Cell_Area.On_Add_Editable
        (Area_View'Access, Cellarea_Helpers.On_Add_Edit'Access);
      Gtk.Cell_Area.On_Add_Editable
        (Area_View'Access, Cellarea_Helpers.On_Add_Edit_Slot'Access,
         Area_View'Access);
      Gtk.Cell_Area.On_Remove_Editable
        (Area_View'Access, Cellarea_Helpers.On_Remove_Edit'Access);
      Gtk.Cell_Area.On_Remove_Editable
        (Area_View'Access, Cellarea_Helpers.On_Remove_Edit_Slot'Access,
         Area_View'Access);

      Gtk.Cell_Area.Attribute_Connect
        (Area_View'Access, Text, "text", 0);
      Gtk.Cell_Area.Add_Attribute (Area_View'Access, Toggle, "active", 1);
      Assert_Cmpint_Eq
        (Gtk.Cell_Area.Attribute_Get_Column
           (Area_View'Access, Text, "text"), 0);
      Gtk.Cell_Area.Apply_Attributes
        (Area_View'Access, +Store, Iter, False, False);
      Assert_True (Cellarea_Helpers.Apply_Calls > 0);
      Assert_True (Cellarea_Helpers.Apply_Slot_Calls > 0);
      Assert_Cmpstr_Eq
        (Gtk.Cell_Area.Get_Current_Path_String (Area_View'Access), "0");
      Assert_Cmpstr_Eq (Get_Property (Text, Text_Property), "model text");
      Assert_True (Toggle.Get_Active);
      Gtk.Cell_Area.Attribute_Disconnect
        (Area_View'Access, Text, "text");
      Assert_Cmpint_Eq
        (Gtk.Cell_Area.Attribute_Get_Column
           (Area_View'Access, Text, "text"), -1);
      Gtk.Cell_Area.Clear_Attributes (Area_View'Access, Toggle);

      --  Explicit cell-data callbacks replace attribute mapping. The generic
      --  closure variant verifies user data and runs its destroy callback.
      Gtk.Cell_Area.Set_Cell_Data_Func
        (Area_View'Access, Text, Cellarea_Helpers.Set_Cell_Data'Access);
      Gtk.Cell_Area.Apply_Attributes
        (Area_View'Access, +Store, Iter, False, False);
      Assert_Cmpstr_Eq (Get_Property (Text, Text_Property), "plain callback");
      Gtk.Cell_Area.Set_Cell_Data_Func (Area_View'Access, Text, null);
      Data_Callbacks.Set_Cell_Data_Func
        (Area_View'Access, Text,
         Cellarea_Helpers.Set_Cell_Data_With_Data'Access, 37);
      Gtk.Cell_Area.Apply_Attributes
        (Area_View'Access, +Store, Iter, False, False);
      Assert_Cmpstr_Eq (Get_Property (Text, Text_Property), " 37");
      Assert_Cmpuint_Eq (Guint (Cellarea_Helpers.Data_Calls), 2);
      Data_Callbacks.Set_Cell_Data_Func
        (Area_View'Access, Text, null, 0);
      Assert_Cmpuint_Eq (Guint (Cellarea_Helpers.Destroy_Calls), 1);

      --  Create and copy contexts and check they retain their originating
      --  area. The context is also used for all subsequent geometry calls.
      Context := Gtk.Cell_Area.Create_Context (Area_View'Access);
      Assert_True (Context_Area (Context) = Area);
      declare
         Copy : constant Gtk_Cell_Area_Context :=
           Gtk.Cell_Area.Copy_Context (Area_View'Access, Context);
      begin
         Assert_True (Context_Area (Copy) = Area);
      end;

      --  Size requests must be nonnegative and natural size cannot be smaller
      --  than the minimum; the renderer-specific request obeys the same rule.
      Gtk.Cell_Area.Get_Preferred_Width
        (Area_View'Access, Context, Widget, Min_W, Nat_W);
      Assert_True (Min_W >= 0 and Nat_W >= Min_W);
      Gtk.Cell_Area.Get_Preferred_Height
        (Area_View'Access, Context, Widget, Min_H, Nat_H);
      Assert_True (Min_H >= 0 and Nat_H >= Min_H);
      Gtk.Cell_Area.Get_Preferred_Height_For_Width
        (Area_View'Access, Context, Widget, 120, Min_H, Nat_H);
      Assert_True (Min_H >= 0 and Nat_H >= Min_H);
      Gtk.Cell_Area.Get_Preferred_Width_For_Height
        (Area_View'Access, Context, Widget, 24, Min_W, Nat_W);
      Assert_True (Min_W >= 0 and Nat_W >= Min_W);
      Gtk.Cell_Area.Request_Renderer
        (Area_View'Access, Text, Orientation_Horizontal, Widget, -1,
         Min_W, Nat_W);
      Assert_True (Min_W >= 0 and Nat_W >= Min_W);
      Assert_True (Gtk.Cell_Area.Get_Request_Mode (Area_View'Access) in Constant_Size |
        Height_For_Width | Width_For_Height);
      Assert_True (Gtk.Cell_Area.Is_Activatable (Area_View'Access));

      --  Focus queries and focus assignment cover both null and renderer
      --  focus states. These calls use Gtk's own focus policy.
      Gtk.Cell_Area.Set_Focus_Cell (Area_View'Access, Text);
      Assert_True
        (Gtk.Cell_Area.Get_Focus_Cell (Area_View'Access)
         = Gtk_Cell_Renderer (Text));
      Assert_True (Cellarea_Helpers.Focus_Calls > 0);
      Assert_True (Cellarea_Helpers.Focus_Slot_Calls > 0);
      Assert_True (Gtk.Cell_Area.Focus (Area_View'Access, Dir_Right)
        or else Gtk.Cell_Area.Get_Focus_Cell (Area_View'Access) /= null);
      Assert_True
        (Gtk.Cell_Area.Get_Edit_Widget (Area_View'Access)
         = Gtk.Cell_Editable.Null_Gtk_Cell_Editable);
      Assert_True (Gtk.Cell_Area.Get_Edited_Cell (Area_View'Access) = null);

      --  Both direct callback forms traverse the renderers, and the userdata
      --  forms exercise their closure allocation and conversion callbacks.
      Cellarea_Helpers.Cell_Calls := 0;
      Gtk.Cell_Area.Foreach
        (Area_View'Access, Cellarea_Helpers.Each_Cell'Access);
      Assert_Cmpuint_Eq (Guint (Cellarea_Helpers.Cell_Calls), 2);
      Cell_Callbacks.Foreach
        (Area_View'Access, Cellarea_Helpers.Each_Cell_With_Data'Access, 3);
      Assert_Cmpuint_Eq (Guint (Cellarea_Helpers.Cell_Calls), 8);
      Cellarea_Helpers.Alloc_Calls := 0;
      Gtk.Cell_Area.Foreach_Alloc
        (Area_View'Access, Context, Widget, Cell_Area, Background,
         Cellarea_Helpers.Each_Cell_Alloc'Access);
      Assert_Cmpuint_Eq (Guint (Cellarea_Helpers.Alloc_Calls), 2);
      Alloc_Callbacks.Foreach_Alloc
        (Area_View'Access, Context, Widget, Cell_Area, Background,
         Cellarea_Helpers.Each_Cell_Alloc_With_Data'Access, 4);
      Assert_Cmpuint_Eq (Guint (Cellarea_Helpers.Alloc_Calls), 10);

      --  Allocation queries and snapshots use a valid context, widget and
      --  rectangle; looking up a point within the area must return a renderer.
      Gtk.Cell_Area.Inner_Cell_Area
        (Area_View'Access, Widget, Cell_Area, Inner);
      Gtk.Cell_Area.Get_Cell_Allocation
        (Area_View'Access, Context, Widget, Text,
         Cell_Area'Access, Allocation'Access);
      Gtk.Cell_Area.Get_Cell_At_Position
        (Area_View'Access, Context, Widget, Cell_Area, 4, 4,
         Found_Area, At_X);
      Assert_True
        (At_X = Gtk_Cell_Renderer (Text)
         or At_X = Gtk_Cell_Renderer (Toggle));
      Gtk.Cell_Area.Snapshot
        (Area_View'Access, Context, Widget, Snapshot, Background, Cell_Area,
         Cell_Renderer_Focused, True);
      --  No renderer is editable, so Edit_Only activation must report False.
      Activated := Gtk.Cell_Area.Activate
        (Area_View'Access, Context, Widget, Cell_Area,
         Cell_Renderer_Focused, True);
      Assert_False (Activated);
      Gtk.Cell_Area.Stop_Editing (Area_View'Access, True);
      Assert_True (Gtk.Cell_Area.Get_Edited_Cell (Area_View'Access) = null);

      --  Layout clearing removes renderers and their implicit cell metadata.
      Gtk.Cell_Area.Clear (Area_View'Access);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length
           (Gtk.Cell_Area.Get_Cells (Area_View'Access)), 0);
   end Test_Cell_Area_Methods;

   ---------------------------------
   -- Test_Cell_Area_Box_Methods --
   ---------------------------------

   --  Exercise the GtkCellAreaBox-specific wrappers and the inherited
   --  GtkCellLayout methods through the concrete binding. A single in-memory
   --  row drives both attribute mapping and the deprecated data callbacks.
   procedure Test_Cell_Area_Box_Methods is
      Box       : Gtk_Cell_Area_Box;
      Area_View : aliased Gtk_Cell_Area_Record;
      Store     : Gtk_List_Store;
      Iter      : Gtk_Tree_Iter;
      Start     : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Finish    : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Layout_Start : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Layout_End   : constant Gtk_Cell_Renderer_Text := Gtk_Cell_Renderer_Text_New;
      Cells     : Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
      Plain_Callback : constant Gtk.Cell_Area_Box.Gtk_Cell_Layout_Data_Func :=
        Cellarea_Helpers.Set_Cell_Data'Access;
      No_Box_Callback : constant Gtk.Cell_Area_Box.Gtk_Cell_Layout_Data_Func :=
        null;
   begin
      --  Cover both constructor forms, and Initialize's no-op guard on a live
      --  object created by Gtk_New.
      Gtk_New (Box);
      Assert_True (Box.Is_Created);
      Gtk.Cell_Area_Box.Initialize (Box);
      Assert_True (Box.Is_Created);

      --  The deprecated spacing accessors still round-trip their property.
      Gtk.Cell_Area_Box.Set_Spacing (Box, 7);
      Assert_Cmpint_Eq (Gtk.Cell_Area_Box.Get_Spacing (Box), 7);

      --  Both orientations are observable through the box's Orientable API.
      Gtk.Cell_Area_Box.Set_Orientation (Box, Orientation_Vertical);
      Assert_True
        (Gtk.Cell_Area_Box.Get_Orientation (Box) = Orientation_Vertical);
      Gtk.Cell_Area_Box.Set_Orientation (Box, Orientation_Horizontal);
      Assert_True
        (Gtk.Cell_Area_Box.Get_Orientation (Box) = Orientation_Horizontal);

      --  Box packing accepts its expand/align/fixed flags; the inherited
      --  CellLayout overloads accept the simpler expand flag.
      Gtk.Cell_Area_Box.Pack_Start (Box, Start, True, True, False);
      Gtk.Cell_Area_Box.Pack_End (Box, Finish, False, False, True);
      Gtk.Cell_Area_Box.Pack_Start (Box, Layout_Start, True);
      Gtk.Cell_Area_Box.Pack_End (Box, Layout_End, False);
      Cells := Gtk.Cell_Area_Box.Get_Cells (Box);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length (Cells), 4);
      Gtk.Cell_Area_Box.Reorder (Box, Finish, 0);
      Cells := Gtk.Cell_Area_Box.Get_Cells (Box);
      Assert_True
        (Gtk.Cell_Renderer.Cell_Renderer_List.Nth_Data (Cells, 0)
         = Gtk.Cell_Renderer.Gtk_Cell_Renderer (Finish));

      --  Attribute mapping reads the model row and can be cleared for one
      --  renderer without disturbing the rest of the layout.
      Gtk_New (Store, (0 => GType_String));
      Store.Append (Iter);
      Store.Set (Iter, 0, "box model row");
      Gtk.Cell_Area_Box.Add_Attribute (Box, Start, "text", 0);
      Area_View := Gtk_Cell_Area_Record (Box.all);
      Gtk.Cell_Area.Apply_Attributes
        (Area_View'Access, +Store, Iter, False, False);
      Assert_Cmpstr_Eq (Get_Property (Start, Text_Property), "box model row");
      Gtk.Cell_Area_Box.Clear_Attributes (Box, Start);

      --  The non-generic callback path invokes the box trampoline with the
      --  actual renderer, model and iter; passing null removes that callback.
      Cellarea_Helpers.Data_Calls := 0;
      Gtk.Cell_Area_Box.Set_Cell_Data_Func
        (Box, Start, Plain_Callback);
      Gtk.Cell_Area.Apply_Attributes
        (Area_View'Access, +Store, Iter, False, False);
      Assert_Cmpstr_Eq (Get_Property (Start, Text_Property), "plain callback");
      Assert_Cmpuint_Eq (Guint (Cellarea_Helpers.Data_Calls), 1);
      Gtk.Cell_Area_Box.Set_Cell_Data_Func (Box, Start, No_Box_Callback);

      --  The generic closure carries its integer and runs its destroy hook
      --  when the null callback replaces it.
      Cellarea_Helpers.Data_Calls := 0;
      Cellarea_Helpers.Destroy_Calls := 0;
      Box_Data_Callbacks.Set_Cell_Data_Func
        (Box, Start, Cellarea_Helpers.Set_Cell_Data_With_Data'Access, 42);
      Gtk.Cell_Area.Apply_Attributes
        (Area_View'Access, +Store, Iter, False, False);
      Assert_Cmpstr_Eq (Get_Property (Start, Text_Property), " 42");
      Assert_Cmpuint_Eq (Guint (Cellarea_Helpers.Data_Calls), 1);
      Box_Data_Callbacks.Set_Cell_Data_Func (Box, Start, null, 0);
      Assert_Cmpuint_Eq (Guint (Cellarea_Helpers.Destroy_Calls), 1);

      --  Clear removes all four packed renderers through the box wrapper.
      Gtk.Cell_Area_Box.Clear (Box);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length
           (Gtk.Cell_Area_Box.Get_Cells (Box)), 0);
   end Test_Cell_Area_Box_Methods;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/cellarea/cell-view-context", Test_Cell_View_Context'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cellarea/cell-view-constructors",
      Test_Cell_View_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cellarea/cell-view-methods",
      Test_Cell_View_Methods'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cellarea/cell-view-accessibility",
      Test_Cell_View_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cellarea/context",
      Test_Cell_Area_Context'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cellarea/methods", Test_Cell_Area_Methods'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/cellarea/box-methods",
      Test_Cell_Area_Box_Methods'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
