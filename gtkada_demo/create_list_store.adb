
with Ada.Text_IO;
with System;

with Glib;
with Glib.List_Store;   use Glib.List_Store;
with Glib.List_Model;   use Glib.List_Model;
with Glib.Object;       use Glib.Object;

with Gtk;               use Gtk;
with Gtk.Adjustment;    use Gtk.Adjustment;
with Gtk.Box;           use Gtk.Box;
with Gtk.Button;        use Gtk.Button;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.Grid;          use Gtk.Grid;
with Gtk.Label;         use Gtk.Label;
with Gtk.Spin_Button;   use Gtk.Spin_Button;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget;        use Gtk.Widget;

package body Create_List_Store is

   --  Global objects for signal handlers

   Main_Store_UI      : aliased Store_UI;
   Main_Store_Ptr     : Store_Ptr := Main_Store_UI'Access;
   The_Store          : Glist_Store renames Main_Store_Ptr.Store;
   The_UI             : Gtk_Grid renames Main_Store_Ptr.Grid;
   Spin_Add, Spin_Del : Gtk.Spin_Button.Gtk_Spin_Button;
   Adj_Add, Adj_Del   : Gtk.Adjustment.Gtk_Adjustment;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGlist_Store@B is a simple data type for holding @bGObject@Bs. "
        & "It replaces the old @bGtk_List_Store@B type and implements the "
        & " @bGlist_Model@B interface, providing array functionality such as "
        & "appending, inserting, removing and sorting items. "
        & ASCII.LF
        & "A list store must be initialised with a @bGObject@B-derived type "
        & "and will only store items of this type, including custom types."
        & ASCII.LF & ASCII.LF
        & "List stores are data types rather than widgets, so they have no "
        & "interactive or visual elements unless they are connected to "
        & " interactive widgets through signal handlers."
        & ASCII.LF
        & "They can be used as underlying databases for view widgets "
        & "such as @bGtk_Tree_View@B, @bGtk_List_View@B, @bGtk_Column_View@B, and so on."
        & ASCII.LF
        & "This demo showcases common @bGlist_Store@B operations using only "
        & " simple UI elements such as buttons and labels."
        & ASCII.LF;
   end Help;

   -----------------------
   --  Store_UI methods --
   -----------------------

   procedure New_Store_UI (S : out Store_UI; Item_T : Glib.GType);

   function Is_Init (S : Store_Ptr) return Boolean
   is (S.Grid /= null and S.Store /= null);

   procedure On_List_Changed
     (Self     : Glist_Model;
      Position : Glib.Guint;
      Removed  : Glib.Guint;
      Added    : Glib.Guint);

   procedure Add_Item
     (Self : Store_Ptr; Idx : Natural; Insert : Boolean := False);

   --  Comparison and equality functions --

   function Cmp_Idx_Ascending
      (A, B : not null access GObject_Record'Class) return Glib.Gint;
   pragma Convention (C, Cmp_Idx_Ascending);
   --  Positive if A > B, negative if A < B, 0 if equal
   function Cmp_Idx_Descending
      (A, B : not null access GObject_Record'Class) return Glib.Gint;
   pragma Convention (C, Cmp_Idx_Descending);
   --  Negative if A > B, positive if A < B, 0 if equal
   function Eq_Idx
      (A, B : not null access GObject_Record'Class) return Glib.Gboolean;
   --  pragma Convention (C, Eq_Idx);

   --  Wrappers for List_Store methods
   procedure Store_Append (Self : access GObject_Record'Class);
   procedure Store_Insert (Self : access GObject_Record'Class);
   procedure Store_Remove (Self : access GObject_Record'Class);
   procedure Store_Smash (Self : access GObject_Record'Class);
   procedure Store_Sort_Ascending (Self : access GObject_Record'Class);
   procedure Store_Sort_Descending (Self : access GObject_Record'Class);

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Self : Store_Ptr; Idx : Natural; Insert : Boolean := False)
   is
      Len                 : constant Natural :=
        Natural (The_Store.Get_N_Items);
      R                   : constant Glib.Gint := Glib.Gint (Idx + 1);
      Store_Item, UI_Item : Gtk_Toggle_Button;
      Index               : Gtk_Label := Gtk_Label_New (Idx'Img);
   begin
      --  New buttons labelled with store size (basically their insertion order)
      --  Add to UI display
      Toggle_Button.Gtk_New_With_Label (UI_Item, Len'Img);
      Self.Grid.Insert_Row (R);
      Self.Grid.Attach (Child => Index, Column => 0, Row => R);
      Self.Grid.Attach (Child => UI_Item, Column => 1, Row => R);
      --  Add separate copy to internal list store
      --  (this way we can be lazy when sorting)
      Toggle_Button.Gtk_New_With_Label (Store_Item, Len'Img);
      if Insert then
         Self.Store.Insert (Glib.Guint (Idx), Store_Item);
         UI_Item.Set_Active (True);
      else
         Self.Store.Append (Store_Item);
      end if;
   end Add_Item;

   ------------------
   -- New_Store_UI --
   ------------------

   procedure New_Store_UI (S : out Store_UI; Item_T : Glib.GType) is
      type Row_Title is (Index, Item);
      L : Gtk.Label.Gtk_Label;
   begin
      Glib.List_Store.G_New (S.Store, Item_T);
      Glib.List_Model.On_Items_Changed (+S.Store, On_List_Changed'Access);

      --  Grid setup
      Gtk.Grid.Gtk_New (S.Grid);
      S.Grid.Set_Orientation (Orientation_Vertical);
      S.Grid.Insert_Row (0);
      for R in Row_Title'Range loop
         Label.Gtk_New (L);
         L.Set_Markup ("<span weight=""bold"">" & R'Img & "</span>");
         S.Grid.Insert_Column (Row_Title'Enum_Rep (R));
         S.Grid.Attach
           (Child => L, Column => Row_Title'Enum_Rep (R), Row => 0);
      end loop;
      S.Grid.Set_Column_Spacing (10);
      S.Grid.Set_Row_Spacing (2);
      S.Grid.Set_Row_Homogeneous (True);
      S.Grid.Set_Baseline_Row (0);
      S.Grid.Set_Row_Baseline_Position (0, Baseline_Position_Top);
   end New_Store_UI;

   ----------------
   -- Store_Append --
   ----------------

   procedure Store_Append (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
      N : constant Natural := Natural (The_Store.Get_N_Items);
   begin
      Add_Item (Main_Store_Ptr, N);
   end Store_Append;

   ------------------
   -- Store_Insert --
   ------------------

   procedure Store_Insert (Self : access GObject_Record'Class) is
      Spinner : constant Gtk_Spin_Button := Gtk_Spin_Button (Self);
      Len     : constant Natural := Natural (The_Store.Get_N_Items);
      Val     : constant Natural := Natural (Spinner.Get_Value);
      Row_Idx : constant Glib.Gint := Glib.Gint (Val + 1);
      Idx     : constant Glib.Guint := Glib.Guint (Val);
      use type Glib.Guint;
   begin
      Add_Item (Main_Store_Ptr, Val, Insert => True);

      --  Now update remaining rows
      for I in Val + 1 .. Len loop
         declare
            Row_Label : Gtk_Label :=
              Gtk_Label (The_UI.Get_Child_At (0, Glib.Gint (I + 1)));
            New_Idx   : constant Positive := I;
         begin
            if Row_Label /= null then
               Row_Label.Set_Text (New_Idx'Img);
            end if;
         end;
      end loop;
   end Store_Insert;

   ------------------
   -- Store_Remove --
   ------------------

   procedure Store_Remove (Self : access GObject_Record'Class) is
      Spinner : constant Gtk_Spin_Button := Gtk_Spin_Button (Self);
      Val     : constant Natural := Natural (Spinner.Get_Value);
      Idx     : constant Glib.Guint := Glib.Guint (Val);
      Row_Idx : constant Glib.Gint := Glib.Gint (Natural (Idx) + 1);
      Len     : constant Glib.Guint := The_Store.Get_N_Items;
   begin
      The_Store.Remove (Idx);
      The_UI.Remove_Row (Row_Idx);
      --  Now update index labels for all rows below
      for I in Val .. Natural (Len) - 1 loop
         declare
            Row_Label : Gtk_Label :=
              Gtk_Label (The_UI.Get_Child_At (0, Glib.Gint (I)));
            New_Idx   : constant Integer := Integer (I) - 1;
         begin
            if Row_Label /= null and New_Idx >= 0 then
               Row_Label.Set_Text (New_Idx'Img);
            end if;
         end;
      end loop;
   end Store_Remove;

   -----------------
   -- Store_Smash --
   -----------------

   procedure Store_Smash (Self : access GObject_Record'Class) is
      pragma Unreferenced (Self);
      Len : constant Natural := Natural (The_Store.Get_N_Items);
   begin
      The_Store.Remove_All;
      for I in reverse 1 .. Len loop
         The_UI.Remove_Row (Glib.Gint (I));
      end loop;
   end Store_Smash;

   -----------------------
   -- Cmp_Idx_Ascending --
   -----------------------

   function Cmp_Idx_Ascending
     (A, B : not null access GObject_Record'Class) return Glib.Gint
   is
      Label_A : constant String := Gtk_Toggle_Button (A).Get_Label;
      Label_B : constant String := Gtk_Toggle_Button (B).Get_Label;
      Idx_A   : constant Natural := Natural'Value (Label_A);
      Idx_B   : constant Natural := Natural'Value (Label_B);
      Cmp_Val : constant Integer :=
        (if Idx_A > Idx_B then 1 elsif Idx_A = Idx_B then 0 else -1);
   begin
      return Glib.Gint (Cmp_Val);
   exception
      when Constraint_Error =>
         return Glib.Gint (0);
   end Cmp_Idx_Ascending;

   ------------------------
   -- Cmp_Idx_Descending --
   ------------------------

   function Cmp_Idx_Descending
     (A, B : not null access GObject_Record'Class) return Glib.Gint
   is
      use type Glib.Gint;
   begin
      return Glib.Gint (-1 * Cmp_Idx_Ascending (A, B));
   exception
      when Constraint_Error =>
         return Glib.Gint (0);
   end Cmp_Idx_Descending;

   ------------
   -- Eq_Idx --
   ------------

   function Eq_Idx
      (A, B : not null access GObject_Record'Class) return Glib.Gboolean
   is
      use type Glib.Gint;
   begin
      return Glib.Gboolean (Cmp_Idx_Ascending (A, B));
   exception
      when E : others => return Glib.Gboolean (0);
   end Eq_Idx;

   ------------------
   -- Sort_Display --
   ------------------

   procedure Sort_Display (S : Store_Ptr) is
      use type Glib.Guint;
      use type Glib.Gint;
      Len : constant Glib.Guint := The_Store.Get_N_Items;
   begin
      for Idx in 0 .. Len - 1 loop
         declare
            Store_Idx  : constant Glib.Guint := Glib.Guint (Idx);
            Row_Idx    : constant Glib.Gint := Glib.Gint (Idx + 1);
            Store_Item : Gtk_Toggle_Button :=
              Gtk_Toggle_Button (S.Store.Get_Item (Store_Idx));
            UI_Item    : Gtk_Toggle_Button :=
              Gtk_Toggle_Button (S.Grid.Get_Child_At (1, Row_Idx));
            Sorted_Idx : constant String := Store_Item.Get_Label;
            To_Sort    : constant String := UI_Item.Get_Label;
         begin
            --  A little cheat
            if To_Sort /= Sorted_Idx then
               UI_Item.Set_Label (Sorted_Idx);
            end if;
         end;
      end loop;

   end Sort_Display;

   --------------------------
   -- Store_Sort_Ascending --
   --------------------------

   procedure Store_Sort_Ascending (Self : access GObject_Record'Class) is
   begin
      The_Store.Sort (Cmp_Idx_Ascending'Access);
      Sort_Display (Main_Store_Ptr);
   end Store_Sort_Ascending;

   ---------------------------
   -- Store_Sort_Descending --
   ---------------------------

   procedure Store_Sort_Descending (Self : access GObject_Record'Class) is
   begin
      The_Store.Sort (Cmp_Idx_Descending'Access);
      Sort_Display (Main_Store_Ptr);
   end Store_Sort_Descending;

   ---------------------
   -- On_List_Changed --
   ---------------------

   procedure On_List_Changed
     (Self     : Glist_Model;
      Position : Glib.Guint;
      Removed  : Glib.Guint;
      Added    : Glib.Guint)
   is
      use type Glib.Guint;
      Len     : constant Glib.Guint := The_Store.Get_N_Items;
      Max_Row : constant Glib.Gdouble :=
        Glib.Gdouble (if Len = 0 then 0 else Len - 1);
   begin
      --  Update spinner range whenever list length changes
      if Removed /= Added then
         Adj_Add.Set_Upper (Max_Row);
         Adj_Del.Set_Upper (Max_Row);
         Spin_Add.Set_Value (Max_Row);
         Spin_Del.Set_Value (Max_Row);
      end if;
   end On_List_Changed;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Frame_Box, Store_Box, UI_Box, Spin_Box  : Gtk_Box;
      Add, Del, Empty, Insert, Sort_A, Sort_D : Gtk_Button;
      UI_Frame, Demo_Frame                    : Gtk.Frame.Gtk_Frame;
   begin
      --  Three subframes will look like this:
      --  | Explanation | Store API buttons | Store UI |
      Frame.Set_Label ("List Store");
      Frame.Set_Label_Align (0.5);
      --  outer frame box
      Gtk_New (Frame_Box, Orientation_Horizontal, Spacing => 5);
      Frame_Box.Set_Homogeneous (True);
      Frame_Box.Set_Margin_Top (10);
      Frame_Box.Set_Margin_Bottom (10);
      Frame_Box.Set_Margin_Start (10);
      Frame_Box.Set_Margin_End (10);
      Frame.Set_Child (Frame_Box);

      --  Middle frame shows GListStore API
      Gtk.Frame.Gtk_New (UI_Frame, "Store Menu");
      Frame_Box.Append (UI_Frame);

      --  Right-hand frame shows store UI
      Gtk.Frame.Gtk_New (Demo_Frame, "Store");
      --  Add ListStore box
      Gtk_New (Store_Box, Orientation_Vertical, Spacing => 5);
      Store_Box.Set_Homogeneous (False);
      Store_Box.Set_Hexpand (False);
      Demo_Frame.Set_Child (Store_Box);

      --  Add interaction box
      Gtk_New (UI_Box, Orientation_Vertical, Spacing => 10);
      UI_Box.Set_Homogeneous (False);
      UI_Box.Set_Margin_Top (10);
      UI_Box.Set_Margin_Bottom (10);
      UI_Box.Set_Margin_Start (10);
      UI_Box.Set_Margin_End (5);
      UI_Box.Set_Hexpand (False);
      UI_Frame.Set_Child (UI_Box);

      --  Initialise store
      New_Store_UI (Main_Store_UI, Gtk.Toggle_Button.Get_Type);
      --  Add store to frame
      Store_Box.Append (The_UI);
      The_UI.Set_Halign (Align_Center);
      The_UI.Set_Valign (Align_Center);
      Store_Box.Set_Margin_Start (10);
      Store_Box.Set_Margin_End (10);
      Store_Box.Set_Margin_Top (10);
      Store_Box.Set_Margin_Bottom (10);
      Frame_Box.Append (Demo_Frame);
      Demo_Frame.Set_Halign (Align_Fill);

      --  Add buttons to show GListStore functionality
      Gtk.Button.Gtk_New (Add, "Append");
      Add.On_Clicked (Store_Append'Access, Slot => Demo_Frame);
      UI_Box.Append (Add);
      Add.Set_Valign (Align_Start);
      Add.Set_Halign (Align_Start);

      --  Insert takes an index
      Gtk.Box.Gtk_New (Spin_Box, Orientation_Horizontal, Spacing => 4);
      Spin_Box.Set_Homogeneous (True);
      Gtk.Button.Gtk_New (Add, "Insert");
      Spin_Box.Append (Add);
      Add.Set_Valign (Align_Start);
      Add.Set_Halign (Align_Start);
      --  which the user selects with a spin button
      Gtk.Adjustment.Gtk_New (Adj_Add, 0.0, 0.0, 0.0, 1.0, 5.0);
      Gtk.Spin_Button.Gtk_New (Spin_Add, Adj_Add, 1.0);
      Spin_Box.Append (Spin_Add);
      Spin_Add.Set_Halign (Align_Start);
      Add.On_Clicked (Store_Insert'Access, Slot => Spin_Add);
      UI_Box.Append (Spin_Box);

      --  Remove takes an index
      Gtk.Box.Gtk_New (Spin_Box, Orientation_Horizontal, Spacing => 4);
      Spin_Box.Set_Homogeneous (True);
      Gtk.Button.Gtk_New (Del, "Remove");
      Spin_Box.Append (Del);
      Del.Set_Valign (Align_Start);
      Del.Set_Halign (Align_Start);
      --  which the user selects with a spin button
      Gtk.Adjustment.Gtk_New (Adj_Del, 0.0, 0.0, 0.0, 1.0, 5.0);
      Gtk.Spin_Button.Gtk_New (Spin_Del, Adj_Del, 1.0);
      Spin_Box.Append (Spin_Del);
      Spin_Del.Set_Halign (Align_Start);
      Del.On_Clicked (Store_Remove'Access, Slot => Spin_Del);
      UI_Box.Append (Spin_Box);

      Gtk.Button.Gtk_New (Empty, "Remove_All");
      Empty.On_Clicked (Store_Smash'Access, Slot => Demo_Frame);
      UI_Box.Append (Empty);
      Empty.Set_Valign (Align_Start);
      Empty.Set_Halign (Align_Start);

      Gtk.Button.Gtk_New (Sort_A, "Sort (ascending)");
      Sort_A.On_Clicked (Store_Sort_Ascending'Access, Slot => Demo_Frame);
      UI_Box.Append (Sort_A);
      Sort_A.Set_Valign (Align_Start);
      Sort_A.Set_Halign (Align_Start);

      Gtk.Button.Gtk_New (Sort_D, "Sort (descending)");
      Sort_D.On_Clicked (Store_Sort_Descending'Access, Slot => Demo_Frame);
      UI_Box.Append (Sort_D);
      Sort_D.Set_Valign (Align_Start);
      Sort_D.Set_Halign (Align_Start);
   end Run;

end Create_List_Store;
