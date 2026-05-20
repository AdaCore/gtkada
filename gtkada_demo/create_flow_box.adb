------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Glib;                use Glib;
with Glib.Object;         use Glib.Object;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Combo_Box;       use Gtk.Combo_Box;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Expander;        use Gtk.Expander;
with Gtk.Flow_Box;        use Gtk.Flow_Box;
with Gtk.Flow_Box_Child;  use Gtk.Flow_Box_Child;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Label;           use Gtk.Label;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtk.Toggle_Button;   use Gtk.Toggle_Button;
with Gtk.Widget;          use Gtk.Widget;
with GNAT.Strings;        use GNAT.Strings;
with Pango.Enums;         use Pango.Enums;

package body Create_Flow_Box is

   Strings : constant GNAT.Strings.String_List :=
      (new String'("These are"),
       new String'("some wrappy label"),
       new String'("texts"),
       new String'("of various"),
       new String'("lengths."),
       new String'("They should always be"),
       new String'("shown"),
       new String'("consecutively, Except it's"),
       new String'("hard to say"),
       new String'("where exactly the"),
       new String'("label"),
       new String'("will wrap"),
       new String'("and where exactly"),
       new String'("the actual"),
       new String'("container"),
       new String'("will wrap."),
       new String'("This label is really really really long !"),
       new String'("Let's add some more"),
       new String'("labels to the"),
       new String'("Mix. Just to"),
       new String'("make sure we"),
       new String'("got something to work"),
       new String'("with here."));

   Initial_Halign         : constant Gtk_Align := Align_Fill;
   Initial_Valign         : constant Gtk_Align := Align_Start;
   Initial_Minimum_Length : constant := 3;
   Initial_Maximum_Length : constant := 6;
   Initial_Cspacing       : constant := 2;
   Initial_Rspacing       : constant := 2;
   N_Items                : constant := 1000;

   type Item_Types is
      (Simple_Items,
       Focus_Items,
       Wrappy_Items);
   Items_Type : Item_Types := Simple_Items;

   Orientation : Gtk_Orientation := Orientation_Horizontal;

   package Flow_Cb is new Gtk.Handlers.User_Callback
      (Gtk_Widget_Record, Gtk_Flow_Box);

   procedure Populate_Items (Flow : not null access Gtk_Flow_Box_Record'Class);
   --   Add items to the flow box

   procedure Populate_Simple
      (Flow : not null access Gtk_Flow_Box_Record'Class);
   procedure Populate_Focus (Flow : not null access Gtk_Flow_Box_Record'Class);
   procedure Populate_Wrappy
      (Flow : not null access Gtk_Flow_Box_Record'Class);
   --  Add simple items to the flow box

   procedure Homogeneous_Toggled (Flow : access GObject_Record'Class);
   procedure Vertical_Alignment_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   procedure Horizontal_Alignment_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   procedure Orientation_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   procedure Text_Orientation_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   procedure Items_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   procedure Line_Length_Changed
      (Spin : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   procedure Max_Line_Length_Changed
      (Spin : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   procedure H_Spacing_Changed
      (Spin : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   procedure V_Spacing_Changed
      (Spin : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   procedure Filter_Toggled
      (Button : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   procedure Sort_Toggled
      (Button : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   procedure Selection_Mode_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box);
   --  Callbacks

   function Filter_Func
      (Child : not null access Gtk_Flow_Box_Child_Record'Class)
      return Boolean;
   --  Decides which children should be visible

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "";
   end Help;

   -------------------------
   -- Homogeneous_Toggled --
   -------------------------

   procedure Homogeneous_Toggled (Flow : access GObject_Record'Class) is
      F : constant Gtk_Flow_Box := Gtk_Flow_Box (Flow);
   begin
      F.Set_Homogeneous (not F.Get_Homogeneous);
   end Homogeneous_Toggled;

   ----------------------------------
   -- Horizontal_Alignment_Changed --
   ----------------------------------

   procedure Horizontal_Alignment_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      Flow.Set_Halign (Gtk_Align'Val (Gtk_Combo_Box_Text (Combo).Get_Active));
   end Horizontal_Alignment_Changed;

   --------------------------------
   -- Vertical_Alignment_Changed --
   --------------------------------

   procedure Vertical_Alignment_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      Flow.Set_Valign (Gtk_Align'Val (Gtk_Combo_Box_Text (Combo).Get_Active));
   end Vertical_Alignment_Changed;

   ------------------------------
   -- Text_Orientation_Changed --
   ------------------------------

   procedure Text_Orientation_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      Orientation :=
         Gtk_Orientation'Val (Gtk_Combo_Box_Text (Combo).Get_Active);
      Populate_Items (Flow);
   end Text_Orientation_Changed;

   ----------------------------
   -- Selection_Mode_Changed --
   ----------------------------

   procedure Selection_Mode_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      Flow.Set_Selection_Mode
         (Gtk_Selection_Mode'Val (Gtk_Combo_Box_Text (Combo).Get_Active));
   end Selection_Mode_Changed;

   -------------------
   -- Items_Changed --
   -------------------

   procedure Items_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      Items_Type :=
         Item_Types'Val (Gtk_Combo_Box_Text (Combo).Get_Active);
      Populate_Items (Flow);
   end Items_Changed;

   -----------------------
   -- V_Spacing_Changed --
   -----------------------

   procedure V_Spacing_Changed
      (Spin : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      Flow.Set_Row_Spacing
         (Guint (Gtk_Spin_Button (Spin).Get_Value_As_Int));
   end V_Spacing_Changed;

   -----------------------
   -- H_Spacing_Changed --
   -----------------------

   procedure H_Spacing_Changed
      (Spin : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      Flow.Set_Column_Spacing
         (Guint (Gtk_Spin_Button (Spin).Get_Value_As_Int));
   end H_Spacing_Changed;

   -----------------
   -- Filter_Func --
   -----------------

   function Filter_Func
      (Child : not null access Gtk_Flow_Box_Child_Record'Class)
      return Boolean
   is
   begin
      return Child.Get_Index mod 3 = 0;
   end Filter_Func;

   --------------------
   -- Filter_Toggled --
   --------------------

   procedure Filter_Toggled
      (Button : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      if Gtk_Check_Button (Button).Get_Active then
         Flow.Set_Filter_Func (Filter_Func'Access);
      else
         Flow.Set_Filter_Func (null);
      end if;
   end Filter_Toggled;

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
      (A, B : not null access Gtk_Flow_Box_Child_Record'Class)
      return Gint
   is
      Str1 : constant String :=
         Gtk_Label (Gtk_Frame (A.Get_Child).Get_Child).Get_Label;
      Str2 : constant String :=
         Gtk_Label (Gtk_Frame (B.Get_Child).Get_Child).Get_Label;
   begin
      if Str1 < Str2 then
         return -1;
      else
         return 1;
      end if;
   end Sort_Func;

   ------------------
   -- Sort_Toggled --
   ------------------

   procedure Sort_Toggled
      (Button : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      if Items_Type /= Simple_Items then
         Flow.Set_Sort_Func (null);
      elsif Gtk_Check_Button (Button).Get_Active then
         Flow.Set_Sort_Func (Sort_Func'Access);
      else
         Flow.Set_Sort_Func (null);
      end if;
   end Sort_Toggled;

   -------------------------
   -- Line_Length_Changed --
   -------------------------

   procedure Line_Length_Changed
      (Spin : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      Flow.Set_Min_Children_Per_Line
         (Guint (Gtk_Spin_Button (Spin).Get_Value_As_Int));
   end Line_Length_Changed;

   -----------------------------
   -- Max_Line_Length_Changed --
   -----------------------------

   procedure Max_Line_Length_Changed
      (Spin : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      Flow.Set_Max_Children_Per_Line
         (Guint (Gtk_Spin_Button (Spin).Get_Value_As_Int));
   end Max_Line_Length_Changed;

   -------------------------
   -- Orientation_Changed --
   -------------------------

   procedure Orientation_Changed
      (Combo : access Gtk_Widget_Record'Class; Flow : Gtk_Flow_Box)
   is
   begin
      Flow.Set_Orientation
         (Gtk_Orientation'Val (Gtk_Combo_Box_Text (Combo).Get_Active));
   end Orientation_Changed;

   ---------------------
   -- Populate_Simple --
   ---------------------

   procedure Populate_Simple
      (Flow : not null access Gtk_Flow_Box_Record'Class)
   is
      Label : Gtk_Label;
      Frame : Gtk_Frame;
   begin
      for J in 0 .. N_Items loop
         Gtk_New (Label, "Item" & J'Img);
         Gtk_New (Frame);
         Frame.Add (Label);

         if Orientation = Orientation_Vertical then
            Label.Set_Angle (90.0);
         end if;
         Flow.Add (Frame);
      end loop;
   end Populate_Simple;

   --------------------
   -- Populate_Focus --
   --------------------

   procedure Populate_Focus
      (Flow : not null access Gtk_Flow_Box_Record'Class)
   is
      Label : Gtk_Label;
      Frame : Gtk_Frame;
      Box   : Gtk_Box;
      Ent   : Gtk_Entry;
      Button : Gtk_Button;
   begin
      for J in 0 .. N_Items loop
         Gtk_New (Frame);
         Gtk_New (Box, Orientation_Horizontal, Spacing => 6);
         Frame.Add (Box);

         Gtk_New (Label, "Item" & J'Img);
         Box.Add (Label);

         case J mod 4 is
            when 0 =>
               Gtk_New (Ent);
               Box.Add (Ent);
            when 1 =>
               Gtk_New (Button, "Button");
               Box.Add (Button);
            when 2 =>
               Gtk_New (Label, "bla");
               Box.Add (Label);
            when others =>
               Gtk_New (Label, "bla");
               Label.Set_Sensitive (False);
               Box.Add (Label);
         end case;

         Flow.Add (Frame);
      end loop;
   end Populate_Focus;

   ---------------------
   -- Populate_Wrappy --
   ---------------------

   procedure Populate_Wrappy
      (Flow : not null access Gtk_Flow_Box_Record'Class)
   is
      Label : Gtk_Label;
      Frame : Gtk_Frame;
   begin
      for J in Strings'Range loop
         Gtk_New (Label, Strings (J).all);
         Gtk_New (Frame);
         Frame.Add (Label);

         if Orientation = Orientation_Vertical then
            Label.Set_Angle (90.0);
         end if;

         Label.Set_Line_Wrap (True);
         Label.Set_Line_Wrap_Mode (Pango_Wrap_Word);
         Label.Set_Width_Chars (10);
         Flow.Add (Frame);
      end loop;
   end Populate_Wrappy;

   --------------------
   -- Populate_Items --
   --------------------

   procedure Populate_Items
      (Flow : not null access Gtk_Flow_Box_Record'Class)
   is
      use Widget_List;
      Children : Widget_List.Glist := Flow.Get_Children;
      Iter     : Widget_List.Glist := Children;
   begin
      while Iter /= Null_List loop
         Flow.Remove (Widget_List.Get_Data (Iter));
         Iter := Next (Iter);
      end loop;
      Widget_List.Free (Children);

      case Items_Type is
         when Simple_Items =>
            Populate_Simple (Flow);
         when Focus_Items =>
            Populate_Focus (Flow);
         when Wrappy_Items =>
            Populate_Wrappy (Flow);
      end case;

      Flow.Show_All;
   end Populate_Items;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Hbox, Vbox : Gtk_Box;
      Scrolled   : Gtk_Scrolled_Window;
      Flow       : Gtk_Flow_Box;
      Expander   : Gtk_Expander;
      Control    : Gtk_Box;
      Button     : Gtk_Check_Button;
      Combo      : Gtk_Combo_Box_Text;
      Spin       : Gtk_Spin_Button;
      Label      : Gtk_Label;
   begin
      Gtk_New (Hbox, Orientation_Horizontal, Spacing => 6);
      Frame.Add (Hbox);

      Gtk_New (Vbox, Orientation_Vertical, Spacing => 6);
      Hbox.Pack_Start (Vbox, Expand => False, Fill => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Hbox.Pack_Start (Scrolled, Expand => True, Fill => True);

      Gtk_New (Flow);
      Flow.Set_Halign (Initial_Halign);
      Flow.Set_Valign (Initial_Valign);
      Flow.Set_Column_Spacing (Initial_Cspacing);
      Flow.Set_Row_Spacing (Initial_Rspacing);
      Flow.Set_Min_Children_Per_Line (Initial_Minimum_Length);
      Flow.Set_Max_Children_Per_Line (Initial_Maximum_Length);
      Scrolled.Add (Flow);

      Gtk_New (Expander, "Flow box controls");
      Expander.Set_Expanded (True);
      Vbox.Pack_Start (Expander);

      Gtk_New (Control, Orientation_Vertical, Spacing => 2);
      Expander.Add (Control);

      Gtk_New (Button, "Homogeneous");
      Button.Set_Active (False);
      Button.Set_Tooltip_Text
         ("Set whether the items should be displayed at the same size");
      Control.Pack_Start (Button, Expand => False, Fill => False);
      Button.On_Toggled (Homogeneous_Toggled'Access, Flow);

      Gtk_New (Combo);
      Combo.Append_Text ("Fill");
      Combo.Append_Text ("Start");
      Combo.Append_Text ("End");
      Combo.Append_Text ("Center");
      Combo.Set_Active (Gtk_Align'Pos (Initial_Halign));
      Combo.Set_Tooltip_Text ("Set the horizontal alignment policy");
      Control.Pack_Start (Combo, Expand => False, Fill => False);
      Flow_Cb.Connect
         (Combo, Gtk.Combo_Box.Signal_Changed,
          Horizontal_Alignment_Changed'Access, Flow);

      Gtk_New (Combo);
      Combo.Append_Text ("Fill");
      Combo.Append_Text ("Start");
      Combo.Append_Text ("End");
      Combo.Append_Text ("Center");
      Combo.Set_Active (Gtk_Align'Pos (Initial_Valign));
      Combo.Set_Tooltip_Text ("Set the vertical alignment policy");
      Control.Pack_Start (Combo, Expand => False, Fill => False);
      Flow_Cb.Connect
         (Combo, Gtk.Combo_Box.Signal_Changed,
          Vertical_Alignment_Changed'Access, Flow);

      Gtk_New (Combo);
      Combo.Append_Text ("Horizontal");
      Combo.Append_Text ("Vertical");
      Combo.Set_Active (0);
      Combo.Set_Tooltip_Text ("Set the flowbox orientation");
      Control.Pack_Start (Combo, Expand => False, Fill => False);
      Flow_Cb.Connect
         (Combo, Gtk.Combo_Box.Signal_Changed,
          Orientation_Changed'Access, Flow);

      Gtk_New (Combo);
      Combo.Append_Text ("None");
      Combo.Append_Text ("Single");
      Combo.Append_Text ("Browse");
      Combo.Append_Text ("Multiple");
      Combo.Set_Active (1);
      Combo.Set_Tooltip_Text ("Set the selection mode");
      Control.Pack_Start (Combo, Expand => False, Fill => False);
      Flow_Cb.Connect
         (Combo, Gtk.Combo_Box.Signal_Changed,
          Selection_Mode_Changed'Access, Flow);

      Gtk_New (Spin, 1.0, 10.0, 1.0);
      Spin.Set_Value (Gdouble (Initial_Minimum_Length));
      Spin.Set_Tooltip_Text ("Set the minimum amount of items per line");
      Control.Pack_Start (Spin, Expand => False, Fill => False);
      Flow_Cb.Connect
         (Spin, Gtk.Spin_Button.Signal_Value_Changed,
          Line_Length_Changed'Access, Flow);

      Gtk_New (Spin, 1.0, 10.0, 1.0);
      Spin.Set_Value (Gdouble (Initial_Maximum_Length));
      Spin.Set_Tooltip_Text ("Set the natural amount of items per line");
      Control.Pack_Start (Spin, Expand => False, Fill => False);
      Flow_Cb.Connect
         (Spin, Gtk.Spin_Button.Signal_Value_Changed,
          Max_Line_Length_Changed'Access, Flow);

      Gtk_New (Hbox, Orientation_Horizontal, Spacing => 2);
      Control.Pack_Start (Hbox, Expand => False, Fill => False);

      Gtk_New (Label, "H Spacing");
      Hbox.Pack_Start (Label, Expand => True, Fill => True);

      Gtk_New (Spin, 0.0, 30.0, 1.0);
      Spin.Set_Value (Gdouble (Initial_Cspacing));
      Spin.Set_Tooltip_Text ("Set the horizontal spacing between children");
      Hbox.Pack_Start (Spin, Expand => False, Fill => False);
      Flow_Cb.Connect
         (Spin, Gtk.Spin_Button.Signal_Value_Changed,
          H_Spacing_Changed'Access, Flow);

      Gtk_New (Hbox, Orientation_Horizontal, Spacing => 2);
      Control.Pack_Start (Hbox, Expand => False, Fill => False);

      Gtk_New (Label, "V Spacing");
      Hbox.Pack_Start (Label, Expand => True, Fill => True);

      Gtk_New (Spin, 0.0, 30.0, 1.0);
      Spin.Set_Value (Gdouble (Initial_Cspacing));
      Spin.Set_Tooltip_Text ("Set the vertical spacing between children");
      Hbox.Pack_Start (Spin, Expand => False, Fill => False);
      Flow_Cb.Connect
         (Spin, Gtk.Spin_Button.Signal_Value_Changed,
          V_Spacing_Changed'Access, Flow);

      Gtk_New (Button, "Filter");
      Button.Set_Active (False);
      Button.Set_Tooltip_Text
         ("Set whether some items should be filtered out");
      Control.Pack_Start (Button, Expand => False, Fill => False);
      Flow_Cb.Connect
         (Button, Gtk.Toggle_Button.Signal_Toggled,
          Filter_Toggled'Access, Flow);

      Gtk_New (Button, "Sort");
      Button.Set_Active (False);
      Control.Pack_Start (Button, Expand => False, Fill => False);
      Button.Set_Tooltip_Text ("Set whether items should be sorted");
      Flow_Cb.Connect
         (Button, Gtk.Toggle_Button.Signal_Toggled,
          Sort_Toggled'Access, Flow);

      Gtk_New (Expander, "Test item controls");
      Expander.Set_Expanded (True);

      Gtk_New (Control, Orientation_Vertical, Spacing => 2);
      Expander.Add (Control);
      Vbox.Pack_Start (Expander, Expand => False, Fill => False);

      Gtk_New (Combo);
      Combo.Append_Text ("Simple");
      Combo.Append_Text ("Focus");
      Combo.Append_Text ("Wrappy");
      Combo.Set_Active (0);
      Combo.Set_Tooltip_Text ("Set the item set to use");
      Control.Pack_Start (Combo, Expand => False, Fill => False);
      Flow_Cb.Connect
         (Combo, Gtk.Combo_Box.Signal_Changed,
          Items_Changed'Access, Flow);

      Gtk_New (Combo);
      Combo.Append_Text ("Horizontal");
      Combo.Append_Text ("Vertical");
      Combo.Set_Active (0);
      Combo.Set_Tooltip_Text
         ("Set the item's text orientation (can't be done for stock buttons)");
      Control.Pack_Start (Combo, Expand => False, Fill => False);
      Flow_Cb.Connect
         (Combo, Gtk.Combo_Box.Signal_Changed,
          Text_Orientation_Changed'Access, Flow);

      Populate_Items (Flow);

      Frame.Show_All;
   end Run;

end Create_Flow_Box;
