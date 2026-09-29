------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2010-2026, AdaCore                     --
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

with GNAT.Strings;        use GNAT.Strings;
with Glib;                use Glib;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Flow_Box;        use Gtk.Flow_Box;
with Gtk.Flow_Box_Child;  use Gtk.Flow_Box_Child;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Label;           use Gtk.Label;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtk.Widget;          use Gtk.Widget;

package body Create_Flow_Box is

   Texts : constant GNAT.Strings.String_List :=
     (new String'("These are"),
      new String'("some wrappy label"),
      new String'("texts"),
      new String'("of various"),
      new String'("lengths."),
      new String'("They should always be"),
      new String'("shown"),
      new String'("consecutively, except it's"),
      new String'("hard to say"),
      new String'("where exactly the"),
      new String'("label"),
      new String'("will wrap"),
      new String'("and where exactly"),
      new String'("the actual"),
      new String'("container"),
      new String'("will wrap."),
      new String'("This label is really really really long!"));

   N_Items : constant := 60;

   Flow : Gtk_Flow_Box;
   --  The flow box the controls act on

   Scroll : Gtk_Scrolled_Window;
   --  The scrolled window around Flow. Its scrolling policy follows the
   --  orientation of Flow: lines grow across the width of a horizontal flow
   --  box, which has to fit it, and across the height of a vertical one.

   Mode : Gtk_Selection_Mode := Selection_Single;
   --  Its current selection mode, cycled by the mode button

   procedure Populate;
   --  Fill Flow with N_Items framed labels

   function Filter_Func
     (Child : not null access Gtk_Flow_Box_Child_Record'Class)
      return Boolean;
   function Sort_Func
     (A, B : not null access Gtk_Flow_Box_Child_Record'Class) return Gint;

   procedure On_Homogeneous (Self : access Gtk_Check_Button_Record'Class);
   procedure On_Vertical (Self : access Gtk_Check_Button_Record'Class);
   procedure On_Filter (Self : access Gtk_Check_Button_Record'Class);
   procedure On_Sort (Self : access Gtk_Check_Button_Record'Class);
   procedure On_Min (Self : access Gtk_Spin_Button_Record'Class);
   procedure On_Max (Self : access Gtk_Spin_Button_Record'Class);
   procedure On_Column_Spacing (Self : access Gtk_Spin_Button_Record'Class);
   procedure On_Row_Spacing (Self : access Gtk_Spin_Button_Record'Class);
   procedure On_Mode (Self : access Gtk_Button_Record'Class);

   function Add_Check
     (Row   : not null access Gtk_Box_Record'Class;
      Title : String;
      Call  : Cb_Gtk_Check_Button_Void) return Gtk_Check_Button;
   function Add_Spin
     (Row   : not null access Gtk_Box_Record'Class;
      Title : String;
      Min, Max, Start : Gdouble;
      Call  : Cb_Gtk_Spin_Button_Void) return Gtk_Spin_Button;
   --  Add a labelled control to Row

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Flow_Box@B lays out its children in a reflowing"
        & " grid: as many items per line as fit, wrapping to a new line"
        & " when needed, like text wrapping."
        & ASCII.LF
        & "The controls above change its homogeneity, orientation, selection"
        & " mode, minimum and maximum children per line and spacing. The"
        & " filter check keeps every third child through"
        & " @bSet_Filter_Func@B, and the sort check orders the children by"
        & " their text through @bSet_Sort_Func@B; both are undone by"
        & " passing @bnull@B.";
   end Help;

   -----------------
   -- Filter_Func --
   -----------------

   function Filter_Func
     (Child : not null access Gtk_Flow_Box_Child_Record'Class)
      return Boolean is
   begin
      return Child.Get_Index mod 3 = 0;
   end Filter_Func;

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
     (A, B : not null access Gtk_Flow_Box_Child_Record'Class) return Gint
   is
      Text_A : constant String :=
        Gtk_Label (Gtk_Frame (A.Get_Child).Get_Child).Get_Text;
      Text_B : constant String :=
        Gtk_Label (Gtk_Frame (B.Get_Child).Get_Child).Get_Text;
   begin
      return (if Text_A < Text_B then -1 elsif Text_A > Text_B then 1 else 0);
   end Sort_Func;

   --------------------
   -- On_Homogeneous --
   --------------------

   procedure On_Homogeneous (Self : access Gtk_Check_Button_Record'Class) is
   begin
      Flow.Set_Homogeneous (Self.Get_Active);
   end On_Homogeneous;

   -----------------
   -- On_Vertical --
   -----------------

   procedure On_Vertical (Self : access Gtk_Check_Button_Record'Class) is
   begin
      if Self.Get_Active then
         Flow.Set_Orientation (Orientation_Vertical);
         Scroll.Set_Policy (Policy_Automatic, Policy_Never);
      else
         Flow.Set_Orientation (Orientation_Horizontal);
         Scroll.Set_Policy (Policy_Never, Policy_Automatic);
      end if;
   end On_Vertical;

   ---------------
   -- On_Filter --
   ---------------

   procedure On_Filter (Self : access Gtk_Check_Button_Record'Class) is
   begin
      Flow.Set_Filter_Func (if Self.Get_Active then Filter_Func'Access
                            else null);
   end On_Filter;

   -------------
   -- On_Sort --
   -------------

   procedure On_Sort (Self : access Gtk_Check_Button_Record'Class) is
   begin
      Flow.Set_Sort_Func (if Self.Get_Active then Sort_Func'Access else null);
   end On_Sort;

   ------------
   -- On_Min --
   ------------

   procedure On_Min (Self : access Gtk_Spin_Button_Record'Class) is
   begin
      Flow.Set_Min_Children_Per_Line (Guint (Self.Get_Value_As_Int));
   end On_Min;

   ------------
   -- On_Max --
   ------------

   procedure On_Max (Self : access Gtk_Spin_Button_Record'Class) is
   begin
      Flow.Set_Max_Children_Per_Line (Guint (Self.Get_Value_As_Int));
   end On_Max;

   -----------------------
   -- On_Column_Spacing --
   -----------------------

   procedure On_Column_Spacing (Self : access Gtk_Spin_Button_Record'Class) is
   begin
      Flow.Set_Column_Spacing (Guint (Self.Get_Value_As_Int));
   end On_Column_Spacing;

   --------------------
   -- On_Row_Spacing --
   --------------------

   procedure On_Row_Spacing (Self : access Gtk_Spin_Button_Record'Class) is
   begin
      Flow.Set_Row_Spacing (Guint (Self.Get_Value_As_Int));
   end On_Row_Spacing;

   -------------
   -- On_Mode --
   -------------

   procedure On_Mode (Self : access Gtk_Button_Record'Class) is
   begin
      Mode := (if Mode = Gtk_Selection_Mode'Last then Gtk_Selection_Mode'First
               else Gtk_Selection_Mode'Succ (Mode));
      Flow.Set_Selection_Mode (Mode);
      Self.Set_Label
        ("Selection: "
         & (case Mode is
              when Selection_None     => "none",
              when Selection_Single   => "single",
              when Selection_Browse   => "browse",
              when Selection_Multiple => "multiple"));
   end On_Mode;

   --------------
   -- Populate --
   --------------

   procedure Populate is
   begin
      for I in 1 .. N_Items loop
         declare
            Item  : constant Gtk_Frame := Gtk_Frame_New;
            Image : constant String := Integer'Image (I);
            Text  : constant Gtk_Label :=
              Gtk_Label_New
                (Texts (Texts'First + (I - 1) mod Texts'Length).all
                 & " (" & Image (Image'First + 1 .. Image'Last) & ")");
         begin
            Text.Set_Wrap (True);
            Text.Set_Margin_Top (6);
            Text.Set_Margin_Bottom (6);
            Text.Set_Margin_Start (6);
            Text.Set_Margin_End (6);
            Item.Set_Child (Text);
            Flow.Append (Item);
         end;
      end loop;
   end Populate;

   ---------------
   -- Add_Check --
   ---------------

   function Add_Check
     (Row   : not null access Gtk_Box_Record'Class;
      Title : String;
      Call  : Cb_Gtk_Check_Button_Void) return Gtk_Check_Button
   is
      Check : constant Gtk_Check_Button := Gtk_Check_Button_New_With_Label (Title);
   begin
      Check.On_Toggled (Call);
      Row.Append (Check);
      return Check;
   end Add_Check;

   --------------
   -- Add_Spin --
   --------------

   function Add_Spin
     (Row   : not null access Gtk_Box_Record'Class;
      Title : String;
      Min, Max, Start : Gdouble;
      Call  : Cb_Gtk_Spin_Button_Void) return Gtk_Spin_Button
   is
      Spin : constant Gtk_Spin_Button :=
        Gtk_Spin_Button_New_With_Range (Min, Max, 1.0);
   begin
      Row.Append (Gtk_Label_New (Title));
      Spin.Set_Value (Start);
      Spin.On_Value_Changed (Call);
      Row.Append (Spin);
      return Spin;
   end Add_Spin;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Page   : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 6);
      Checks : constant Gtk_Box := Gtk_Box_New (Orientation_Horizontal, 12);
      Spins  : constant Gtk_Box := Gtk_Box_New (Orientation_Horizontal, 6);
      Mode_Button : constant Gtk_Button :=
        Gtk_Button_New_With_Label ("Selection: single");
      Ignored_Check : Gtk_Check_Button;
      Ignored_Spin  : Gtk_Spin_Button;
   begin
      Frame.Set_Label ("Flow Box");

      Page.Set_Margin_Top (12);
      Page.Set_Margin_Bottom (12);
      Page.Set_Margin_Start (12);
      Page.Set_Margin_End (12);
      Frame.Set_Child (Page);

      Ignored_Check := Add_Check (Checks, "Homogeneous", On_Homogeneous'Access);
      Ignored_Check := Add_Check (Checks, "Vertical", On_Vertical'Access);
      Ignored_Check := Add_Check (Checks, "Filter", On_Filter'Access);
      Ignored_Check := Add_Check (Checks, "Sort", On_Sort'Access);
      Mode := Selection_Single;
      Mode_Button.On_Clicked (On_Mode'Access);
      Checks.Append (Mode_Button);
      Page.Append (Checks);

      Ignored_Spin := Add_Spin (Spins, "Min per line", 1.0, 10.0, 3.0, On_Min'Access);
      Ignored_Spin := Add_Spin (Spins, "Max per line", 1.0, 10.0, 6.0, On_Max'Access);
      Ignored_Spin := Add_Spin (Spins, "Column spacing", 0.0, 30.0, 2.0, On_Column_Spacing'Access);
      Ignored_Spin := Add_Spin (Spins, "Row spacing", 0.0, 30.0, 2.0, On_Row_Spacing'Access);
      Page.Append (Spins);

      Gtk_New (Flow);
      Flow.Set_Valign (Align_Start);
      Flow.Set_Min_Children_Per_Line (3);
      Flow.Set_Max_Children_Per_Line (6);
      Flow.Set_Column_Spacing (2);
      Flow.Set_Row_Spacing (2);
      Populate;

      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Never, Policy_Automatic);
      Scroll.Set_Vexpand (True);
      Scroll.Set_Min_Content_Height (240);
      Scroll.Set_Child (Flow);
      Page.Append (Scroll);
   end Run;

end Create_Flow_Box;
