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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Glib;                    use Glib;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Editable;            use Gtk.Editable;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Frame;               use Gtk.Frame;
with Gtk.Label;               use Gtk.Label;
with Gtk.List_Box;            use Gtk.List_Box;
with Gtk.List_Box_Row;        use Gtk.List_Box_Row;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Search_Entry;        use Gtk.Search_Entry;
with Gtk.Separator;           use Gtk.Separator;
with Gtk.Widget;              use Gtk.Widget;

package body Create_List_Box_Complex is

   type Message is record
      Sender  : access constant String;
      Subject : access constant String;
      Age     : Natural;
      --  Hours since the message arrived
   end record;

   function "+" (S : aliased String) return access constant String
   is (S'Unrestricted_Access);

   S1 : aliased constant String := "Alice";
   S2 : aliased constant String := "Bob";
   S3 : aliased constant String := "Carol";
   S4 : aliased constant String := "Dave";
   T1 : aliased constant String := "Lunch on Friday?";
   T2 : aliased constant String := "Build is green again";
   T3 : aliased constant String := "Draft of the release notes";
   T4 : aliased constant String := "Re: Lunch on Friday?";
   T5 : aliased constant String := "Your invoice";
   T6 : aliased constant String := "Weekend hike";
   T7 : aliased constant String := "Meeting moved to 3pm";
   T8 : aliased constant String := "Welcome aboard";

   Messages : constant array (Positive range <>) of Message :=
     ((+S1, +T1, 2), (+S2, +T2, 5), (+S3, +T3, 8), (+S4, +T4, 26),
      (+S1, +T5, 30), (+S2, +T6, 49), (+S3, +T7, 53), (+S4, +T8, 120));

   type Order is (By_Age, By_Sender);

   Current_Order : Order := By_Age;
   --  What the list is sorted by; the header function follows suit

   List   : Gtk_List_Box;
   Search : Gtk_Search_Entry;
   Detail : Gtk_Label;
   --  The widgets the callbacks work on

   function Index_Of
     (Row : not null access Gtk_List_Box_Row_Record'Class) return Positive
   is (Positive'Value (Row.Get_Name));
   --  Index in Messages of the message Row shows. Kept in the row's widget
   --  name, which is otherwise unused.

   function Contains (Text, Pattern : String) return Boolean
   is (Pattern = "" or else Index (To_Lower (Text), To_Lower (Pattern)) > 0);

   function Sort_Func
     (Row1, Row2 : not null access Gtk_List_Box_Row_Record'Class) return Gint;
   function Filter_Func
     (Row : not null access Gtk_List_Box_Row_Record'Class) return Boolean;
   procedure Header_Func
     (Row    : not null access Gtk_List_Box_Row_Record'Class;
      Before : access Gtk_List_Box_Row_Record'Class);
   procedure On_Row_Selected
     (Self : access Gtk_List_Box_Record'Class;
      Row  : access Gtk_List_Box_Row_Record'Class);
   procedure On_Search_Changed (Self : access Gtk_Search_Entry_Record'Class);
   procedure On_Sort_Age (Self : access Gtk_Button_Record'Class);
   procedure On_Sort_Sender (Self : access Gtk_Button_Record'Class);

   function New_Row (Index : Positive) return Gtk_List_Box_Row;
   --  A row showing Messages (Index)

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_List_Box@B keeps its rows in the order they were added"
        & " until it is given three functions that let it arrange them"
        & " itself, without the application touching the rows again."
        & ASCII.LF
        & "@bSet_Sort_Func@B orders the rows, and is called again after"
        & " @bInvalidate_Sort@B. @bSet_Filter_Func@B decides which rows show,"
        & " and is re-run by @bInvalidate_Filter@B, here as you type in the"
        & " search entry. @bSet_Header_Func@B puts a heading in front of a"
        & " row that begins a new group: it receives the row and the one"
        & " before it, and either sets a header or clears it. Sorting by"
        & " sender groups the messages under each sender; sorting by age"
        & " needs no headers."
        & ASCII.LF
        & "Selecting a row shows its message underneath. The signal comes"
        & " with a null row when the selection is cleared, for instance when"
        & " the search filters the selected row out.";
   end Help;

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
     (Row1, Row2 : not null access Gtk_List_Box_Row_Record'Class) return Gint
   is
      M1 : Message renames Messages (Index_Of (Row1));
      M2 : Message renames Messages (Index_Of (Row2));
   begin
      case Current_Order is
         when By_Age =>
            return Gint (M1.Age) - Gint (M2.Age);
         when By_Sender =>
            if M1.Sender.all /= M2.Sender.all then
               return (if M1.Sender.all < M2.Sender.all then -1 else 1);
            end if;
            return Gint (M1.Age) - Gint (M2.Age);
      end case;
   end Sort_Func;

   -----------------
   -- Filter_Func --
   -----------------

   function Filter_Func
     (Row : not null access Gtk_List_Box_Row_Record'Class) return Boolean
   is
      M : Message renames Messages (Index_Of (Row));
      Pattern : constant String := Search.Get_Text;
   begin
      return Contains (M.Sender.all, Pattern)
        or else Contains (M.Subject.all, Pattern);
   end Filter_Func;

   -----------------
   -- Header_Func --
   -----------------

   procedure Header_Func
     (Row    : not null access Gtk_List_Box_Row_Record'Class;
      Before : access Gtk_List_Box_Row_Record'Class)
   is
      Sender : constant String := Messages (Index_Of (Row)).Sender.all;
   begin
      if Current_Order = By_Sender
        and then (Before = null
                  or else Messages (Index_Of (Before)).Sender.all /= Sender)
      then
         declare
            Header : constant Gtk_Label := Gtk_Label_New (Sender);
         begin
            Header.Add_Css_Class ("heading");
            Header.Set_Xalign (0.0);
            Header.Set_Margin_Top (6);
            Header.Set_Margin_Start (12);
            Row.Set_Header (Header);
         end;
      else
         Row.Set_Header (null);
      end if;
   end Header_Func;

   ---------------------
   -- On_Row_Selected --
   ---------------------

   procedure On_Row_Selected
     (Self : access Gtk_List_Box_Record'Class;
      Row  : access Gtk_List_Box_Row_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      if Row = null then
         Detail.Set_Text ("No message selected");
      else
         declare
            M : Message renames Messages (Index_Of (Row));
         begin
            Detail.Set_Text
              (M.Subject.all & " (from " & M.Sender.all & ","
               & Natural'Image (M.Age) & "h ago)");
         end;
      end if;
   end On_Row_Selected;

   -----------------------
   -- On_Search_Changed --
   -----------------------

   procedure On_Search_Changed (Self : access Gtk_Search_Entry_Record'Class) is
      pragma Unreferenced (Self);
   begin
      List.Invalidate_Filter;
   end On_Search_Changed;

   -----------------
   -- On_Sort_Age --
   -----------------

   procedure On_Sort_Age (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Current_Order := By_Age;
      List.Invalidate_Sort;
      List.Invalidate_Headers;
   end On_Sort_Age;

   --------------------
   -- On_Sort_Sender --
   --------------------

   procedure On_Sort_Sender (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Current_Order := By_Sender;
      List.Invalidate_Sort;
      List.Invalidate_Headers;
   end On_Sort_Sender;

   -------------
   -- New_Row --
   -------------

   function New_Row (Index : Positive) return Gtk_List_Box_Row is
      M       : Message renames Messages (Index);
      Row     : constant Gtk_List_Box_Row := Gtk_List_Box_Row_New;
      Column  : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 2);
      Subject : constant Gtk_Label := Gtk_Label_New (M.Subject.all);
      Sender  : constant Gtk_Label :=
        Gtk_Label_New (M.Sender.all & "," & Natural'Image (M.Age) & "h ago");
   begin
      Column.Set_Margin_Top (6);
      Column.Set_Margin_Bottom (6);
      Column.Set_Margin_Start (12);
      Column.Set_Margin_End (12);

      Subject.Set_Xalign (0.0);
      Subject.Add_Css_Class ("heading");
      Sender.Set_Xalign (0.0);
      Sender.Add_Css_Class ("dim-label");
      Column.Append (Subject);
      Column.Append (Sender);

      Row.Set_Child (Column);
      Row.Set_Name (Trim (Positive'Image (Index), Ada.Strings.Left));
      return Row;
   end New_Row;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Page     : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 6);
      Controls : constant Gtk_Box := Gtk_Box_New (Orientation_Horizontal, 6);
      Scroll   : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window_New;
      By_Time  : constant Gtk_Button := Gtk_Button_New_With_Label ("By age");
      By_Who   : constant Gtk_Button :=
        Gtk_Button_New_With_Label ("By sender");
   begin
      Frame.Set_Label ("List Box Complex");

      Page.Set_Margin_Top (12);
      Page.Set_Margin_Bottom (12);
      Page.Set_Margin_Start (12);
      Page.Set_Margin_End (12);
      Frame.Set_Child (Page);

      Current_Order := By_Age;

      Gtk_New (Search);
      Search.Set_Hexpand (True);
      Search.On_Search_Changed (On_Search_Changed'Access);
      By_Time.On_Clicked (On_Sort_Age'Access);
      By_Who.On_Clicked (On_Sort_Sender'Access);
      Controls.Append (Search);
      Controls.Append (By_Time);
      Controls.Append (By_Who);
      Page.Append (Controls);

      Gtk_New (List);
      List.Add_Css_Class ("boxed-list");
      List.Set_Selection_Mode (Selection_Single);
      for I in Messages'Range loop
         List.Append (New_Row (I));
      end loop;
      List.Set_Sort_Func (Sort_Func'Access);
      List.Set_Filter_Func (Filter_Func'Access);
      List.Set_Header_Func (Header_Func'Access);
      List.Set_Placeholder (Gtk_Label_New ("No message matches"));
      List.On_Row_Selected (On_Row_Selected'Access);

      Scroll.Set_Policy (Policy_Never, Policy_Automatic);
      Scroll.Set_Vexpand (True);
      Scroll.Set_Min_Content_Height (240);
      Scroll.Set_Child (List);
      Page.Append (Scroll);

      Gtk_New (Detail, "No message selected");
      Detail.Set_Xalign (0.0);
      Page.Append (Detail);
   end Run;

end Create_List_Box_Complex;
