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
with Gtk.Drop_Down;       use Gtk.Drop_Down;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Label;           use Gtk.Label;
with Gtk.List_Box;        use Gtk.List_Box;
with Gtk.List_Box_Row;    use Gtk.List_Box_Row;
with Gtk.Toggle_Button;   use Gtk.Toggle_Button;
with Gtk.Widget;          use Gtk.Widget;

package body Create_List_Box_Controls is

   List : Gtk_List_Box;
   --  The list being demonstrated

   Status : Gtk_Label;
   --  Reports the last thing that happened in List

   procedure Report (Text : String);
   --  Show Text in Status

   procedure On_Row_Activated
     (Self : access Gtk_List_Box_Record'Class;
      Row  : not null access Gtk_List_Box_Row_Record'Class);
   procedure On_Row_Selected
     (Self : access Gtk_List_Box_Record'Class;
      Row  : access Gtk_List_Box_Row_Record'Class);
   procedure On_Check_Toggled (Self : access Gtk_Check_Button_Record'Class);
   procedure On_Toggle_Toggled (Self : access Gtk_Toggle_Button_Record'Class);
   procedure On_Press (Self : access Gtk_Button_Record'Class);
   procedure On_Single (Self : access Gtk_Button_Record'Class);
   procedure On_Multiple (Self : access Gtk_Button_Record'Class);
   procedure On_None (Self : access Gtk_Button_Record'Class);

   function Add_Row
     (Title    : String;
      Control  : not null access Gtk_Widget_Record'Class;
      Activate : Boolean) return Gtk_List_Box_Row;
   --  Append a row holding Title on the left and Control on the right. Only
   --  a row with Activate set answers to a click of its own.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_List_Box@B is a vertical list of @bGtk_List_Box_Row@Bs,"
        & " each of which may hold any widget, including widgets that take"
        & " input of their own."
        & ASCII.LF
        & "Clicking a row selects it and emits @bOn_Row_Selected@B, and"
        & " activates it, emitting @bOn_Row_Activated@B, unless"
        & " @bSet_Activatable@B was turned off for that row. The row holding"
        & " a button here is the one that stays activatable; clicking the"
        & " other controls acts on the control instead. The last row is"
        & " not @bSelectable@B at all."
        & ASCII.LF
        & "@bSet_Selection_Mode@B, driven by the three buttons underneath,"
        & " chooses how many rows may be selected at once. Switching to"
        & " none clears the selection, and @bOn_Row_Selected@B then sees a"
        & " null row.";
   end Help;

   ------------
   -- Report --
   ------------

   procedure Report (Text : String) is
   begin
      Status.Set_Text (Text);
   end Report;

   ----------------------
   -- On_Row_Activated --
   ----------------------

   procedure On_Row_Activated
     (Self : access Gtk_List_Box_Record'Class;
      Row  : not null access Gtk_List_Box_Row_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Report ("Row" & Gint'Image (Row.Get_Index) & " activated");
   end On_Row_Activated;

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
         Report ("Nothing selected");
      else
         Report ("Row" & Gint'Image (Row.Get_Index) & " selected");
      end if;
   end On_Row_Selected;

   ----------------------
   -- On_Check_Toggled --
   ----------------------

   procedure On_Check_Toggled (Self : access Gtk_Check_Button_Record'Class) is
   begin
      Report ("Check button is " & (if Self.Get_Active then "on" else "off"));
   end On_Check_Toggled;

   -----------------------
   -- On_Toggle_Toggled --
   -----------------------

   procedure On_Toggle_Toggled
     (Self : access Gtk_Toggle_Button_Record'Class) is
   begin
      Report ("Toggle button is " & (if Self.Get_Active then "on" else "off"));
   end On_Toggle_Toggled;

   --------------
   -- On_Press --
   --------------

   procedure On_Press (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Report ("Button pressed");
   end On_Press;

   ---------------
   -- On_Single --
   ---------------

   procedure On_Single (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      List.Set_Selection_Mode (Selection_Single);
   end On_Single;

   -----------------
   -- On_Multiple --
   -----------------

   procedure On_Multiple (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      List.Set_Selection_Mode (Selection_Multiple);
   end On_Multiple;

   -------------
   -- On_None --
   -------------

   procedure On_None (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      List.Set_Selection_Mode (Selection_None);
   end On_None;

   -------------
   -- Add_Row --
   -------------

   function Add_Row
     (Title    : String;
      Control  : not null access Gtk_Widget_Record'Class;
      Activate : Boolean) return Gtk_List_Box_Row
   is
      Line  : constant Gtk_Box := Gtk_Box_New (Orientation_Horizontal, 12);
      Label : constant Gtk_Label := Gtk_Label_New (Title);
      Row   : constant Gtk_List_Box_Row := Gtk_List_Box_Row_New;
   begin
      Line.Set_Margin_Top (6);
      Line.Set_Margin_Bottom (6);
      Line.Set_Margin_Start (12);
      Line.Set_Margin_End (12);

      Label.Set_Xalign (0.0);
      Label.Set_Hexpand (True);
      Line.Append (Label);

      Control.Set_Valign (Align_Center);
      Line.Append (Control);

      Row.Set_Child (Line);
      Row.Set_Activatable (Activate);
      List.Append (Row);
      return Row;
   end Add_Row;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Page    : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 12);
      Modes   : constant Gtk_Box := Gtk_Box_New (Orientation_Horizontal, 6);
      Check   : constant Gtk_Check_Button := Gtk_Check_Button_New;
      Toggle  : constant Gtk_Toggle_Button :=
        Gtk_Toggle_Button_New_With_Label ("Off / on");
      Press   : constant Gtk_Button := Gtk_Button_New_With_Label ("Press");
      Choices : constant Gtk_Drop_Down :=
        Gtk_Drop_Down_New_From_Strings
          ((new String'("First"), new String'("Second"),
            new String'("Third")));
      Single  : constant Gtk_Button := Gtk_Button_New_With_Label ("Single");
      Multi   : constant Gtk_Button := Gtk_Button_New_With_Label ("Multiple");
      None    : constant Gtk_Button := Gtk_Button_New_With_Label ("None");
      Row     : Gtk_List_Box_Row;
   begin
      Frame.Set_Label ("List Box Controls");

      Page.Set_Margin_Top (12);
      Page.Set_Margin_Bottom (12);
      Page.Set_Margin_Start (12);
      Page.Set_Margin_End (12);
      Frame.Set_Child (Page);

      Gtk_New (List);
      List.Add_Css_Class ("boxed-list");
      List.Set_Valign (Align_Start);
      Page.Append (List);

      Check.On_Toggled (On_Check_Toggled'Access);
      Toggle.On_Toggled (On_Toggle_Toggled'Access);
      Press.On_Clicked (On_Press'Access);

      Row := Add_Row ("Check button", Check, Activate => False);
      Row := Add_Row ("Toggle button", Toggle, Activate => False);
      Row := Add_Row ("Button", Press, Activate => True);
      Row := Add_Row ("Drop down", Choices, Activate => False);
      Row := Add_Row
        ("Not selectable", Gtk_Label_New ("just a label"), Activate => False);
      Row.Set_Selectable (False);

      List.On_Row_Activated (On_Row_Activated'Access);
      List.On_Row_Selected (On_Row_Selected'Access);

      Page.Append (Gtk_Label_New ("Selection mode:"));
      Modes.Set_Halign (Align_Start);
      Single.On_Clicked (On_Single'Access);
      Multi.On_Clicked (On_Multiple'Access);
      None.On_Clicked (On_None'Access);
      Modes.Append (Single);
      Modes.Append (Multi);
      Modes.Append (None);
      Page.Append (Modes);

      Gtk_New (Status, "Nothing happened yet");
      Status.Set_Xalign (0.0);
      Page.Append (Status);
   end Run;

end Create_List_Box_Controls;
