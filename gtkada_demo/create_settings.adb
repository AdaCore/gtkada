------------------------------------------------------------------------------
--               GtkAda - Ada binding for the Gimp Toolkit                  --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

with GNAT.Strings;
with Glib; use Glib;
with Glib.List_Model;
with Glib.Object; use Glib.Object;
with Glib.Settings; use Glib.Settings;
with Glib.Settings_Backend; use Glib.Settings_Backend;
with Glib.Settings_Schema;
with Glib.Settings_Schema_Source;
with Glib.Variant;
with Gtk.Box; use Gtk.Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Column_View; use Gtk.Column_View;
with Gtk.Column_View_Column; use Gtk.Column_View_Column;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Label; use Gtk.Label;
with Gtk.List_Item; use Gtk.List_Item;
with Gtk.List_View; use Gtk.List_View;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Signal_List_Item_Factory; use Gtk.Signal_List_Item_Factory;
with Gtk.Single_Selection; use Gtk.Single_Selection;
with Gtk.String_List; use Gtk.String_List;
with Gtk.String_Object; use Gtk.String_Object;
with Gtk.Widget; use Gtk.Widget;

package body Create_Settings is
   package Editor_Data is new Glib.Object.User_Data (Boolean);
   package Settings_Data is new Glib.Object.User_Data (Gsettings);
   type Schema_Context is record
      Selection : Gtk_Single_Selection;
      Backend : Gsettings_Backend;
      Status : Gtk_Label;
   end record;
   package Schema_Data is new Glib.Object.User_Data (Schema_Context);

   function Help return String is
     ("Select an installed, non-relocatable GSettings schema on the left."
      & " Its keys appear on the right, using a signal list-item factory."
      & " Boolean and string keys have editors bound directly to GSettings;"
      & " other values are shown in GVariant notation."
      & " Changes live in a private memory backend for this demo session."
      & " Desktop preferences are not changed. The alternative uses columns.");

   procedure Release_Settings (Settings : Gsettings) is
   begin
      Unref (Settings);
   end Release_Settings;

   procedure Release_Context (Context : Schema_Context) is
   begin
      Unref (Context.Backend);
      Unref (Context.Selection);
   end Release_Context;

   procedure Setup_Label
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Label : Gtk_Label;
   begin
      Gtk_New (Label, "");
      Label.Set_Xalign (0.0);
      Label.Set_Margin_Start (8);
      Item.Set_Child (Label);
   end Setup_Label;

   procedure Bind_Label
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Gtk_Label (Item.Get_Child).Set_Text
        (Gtk_String_Object (Item.Get_Item).Get_String);
   end Bind_Label;

   procedure Setup_Editor
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      Box : Gtk_Box;
      Name, Value : Gtk_Label;
      Check : Gtk_Check_Button;
      Editor : Gtk_Entry;
   begin
      Gtk_New (Box, Orientation_Horizontal, 12);
      Box.Set_Size_Request (-1, 48);
      Box.Set_Margin_Start (8);
      Box.Set_Margin_End (8);
      Box.Set_Margin_Top (6);
      Box.Set_Margin_Bottom (6);
      Gtk_New (Name, "");
      Name.Set_Xalign (0.0);
      Name.Set_Size_Request (180, -1);
      Name.Set_Visible (not Editor_Data.Get (Self, Default => False));
      Gtk_New_With_Label (Check, "Enabled");
      Gtk_New (Editor);
      Gtk_New (Value, "");
      Value.Set_Xalign (0.0);
      Value.Set_Wrap (True);
      Box.Append (Name);
      Box.Append (Check);
      Box.Append (Editor);
      Box.Append (Value);
      Item.Set_Child (Box);
   end Setup_Editor;

   procedure Bind_Editor
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Obj : constant Gtk_String_Object := Gtk_String_Object (Item.Get_Item);
      Settings : constant Gsettings := Settings_Data.Get (Obj);
      Key : constant String := Obj.Get_String;
      Value : constant Glib.Variant.Gvariant := Settings.Get_Value (Key);
      Kind : constant String := Glib.Variant.Get_Type_String (Value);
      Name : constant Gtk_Label := Gtk_Label (Item.Get_Child.Get_First_Child);
      Check : constant Gtk_Check_Button := Gtk_Check_Button (Name.Get_Next_Sibling);
      Editor : constant Gtk_Entry := Gtk_Entry (Check.Get_Next_Sibling);
      Display : constant Gtk_Label := Gtk_Label (Editor.Get_Next_Sibling);
   begin
      Name.Set_Text (Key);
      Check.Set_Visible (Kind = "b");
      Editor.Set_Visible (Kind = "s");
      Display.Set_Visible (Kind /= "b" and Kind /= "s");
      if Kind = "b" then
         Settings.Bind (Key, Check, "active", Glib.Settings.Default);
      elsif Kind = "s" then
         Settings.Bind (Key, Editor, "text", Glib.Settings.Default);
      else
         Display.Set_Text (Glib.Variant.Print (Value, True));
      end if;
      Glib.Variant.Unref (Value);
   end Bind_Editor;

   procedure Unbind_Editor
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Name : constant Gtk_Widget := Item.Get_Child.Get_First_Child;
      Check : constant Gtk_Widget := Name.Get_Next_Sibling;
      Editor : constant Gtk_Widget := Check.Get_Next_Sibling;
   begin
      --  Recycled rows must drop the previous key's bindings first.
      Glib.Settings.Unbind (Check, "active");
      Glib.Settings.Unbind (Editor, "text");
   end Unbind_Editor;

   procedure Load_Schema (Context : Schema_Context; Id : String) is
      Source : constant Glib.Settings_Schema_Source.Gsettings_Schema_Source :=
        Glib.Settings_Schema_Source.Get_Default;
      Schema : Glib.Settings_Schema.Gsettings_Schema :=
        Glib.Settings_Schema_Source.Lookup (Source, Id, True);
      Settings : Gsettings;
      Keys : GNAT.Strings.String_List := Glib.Settings_Schema.List_Keys (Schema);
      Model : Gtk.String_List.Gtk_String_List;
      Obj : GObject;
   begin
      G_New_Full (Settings, Schema, Context.Backend);
      Glib.Settings_Schema.Unref (Schema);
      Gtk.String_List.Gtk_New (Model, Keys);
      for Key of Keys loop
         GNAT.Strings.Free (Key);
      end loop;
      for I in 1 .. Model.Get_N_Items loop
         Obj := Model.Get_Item (I - 1);
         Ref (Settings);
         Settings_Data.Set (Obj, Settings, On_Destroyed => Release_Settings'Access);
         Unref (Obj);
      end loop;
      Context.Selection.Set_Model (+Model);
      Unref (Model);
      Unref (Settings);
      Context.Status.Set_Text (Id & " — session changes only");
   end Load_Schema;

   procedure Activate_Schema
     (Self : access Gtk_List_View_Record'Class; Position : Guint)
   is
      Obj : constant GObject := Glib.List_Model.Get_Item
        (Glib.List_Model.Glist_Model (Self.Get_Model), Position);
   begin
      Load_Schema (Schema_Data.Get (Self), Gtk_String_Object (Obj).Get_String);
      Unref (Obj);
   end Activate_Schema;

   procedure Build (Frame : access Gtk.Frame.Gtk_Frame_Record'Class; Columns : Boolean) is
      Source : constant Glib.Settings_Schema_Source.Gsettings_Schema_Source :=
        Glib.Settings_Schema_Source.Get_Default;
      Schemas : Gtk.String_List.Gtk_String_List;
      Schema_Selection, Key_Selection : Gtk_Single_Selection;
      Schema_Factory, Editor_Factory, Key_Factory : Gtk_Signal_List_Item_Factory;
      Schema_View, Key_View : Gtk_List_View;
      Table : Gtk_Column_View;
      Key_Column, Value_Column : Gtk_Column_View_Column;
      Left, Right : Gtk_Scrolled_Window;
      Pane : Gtk_Paned;
      Box : Gtk_Box;
      Status : Gtk_Label;
      Backend : Gsettings_Backend;
   begin
      if Glib.Is_Null (Source) then
         Gtk_New (Status, "No GSettings schemas are installed");
         Frame.Set_Child (Status);
         return;
      end if;
      declare
         Fixed, Relocatable : GNAT.Strings.String_List_Access;
      begin
         Glib.Settings_Schema_Source.List_Schemas (Source, True, Fixed, Relocatable);
         Gtk.String_List.Gtk_New (Schemas, Fixed.all);
         GNAT.Strings.Free (Fixed);
         GNAT.Strings.Free (Relocatable);
      end;
      Gtk_New (Schema_Selection, +Schemas);
      Gtk_New (Schema_Factory);
      Schema_Factory.On_Setup (Setup_Label'Access);
      Schema_Factory.On_Bind (Bind_Label'Access);
      Gtk_New (Schema_View, +Schema_Selection, Schema_Factory);
      Schema_View.Set_Single_Click_Activate (True);
      Gtk_New (Key_Selection, Glib.List_Model.Null_Glist_Model);
      Gtk_New (Editor_Factory);
      Editor_Data.Set (Editor_Factory, Columns);
      Editor_Factory.On_Setup (Setup_Editor'Access);
      Editor_Factory.On_Bind (Bind_Editor'Access);
      Editor_Factory.On_Unbind (Unbind_Editor'Access);
      Gtk_New (Left);
      Left.Set_Size_Request (280, -1);
      Left.Set_Child (Schema_View);
      Gtk_New (Right);
      if Columns then
         Ref (Key_Selection);
         Gtk_New (Table, +Key_Selection);
         Gtk_New (Key_Factory);
         Key_Factory.On_Setup (Setup_Label'Access);
         Key_Factory.On_Bind (Bind_Label'Access);
         Gtk_New (Key_Column, "Key", Key_Factory);
         Gtk_New (Value_Column, "Value", Editor_Factory);
         Value_Column.Set_Expand (True);
         Table.Append_Column (Key_Column);
         Table.Append_Column (Value_Column);
         Unref (Key_Column);
         Unref (Value_Column);
         Right.Set_Child (Table);
      else
         Ref (Key_Selection);
         Gtk_New (Key_View, +Key_Selection, Editor_Factory);
         Right.Set_Child (Key_View);
      end if;
      Gtk_New (Pane, Orientation_Horizontal);
      Pane.Set_Start_Child (Left);
      Pane.Set_End_Child (Right);
      Pane.Set_Vexpand (True);
      Gtk_New (Status, "Select a schema");
      Backend := Memory_New;
      Schema_Data.Set
        (Schema_View, (Key_Selection, Backend, Status),
         On_Destroyed => Release_Context'Access);
      Schema_View.On_Activate (Activate_Schema'Access);
      if Schemas.Get_N_Items > 0 then
         Load_Schema ((Key_Selection, Backend, Status), Schemas.Get_String (0));
      end if;
      Gtk_New (Box, Orientation_Vertical, 6);
      Box.Append (Pane);
      Box.Append (Status);
      Frame.Set_Child (Box);
   end Build;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Build (Frame, False);
   end Run;
   procedure Run_Alternative (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Build (Frame, True);
   end Run_Alternative;
end Create_Settings;
