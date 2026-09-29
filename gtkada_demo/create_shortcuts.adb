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

with Glib;                    use Glib;
with Glib.Variant;            use Glib.Variant;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Callback_Action;     use Gtk.Callback_Action;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Event_Controller;    use Gtk.Event_Controller;
with Gtk.Frame;               use Gtk.Frame;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Label;               use Gtk.Label;
with Gtk.Shortcut;            use Gtk.Shortcut;
with Gtk.Shortcut_Controller; use Gtk.Shortcut_Controller;
with Gtk.Shortcut_Trigger;    use Gtk.Shortcut_Trigger;
with Gtk.Widget;              use Gtk.Widget;

package body Create_Shortcuts is

   Status : Gtk_Label;
   --  Reports the shortcut that fired last

   function Activated
     (Widget : Gtk_Widget; Args : Gvariant; Name : String) return Gboolean;
   --  Callback of every shortcut: say which one fired

   package Named_Action is new Callback_Action_With_Data (String);

   procedure Add
     (Controller  : not null access Gtk_Shortcut_Controller_Record'Class;
      List        : not null access Gtk_Box_Record'Class;
      Trigger     : String;
      Description : String);
   --  Add a shortcut for Trigger to Controller, and a line saying what it is
   --  to List

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Shortcut@B pairs a trigger, the keys that fire it, with an"
        & " action. A @bGtk_Shortcut_Controller@B holds a set of them and"
        & " is attached to a widget with @bAdd_Controller@B."
        & ASCII.LF
        & "Triggers here are written as strings, such as"
        & " @b<Control><Shift>a@B or @bF5@B, and parsed by"
        & " @bGtk_Shortcut_Trigger.Gtk_New@B; the label of each line shows"
        & " the parsed trigger printed back. The action is a"
        & " @bGtk_Callback_Action@B running an Ada function."
        & ASCII.LF
        & "The controller has the @bLocal@B scope, so it only reacts while"
        & " focus is inside the page: click the entry, then press one of the"
        & " combinations. It listens in the capture phase, which lets it"
        & " win over the entry's own bindings, Ctrl+A included.";
   end Help;

   ---------------
   -- Activated --
   ---------------

   function Activated
     (Widget : Gtk_Widget; Args : Gvariant; Name : String) return Gboolean
   is
      pragma Unreferenced (Widget, Args);
   begin
      Status.Set_Text ("Activated: " & Name);
      return 1;
   end Activated;

   ---------
   -- Add --
   ---------

   procedure Add
     (Controller  : not null access Gtk_Shortcut_Controller_Record'Class;
      List        : not null access Gtk_Box_Record'Class;
      Trigger     : String;
      Description : String)
   is
      T      : Gtk_Shortcut_Trigger;
      Action : Gtk_Callback_Action;
      Line   : constant Gtk_Box := Gtk_Box_New (Orientation_Horizontal, 12);
      Keys   : Gtk_Label;
      Text   : constant Gtk_Label := Gtk_Label_New (Description);
   begin
      Gtk_New (T, Trigger);
      Named_Action.Gtk_New (Action, Activated'Access, Description);

      --  Both are consumed by the shortcut, which the controller then owns
      Controller.Add_Shortcut (Gtk_Shortcut_New (T, Action));

      Gtk_New (Keys, Trigger);
      Keys.Set_Xalign (0.0);
      Keys.Set_Width_Chars (24);
      Keys.Add_Css_Class ("monospace");
      Text.Set_Xalign (0.0);
      Line.Append (Keys);
      Line.Append (Text);
      List.Append (Line);
   end Add;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Page       : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 12);
      List       : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 4);
      Input      : constant Gtk_Entry := Gtk_Entry_New;
      Controller : constant Gtk_Shortcut_Controller :=
        Gtk_Shortcut_Controller_New;
   begin
      Frame.Set_Label ("Shortcuts");

      Page.Set_Margin_Top (12);
      Page.Set_Margin_Bottom (12);
      Page.Set_Margin_Start (12);
      Page.Set_Margin_End (12);
      Frame.Set_Child (Page);

      Add (Controller, List, "<Control>a", "Select everything");
      Add (Controller, List, "<Control><Shift>a", "Select nothing");
      Add (Controller, List, "F5", "Refresh");
      Add (Controller, List, "<Alt>q", "Quit");
      Add (Controller, List, "<Control>Return", "Send");

      Page.Append
        (Gtk_Label_New
           ("Focus the entry, then press one of these combinations:"));
      Page.Append (List);

      Input.Set_Placeholder_Text ("Click here to give the page focus");
      Page.Append (Input);

      Gtk_New (Status, "Nothing activated yet");
      Status.Set_Xalign (0.0);
      Page.Append (Status);

      Controller.Set_Scope (Local);
      Gtk_Event_Controller (Controller).Set_Propagation_Phase (Phase_Capture);
      Page.Add_Controller (Controller);
   end Run;

end Create_Shortcuts;
