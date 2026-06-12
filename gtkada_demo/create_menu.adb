------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2026, AdaCore                     --
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

with Glib.Action;              use Glib.Action;
with Glib.Action_Map;          use Glib.Action_Map;
with Glib.Menu;                use Glib.Menu;
with Glib.Object;              use Glib.Object;
with Glib.Simple_Action;       use Glib.Simple_Action;
with Glib.Simple_Action_Group; use Glib.Simple_Action_Group;
with Glib.Variant;             use Glib.Variant;

with Gtk.Box;              use Gtk.Box;
with Gtk.Button;           use Gtk.Button;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Label;            use Gtk.Label;
with Gtk.Menu_Button;      use Gtk.Menu_Button;
with Gtk.Popover_Menu;     use Gtk.Popover_Menu;
with Gtk.Popover_Menu_Bar; use Gtk.Popover_Menu_Bar;
with Gtk.Widget;           use Gtk.Widget;

package body Create_Menu is

   Feedback : Gtk_Label;
   --  Shows the name of the last activated action

   procedure On_Action_Activated
     (Action : access Gsimple_Action_Record'Class; Parameter : Gvariant);
   --  Common handler for all the stateless actions of the demo

   procedure Add_Action
     (Group : Gsimple_Action_Group; Name : String);
   --  Create a stateless action called Name, connect On_Action_Activated to
   --  it and add it to Group.

   procedure On_Context_Button_Clicked
     (Widget : access GObject_Record'Class);
   --  Pop up the context menu attached to the button

   procedure On_Context_Button_Destroy
     (Widget : access GObject_Record'Class);
   --  Unparent the context popover when its anchor button goes away

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "In Gtk4, menus are described by a @bmenu model@B (@bGmenu@B, from"
        & " the @bGlib.Menu@B package): a tree of items, sections and"
        & " submenus, where each item carries a label and the name of an"
        & " @baction@B to activate. Actions (@bGlib.Simple_Action@B) are"
        & " collected in action groups (@bGlib.Simple_Action_Group@B), which"
        & " are made available to a widget hierarchy with"
        & " @bGtk.Widget.Insert_Action_Group@B."
        & ASCII.LF
        & "Three widgets display a menu model: @bGtk_Popover_Menu_Bar@B is"
        & " the horizontal menu bar at the top of a window;"
        & " @bGtk_Menu_Button@B pops up a menu (or any popover) when"
        & " clicked; and @bGtk_Popover_Menu@B is the popover that shows a"
        & " menu model, used directly for contextual menus."
        & ASCII.LF
        & "In this demo, all the menu items activate actions from the same"
        & " action group; the label at the bottom tracks the most recently"
        & " activated action. The @bCheck me@B item is backed by a stateful"
        & " boolean action, which is rendered as a check menu item and"
        & " toggles itself with no handler code.";
   end Help;

   -------------------------
   -- On_Action_Activated --
   -------------------------

   procedure On_Action_Activated
     (Action : access Gsimple_Action_Record'Class; Parameter : Gvariant)
   is
      pragma Unreferenced (Parameter);
   begin
      Feedback.Set_Text
        ("Action ""demo." & Get_Name (+Action) & """ activated");
   end On_Action_Activated;

   ----------------
   -- Add_Action --
   ----------------

   procedure Add_Action
     (Group : Gsimple_Action_Group; Name : String)
   is
      Action : Gsimple_Action;
   begin
      G_New (Action, Name, Parameter_Type => null);
      Action.On_Activate (On_Action_Activated'Access);
      Add_Action (+Group, +Action);
   end Add_Action;

   -------------------------------
   -- On_Context_Button_Clicked --
   -------------------------------

   procedure On_Context_Button_Clicked
     (Widget : access GObject_Record'Class) is
   begin
      Gtk_Popover_Menu (Widget).Popup;
   end On_Context_Button_Clicked;

   -------------------------------
   -- On_Context_Button_Destroy --
   -------------------------------

   procedure On_Context_Button_Destroy
     (Widget : access GObject_Record'Class) is
   begin
      Gtk_Popover_Menu (Widget).Unparent;
   end On_Context_Button_Destroy;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box       : Gtk_Box;
      Group     : Gsimple_Action_Group;
      Check     : Gsimple_Action;

      Bar_Model    : Gmenu;
      File_Menu    : Gmenu;
      Edit_Menu    : Gmenu;
      Extras       : Gmenu;
      Button_Menu  : Gmenu;
      Context_Menu : Gmenu;

      Bar            : Gtk_Popover_Menu_Bar;
      Menu_Button    : Gtk_Menu_Button;
      Context_Button : Gtk_Button;
      Popover        : Gtk_Popover_Menu;
   begin
      Set_Label (Frame, "Menus");

      Gtk_New (Box, Orientation_Vertical, Spacing => 10);
      Box.Set_Homogeneous (False);
      Frame.Set_Child (Box);

      --  The actions that the menu items activate. The group is inserted
      --  on the demo's toplevel box with the "demo" prefix, so every
      --  widget below it (the menu bar, the menu button, the context
      --  popover) resolves "demo.xxx" action names against it.

      G_New (Group);
      Add_Action (Group, "new");
      Add_Action (Group, "open");
      Add_Action (Group, "save");
      Add_Action (Group, "quit");
      Add_Action (Group, "cut");
      Add_Action (Group, "copy");
      Add_Action (Group, "paste");
      Add_Action (Group, "about");

      --  A stateful boolean action with no activate handler: activating
      --  it toggles its state, and menu items bound to it are displayed
      --  as check menu items.
      G_New_Stateful
        (Check, "check", Parameter_Type => null,
         State => Gvariant_New_Boolean (True));
      Add_Action (+Group, +Check);

      Box.Insert_Action_Group ("demo", +Group);

      --  The menu bar, driven by a menu model with two submenus

      G_New (File_Menu);
      File_Menu.Append ("_New", "demo.new");
      File_Menu.Append ("_Open", "demo.open");
      File_Menu.Append ("_Save", "demo.save");

      declare
         Quit_Section : Gmenu;
      begin
         G_New (Quit_Section);
         Quit_Section.Append ("_Quit", "demo.quit");
         File_Menu.Append_Section ("", Quit_Section);
      end;

      G_New (Edit_Menu);
      Edit_Menu.Append ("Cu_t", "demo.cut");
      Edit_Menu.Append ("_Copy", "demo.copy");
      Edit_Menu.Append ("_Paste", "demo.paste");

      G_New (Extras);
      Extras.Append ("Check me", "demo.check");
      Edit_Menu.Append_Section ("", Extras);

      G_New (Bar_Model);
      Bar_Model.Append_Submenu ("_File", File_Menu);
      Bar_Model.Append_Submenu ("_Edit", Edit_Menu);

      Bar := Gtk_Popover_Menu_Bar_New_From_Model (Bar_Model);
      Box.Append (Bar);

      --  A menu button popping up a menu built from a model

      G_New (Button_Menu);
      Button_Menu.Append ("_About", "demo.about");
      Button_Menu.Append ("Check me", "demo.check");

      Gtk_New (Menu_Button);
      Menu_Button.Set_Label ("A Gtk_Menu_Button");
      Menu_Button.Set_Menu_Model (Button_Menu);
      Menu_Button.Set_Halign (Align_Start);
      Menu_Button.Set_Margin_Start (10);
      Box.Append (Menu_Button);

      --  A contextual menu: a Gtk_Popover_Menu parented to a plain button.
      --  Real applications would typically pop it up from a right-click
      --  gesture; the plain click keeps the demo simple.

      G_New (Context_Menu);
      Context_Menu.Append ("_Copy", "demo.copy");
      Context_Menu.Append ("_Paste", "demo.paste");

      Popover := Gtk_Popover_Menu_New_From_Model (Context_Menu);

      Gtk_New (Context_Button, "Click for a contextual menu");
      Context_Button.Set_Halign (Align_Start);
      Context_Button.Set_Margin_Start (10);
      Box.Append (Context_Button);

      Popover.Set_Parent (Context_Button);
      Context_Button.On_Clicked
        (On_Context_Button_Clicked'Access, Popover);
      Context_Button.On_Destroy
        (On_Context_Button_Destroy'Access, Popover);

      --  The feedback label

      Gtk_New (Feedback, "No action activated yet");
      Feedback.Set_Halign (Align_Start);
      Feedback.Set_Margin_Start (10);
      Box.Append (Feedback);
   end Run;

end Create_Menu;
