-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--
--  A Gtk_Radio_Button is a simple button that has two states, like a
--  Gtk_Toggle_Button.
--  However, Gtk_Radio_Buttons can be grouped together to get a special
--  behavior: only one button in the group can be active at any given time.
--  Thus, when the user selects one of the buttons from the group, the button
--  that was previously selected is disabled.
--
--  The radio buttons always belongs to a group, even if there is only one in
--  this group
--
--  </description>
--  <c_version>1.3.6</c_version>

with Gtk.Check_Button;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Radio_Button is

   type Gtk_Radio_Button_Record is new Check_Button.Gtk_Check_Button_Record
     with private;
   type Gtk_Radio_Button is access all Gtk_Radio_Button_Record'Class;

   procedure Gtk_New
     (Radio_Button : out Gtk_Radio_Button;
      Group        : Widget_SList.GSlist := Widget_SList.Null_List;
      Label        : String := "");
   --  Create a new radio button, belonging to Group.
   --  If Label is left as the empty string, then the button will not have any
   --  child and you are free to put any thing you want in it, including a
   --  pixmap.
   --  To initialize the group (when creating the first button), leave Group
   --  to the Null_List. You can later get the new group that is created with
   --  a call to the Group subprogram below.

   procedure Gtk_New
     (Radio_Button : out Gtk_Radio_Button;
      Group        : Gtk_Radio_Button;
      Label        : String := "");
   --  Create a new radio button in the same group as Group.
   --  If Label is left as the empty string, Radio_Button is created without
   --  any child and you can put whatever you want in it, including a pixmap.
   --
   --  To initialize a new group (when creating the first button), you should
   --  pass it null or a button that has not been created with Gtk_New, as in
   --  the example below.

   procedure Gtk_New_With_Mnemonic
     (Radio_Button : out Gtk_Radio_Button;
      Group        : Widget_SList.GSlist := Widget_SList.Null_List;
      Label        : String);
   --  Create a new Gtk_Radio_Button containing a Label. The Label is created
   --  using Gtk.Label.New_With_Mnemonic, so underscores in Label indicate
   --  the mnemonic for the button.
   --
   --  To initialize a new group (when creating the first button), you should
   --  pass it null or a button that has not been created with Gtk_New, as in
   --  the example below.

   procedure Gtk_New_With_Mnemonic
     (Radio_Button : out Gtk_Radio_Button;
      Group        : Gtk_Radio_Button;
      Label        : String);
   --  Create a new radio button in the same group as Group. The label is
   --  created using Gtk.Label.New_With_Mnemonic, so underscores in Label
   --  indicate the mnemonic for the button.
   --
   --  To initialize a new group (when creating the first button), you should
   --  pass it null or a button that has not been created with Gtk_New, as in
   --  the example below.


   procedure Initialize
     (Radio_Button : access Gtk_Radio_Button_Record'Class;
      Group        : Widget_SList.GSlist;
      Label        : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize
     (Radio_Button : access Gtk_Radio_Button_Record'Class;
      Group        : Gtk_Radio_Button;
      Label        : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize_With_Mnemonic
     (Radio_Button : access Gtk_Radio_Button_Record'Class;
      Group        : Widget_SList.GSlist;
      Label        : String);

   procedure Initialize_With_Mnemonic
     (Radio_Button : access Gtk_Radio_Button_Record'Class;
      Group        : Gtk_Radio_Button;
      Label        : String);

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Radio_Button.

   function Get_Group
     (Radio_Button : access Gtk_Radio_Button_Record)
      return Widget_SList.GSlist;
   --  Return the group to which Radio_Button belongs.
   --  This can be used as an argument to the first version of Gtk_New above,
   --  or the list can also be traversed to get all the buttons.

   function Group
     (Radio_Button : access Gtk_Radio_Button_Record)
     return Widget_SList.GSlist
     renames Get_Group;
   --  This function is deprecated.

   procedure Set_Group
     (Radio_Button : access Gtk_Radio_Button_Record;
      Group        : Widget_SList.GSlist);
   --  Modify the group to which the button belongs.
   --  This will not change anything visually.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Radio_Button_Record is new Check_Button.Gtk_Check_Button_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_radio_button_get_type");
end Gtk.Radio_Button;

--  <example>
--  --  This creates a group of two buttons. Note how the group is initialized.
--
--  declare
--     Radio_Button : Gtk_Radio_Button;
--  begin
--     Gtk_New (Radio_Button,
--              Group       => Radio_Button,
--              Label       => "First button");
--     Gtk_New (Radio_Button,
--              Group       => Radio_Button,
--              Label       => "Second button");
--  end;
--  </example>
