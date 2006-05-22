-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2003 ACT-Europe                 --
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
--  The Gtk_Combo widget consists of a single-line text entry field and a
--  drop-down list. The drop-down list is displayed when the user clicks on a
--  small arrow button to the right of the entry field.
--
--  The drop-down list is a Gtk_List widget and can be accessed using the list
--  member of the Gtk_Combo. List elements can contain arbitrary widgets, but
--  if an element is not a plain label, then you must use the
--  Gtk_List.Set_Item_String function. This sets the string which will be
--  placed in the text entry field when the item is selected.
--
--  By default, the user can step through the items in the list using the arrow
--  (cursor) keys, though this behaviour can be turned off with Set_Use_Arrows.
--
--  Normally the arrow keys are only active when the contents of the text entry
--  field matches one of the items in the list. If the contents of the entry
--  field do not match any of the list items, then pressing the arrow keys does
--  nothing. However, by calling Set_Use_Arrows_Always you can specify that the
--  arrow keys are always active. If the contents of the entry field does not
--  match any of the items in the list, then pressing the up or down arrow key
--  will set the entry field to the last or first item in the list,
--  respectively.
--  </description>
--  <c_version>1.3.11</c_version>

with Glib.Properties;
with Gtk.GEntry;
pragma Warnings (Off);  --  Gtk.List is obsolescent
with Gtk.List;
pragma Warnings (On);
with Gtk.Box;
with Gtk.Item;
with Gtk.Window;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Combo is

   type Gtk_Combo_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Combo is access all Gtk_Combo_Record'Class;

   procedure Gtk_New (Combo_Box : out Gtk_Combo);
   --  Create a new Gtk_Combo.

   procedure Initialize (Combo_Box : access Gtk_Combo_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Combo.

   procedure Set_Value_In_List
     (Combo_Box   : access Gtk_Combo_Record;
      Val         : Boolean := True;
      Ok_If_Empty : Boolean := False);
   --  Specify whether the value entered in the text entry field must match one
   --  of the values in the list. If this is set then the user will not be able
   --  to perform any other action until a valid value has been entered.
   --  If an empty field is acceptable, the Ok_If_Empty parameter should be
   --  True.
   --  If the value entered must match one of the values in the list, val
   --  should be True.

   procedure Set_Use_Arrows
     (Combo_Box : access Gtk_Combo_Record; Val : Boolean := True);
   --  Specify if the arrow (cursor) keys can be used to step through the
   --  items in the list. This is on by default.

   procedure Set_Use_Arrows_Always
     (Combo_Box : access Gtk_Combo_Record; Val : Boolean := True);
   --  Specify if the arrow keys will still work even if the current contents
   --  of the Gtk_Entry field do not match any of the list items.

   procedure Set_Case_Sensitive
     (Combo_Box : access Gtk_Combo_Record; Val : Boolean := True);
   --  Specify whether the text entered into the Gtk_Entry field and the text
   --  in the list items are case sensitive.
   --  This may be useful, for example, when you have called Set_Value_In_List
   --  to limit the values entered, but you are not worried about differences
   --  in case.

   procedure Set_Item_String
     (Combo_Box  : access Gtk_Combo_Record;
      Item       : Gtk.Item.Gtk_Item;
      Item_Value : UTF8_String);
   --  Set the string to place in the Gtk_Entry field when a particular list
   --  item is selected. This is needed if the list item is not a simple label.

   procedure Set_Popdown_Strings
     (Combo_Box : access Gtk_Combo_Record;
      Strings   : String_List.Glist);
   --  Set all the items in the popup list.

   procedure Disable_Activate (Combo_Box : access Gtk_Combo_Record);
   --  Disable the standard handler for the <return> key in the entry field.
   --  The default behavior is to popdown the combo box list, so that the user
   --  can choose from it. However, if you want to add your own callback
   --  for the return key, you need to call this subprogram, and connect
   --  a handler to the "activate" signal for the entry.

   function Get_Entry
     (Combo_Box : access Gtk_Combo_Record) return Gtk.GEntry.Gtk_Entry;
   --  Return the Gtk_Entry associated with a Combo_Box.

   procedure Set_Entry
     (Combo_Box : access Gtk_Combo_Record;
      GEntry    : Gtk.GEntry.Gtk_Entry);
   --  Set the entry field for the combo box.

   function Get_List
     (Combo_Box : access Gtk_Combo_Record) return Gtk.List.Gtk_List;
   --  Return the list of items associated with a Combo_Box.
   --  Add (Gtk.Container.Add) Gtk_List_Items to this list to insert new
   --  entries in the popdown menu.

   function Get_Popup_Window
     (Combo_Box : access Gtk_Combo_Record) return Gtk.Window.Gtk_Window;
   --  Return the popup window associated with a Combo_Box.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Enable_Arrow_Keys_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether the arrow keys move through the list of items
   --    See also:  Set_Use_Arrows
   --
   --  - Name:  Enable_Arrow_Always_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether the arrow keys work, even if the entry contents are
   --           not in the list
   --    See also:  Set_Use_Arrows_Always
   --
   --  - Name:  Case_Sensitive_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether list item matching is case sensitive
   --    See also:  Set_Case_Sensitive
   --
   --  </properties>

   Enable_Arrow_Keys_Property   : constant Glib.Properties.Property_Boolean;
   Enable_Arrow_Always_Property : constant Glib.Properties.Property_Boolean;
   Case_Sensitive_Property      : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Combo_Record is new Gtk.Box.Gtk_Box_Record with null record;

   Enable_Arrow_Keys_Property   : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable_arrow_keys");
   Enable_Arrow_Always_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable_arrow_always");
   Case_Sensitive_Property      : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("case_sensitive");

   pragma Import (C, Get_Type, "gtk_combo_get_type");
end Gtk.Combo;

--  <example>
--  Creating a Gtk_Combo widget with simple text items.
--
--    Combo : Gtk_Combo;
--    Items : String_List.Glist;
--
--    String_List.Append (Items, "First Item");
--    String_List.Append (Items, "Second Item");
--    String_List.Append (Items, "Third Item");
--    String_List.Append (Items, "Fourth Item");
--    String_List.Append (Items, "Fifth Item");
--
--    Gtk_New (Combo);
--    Set_Popdown_Strings (Combo, Items);
--    Free (Items);
--  </example>
