-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

--  A Gtk_Toggle_Button is like a regular button, but can be in one of
--  two states, "active" or "inactive". Its visual aspect is modified
--  when the state is changed.
--
--  You should consider using a Gtk_Check_Button instead, since it looks
--  nicer and provides more visual clues that the button can be
--  toggled.

--  </description>
--  <c_version> 1.2.6 </c_version>
--  <screenshot>togglebutton</screenshot>

with Gtk.Button;

package Gtk.Toggle_Button is

   type Gtk_Toggle_Button_Record is new Gtk.Button.Gtk_Button_Record
     with private;
   type Gtk_Toggle_Button is access all Gtk_Toggle_Button_Record'Class;

   procedure Gtk_New (Toggle_Button : out Gtk_Toggle_Button;
                      Label         : in String := "");
   --  Initialize a button.
   --  If Label is "", then no label is created inside the button and
   --  you will have to provide your own child through a call to
   --  Gtk.Container.Add. This is the recommended way to put a pixmap
   --  inside a toggle button.

   procedure Initialize (Toggle_Button : access Gtk_Toggle_Button_Record'Class;
                         Label         : in String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Toggle_Button.

   procedure Set_Mode (Toggle_Button  : access Gtk_Toggle_Button_Record;
                       Draw_Indicator : in Boolean);
   --  Change the mode of the button.
   --  If Draw_Indicator is False, then the button is hidden.

   procedure Set_Active (Toggle_Button : access Gtk_Toggle_Button_Record;
                         Is_Active     : in Boolean);
   --  Change the state of the button.
   --  When Is_Active is True, the button is drawn as a pressed button.

   function Get_Active (Toggle_Button : access Gtk_Toggle_Button_Record)
                      return Boolean;
   --  Return true if the button is in its active state, i.e is pressed.

   function Is_Active (Toggle_Button : access Gtk_Toggle_Button_Record)
                      return Boolean;
   --  Deprecated: this is the old name of Get_Active.

   ----------------------
   -- Signals emission --
   ----------------------

   procedure Toggled (Toggle_Button : access Gtk_Toggle_Button_Record);
   --  Emit the toggled signal on this widget.
   --  Note that the state of the button is not changed, only the callbacks
   --  are called.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "toggled"
   --    procedure Handler (Toggle : access Gtk_Toggle_Button_Record'Class);
   --
   --    This signal is emitted every time the state of the button is
   --    modified.
   --  </signals>

private
   type Gtk_Toggle_Button_Record is new Gtk.Button.Gtk_Button_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_toggle_button_get_type");
end Gtk.Toggle_Button;

--  <example>
--  <include>../examples/documentation/toggle.adb</include>
--  </example>
