-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
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
--
--  This package implements a toggle button that has a different visual
--  aspect than the standard button. When a Gtk_Check_Item is active, a
--  tick is drawn into the button, instead of the standard "in/out/ aspect.
--
--  Note that the style given could be achieved easily with the standard
--  Gtk_Toggle_Button widget by defining a "theme", ie a special configuration
--  file that specifies a pixmap to use for toggle buttons. However, this
--  Gtk_Check_Item does not need any pixmap, and thus might be a little bit
--  lighter if you need to use a lot of them in your application.
--
--  Note also that the visual aspect of the buttons should probably be left up
--  to the user of your application through configuration files.
--
--  </description>
--  <c_version>gtk+extra0.99.4</c_version>

with Gtk.Toggle_Button;

package Gtk.Extra.Check_Item is

   type Gtk_Check_Item_Record is
     new Gtk.Toggle_Button.Gtk_Toggle_Button_Record with private;
   type Gtk_Check_Item is access all Gtk_Check_Item_Record'Class;

   procedure Gtk_New (Item   : out Gtk_Check_Item;
                      Label  : in  String := "");
   --  Initialize a button.
   --  If Label is "", then no label is created inside the button and
   --  you will have to provide your own child through a call to
   --  Gtk.Container.Add.

   procedure Initialize (Item : access Gtk_Check_Item_Record'Class;
                         Label  : in     String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Check_Item.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Check_Item_Record is
     new Gtk.Toggle_Button.Gtk_Toggle_Button_Record with null record;
   pragma Import (C, Get_Type, "gtk_check_item_get_type");
end Gtk.Extra.Check_Item;
