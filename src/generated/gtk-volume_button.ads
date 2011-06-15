-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
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
--  Gtk_Volume_Button is a subclass of Gtk_Scale_Button that has been tailored
--  for use as a volume control widget with suitable icons, tooltips and
--  accessible labels.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Types;       use Glib.Types;
with Gtk.Activatable;  use Gtk.Activatable;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Orientable;   use Gtk.Orientable;
with Gtk.Scale_Button; use Gtk.Scale_Button;

package Gtk.Volume_Button is

   type Gtk_Volume_Button_Record is new Gtk_Scale_Button_Record with null record;
   type Gtk_Volume_Button is access all Gtk_Volume_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Widget : out Gtk_Volume_Button);
   procedure Initialize (Widget : access Gtk_Volume_Button_Record'Class);

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_volume_button_get_type");

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Activatable"
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Volume_Button_Record, Gtk_Volume_Button);
   function "+"
     (Widget : access Gtk_Volume_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Volume_Button
   renames Implements_Activatable.To_Object;

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Volume_Button_Record, Gtk_Volume_Button);
   function "+"
     (Widget : access Gtk_Volume_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Volume_Button
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Volume_Button_Record, Gtk_Volume_Button);
   function "+"
     (Widget : access Gtk_Volume_Button_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Volume_Button
   renames Implements_Orientable.To_Object;

end Gtk.Volume_Button;
