-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Glib.GObjects;

package Gtk.Text_Mark is

   type Gtk_Text_Mark_Record is new Glib.GObjects.GObject_Record with private;
   type Gtk_Text_Mark is access all Gtk_Text_Mark_Record'Class;

   --  ??? There is no _new function!

   function Get_Type return Gtk.Gtk_Type;

   procedure Set_Visible (Mark    : access Gtk_Text_Mark_Record;
                          Setting :        Boolean := True);

   function Get_Visible (Mark   : access Gtk_Text_Mark_Record)
                         return Boolean;

   function Get_Name (Mark : access Gtk_Text_Mark_Record)
                      return String;
   --  ??? Check that this procedure does not cause any memory leak.

   function Get_Deleted (Mark   : access Gtk_Text_Mark_Record)
                         return Boolean;

   --  function Get_Buffer (Mark   : access Gtk_Text_Mark_Record)
   --                       return Gtk.Text_Buffer.Gtk_Text_Buffer;
   --  ??? Can not be bound here to avoid circular dependency with
   --  ??? Gtk.Text_Buffer.

   function Get_Left_Gravity (Mark   : access Gtk_Text_Mark_Record)
                              return Boolean;

private

   type Gtk_Text_Mark_Record is new Glib.GObjects.GObject_Record with
     null record;

   pragma Import (C, Get_Type, "gtk_text_mark_get_type");

end Gtk.Text_Mark;
