-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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

with Gtk.Box;
with Interfaces.C.Strings;
with Glib.GSlist;
with System;

package Gtk.Status_Bar is

   type Gtk_Status_Bar is new Gtk.Box.Gtk_Box with private;
   type Context_Id is new Guint;
   type Message_Id is new Guint;

   type Status_Bar_Msg is
      record
         Text    : Interfaces.C.Strings.chars_ptr;
         Context : Context_Id;
         Message : Message_Id;
      end record;
   function Convert (Msg : Status_Bar_Msg) return System.Address;
   function Convert (Msg : System.Address) return Status_Bar_Msg;
   package Messages_List is new Glib.GSlist.Generic_SList (Status_Bar_Msg);

   procedure Gtk_New (Widget : out Gtk_Status_Bar);

   function Get_Context_Id (Statusbar           : in Gtk_Status_Bar'Class;
                            Context_Description : in String)
                            return Context_Id;

   function Get_Messages (Statusbar : in Gtk_Status_Bar'Class)
                          return Messages_List.GSlist;

   function Push
     (Statusbar : in Gtk_Status_Bar'Class;
      Context   : in Context_Id;
      Text      : in String)
      return Message_Id;

   procedure Pop
     (Statusbar : in Gtk_Status_Bar'Class;
      Context   : in Context_Id);

   procedure Remove (Statusbar  : in Gtk_Status_Bar'Class;
                     Context    : in Context_Id;
                     Message    : in Message_Id);



private

   type Gtk_Status_Bar is new Gtk.Box.Gtk_Box with null record;

end Gtk.Status_Bar;
