-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                       Copyright (C) 1999                          --
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

--  This package contains support for the Glade GUI builder. It provides
--  routines to generate Ada code and create widgets dynamically from an
--  XML definition file.

with Gtk.Object;
with Glib.Glade; use Glib.Glade; use Glib.Glade.Glib_XML;

package Gtk.Glade is

   type Generate_Ptr is access procedure (N : Node_Ptr; File : File_Type);
   type Dynamic_Generate_Ptr is access
     procedure (Object : in out Gtk.Object.Gtk_Object; N : Node_Ptr);

   type Generate_Rec is record
      Gate  : Generate_Ptr;
      Dgate : Dynamic_Generate_Ptr;
   end record;
 
   function Get_Gate (Class : String) return Generate_Ptr;
   --  Return a Generate_Ptr corresponding to a specific Class.
   --  If class isn't found, return null.

   function Get_Dgate (Class : String) return Dynamic_Generate_Ptr;
   --  Return a Dynamic_Generate_Ptr corresponding to a specific Class.
   --  If class isn't found, return null.

   procedure Generate (File : String);
   --  Parse file File and generate the corresponding Ada code on standard
   --  output.

   procedure Generate (N : Node_Ptr);
   --  Generate the Ada code corresponding the creation of to N and its
   --  children on standard output.

   procedure Instanciate (File : String);
   --  Parse File and create the corresponding widgets.

   procedure Instanciate (N : Node_Ptr);
   --  Create the widgets corresponding to N and its children.

end Gtk.Glade;
