-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1999-2000                       --
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
--  This package contains high level support for the Glade GUI builder.
--  It provides routines to generate Ada code and create widgets dynamically
--  from an XML definition file.
--  @pxref{Package_Glib.Glade} for the low level GUI builder support.
--  </description>

with Ada.Text_IO; use Ada.Text_IO;
with Glib.Glade; use Glib.Glade; use Glib.Glade.Glib_XML;

package Gtk.Glade is

   ---------------------
   -- Code Generation --
   ---------------------

   --  <doc_ignore>

   type Generate_Ptr is access procedure (N : Node_Ptr; File : File_Type);

   procedure Generic_Ptr (N : Node_Ptr; File : File_Type);
   --  Dummy Generate_Ptr that does nothing.

   --  </doc_ignore>

   function Get_Gate (Class : String) return Generate_Ptr;
   --  Return a Generate_Ptr corresponding to a specific Class.
   --  If class isn't found, return a pointer to Generic_Ptr.

   procedure Generate (File : String);
   --  Parse file File and generate the corresponding Ada code on standard
   --  output.

   procedure Generate (N : Node_Ptr);
   --  Generate the Ada code corresponding the creation of to N and its
   --  children on standard output.

   procedure Register_Generate (Widget : String; Generate : Generate_Ptr);
   --  Register Callback as a procedure that knows how to generate code
   --  for a given Widget class.
   --  Widget is the C string representing the widget, e.g "GnomeCanvas".

end Gtk.Glade;
