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

--  <description>
--
--  This package contains high level support for the Glade GUI builder.
--  It provides routines to generate Ada code and create widgets dynamically
--  from an XML definition file.
--  @pxref{Package_Glib.Glade} for the low level GUI builder support.
--  @pxref{Package_Gtk.Util} for the additional API to support easily dynamic
--  loading of files.
--
--  </description>

with Gtk.Object;
with Glib.Glade; use Glib.Glade; use Glib.Glade.Glib_XML;

package Gtk.Glade is

   --  <doc_ignore>

   type Generate_Ptr is access procedure (N : Node_Ptr; File : File_Type);
   type Dynamic_Generate_Ptr is access
     procedure (Object : in out Gtk.Object.Gtk_Object; N : Node_Ptr);

   type Generate_Rec is record
      Gate  : Generate_Ptr;
      Dgate : Dynamic_Generate_Ptr;
   end record;

   procedure Generic_Ptr (N : Node_Ptr; File : File_Type);
   --  Dummy Generate_Ptr that does nothing.

   procedure Generic_DPtr
     (Object : in out Gtk.Object.Gtk_Object; N : Node_Ptr);
   --  Dummy Dynamic_Generate_Ptr that does nothing.
   --  </doc_ignore>

   function Get_Gate (Class : String) return Generate_Ptr;
   --  Return a Generate_Ptr corresponding to a specific Class.
   --  If class isn't found, return a pointer to Generic_Ptr.

   function Get_Dgate (Class : String) return Dynamic_Generate_Ptr;
   --  Return a Dynamic_Generate_Ptr corresponding to a specific Class.
   --  If class isn't found, return a pointer to Generic_DPtr.

   procedure Generate (File : String);
   --  Parse file File and generate the corresponding Ada code on standard
   --  output.

   procedure Generate (N : Node_Ptr);
   --  Generate the Ada code corresponding the creation of to N and its
   --  children on standard output.

   procedure Instantiate (File : String);
   --  Parse File, create and display the corresponding widgets.

   procedure Instantiate (N : Node_Ptr; Display : Boolean := True);
   --  Create the widgets corresponding to N and its children.
   --  If Display is True, display them.

end Gtk.Glade;
