-----------------------------------------------------------------------
--                   Gate - GtkAda Components                        --
--                                                                   --
--                   Copyright (C) 1999-2001                         --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- GATE is free software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package contains high level support for the Glade GUI builder.
--  It provides routines to generate Ada code from an XML definition file.
--  See package Glib.Glade for the low level GUI builder support.

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
   --  output. Note the .glade file should be passed here and not the
   --  .gladep file.

   procedure Generate (Project : Node_Ptr; Interface : Node_Ptr);
   --  Generate the Ada code corresponding the creation of to
   --  Project and Interface and its children on
   --  standard input.

   procedure Register_Generate (Widget : String; Generate : Generate_Ptr);
   --  Register Callback as a procedure that knows how to generate code
   --  for a given Widget class.
   --  Widget is the C string representing the widget, e.g "GnomeCanvas".

end Gtk.Glade;
