-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                      Copyright (C) 1999                           --
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

with Ada.Text_IO; use Ada.Text_IO;
with Glib.XML;

package Glib.Glade is

   type XML_Data is record
      Created : Boolean := False;
      --  True if the corresponding object has been created
   end record;

   package Glib_XML is new Glib.XML (XML_Data);
   use Glib_XML;

   function Find_Parent (N : Node_Ptr; Class : String) return Node_Ptr;
   --  Find a node in the ancestors of N with a given class

   function To_Ada (S : String; Separator : Character := '_') return String;
   --  Convert S by adding a separator before each upper case character and
   --  by putting in upper case each character following a separator.

   function Get_Part
     (S : String; Part : Positive; Separator : Character := ':') return String;
   --  Get the Part-th part of S delimited by Separator

   procedure Gen_Set
     (N : Node_Ptr; Class, Name : String;
      File : File_Type; Delim : Character := ' ');
   --  Generate a Set_<Name> call in File. If Delim is not a space, Name is
   --  surrounded by it.

   procedure Gen_Set
     (N : Node_Ptr; Class, Name, Field1, Field2, Field3 : String;
      File : File_Type);
   --  Generate a Set_<Name> (Field1, Field2) call in File if Field3 is a null
   --  string or Set_<Name> (Field1, Field2, Field3) otherwise.

   procedure Gen_New
     (N : Node_Ptr; Class, Param1, Param2, New_Name : String := "";
      File : File_Type; Delim : Character := ' ');
   --  Output a call to Gtk_New in File. ??? Need more comments

   procedure Gen_Child (N, Child : Node_Ptr; File : File_Type);
   --  Output an assignment in File. ??? Need more comments here

   procedure Gen_Call_Child (N, Child : Node_Ptr;
     Class, Call : String;
     Param1, Param2, Param3 : String := "";
     File : File_Type);
   --  Output a call to Call in File. ??? Need more comments here

   procedure Gen_Packages (File : File_Type);
   --  Output to file all the packages that have been referenced in previous
   --  calls to the Gen_* procedures. The output has the form:
   --
   --  with Gtk.xxx; use Gtk.xxx;
   --  with Gtk.yyy; use Gtk.yyy;

   procedure Reset_Packages;
   --  Reset the global table of packages

   procedure Gen_Signal (N : Node_Ptr; File : File_Type);
   --  Output to file calls to connect if N contains any signal
   --  Also register the class of the widget that uses signals.

   function Gen_Signal_Instanciations (File : File_Type) return Natural;
   --  Output to file all the instanciations of Gtk.Signal that have been
   --  referenced in previous calls to Gen_Signal.
   --  Return the number of instanciations generated.

end Glib.Glade;
