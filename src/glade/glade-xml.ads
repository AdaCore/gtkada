-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                       Copyright (C) 2000                          --
--                           ACT-Europe                              --
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
--  This package is a binding to the libglade library that provides routines
--  to create widgets dynamically from an XML definition file.
--  @pxref{Package_Glade}.
--
--  </description>

with System;
with Glib;
with Gtk.Data;
with Gtk.Widget; use Gtk.Widget;

package Glade.XML is

   type Glade_XML_Record is new Gtk.Data.Gtk_Data_Record with private;
   type Glade_XML is access all Glade_XML_Record'Class;

   procedure Gtk_New
     (XML    : out Glade_XML;
      Fname  : String;
      Root   : String := "";
      Domain : String := "");
   --  Create a new Glade_XML.
   --  Fname is the file name of the XML file to load into XML.
   --  Root if not null is the root widget to start from.
   --  Domain if not null is the international domain to use for string
   --  translation. @pxref{Package_Gtkada.Intl} for more information.

   procedure Gtk_New_From_Memory
     (XML    : out Glade_XML;
      Buffer : String;
      Root   : String := "";
      Domain : String := "");
   --  Create a new Glade_XML.
   --  Similar to previous procedure, but the XML contents are read from memory
   --  directly.

   procedure Initialize
     (XML    : access Glade_XML_Record'Class;
      Fname  : String;
      Root   : String := "";
      Domain : String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize_From_Memory
     (XML    : access Glade_XML_Record'Class;
      Buffer : String;
      Root   : String := "";
      Domain : String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Glade_XML.

   procedure Signal_Connect
     (XML         : access Glade_XML_Record;
      Handlername : String;
      Func        : System.Address;
      User_Data   : System.Address);

   procedure Signal_Autoconnect (XML : access Glade_XML_Record);
   --  Use gmodule to connect signals automatically.
   --  Basically a symbol with the name of the signal handler is searched for,
   --  and that is connected to the associated symbols. So setting
   --  gtk_main_quit as a signal handler for the destroy signal of a window
   --  will do what you expect.

   function Get_Widget
     (XML  : access Glade_XML_Record; Name : String) return Gtk_Widget;

   --  function Get_Widget_Prefix
   --    (XML : access Glade_XML_Record; Name : String) return GList; ???

   function Get_Widget_By_Long_Name
     (XML : access Glade_XML_Record; Longname : String) return Gtk_Widget;

   function Relative_File
     (XML : access Glade_XML_Record; Filename : String) return String;

   function Get_Widget_Name
     (Widget : access Gtk_Widget_Record'Class) return String;

   function Get_Widget_Long_Name
     (Widget : access Gtk_Widget_Record'Class) return String;

   function Get_Widget_Tree
     (Widget : access Gtk_Widget_Record'Class) return Glade_XML;

   type Custom_Widget_Handler is access function
     (XML       : access Glade_XML_Record'Class;
      Func_Name : String;
      Name      : String;
      String1   : String;
      String2   : String;
      Int1      : Glib.Gint;
      Int2      : Glib.Gint) return Gtk_Widget;
   --  Interface for changing the custom widget handling

   procedure Set_Custom_Handler (Handler : Custom_Widget_Handler);

private
   type Glade_XML_Record is new Gtk.Data.Gtk_Data_Record with null record;

   pragma Import (C, Get_Type, "glade_xml_get_type");
end Glade.XML;
