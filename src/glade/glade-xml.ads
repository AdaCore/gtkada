-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  See also glade.ads
--
--  </description>

with System;
with Glib;
with Glib.Object;
with Gtk.Widget; use Gtk.Widget;

package Glade.XML is

   type Glade_XML_Record is new Glib.Object.GObject_Record with private;
   type Glade_XML is access all Glade_XML_Record'Class;

   procedure Gtk_New
     (XML    : out Glade_XML;
      Fname  : String;
      Root   : String := "";
      Domain : String := "");
   --  Create a new GladeXML object (and the corresponding widgets)
   --  from the XML file fname. Optionally it will only build the
   --  interface from the widget node Root (if it is not empty). This
   --  feature is useful if you only want to build say a toolbar or
   --  menu from the XML file, but not the window it is embedded
   --  in. Note also that the XML parse tree is cached to speed up
   --  creating another GladeXML object for the same file.
   --
   --  Domain, if not null, is the international domain to use for
   --  string translation. See Gtkada.Intl for more information.

   procedure Gtk_New_From_Buffer
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

   procedure Initialize_From_Buffer
     (XML    : access Glade_XML_Record'Class;
      Buffer : String;
      Root   : String := "";
      Domain : String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Glade_XML.

   procedure Signal_Connect
     (XML         : access Glade_XML_Record;
      Handlername : String;
      Func        : System.Address;
      User_Data   : System.Address);
   --  Warning: Func should be a lowel level C callback, taking a low level
   --  C widget as the first parameter, e.g:
   --  procedure Func (Widget : Gtk.Item_Factory.Limited_Widget);

   function Get_Widget
     (XML  : access Glade_XML_Record; Name : String) return Gtk_Widget;
   --  This function is used to get the Gtk_Widget corresponding to
   --  name in the interface description. You would use this if you
   --  have to do anything to the widget after loading.

   --  function Get_Widget_Prefix
   --    (XML : access Glade_XML_Record; Name : String) return GList; ???
   --  This function is used to get a list of Gtk_Widgets with names
   --  that start with the string name in the interface
   --  description. You would use this if you have to do something to
   --  all of these widgets after loading.

   function Relative_File
     (XML : access Glade_XML_Record; Filename : String) return String;
   --  This function resolves a relative pathname, using the directory
   --  of the XML file as a base. If the pathname is absolute, then
   --  the original filename is returned.

   function Get_Widget_Name
     (Widget : access Gtk_Widget_Record'Class) return String;

   function Get_Widget_Tree
     (Widget : access Gtk_Widget_Record'Class) return Glade_XML;
   --  This function is used to get the GladeXML object that built
   --  this widget.

private
   type Glade_XML_Record is new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "glade_xml_get_type");
end Glade.XML;
