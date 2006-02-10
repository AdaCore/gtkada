-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2000-2006                        --
--                           AdaCore                                 --
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

with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gtk; use Gtk;
with Gtk.Type_Conversion;
pragma Elaborate_All (Gtk.Type_Conversion);
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Glade.XML is

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget
     (XML  : access Glade_XML_Record;
      Name : String) return Gtk_Widget
   is
      function Internal
        (XML  : System.Address;
         Name : String) return System.Address;
      pragma Import (C, Internal, "glade_xml_get_widget");

   begin
      return Convert (Internal (Get_Object (XML), Name & ASCII.NUL));
   end Get_Widget;

   ---------------------
   -- Get_Widget_Name --
   ---------------------

   function Get_Widget_Name
     (Widget : access Gtk_Widget_Record'Class) return String
   is
      function Internal (Widget : System.Address) return chars_ptr;
      pragma Import (C, Internal, "glade_get_widget_name");

   begin
      return Value (Internal (Get_Object (Widget)));
   end Get_Widget_Name;

   ---------------------
   -- Get_Widget_Tree --
   ---------------------

   function Get_Widget_Tree
     (Widget : access Gtk_Widget_Record'Class) return Glade_XML
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "glade_get_widget_tree");

      Stub : Glade_XML_Record;

   begin
      return Glade_XML (Get_User_Data (Internal (Get_Object (Widget)), Stub));
   end Get_Widget_Tree;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (XML    : out Glade_XML;
      Fname  : String;
      Root   : String := "";
      Domain : String := "") is
   begin
      XML := new Glade_XML_Record;
      Initialize (XML, Fname, Root, Domain);
   end Gtk_New;

   -------------------------
   -- Gtk_New_From_Buffer --
   -------------------------

   procedure Gtk_New_From_Buffer
     (XML    : out Glade_XML;
      Buffer : String;
      Root   : String := "";
      Domain : String := "") is
   begin
      XML := new Glade_XML_Record;
      Initialize_From_Buffer (XML, Buffer, Root, Domain);
   end Gtk_New_From_Buffer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (XML    : access Glade_XML_Record'Class;
      Fname  : String;
      Root   : String := "";
      Domain : String := "")
   is
      function Internal
        (Fname  : String;
         Root   : System.Address;
         Domain : System.Address) return System.Address;
      pragma Import (C, Internal, "glade_xml_new");

      Str1           : aliased constant String := Root & ASCII.NUL;
      Root_Address   : System.Address := Str1'Address;
      Str2           : aliased constant String := Domain & ASCII.NUL;
      Domain_Address : System.Address := Str2'Address;

   begin
      if Root = "" then
         Root_Address := System.Null_Address;
      end if;

      if Domain = "" then
         Domain_Address := System.Null_Address;
      end if;

      Set_Object
        (XML, Internal (Fname & ASCII.NUL, Root_Address, Domain_Address));
   end Initialize;

   ----------------------------
   -- Initialize_From_Buffer --
   ----------------------------

   procedure Initialize_From_Buffer
     (XML    : access Glade_XML_Record'Class;
      Buffer : String;
      Root   : String := "";
      Domain : String := "")
   is
      function Internal
        (Buffer : String;
         Size   : Integer;
         Root   : System.Address;
         Domain : System.Address) return System.Address;
      pragma Import (C, Internal, "glade_xml_new_from_buffer");

      Str1  : aliased constant String := Root & ASCII.NUL;
      Addr1 : System.Address := Str1'Address;
      Str2  : aliased constant String := Domain & ASCII.NUL;
      Addr2 : System.Address := Str2'Address;

   begin
      if Root = "" then
         Addr1 := System.Null_Address;
      end if;

      if Domain = "" then
         Addr2 := System.Null_Address;
      end if;

      Set_Object (XML, Internal (Buffer, Buffer'Length, Addr1, Addr2));
   end Initialize_From_Buffer;

   -------------------
   -- Relative_File --
   -------------------

   function Relative_File
     (XML : access Glade_XML_Record;
      Filename : String) return String
   is
      function Internal
        (Widget : System.Address; Filename : String) return chars_ptr;
      pragma Import (C, Internal, "glade_xml_relative_file");

   begin
      return Value (Internal (Get_Object (XML), Filename & ASCII.NUL));
   end Relative_File;

   --------------------
   -- Signal_Connect --
   --------------------

   procedure Signal_Connect
     (XML         : access Glade_XML_Record;
      Handlername : String;
      Func        : System.Address;
      User_Data   : System.Address)
   is
      procedure Internal
        (XML     : System.Address;
         Handler : String;
         Func    : System.Address;
         Data    : System.Address);
      pragma Import (C, Internal, "glade_xml_signal_connect_data");

   begin
      Internal (Get_Object (XML), Handlername & ASCII.NUL, Func, User_Data);
   end Signal_Connect;

begin
   Gtk.Type_Conversion.Init;
end Glade.XML;
