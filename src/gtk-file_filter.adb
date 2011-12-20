------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C.Strings;  use Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.File_Filter is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_File_Filter_Record);
   pragma Warnings (Off, Type_Conversion);

   ----------------
   -- Add_Custom --
   ----------------

   procedure Add_Custom
     (Filter : access Gtk_File_Filter_Record;
      Needed : File_Filter_Flags;
      Func   : File_Filter_Func;
      Data   : System.Address := System.Null_Address;
      Notify : G_Destroy_Notify_Address := null)
   is
      procedure Internal
        (Filter : System.Address;
         Needed : File_Filter_Flags;
         Func   : File_Filter_Func;
         Data   : System.Address;
         Notify : G_Destroy_Notify_Address);
      pragma Import (C, Internal, "gtk_file_filter_add_custom");
   begin
      Internal (Get_Object (Filter), Needed, Func, Data, Notify);
   end Add_Custom;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Info : File_Filter_Info) return String is
      function Internal (Info : File_Filter_Info) return chars_ptr;
      pragma Import (C, Internal, "ada_file_filter_info_get_filename");
      Result : constant chars_ptr := Internal (Info);
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Filename;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri (Info : File_Filter_Info) return String is
      function Internal (Info : File_Filter_Info) return chars_ptr;
      pragma Import (C, Internal, "ada_file_filter_info_get_uri");
      Result : constant chars_ptr := Internal (Info);
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Uri;

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name (Info : File_Filter_Info) return String is
      function Internal (Info : File_Filter_Info) return chars_ptr;
      pragma Import (C, Internal, "ada_file_filter_info_get_display_name");
      Result : constant chars_ptr := Internal (Info);
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Display_Name;

   -------------------
   -- Get_Mime_Type --
   -------------------

   function Get_Mime_Type (Info : File_Filter_Info) return String is
      function Internal (Info : File_Filter_Info) return chars_ptr;
      pragma Import (C, Internal, "ada_file_filter_info_get_mime_type");
      Result : constant chars_ptr := Internal (Info);
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Mime_Type;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Filter : access Gtk_File_Filter_Record)
      return String
   is
      function Internal (Filter : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_file_filter_get_name");
      Result : constant chars_ptr := Internal (Get_Object (Filter));
   begin
      if Result = Null_Ptr then
         return "";
      else
         --  Returned value owned by gtk+
         return Value (Result);
      end if;
   end Get_Name;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Filter : out Gtk_File_Filter) is
   begin
      Filter := new Gtk_File_Filter_Record;
      Gtk.File_Filter.Initialize (Filter);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Filter : access Gtk_File_Filter_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_file_filter_new");
   begin
      Set_Object (Filter, Internal);
   end Initialize;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Filter : access Gtk_File_Filter_Record;
      Name   : String)
   is
      procedure Internal (Filter : System.Address; Name   : String);
      pragma Import (C, Internal, "gtk_file_filter_set_name");
   begin
      Internal (Get_Object (Filter), Name & ASCII.NUL);
   end Set_Name;

   -------------------
   -- Add_Mime_Type --
   -------------------

   procedure Add_Mime_Type
     (Filter    : access Gtk_File_Filter_Record;
      Mime_Type : String)
   is
      procedure Internal
        (Filter    : System.Address;
         Mime_Type : String);
      pragma Import (C, Internal, "gtk_file_filter_add_mime_type");
   begin
      Internal (Get_Object (Filter), Mime_Type & ASCII.NUL);
   end Add_Mime_Type;

   -----------------
   -- Add_Pattern --
   -----------------

   procedure Add_Pattern
     (Filter  : access Gtk_File_Filter_Record;
      Pattern : String)
   is
      procedure Internal
        (Filter  : System.Address;
         Pattern : String);
      pragma Import (C, Internal, "gtk_file_filter_add_pattern");
   begin
      Internal (Get_Object (Filter), Pattern & ASCII.NUL);
   end Add_Pattern;

   ------------------------
   -- Add_Pixbuf_Formats --
   ------------------------

   procedure Add_Pixbuf_Formats
     (Filter : access Gtk_File_Filter_Record)
   is
      procedure Internal
        (Filter : System.Address);
      pragma Import (C, Internal, "gtk_file_filter_add_pixbuf_formats");
   begin
      Internal (Get_Object (Filter));
   end Add_Pixbuf_Formats;

end Gtk.File_Filter;
