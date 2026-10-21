------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Css_Section is

   function From_Object_Free
     (B : access Gtk_Css_Section'Class) return Gtk_Css_Section
   is
      Result : constant Gtk_Css_Section := Gtk_Css_Section (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Css_Section is
      S : Gtk_Css_Section;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -------------------------
   -- Gtk_Css_Section_New --
   -------------------------

   function Gtk_Css_Section_New
      (File    : Glib.GFile.Gfile;
       Start   : Gtk_Css_Location;
       The_End : Gtk_Css_Location) return Gtk_Css_Section
   is
      function Internal
         (File    : Glib.GFile.Gfile;
          Start   : Gtk_Css_Location;
          The_End : Gtk_Css_Location) return System.Address;
      pragma Import (C, Internal, "gtk_css_section_new");
      Self : Gtk_Css_Section;
   begin
      Self.Set_Object (Internal (File, Start, The_End));
      return Self;
   end Gtk_Css_Section_New;

   ------------------------------------
   -- Gtk_Css_Section_New_With_Bytes --
   ------------------------------------

   function Gtk_Css_Section_New_With_Bytes
      (File    : Glib.GFile.Gfile;
       Bytes   : Glib.Bytes.Gbytes;
       Start   : Gtk_Css_Location;
       The_End : Gtk_Css_Location) return Gtk_Css_Section
   is
      function Internal
         (File    : Glib.GFile.Gfile;
          Bytes   : System.Address;
          Start   : Gtk_Css_Location;
          The_End : Gtk_Css_Location) return System.Address;
      pragma Import (C, Internal, "gtk_css_section_new_with_bytes");
      Self : Gtk_Css_Section;
   begin
      Self.Set_Object (Internal (File, Get_Object (Bytes), Start, The_End));
      return Self;
   end Gtk_Css_Section_New_With_Bytes;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self    : out Gtk_Css_Section;
       File    : Glib.GFile.Gfile;
       Start   : Gtk_Css_Location;
       The_End : Gtk_Css_Location)
   is
      function Internal
         (File    : Glib.GFile.Gfile;
          Start   : Gtk_Css_Location;
          The_End : Gtk_Css_Location) return System.Address;
      pragma Import (C, Internal, "gtk_css_section_new");
   begin
      Self.Set_Object (Internal (File, Start, The_End));
   end Gtk_New;

   ------------------------
   -- Gtk_New_With_Bytes --
   ------------------------

   procedure Gtk_New_With_Bytes
      (Self    : out Gtk_Css_Section;
       File    : Glib.GFile.Gfile;
       Bytes   : Glib.Bytes.Gbytes;
       Start   : Gtk_Css_Location;
       The_End : Gtk_Css_Location)
   is
      function Internal
         (File    : Glib.GFile.Gfile;
          Bytes   : System.Address;
          Start   : Gtk_Css_Location;
          The_End : Gtk_Css_Location) return System.Address;
      pragma Import (C, Internal, "gtk_css_section_new_with_bytes");
   begin
      Self.Set_Object (Internal (File, Get_Object (Bytes), Start, The_End));
   end Gtk_New_With_Bytes;

   ---------------
   -- Get_Bytes --
   ---------------

   function Get_Bytes (Self : Gtk_Css_Section) return Glib.Bytes.Gbytes is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_css_section_get_bytes");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Bytes;

   ----------------------
   -- Get_End_Location --
   ----------------------

   function Get_End_Location
      (Self : Gtk_Css_Section) return Gtk_Css_Location
   is
      function Internal
         (Self : System.Address) return access Gtk_Css_Location;
      pragma Import (C, Internal, "gtk_css_section_get_end_location");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_End_Location;

   --------------
   -- Get_File --
   --------------

   function Get_File (Self : Gtk_Css_Section) return Glib.GFile.Gfile is
      function Internal (Self : System.Address) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_css_section_get_file");
   begin
      return Internal (Get_Object (Self));
   end Get_File;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Self : Gtk_Css_Section) return Gtk_Css_Section is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_css_section_get_parent");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Parent;

   ------------------------
   -- Get_Start_Location --
   ------------------------

   function Get_Start_Location
      (Self : Gtk_Css_Section) return Gtk_Css_Location
   is
      function Internal
         (Self : System.Address) return access Gtk_Css_Location;
      pragma Import (C, Internal, "gtk_css_section_get_start_location");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_Start_Location;

   -----------
   -- Print --
   -----------

   procedure Print (Self : Gtk_Css_Section; String : Glib.String.Gstring) is
      procedure Internal
         (Self   : System.Address;
          String : Glib.String.Gstring);
      pragma Import (C, Internal, "gtk_css_section_print");
   begin
      Internal (Get_Object (Self), String);
   end Print;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gtk_Css_Section) return Gtk_Css_Section is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_css_section_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Gtk_Css_Section) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_css_section_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end To_String;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gtk_Css_Section) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_css_section_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Gtk_Css_Location) return Gtk_Css_Location
   is
      Result : constant Gtk_Css_Location := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

end Gtk.Css_Section;
