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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Css_Provider is

   package Type_Conversion_Gtk_Css_Provider is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Css_Provider_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Css_Provider);

   --------------------------
   -- Gtk_Css_Provider_New --
   --------------------------

   function Gtk_Css_Provider_New return Gtk_Css_Provider is
      Self : constant Gtk_Css_Provider := new Gtk_Css_Provider_Record;
   begin
      Gtk.Css_Provider.Initialize (Self);
      return Self;
   end Gtk_Css_Provider_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Css_Provider) is
   begin
      Self := new Gtk_Css_Provider_Record;
      Gtk.Css_Provider.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Css_Provider_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_css_provider_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------------
   -- Load_From_Bytes --
   ---------------------

   procedure Load_From_Bytes
      (Self : not null access Gtk_Css_Provider_Record;
       Data : Glib.Bytes.Gbytes)
   is
      procedure Internal (Self : System.Address; Data : System.Address);
      pragma Import (C, Internal, "gtk_css_provider_load_from_bytes");
   begin
      Internal (Get_Object (Self), Get_Object (Data));
   end Load_From_Bytes;

   --------------------
   -- Load_From_Data --
   --------------------

   procedure Load_From_Data
      (Self : not null access Gtk_Css_Provider_Record;
       Data : UTF8_String)
   is
      procedure Internal
         (Self   : System.Address;
          Data   : Gtkada.Types.Chars_Ptr;
          Length : Gssize);
      pragma Import (C, Internal, "gtk_css_provider_load_from_data");
      Tmp_Data : Gtkada.Types.Chars_Ptr := New_String (Data);
   begin
      Internal (Get_Object (Self), Tmp_Data, -1);
      Free (Tmp_Data);
   end Load_From_Data;

   --------------------
   -- Load_From_File --
   --------------------

   procedure Load_From_File
      (Self : not null access Gtk_Css_Provider_Record;
       File : Glib.GFile.Gfile)
   is
      procedure Internal (Self : System.Address; File : Glib.GFile.Gfile);
      pragma Import (C, Internal, "gtk_css_provider_load_from_file");
   begin
      Internal (Get_Object (Self), File);
   end Load_From_File;

   --------------------
   -- Load_From_Path --
   --------------------

   procedure Load_From_Path
      (Self : not null access Gtk_Css_Provider_Record;
       Path : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_css_provider_load_from_path");
      Tmp_Path : Gtkada.Types.Chars_Ptr := New_String (Path);
   begin
      Internal (Get_Object (Self), Tmp_Path);
      Free (Tmp_Path);
   end Load_From_Path;

   ------------------------
   -- Load_From_Resource --
   ------------------------

   procedure Load_From_Resource
      (Self          : not null access Gtk_Css_Provider_Record;
       Resource_Path : UTF8_String)
   is
      procedure Internal
         (Self          : System.Address;
          Resource_Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_css_provider_load_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
   begin
      Internal (Get_Object (Self), Tmp_Resource_Path);
      Free (Tmp_Resource_Path);
   end Load_From_Resource;

   ----------------------
   -- Load_From_String --
   ----------------------

   procedure Load_From_String
      (Self   : not null access Gtk_Css_Provider_Record;
       String : UTF8_String)
   is
      procedure Internal
         (Self   : System.Address;
          String : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_css_provider_load_from_string");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
   begin
      Internal (Get_Object (Self), Tmp_String);
      Free (Tmp_String);
   end Load_From_String;

   ----------------
   -- Load_Named --
   ----------------

   procedure Load_Named
      (Self    : not null access Gtk_Css_Provider_Record;
       Name    : UTF8_String;
       Variant : UTF8_String := "")
   is
      procedure Internal
         (Self    : System.Address;
          Name    : Gtkada.Types.Chars_Ptr;
          Variant : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_css_provider_load_named");
      Tmp_Name    : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Variant : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Variant :=
        (if Variant = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Variant));
      Internal (Get_Object (Self), Tmp_Name, Tmp_Variant);
      Free (Tmp_Variant);
      Free (Tmp_Name);
   end Load_Named;

   ---------------
   -- To_String --
   ---------------

   function To_String
      (Self : not null access Gtk_Css_Provider_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_css_provider_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end To_String;

end Gtk.Css_Provider;
