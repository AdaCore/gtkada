------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
with Gtkada.C;        use Gtkada.C;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Selection_Data is

   function From_Object_Free
     (B : access Gtk_Selection_Data'Class) return Gtk_Selection_Data
   is
      Result : constant Gtk_Selection_Data := Gtk_Selection_Data (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Selection_Data is
      S : Gtk_Selection_Data;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   package Atom_Arrays is new Gtkada.C.Unbounded_Arrays
     (Gdk.Types.Gdk_Atom, Gdk.Types.Gdk_None,
      Natural, Gdk.Types.Gdk_Atom_Array);

   ------------------------
   -- Selection_Data_Set --
   ------------------------

   procedure Selection_Data_Set
     (Selection : Gtk_Selection_Data;
      The_Type  : Gdk.Types.Gdk_Atom;
      Format    : Gint;
      Data      : System.Address;
      Length    : Gint)
   is
      procedure Internal
        (Selection : System.Address;
         The_Type  : Gdk.Types.Gdk_Atom;
         Format    : Gint;
         Data      : System.Address;
         Length    : Gint);
      pragma Import (C, Internal, "gtk_selection_data_set");

   begin
      Internal (Get_Object (Selection), The_Type, Format, Data, Length);
   end Selection_Data_Set;

   procedure Selection_Data_Set
     (Selection : Gtk_Selection_Data;
      The_Type  : Gdk.Types.Gdk_Atom;
      Format    : Gint;
      Data      : String) is
   begin
      Selection_Data_Set
        (Selection, The_Type, Format, Data'Address, Data'Length);
   end Selection_Data_Set;

   -----------------
   -- Get_Targets --
   -----------------

   function Get_Targets
     (Selection : Gtk_Selection_Data) return Gdk.Types.Gdk_Atom_Array
   is
      use Atom_Arrays;
      function Internal
        (Selection : System.Address;
         Targets   : access Unbounded_Array_Access;
         N_Atoms   : access Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_get_targets");

      Output    : aliased Unbounded_Array_Access;
      N         : aliased Gint;
   begin
      if Internal
        (Get_Object (Selection),
         Output'Unchecked_Access,
         N'Unchecked_Access) = 0
      then
         Output := null;
      end if;

      declare
         Result : constant Gdk_Atom_Array := To_Array (Output, Integer (N));
      begin
         G_Free (Output);
         return Result;
      end;
   end Get_Targets;

   ------------------------
   -- Get_Data_As_String --
   ------------------------

   function Get_Data_As_String
     (Selection : Gtk_Selection_Data) return String
   is
      function Internal
        (Selection : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_selection_data_get_data");
   begin
      return Gtkada.Types.Value (Internal (Get_Object (Selection)));
   end Get_Data_As_String;

   ----------
   -- Copy --
   ----------

   function Copy (Selection : Gtk_Selection_Data) return Gtk_Selection_Data is
      function Internal (Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_selection_data_copy");
   begin
      return From_Object (Internal (Get_Object (Selection)));
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Selection : Gtk_Selection_Data) is
      procedure Internal (Selection : System.Address);
      pragma Import (C, Internal, "gtk_selection_data_free");
   begin
      Internal (Get_Object (Selection));
   end Free;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Selection : Gtk_Selection_Data) return System.Address is
      function Internal (Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_selection_data_get_data");
   begin
      return Internal (Get_Object (Selection));
   end Get_Data;

   -------------------
   -- Get_Data_Type --
   -------------------

   function Get_Data_Type
      (Selection : Gtk_Selection_Data) return Gdk.Types.Gdk_Atom
   is
      function Internal
         (Selection : System.Address) return Gdk.Types.Gdk_Atom;
      pragma Import (C, Internal, "gtk_selection_data_get_data_type");
   begin
      return Internal (Get_Object (Selection));
   end Get_Data_Type;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Selection : Gtk_Selection_Data) return Gdk.Display.Gdk_Display
   is
      function Internal (Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_selection_data_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display (Get_User_Data (Internal (Get_Object (Selection)), Stub_Gdk_Display));
   end Get_Display;

   ----------------
   -- Get_Format --
   ----------------

   function Get_Format (Selection : Gtk_Selection_Data) return Glib.Gint is
      function Internal (Selection : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_selection_data_get_format");
   begin
      return Internal (Get_Object (Selection));
   end Get_Format;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length (Selection : Gtk_Selection_Data) return Glib.Gint is
      function Internal (Selection : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_selection_data_get_length");
   begin
      return Internal (Get_Object (Selection));
   end Get_Length;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf
      (Selection : Gtk_Selection_Data) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_selection_data_get_pixbuf");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Selection)), Stub_Gdk_Pixbuf));
   end Get_Pixbuf;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
      (Selection : Gtk_Selection_Data) return Gdk.Types.Gdk_Atom
   is
      function Internal
         (Selection : System.Address) return Gdk.Types.Gdk_Atom;
      pragma Import (C, Internal, "gtk_selection_data_get_selection");
   begin
      return Internal (Get_Object (Selection));
   end Get_Selection;

   ----------------
   -- Get_Target --
   ----------------

   function Get_Target
      (Selection : Gtk_Selection_Data) return Gdk.Types.Gdk_Atom
   is
      function Internal
         (Selection : System.Address) return Gdk.Types.Gdk_Atom;
      pragma Import (C, Internal, "gtk_selection_data_get_target");
   begin
      return Internal (Get_Object (Selection));
   end Get_Target;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Selection : Gtk_Selection_Data) return UTF8_String is
      function Internal
         (Selection : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_selection_data_get_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Selection)));
   end Get_Text;

   --------------
   -- Get_Uris --
   --------------

   function Get_Uris
      (Selection : Gtk_Selection_Data) return GNAT.Strings.String_List
   is
      function Internal
         (Selection : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_selection_data_get_uris");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Selection)));
   end Get_Uris;

   ----------------
   -- Set_Pixbuf --
   ----------------

   function Set_Pixbuf
      (Selection : Gtk_Selection_Data;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
       return Boolean
   is
      function Internal
         (Selection : System.Address;
          Pixbuf    : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_set_pixbuf");
   begin
      return Internal (Get_Object (Selection), Get_Object (Pixbuf)) /= 0;
   end Set_Pixbuf;

   --------------
   -- Set_Text --
   --------------

   function Set_Text
      (Selection : Gtk_Selection_Data;
       Str       : UTF8_String;
       Len       : Glib.Gint) return Boolean
   is
      function Internal
         (Selection : System.Address;
          Str       : Gtkada.Types.Chars_Ptr;
          Len       : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_set_text");
      Tmp_Str    : Gtkada.Types.Chars_Ptr := New_String (Str);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Selection), Tmp_Str, Len);
      Free (Tmp_Str);
      return Tmp_Return /= 0;
   end Set_Text;

   --------------
   -- Set_Uris --
   --------------

   function Set_Uris
      (Selection : Gtk_Selection_Data;
       Uris      : GNAT.Strings.String_List) return Boolean
   is
      function Internal
         (Selection : System.Address;
          Uris      : Gtkada.Types.chars_ptr_array) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_set_uris");
      Tmp_Uris   : Gtkada.Types.chars_ptr_array := From_String_List (Uris);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Selection), Tmp_Uris);
      Gtkada.Types.Free (Tmp_Uris);
      return Tmp_Return /= 0;
   end Set_Uris;

   ---------------------------
   -- Targets_Include_Image --
   ---------------------------

   function Targets_Include_Image
      (Selection : Gtk_Selection_Data;
       Writable  : Boolean) return Boolean
   is
      function Internal
         (Selection : System.Address;
          Writable  : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_targets_include_image");
   begin
      return Internal (Get_Object (Selection), Boolean'Pos (Writable)) /= 0;
   end Targets_Include_Image;

   --------------------------
   -- Targets_Include_Text --
   --------------------------

   function Targets_Include_Text
      (Selection : Gtk_Selection_Data) return Boolean
   is
      function Internal (Selection : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_targets_include_text");
   begin
      return Internal (Get_Object (Selection)) /= 0;
   end Targets_Include_Text;

   -------------------------
   -- Targets_Include_Uri --
   -------------------------

   function Targets_Include_Uri
      (Selection : Gtk_Selection_Data) return Boolean
   is
      function Internal (Selection : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_data_targets_include_uri");
   begin
      return Internal (Get_Object (Selection)) /= 0;
   end Targets_Include_Uri;

end Gtk.Selection_Data;
