------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtkada.Bindings;      use Gtkada.Bindings;

package body Gtk.Paper_Size is

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return String is
      function Internal return chars_ptr;
      pragma Import (C, Internal, "gtk_paper_size_get_default");
   begin
      return Value (Internal);
   end Get_Default;

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name (Size : Gtk_Paper_Size) return String is
      function Internal (Size : Gtk_Paper_Size) return chars_ptr;
      pragma Import (C, Internal, "gtk_paper_size_get_display_name");
   begin
      return Value (Internal (Size));
   end Get_Display_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Size : Gtk_Paper_Size) return String is
      function Internal (Size : Gtk_Paper_Size) return chars_ptr;
      pragma Import (C, Internal, "gtk_paper_size_get_name");
   begin
      return Value (Internal (Size));
   end Get_Name;

   ---------------------
   -- Get_Paper_Sizes --
   ---------------------

   function Get_Paper_Sizes
     (Include_Custom : Boolean) return Gtk_Paper_Size_Glist.Glist
   is
      function Internal (Include_Custom : Gboolean) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_get_paper_sizes");
      G : Gtk_Paper_Size_Glist.Glist;
   begin
      Gtk_Paper_Size_Glist.Set_Object
        (G, Internal (Boolean'Pos (Include_Custom)));
      return G;
   end Get_Paper_Sizes;

   ------------------
   -- Get_Ppd_Name --
   ------------------

   function Get_Ppd_Name (Size : Gtk_Paper_Size) return String is
      function Internal (Size : Gtk_Paper_Size) return chars_ptr;
      pragma Import (C, Internal, "gtk_paper_size_get_ppd_name");
      C : chars_ptr;
   begin
      C := Internal (Size);
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Ppd_Name;

   ---------------
   -- Is_Custom --
   ---------------

   function Is_Custom (Size : Gtk_Paper_Size) return Boolean is
      function Internal (Size : Gtk_Paper_Size) return Gboolean;
      pragma Import (C, Internal, "gtk_paper_size_is_custom");
   begin
      return Boolean'Val (Internal (Size));
   end Is_Custom;

   ---------
   -- "=" --
   ---------

   function "=" (Size1, Size2 : Gtk_Paper_Size) return Boolean is
      function Internal (Size1, Size2 : Gtk_Paper_Size) return Gboolean;
      pragma Import (C, Internal, "gtk_paper_size_is_equal");
   begin
      return Boolean'Val (Internal (Size1, Size2));
   end "=";

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Paper_Size;
      Name   : String)
   is
      function Internal (Name : chars_ptr) return Gtk_Paper_Size;
      pragma Import (C, Internal, "gtk_paper_size_new");
      N : chars_ptr := String_Or_Null (Name);
   begin
      Widget := Internal (N);
      Free (N);
   end Gtk_New;

   --------------------
   -- Gtk_New_Custom --
   --------------------

   procedure Gtk_New_Custom
     (Widget       : out Gtk_Paper_Size;
      Name         : String;
      Display_Name : String;
      Width        : Gdouble;
      Height       : Gdouble;
      Unit         : Gtk.Enums.Gtk_Unit)
   is
      function Internal
        (Name         : String;
         Display_Name : chars_ptr;
         Width        : Gdouble;
         Height       : Gdouble;
         Unit         : Gtk.Enums.Gtk_Unit)
         return Gtk_Paper_Size;
      pragma Import (C, Internal, "gtk_paper_size_new_custom");
      D : chars_ptr := String_Or_Null (Display_Name);
   begin
      Widget := Internal
        (Name & ASCII.NUL,
         D,
         Width,
         Height,
         Unit);
      Free (D);
   end Gtk_New_Custom;

   ---------------------------
   -- Gtk_New_From_Key_File --
   ---------------------------

   procedure Gtk_New_From_Key_File
     (Widget     : out Gtk_Paper_Size;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "")
   is
      function Internal
        (Key_File   : Glib.Key_File.G_Key_File;
         Group_Name : chars_ptr)
         return Gtk_Paper_Size;
      pragma Import (C, Internal, "gtk_paper_size_new_from_key_file");

      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Widget := Internal (Key_File, G);
      Free (G);
   end Gtk_New_From_Key_File;

   ----------------------
   -- Gtk_New_From_Ppd --
   ----------------------

   procedure Gtk_New_From_Ppd
     (Widget           : out Gtk_Paper_Size;
      Ppd_Name         : String;
      Ppd_Display_Name : String := "";
      Width            : Gdouble;
      Height           : Gdouble)
   is
      function Internal
        (Ppd_Name         : String;
         Ppd_Display_Name : chars_ptr;
         Width            : Gdouble;
         Height           : Gdouble)
         return Gtk_Paper_Size;
      pragma Import (C, Internal, "gtk_paper_size_new_from_ppd");
      D : chars_ptr := String_Or_Null (Ppd_Display_Name);
   begin
      Widget := Internal (Ppd_Name & ASCII.NUL, D, Width, Height);
      Free (D);
   end Gtk_New_From_Ppd;

   -----------------
   -- To_Key_File --
   -----------------

   procedure To_Key_File
     (Size       : Gtk_Paper_Size;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "")
   is
      procedure Internal
        (Size       : Gtk_Paper_Size;
         Key_File   : Glib.Key_File.G_Key_File;
         Group_Name : chars_ptr);
      pragma Import (C, Internal, "gtk_paper_size_to_key_file");
      G : chars_ptr := String_Or_Null (Group_Name);
   begin
      Internal (Size, Key_File, G);
      Free (G);
   end To_Key_File;

end Gtk.Paper_Size;
