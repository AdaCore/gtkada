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
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;

package body Gtk.Paper_Size is

   function From_Object_Free
     (B : access Gtk_Paper_Size'Class) return Gtk_Paper_Size
   is
      Result : constant Gtk_Paper_Size := Gtk_Paper_Size (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Paper_Size is
      S : Gtk_Paper_Size;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Paper_Size; Name : UTF8_String := "") is
      function Internal
         (Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new");
      Tmp_Name   : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Tmp_Return := Internal (Tmp_Name);
      Free (Tmp_Name);
      Widget.Set_Object (Tmp_Return);
   end Gtk_New;

   --------------------
   -- Gtk_New_Custom --
   --------------------

   procedure Gtk_New_Custom
      (Widget       : out Gtk_Paper_Size;
       Name         : UTF8_String;
       Display_Name : UTF8_String;
       Width        : Gdouble;
       Height       : Gdouble;
       Unit         : Gtk.Enums.Gtk_Unit)
   is
      function Internal
         (Name         : Gtkada.Types.Chars_Ptr;
          Display_Name : Gtkada.Types.Chars_Ptr;
          Width        : Gdouble;
          Height       : Gdouble;
          Unit         : Gtk.Enums.Gtk_Unit) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new_custom");
      Tmp_Name         : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Display_Name : Gtkada.Types.Chars_Ptr := New_String (Display_Name);
      Tmp_Return       : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Name, Tmp_Display_Name, Width, Height, Unit);
      Free (Tmp_Display_Name);
      Free (Tmp_Name);
      Widget.Set_Object (Tmp_Return);
   end Gtk_New_Custom;

   ---------------------------
   -- Gtk_New_From_Gvariant --
   ---------------------------

   procedure Gtk_New_From_Gvariant
      (Widget  : out Gtk_Paper_Size;
       Variant : Glib.Variant.Gvariant)
   is
      function Internal (Variant : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new_from_gvariant");
   begin
      Widget.Set_Object (Internal (Get_Object (Variant)));
   end Gtk_New_From_Gvariant;

   ----------------------
   -- Gtk_New_From_Ipp --
   ----------------------

   procedure Gtk_New_From_Ipp
      (Widget   : out Gtk_Paper_Size;
       Ipp_Name : UTF8_String;
       Width    : Gdouble;
       Height   : Gdouble)
   is
      function Internal
         (Ipp_Name : Gtkada.Types.Chars_Ptr;
          Width    : Gdouble;
          Height   : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new_from_ipp");
      Tmp_Ipp_Name : Gtkada.Types.Chars_Ptr := New_String (Ipp_Name);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Ipp_Name, Width, Height);
      Free (Tmp_Ipp_Name);
      Widget.Set_Object (Tmp_Return);
   end Gtk_New_From_Ipp;

   ---------------------------
   -- Gtk_New_From_Key_File --
   ---------------------------

   procedure Gtk_New_From_Key_File
      (Widget     : out Gtk_Paper_Size;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "")
   is
      function Internal
         (Key_File   : Glib.Key_File.G_Key_File;
          Group_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new_from_key_file");
      Tmp_Group_Name : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : System.Address;
   begin
      if Group_Name = "" then
         Tmp_Group_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Group_Name := New_String (Group_Name);
      end if;
      Tmp_Return := Internal (Key_File, Tmp_Group_Name);
      Free (Tmp_Group_Name);
      Widget.Set_Object (Tmp_Return);
   end Gtk_New_From_Key_File;

   ----------------------
   -- Gtk_New_From_Ppd --
   ----------------------

   procedure Gtk_New_From_Ppd
      (Widget           : out Gtk_Paper_Size;
       Ppd_Name         : UTF8_String;
       Ppd_Display_Name : UTF8_String;
       Width            : Gdouble;
       Height           : Gdouble)
   is
      function Internal
         (Ppd_Name         : Gtkada.Types.Chars_Ptr;
          Ppd_Display_Name : Gtkada.Types.Chars_Ptr;
          Width            : Gdouble;
          Height           : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new_from_ppd");
      Tmp_Ppd_Name         : Gtkada.Types.Chars_Ptr := New_String (Ppd_Name);
      Tmp_Ppd_Display_Name : Gtkada.Types.Chars_Ptr := New_String (Ppd_Display_Name);
      Tmp_Return           : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Ppd_Name, Tmp_Ppd_Display_Name, Width, Height);
      Free (Tmp_Ppd_Display_Name);
      Free (Tmp_Ppd_Name);
      Widget.Set_Object (Tmp_Return);
   end Gtk_New_From_Ppd;

   ------------------------
   -- Gtk_Paper_Size_New --
   ------------------------

   function Gtk_Paper_Size_New
      (Name : UTF8_String := "") return Gtk_Paper_Size
   is
      function Internal
         (Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new");
      Tmp_Name   : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
      Widget     : Gtk_Paper_Size;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Tmp_Return := Internal (Tmp_Name);
      Free (Tmp_Name);
      Widget.Set_Object (Tmp_Return);
      return Widget;
   end Gtk_Paper_Size_New;

   -------------------------------
   -- Gtk_Paper_Size_New_Custom --
   -------------------------------

   function Gtk_Paper_Size_New_Custom
      (Name         : UTF8_String;
       Display_Name : UTF8_String;
       Width        : Gdouble;
       Height       : Gdouble;
       Unit         : Gtk.Enums.Gtk_Unit) return Gtk_Paper_Size
   is
      function Internal
         (Name         : Gtkada.Types.Chars_Ptr;
          Display_Name : Gtkada.Types.Chars_Ptr;
          Width        : Gdouble;
          Height       : Gdouble;
          Unit         : Gtk.Enums.Gtk_Unit) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new_custom");
      Tmp_Name         : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Display_Name : Gtkada.Types.Chars_Ptr := New_String (Display_Name);
      Tmp_Return       : System.Address;
      Widget           : Gtk_Paper_Size;
   begin
      Tmp_Return := Internal (Tmp_Name, Tmp_Display_Name, Width, Height, Unit);
      Free (Tmp_Display_Name);
      Free (Tmp_Name);
      Widget.Set_Object (Tmp_Return);
      return Widget;
   end Gtk_Paper_Size_New_Custom;

   --------------------------------------
   -- Gtk_Paper_Size_New_From_Gvariant --
   --------------------------------------

   function Gtk_Paper_Size_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_Paper_Size
   is
      function Internal (Variant : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new_from_gvariant");
      Widget : Gtk_Paper_Size;
   begin
      Widget.Set_Object (Internal (Get_Object (Variant)));
      return Widget;
   end Gtk_Paper_Size_New_From_Gvariant;

   ---------------------------------
   -- Gtk_Paper_Size_New_From_Ipp --
   ---------------------------------

   function Gtk_Paper_Size_New_From_Ipp
      (Ipp_Name : UTF8_String;
       Width    : Gdouble;
       Height   : Gdouble) return Gtk_Paper_Size
   is
      function Internal
         (Ipp_Name : Gtkada.Types.Chars_Ptr;
          Width    : Gdouble;
          Height   : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new_from_ipp");
      Tmp_Ipp_Name : Gtkada.Types.Chars_Ptr := New_String (Ipp_Name);
      Tmp_Return   : System.Address;
      Widget       : Gtk_Paper_Size;
   begin
      Tmp_Return := Internal (Tmp_Ipp_Name, Width, Height);
      Free (Tmp_Ipp_Name);
      Widget.Set_Object (Tmp_Return);
      return Widget;
   end Gtk_Paper_Size_New_From_Ipp;

   --------------------------------------
   -- Gtk_Paper_Size_New_From_Key_File --
   --------------------------------------

   function Gtk_Paper_Size_New_From_Key_File
      (Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "") return Gtk_Paper_Size
   is
      function Internal
         (Key_File   : Glib.Key_File.G_Key_File;
          Group_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new_from_key_file");
      Tmp_Group_Name : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : System.Address;
      Widget         : Gtk_Paper_Size;
   begin
      if Group_Name = "" then
         Tmp_Group_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Group_Name := New_String (Group_Name);
      end if;
      Tmp_Return := Internal (Key_File, Tmp_Group_Name);
      Free (Tmp_Group_Name);
      Widget.Set_Object (Tmp_Return);
      return Widget;
   end Gtk_Paper_Size_New_From_Key_File;

   ---------------------------------
   -- Gtk_Paper_Size_New_From_Ppd --
   ---------------------------------

   function Gtk_Paper_Size_New_From_Ppd
      (Ppd_Name         : UTF8_String;
       Ppd_Display_Name : UTF8_String;
       Width            : Gdouble;
       Height           : Gdouble) return Gtk_Paper_Size
   is
      function Internal
         (Ppd_Name         : Gtkada.Types.Chars_Ptr;
          Ppd_Display_Name : Gtkada.Types.Chars_Ptr;
          Width            : Gdouble;
          Height           : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_new_from_ppd");
      Tmp_Ppd_Name         : Gtkada.Types.Chars_Ptr := New_String (Ppd_Name);
      Tmp_Ppd_Display_Name : Gtkada.Types.Chars_Ptr := New_String (Ppd_Display_Name);
      Tmp_Return           : System.Address;
      Widget               : Gtk_Paper_Size;
   begin
      Tmp_Return := Internal (Tmp_Ppd_Name, Tmp_Ppd_Display_Name, Width, Height);
      Free (Tmp_Ppd_Display_Name);
      Free (Tmp_Ppd_Name);
      Widget.Set_Object (Tmp_Return);
      return Widget;
   end Gtk_Paper_Size_New_From_Ppd;

   ----------
   -- Copy --
   ----------

   function Copy (Widget : Gtk_Paper_Size) return Gtk_Paper_Size is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_copy");
   begin
      return From_Object (Internal (Get_Object (Widget)));
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Widget : Gtk_Paper_Size) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_paper_size_free");
   begin
      Internal (Get_Object (Widget));
   end Free;

   -------------------------------
   -- Get_Default_Bottom_Margin --
   -------------------------------

   function Get_Default_Bottom_Margin
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Widget : System.Address;
          Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_paper_size_get_default_bottom_margin");
   begin
      return Internal (Get_Object (Widget), Unit);
   end Get_Default_Bottom_Margin;

   -----------------------------
   -- Get_Default_Left_Margin --
   -----------------------------

   function Get_Default_Left_Margin
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Widget : System.Address;
          Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_paper_size_get_default_left_margin");
   begin
      return Internal (Get_Object (Widget), Unit);
   end Get_Default_Left_Margin;

   ------------------------------
   -- Get_Default_Right_Margin --
   ------------------------------

   function Get_Default_Right_Margin
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Widget : System.Address;
          Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_paper_size_get_default_right_margin");
   begin
      return Internal (Get_Object (Widget), Unit);
   end Get_Default_Right_Margin;

   ----------------------------
   -- Get_Default_Top_Margin --
   ----------------------------

   function Get_Default_Top_Margin
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Widget : System.Address;
          Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_paper_size_get_default_top_margin");
   begin
      return Internal (Get_Object (Widget), Unit);
   end Get_Default_Top_Margin;

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name (Widget : Gtk_Paper_Size) return UTF8_String is
      function Internal
         (Widget : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_paper_size_get_display_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Widget)));
   end Get_Display_Name;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Widget : System.Address;
          Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_paper_size_get_height");
   begin
      return Internal (Get_Object (Widget), Unit);
   end Get_Height;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Widget : Gtk_Paper_Size) return UTF8_String is
      function Internal
         (Widget : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_paper_size_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Widget)));
   end Get_Name;

   ------------------
   -- Get_Ppd_Name --
   ------------------

   function Get_Ppd_Name (Widget : Gtk_Paper_Size) return UTF8_String is
      function Internal
         (Widget : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_paper_size_get_ppd_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Widget)));
   end Get_Ppd_Name;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Widget : Gtk_Paper_Size;
       Unit   : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Widget : System.Address;
          Unit   : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_paper_size_get_width");
   begin
      return Internal (Get_Object (Widget), Unit);
   end Get_Width;

   ---------------
   -- Is_Custom --
   ---------------

   function Is_Custom (Widget : Gtk_Paper_Size) return Boolean is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_paper_size_is_custom");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Is_Custom;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
      (Widget : Gtk_Paper_Size;
       Size2  : Gtk_Paper_Size) return Boolean
   is
      function Internal
         (Widget : System.Address;
          Size2  : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_paper_size_is_equal");
   begin
      return Internal (Get_Object (Widget), Get_Object (Size2)) /= 0;
   end Is_Equal;

   ------------
   -- Is_Ipp --
   ------------

   function Is_Ipp (Widget : Gtk_Paper_Size) return Boolean is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_paper_size_is_ipp");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Is_Ipp;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
      (Widget : Gtk_Paper_Size;
       Width  : Gdouble;
       Height : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
         (Widget : System.Address;
          Width  : Gdouble;
          Height : Gdouble;
          Unit   : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_paper_size_set_size");
   begin
      Internal (Get_Object (Widget), Width, Height, Unit);
   end Set_Size;

   -----------------
   -- To_Gvariant --
   -----------------

   function To_Gvariant
      (Widget : Gtk_Paper_Size) return Glib.Variant.Gvariant
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_to_gvariant");
   begin
      return From_Object (Internal (Get_Object (Widget)));
   end To_Gvariant;

   -----------------
   -- To_Key_File --
   -----------------

   procedure To_Key_File
      (Widget     : Gtk_Paper_Size;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String)
   is
      procedure Internal
         (Widget     : System.Address;
          Key_File   : Glib.Key_File.G_Key_File;
          Group_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_paper_size_to_key_file");
      Tmp_Group_Name : Gtkada.Types.Chars_Ptr := New_String (Group_Name);
   begin
      Internal (Get_Object (Widget), Key_File, Tmp_Group_Name);
      Free (Tmp_Group_Name);
   end To_Key_File;

   function Convert (R : Gtk.Paper_Size.Gtk_Paper_Size) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gtk.Paper_Size.Gtk_Paper_Size is
   begin
      return From_Object(R);
   end Convert;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return UTF8_String is
      function Internal return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_paper_size_get_default");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal);
   end Get_Default;

   ---------------------
   -- Get_Paper_Sizes --
   ---------------------

   function Get_Paper_Sizes
      (Include_Custom : Boolean) return Gtk_Paper_Size_Glist.Glist
   is
      function Internal
         (Include_Custom : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "gtk_paper_size_get_paper_sizes");
      Tmp_Return : Gtk_Paper_Size_Glist.Glist;
   begin
      Gtk.Paper_Size.Gtk_Paper_Size_Glist.Set_Object (Tmp_Return, Internal (Boolean'Pos (Include_Custom)));
      return Tmp_Return;
   end Get_Paper_Sizes;

end Gtk.Paper_Size;
