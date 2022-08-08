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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Page_Setup is

   package Type_Conversion_Gtk_Page_Setup is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Page_Setup_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Page_Setup);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Page_Setup) is
   begin
      Self := new Gtk_Page_Setup_Record;
      Gtk.Page_Setup.Initialize (Self);
   end Gtk_New;

   -----------------------
   -- Gtk_New_From_File --
   -----------------------

   procedure Gtk_New_From_File
      (Self      : out Gtk_Page_Setup;
       File_Name : UTF8_String)
   is
   begin
      Self := new Gtk_Page_Setup_Record;
      Gtk.Page_Setup.Initialize_From_File (Self, File_Name);
   end Gtk_New_From_File;

   ---------------------------
   -- Gtk_New_From_Gvariant --
   ---------------------------

   procedure Gtk_New_From_Gvariant
      (Self    : out Gtk_Page_Setup;
       Variant : Glib.Variant.Gvariant)
   is
   begin
      Self := new Gtk_Page_Setup_Record;
      Gtk.Page_Setup.Initialize_From_Gvariant (Self, Variant);
   end Gtk_New_From_Gvariant;

   ---------------------------
   -- Gtk_New_From_Key_File --
   ---------------------------

   procedure Gtk_New_From_Key_File
      (Self       : out Gtk_Page_Setup;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "")
   is
   begin
      Self := new Gtk_Page_Setup_Record;
      Gtk.Page_Setup.Initialize_From_Key_File (Self, Key_File, Group_Name);
   end Gtk_New_From_Key_File;

   ------------------------
   -- Gtk_Page_Setup_New --
   ------------------------

   function Gtk_Page_Setup_New return Gtk_Page_Setup is
      Self : constant Gtk_Page_Setup := new Gtk_Page_Setup_Record;
   begin
      Gtk.Page_Setup.Initialize (Self);
      return Self;
   end Gtk_Page_Setup_New;

   ----------------------------------
   -- Gtk_Page_Setup_New_From_File --
   ----------------------------------

   function Gtk_Page_Setup_New_From_File
      (File_Name : UTF8_String) return Gtk_Page_Setup
   is
      Self : constant Gtk_Page_Setup := new Gtk_Page_Setup_Record;
   begin
      Gtk.Page_Setup.Initialize_From_File (Self, File_Name);
      return Self;
   end Gtk_Page_Setup_New_From_File;

   --------------------------------------
   -- Gtk_Page_Setup_New_From_Gvariant --
   --------------------------------------

   function Gtk_Page_Setup_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_Page_Setup
   is
      Self : constant Gtk_Page_Setup := new Gtk_Page_Setup_Record;
   begin
      Gtk.Page_Setup.Initialize_From_Gvariant (Self, Variant);
      return Self;
   end Gtk_Page_Setup_New_From_Gvariant;

   --------------------------------------
   -- Gtk_Page_Setup_New_From_Key_File --
   --------------------------------------

   function Gtk_Page_Setup_New_From_Key_File
      (Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "") return Gtk_Page_Setup
   is
      Self : constant Gtk_Page_Setup := new Gtk_Page_Setup_Record;
   begin
      Gtk.Page_Setup.Initialize_From_Key_File (Self, Key_File, Group_Name);
      return Self;
   end Gtk_Page_Setup_New_From_Key_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Page_Setup_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_From_File --
   --------------------------

   procedure Initialize_From_File
      (Self      : not null access Gtk_Page_Setup_Record'Class;
       File_Name : UTF8_String)
   is
      function Internal
         (File_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_new_from_file");
      Tmp_File_Name : Gtkada.Types.Chars_Ptr := New_String (File_Name);
      Tmp_Return    : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_File_Name);
         Free (Tmp_File_Name);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_File;

   ------------------------------
   -- Initialize_From_Gvariant --
   ------------------------------

   procedure Initialize_From_Gvariant
      (Self    : not null access Gtk_Page_Setup_Record'Class;
       Variant : Glib.Variant.Gvariant)
   is
      function Internal (Variant : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_new_from_gvariant");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Variant)));
      end if;
   end Initialize_From_Gvariant;

   ------------------------------
   -- Initialize_From_Key_File --
   ------------------------------

   procedure Initialize_From_Key_File
      (Self       : not null access Gtk_Page_Setup_Record'Class;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "")
   is
      function Internal
         (Key_File   : Glib.Key_File.G_Key_File;
          Group_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_new_from_key_file");
      Tmp_Group_Name : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : System.Address;
   begin
      if not Self.Is_Created then
         if Group_Name = "" then
            Tmp_Group_Name := Gtkada.Types.Null_Ptr;
         else
            Tmp_Group_Name := New_String (Group_Name);
         end if;
         Tmp_Return := Internal (Key_File, Tmp_Group_Name);
         Free (Tmp_Group_Name);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_Key_File;

   ----------
   -- Copy --
   ----------

   function Copy
      (Self : not null access Gtk_Page_Setup_Record) return Gtk_Page_Setup
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_copy");
      Stub_Gtk_Page_Setup : Gtk_Page_Setup_Record;
   begin
      return Gtk.Page_Setup.Gtk_Page_Setup (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Page_Setup));
   end Copy;

   -----------------------
   -- Get_Bottom_Margin --
   -----------------------

   function Get_Bottom_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_bottom_margin");
   begin
      return Internal (Get_Object (Self), Unit);
   end Get_Bottom_Margin;

   ---------------------
   -- Get_Left_Margin --
   ---------------------

   function Get_Left_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_left_margin");
   begin
      return Internal (Get_Object (Self), Unit);
   end Get_Left_Margin;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Page_Setup_Record)
       return Gtk.Enums.Gtk_Page_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Page_Orientation;
      pragma Import (C, Internal, "gtk_page_setup_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Get_Page_Height --
   ---------------------

   function Get_Page_Height
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_page_height");
   begin
      return Internal (Get_Object (Self), Unit);
   end Get_Page_Height;

   --------------------
   -- Get_Page_Width --
   --------------------

   function Get_Page_Width
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_page_width");
   begin
      return Internal (Get_Object (Self), Unit);
   end Get_Page_Width;

   ----------------------
   -- Get_Paper_Height --
   ----------------------

   function Get_Paper_Height
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_paper_height");
   begin
      return Internal (Get_Object (Self), Unit);
   end Get_Paper_Height;

   --------------------
   -- Get_Paper_Size --
   --------------------

   function Get_Paper_Size
      (Self : not null access Gtk_Page_Setup_Record)
       return Gtk.Paper_Size.Gtk_Paper_Size
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_get_paper_size");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Paper_Size;

   ---------------------
   -- Get_Paper_Width --
   ---------------------

   function Get_Paper_Width
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_paper_width");
   begin
      return Internal (Get_Object (Self), Unit);
   end Get_Paper_Width;

   ----------------------
   -- Get_Right_Margin --
   ----------------------

   function Get_Right_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_right_margin");
   begin
      return Internal (Get_Object (Self), Unit);
   end Get_Right_Margin;

   --------------------
   -- Get_Top_Margin --
   --------------------

   function Get_Top_Margin
      (Self : not null access Gtk_Page_Setup_Record;
       Unit : Gtk.Enums.Gtk_Unit) return Gdouble
   is
      function Internal
         (Self : System.Address;
          Unit : Gtk.Enums.Gtk_Unit) return Gdouble;
      pragma Import (C, Internal, "gtk_page_setup_get_top_margin");
   begin
      return Internal (Get_Object (Self), Unit);
   end Get_Top_Margin;

   ---------------
   -- Load_File --
   ---------------

   function Load_File
      (Self      : not null access Gtk_Page_Setup_Record;
       File_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self      : System.Address;
          File_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_page_setup_load_file");
      Tmp_File_Name : Gtkada.Types.Chars_Ptr := New_String (File_Name);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_File_Name);
      Free (Tmp_File_Name);
      return Tmp_Return /= 0;
   end Load_File;

   -------------------
   -- Load_Key_File --
   -------------------

   function Load_Key_File
      (Self       : not null access Gtk_Page_Setup_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "") return Boolean
   is
      function Internal
         (Self       : System.Address;
          Key_File   : Glib.Key_File.G_Key_File;
          Group_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_page_setup_load_key_file");
      Tmp_Group_Name : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : Glib.Gboolean;
   begin
      if Group_Name = "" then
         Tmp_Group_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Group_Name := New_String (Group_Name);
      end if;
      Tmp_Return := Internal (Get_Object (Self), Key_File, Tmp_Group_Name);
      Free (Tmp_Group_Name);
      return Tmp_Return /= 0;
   end Load_Key_File;

   -----------------------
   -- Set_Bottom_Margin --
   -----------------------

   procedure Set_Bottom_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
         (Self   : System.Address;
          Margin : Gdouble;
          Unit   : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_page_setup_set_bottom_margin");
   begin
      Internal (Get_Object (Self), Margin, Unit);
   end Set_Bottom_Margin;

   ---------------------
   -- Set_Left_Margin --
   ---------------------

   procedure Set_Left_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
         (Self   : System.Address;
          Margin : Gdouble;
          Unit   : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_page_setup_set_left_margin");
   begin
      Internal (Get_Object (Self), Margin, Unit);
   end Set_Left_Margin;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Page_Setup_Record;
       Orientation : Gtk.Enums.Gtk_Page_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Page_Orientation);
      pragma Import (C, Internal, "gtk_page_setup_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   --------------------
   -- Set_Paper_Size --
   --------------------

   procedure Set_Paper_Size
      (Self : not null access Gtk_Page_Setup_Record;
       Size : Gtk.Paper_Size.Gtk_Paper_Size)
   is
      procedure Internal (Self : System.Address; Size : System.Address);
      pragma Import (C, Internal, "gtk_page_setup_set_paper_size");
   begin
      Internal (Get_Object (Self), Get_Object (Size));
   end Set_Paper_Size;

   ----------------------------------------
   -- Set_Paper_Size_And_Default_Margins --
   ----------------------------------------

   procedure Set_Paper_Size_And_Default_Margins
      (Self : not null access Gtk_Page_Setup_Record;
       Size : Gtk.Paper_Size.Gtk_Paper_Size)
   is
      procedure Internal (Self : System.Address; Size : System.Address);
      pragma Import (C, Internal, "gtk_page_setup_set_paper_size_and_default_margins");
   begin
      Internal (Get_Object (Self), Get_Object (Size));
   end Set_Paper_Size_And_Default_Margins;

   ----------------------
   -- Set_Right_Margin --
   ----------------------

   procedure Set_Right_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
         (Self   : System.Address;
          Margin : Gdouble;
          Unit   : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_page_setup_set_right_margin");
   begin
      Internal (Get_Object (Self), Margin, Unit);
   end Set_Right_Margin;

   --------------------
   -- Set_Top_Margin --
   --------------------

   procedure Set_Top_Margin
      (Self   : not null access Gtk_Page_Setup_Record;
       Margin : Gdouble;
       Unit   : Gtk.Enums.Gtk_Unit)
   is
      procedure Internal
         (Self   : System.Address;
          Margin : Gdouble;
          Unit   : Gtk.Enums.Gtk_Unit);
      pragma Import (C, Internal, "gtk_page_setup_set_top_margin");
   begin
      Internal (Get_Object (Self), Margin, Unit);
   end Set_Top_Margin;

   -------------
   -- To_File --
   -------------

   function To_File
      (Self      : not null access Gtk_Page_Setup_Record;
       File_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self      : System.Address;
          File_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_page_setup_to_file");
      Tmp_File_Name : Gtkada.Types.Chars_Ptr := New_String (File_Name);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_File_Name);
      Free (Tmp_File_Name);
      return Tmp_Return /= 0;
   end To_File;

   -----------------
   -- To_Gvariant --
   -----------------

   function To_Gvariant
      (Self : not null access Gtk_Page_Setup_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_page_setup_to_gvariant");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end To_Gvariant;

   -----------------
   -- To_Key_File --
   -----------------

   procedure To_Key_File
      (Self       : not null access Gtk_Page_Setup_Record;
       Key_File   : Glib.Key_File.G_Key_File;
       Group_Name : UTF8_String := "")
   is
      procedure Internal
         (Self       : System.Address;
          Key_File   : Glib.Key_File.G_Key_File;
          Group_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_page_setup_to_key_file");
      Tmp_Group_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Group_Name = "" then
         Tmp_Group_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Group_Name := New_String (Group_Name);
      end if;
      Internal (Get_Object (Self), Key_File, Tmp_Group_Name);
      Free (Tmp_Group_Name);
   end To_Key_File;

end Gtk.Page_Setup;
