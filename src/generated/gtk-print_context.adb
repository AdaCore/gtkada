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

package body Gtk.Print_Context is

   package Type_Conversion_Gtk_Print_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Print_Context_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Print_Context);

   --------------------------
   -- Create_Pango_Context --
   --------------------------

   function Create_Pango_Context
      (Context : not null access Gtk_Print_Context_Record)
       return Pango.Context.Pango_Context
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_context_create_pango_context");
      Stub_Pango_Context : Pango.Context.Pango_Context_Record;
   begin
      return Pango.Context.Pango_Context (Get_User_Data (Internal (Get_Object (Context)), Stub_Pango_Context));
   end Create_Pango_Context;

   -------------------------
   -- Create_Pango_Layout --
   -------------------------

   function Create_Pango_Layout
      (Context : not null access Gtk_Print_Context_Record)
       return Pango.Layout.Pango_Layout
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_context_create_pango_layout");
      Stub_Pango_Layout : Pango.Layout.Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout (Get_User_Data (Internal (Get_Object (Context)), Stub_Pango_Layout));
   end Create_Pango_Layout;

   -----------------------
   -- Get_Cairo_Context --
   -----------------------

   function Get_Cairo_Context
      (Context : not null access Gtk_Print_Context_Record)
       return Cairo.Cairo_Context
   is
      function Internal
         (Context : System.Address) return Cairo.Cairo_Context;
      pragma Import (C, Internal, "gtk_print_context_get_cairo_context");
   begin
      return Internal (Get_Object (Context));
   end Get_Cairo_Context;

   ---------------
   -- Get_Dpi_X --
   ---------------

   function Get_Dpi_X
      (Context : not null access Gtk_Print_Context_Record) return Gdouble
   is
      function Internal (Context : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_print_context_get_dpi_x");
   begin
      return Internal (Get_Object (Context));
   end Get_Dpi_X;

   ---------------
   -- Get_Dpi_Y --
   ---------------

   function Get_Dpi_Y
      (Context : not null access Gtk_Print_Context_Record) return Gdouble
   is
      function Internal (Context : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_print_context_get_dpi_y");
   begin
      return Internal (Get_Object (Context));
   end Get_Dpi_Y;

   ----------------------
   -- Get_Hard_Margins --
   ----------------------

   function Get_Hard_Margins
      (Context : not null access Gtk_Print_Context_Record;
       Top     : access Gdouble;
       Bottom  : access Gdouble;
       Left    : access Gdouble;
       Right   : access Gdouble) return Boolean
   is
      function Internal
         (Context    : System.Address;
          Acc_Top    : access Gdouble;
          Acc_Bottom : access Gdouble;
          Acc_Left   : access Gdouble;
          Acc_Right  : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_print_context_get_hard_margins");
      Acc_Top    : aliased Gdouble;
      Acc_Bottom : aliased Gdouble;
      Acc_Left   : aliased Gdouble;
      Acc_Right  : aliased Gdouble;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Context), Acc_Top'Access, Acc_Bottom'Access, Acc_Left'Access, Acc_Right'Access);
      Top.all := Acc_Top;
      Bottom.all := Acc_Bottom;
      Left.all := Acc_Left;
      Right.all := Acc_Right;
      return Tmp_Return /= 0;
   end Get_Hard_Margins;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
      (Context : not null access Gtk_Print_Context_Record) return Gdouble
   is
      function Internal (Context : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_print_context_get_height");
   begin
      return Internal (Get_Object (Context));
   end Get_Height;

   --------------------
   -- Get_Page_Setup --
   --------------------

   function Get_Page_Setup
      (Context : not null access Gtk_Print_Context_Record)
       return Gtk.Page_Setup.Gtk_Page_Setup
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_context_get_page_setup");
      Stub_Gtk_Page_Setup : Gtk.Page_Setup.Gtk_Page_Setup_Record;
   begin
      return Gtk.Page_Setup.Gtk_Page_Setup (Get_User_Data (Internal (Get_Object (Context)), Stub_Gtk_Page_Setup));
   end Get_Page_Setup;

   -----------------------
   -- Get_Pango_Fontmap --
   -----------------------

   function Get_Pango_Fontmap
      (Context : not null access Gtk_Print_Context_Record)
       return Pango.Font_Map.Pango_Font_Map
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_context_get_pango_fontmap");
      Stub_Pango_Font_Map : Pango.Font_Map.Pango_Font_Map_Record;
   begin
      return Pango.Font_Map.Pango_Font_Map (Get_User_Data (Internal (Get_Object (Context)), Stub_Pango_Font_Map));
   end Get_Pango_Fontmap;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Context : not null access Gtk_Print_Context_Record) return Gdouble
   is
      function Internal (Context : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_print_context_get_width");
   begin
      return Internal (Get_Object (Context));
   end Get_Width;

   -----------------------
   -- Set_Cairo_Context --
   -----------------------

   procedure Set_Cairo_Context
      (Context : not null access Gtk_Print_Context_Record;
       Cr      : Cairo.Cairo_Context;
       Dpi_X   : Gdouble;
       Dpi_Y   : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          Dpi_X   : Gdouble;
          Dpi_Y   : Gdouble);
      pragma Import (C, Internal, "gtk_print_context_set_cairo_context");
   begin
      Internal (Get_Object (Context), Cr, Dpi_X, Dpi_Y);
   end Set_Cairo_Context;

end Gtk.Print_Context;
