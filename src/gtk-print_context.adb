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

package body Gtk.Print_Context is

   --------------------------
   -- Create_Pango_Context --
   --------------------------

   function Create_Pango_Context
     (Context : access Gtk_Print_Context_Record)
      return Pango.Context.Pango_Context
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_context_create_pango_context");
      Stub : Pango.Context.Pango_Context_Record;
   begin
      return Pango.Context.Pango_Context
        (Get_User_Data (Internal (Get_Object (Context)), Stub));
   end Create_Pango_Context;

   -------------------------
   -- Create_Pango_Layout --
   -------------------------

   function Create_Pango_Layout
     (Context : access Gtk_Print_Context_Record)
      return Pango.Layout.Pango_Layout
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_context_create_pango_layout");
      Stub : Pango.Layout.Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout
        (Get_User_Data (Internal (Get_Object (Context)), Stub));
   end Create_Pango_Layout;

   -----------------------
   -- Get_Cairo_Context --
   -----------------------

   function Get_Cairo_Context
     (Context : access Gtk_Print_Context_Record)
      return Cairo.Cairo_Context
   is
      function Internal (Context : System.Address) return Cairo.Cairo_Context;
      pragma Import (C, Internal, "gtk_print_context_get_cairo_context");
   begin
      return Internal (Get_Object (Context));
   end Get_Cairo_Context;

   ---------------
   -- Get_Dpi_X --
   ---------------

   function Get_Dpi_X
     (Context : access Gtk_Print_Context_Record)
      return Gdouble
   is
      function Internal
        (Context : System.Address)
         return Gdouble;
      pragma Import (C, Internal, "gtk_print_context_get_dpi_x");
   begin
      return Internal (Get_Object (Context));
   end Get_Dpi_X;

   ---------------
   -- Get_Dpi_Y --
   ---------------

   function Get_Dpi_Y
     (Context : access Gtk_Print_Context_Record)
      return Gdouble
   is
      function Internal
        (Context : System.Address)
         return Gdouble;
      pragma Import (C, Internal, "gtk_print_context_get_dpi_y");
   begin
      return Internal (Get_Object (Context));
   end Get_Dpi_Y;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
     (Context : access Gtk_Print_Context_Record)
      return Gdouble
   is
      function Internal
        (Context : System.Address)
         return Gdouble;
      pragma Import (C, Internal, "gtk_print_context_get_height");
   begin
      return Internal (Get_Object (Context));
   end Get_Height;

   --------------------
   -- Get_Page_Setup --
   --------------------

   function Get_Page_Setup
     (Context : access Gtk_Print_Context_Record)
      return Gtk.Page_Setup.Gtk_Page_Setup
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_print_context_get_page_setup");
      Stub : Gtk.Page_Setup.Gtk_Page_Setup_Record;
   begin
      return Gtk.Page_Setup.Gtk_Page_Setup
        (Get_User_Data (Internal (Get_Object (Context)), Stub));
   end Get_Page_Setup;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
     (Context : access Gtk_Print_Context_Record)
      return Gdouble
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
     (Context : access Gtk_Print_Context_Record;
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
