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

--  <description>
--  </description>
--  <c_version>2.16.6</c_version>

with Glib.Object;
with Gtk.Page_Setup;
with Pango.Context;
with Pango.Layout;
with Cairo;

package Gtk.Print_Context is

   type Gtk_Print_Context_Record is
     new Glib.Object.GObject_Record with private;
   type Gtk_Print_Context is access all Gtk_Print_Context_Record'Class;

   function Get_Type return GType;

   function Create_Pango_Context
     (Context : access Gtk_Print_Context_Record)
      return Pango.Context.Pango_Context;
   --  Creates a new Pango_Context that can be used with the Gtk_Print_Context.

   function Create_Pango_Layout
     (Context : access Gtk_Print_Context_Record)
      return Pango.Layout.Pango_Layout;
   --  Creates a new Pango_Layout that is suitable for use
   --  with the Gtk_Print_Context.

   function Get_Cairo_Context
     (Context : access Gtk_Print_Context_Record)
      return Cairo.Cairo_Context;
   --  Obtains the cairo context that is associated with the Gtk_Print_Context.

   function Get_Dpi_X
     (Context : access Gtk_Print_Context_Record)
      return Gdouble;
   --  Obtains the horizontal resolution of the Gtk_Print_Context,
   --  in dots per inch.

   function Get_Dpi_Y
     (Context : access Gtk_Print_Context_Record)
      return Gdouble;
   --  Obtains the vertical resolution of the Gtk_Print_Context,
   --  in dots per inch.

   function Get_Height
     (Context : access Gtk_Print_Context_Record)
      return Gdouble;
   --  Obtains the height of the Gtk_Print_Context, in pixels.

   function Get_Page_Setup
     (Context : access Gtk_Print_Context_Record)
      return Gtk.Page_Setup.Gtk_Page_Setup;
   --  Obtains the Gtk_Page_Setup that determines the page
   --  dimensions of the Gtk_Print_Context.

   function Get_Width
     (Context : access Gtk_Print_Context_Record)
      return Gdouble;
   --  Obtains the width of the Gtk_Print_Context, in pixels.

   procedure Set_Cairo_Context
     (Context : access Gtk_Print_Context_Record;
      Cr      : Cairo.Cairo_Context;
      Dpi_X   : Gdouble;
      Dpi_Y   : Gdouble);
   --  Context: a Gtk_Print_Context
   --  Cr: the cairo context
   --  Dpi_X: the horizontal resolution to use with Cr
   --  Dpi_Y: the vertical resolution to use with Cr
   --
   --  Sets a new cairo context on a print context.
   --
   --  This function is intended to be used when implementing
   --  an internal print preview, it is not needed for printing,
   --  since GTK+ itself creates a suitable cairo context in that
   --  case.

private

   type Gtk_Print_Context_Record is
     new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_print_context_get_type");

end Gtk.Print_Context;
