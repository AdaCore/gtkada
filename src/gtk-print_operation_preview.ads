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
--  This package contains functionality for composing a custom print preview
--  facility.
--  </description>
--  <c_version>2.16.6</c_version>

with Glib.Types;

package Gtk.Print_Operation_Preview is

   type Gtk_Print_Operation_Preview is new Glib.Types.GType_Interface;

   function Get_Type return GType;

   procedure End_Preview (Preview : Gtk_Print_Operation_Preview);
   --  Ends a preview.
   --  This function must be called to finish a custom print preview.

   function Is_Selected
     (Preview : Gtk_Print_Operation_Preview;
      Page_Nr : Gint)
      return Boolean;
   --  Returns whether the given page is included in the set of pages that
   --  have been selected for printing.

   procedure Render_Page
     (Preview : Gtk_Print_Operation_Preview;
      Page_Nr : Gint);
   --  Renders a page to the preview, using the print context that
   --  was passed to the "preview" handler together with Preview.
   --
   --  A custom iprint preview should use this function in its "expose"
   --  handler to render the currently selected page.
   --
   --  Note that this function requires a suitable cairo context to
   --  be associated with the print context.

private

   pragma Import (C, Get_Type, "gtk_print_operation_preview_get_type");
   pragma Import (C, End_Preview, "gtk_print_operation_preview_end_preview");
   pragma Import (C, Render_Page, "gtk_print_operation_preview_render_page");

end Gtk.Print_Operation_Preview;
