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


pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Types;        use Glib.Types;
with Gtk.Page_Setup;    use Gtk.Page_Setup;
with Gtk.Print_Context; use Gtk.Print_Context;

package Gtk.Print_Operation_Preview is

   type Gtk_Print_Operation_Preview is new Glib.Types.GType_Interface;
   Null_Gtk_Print_Operation_Preview : constant Gtk_Print_Operation_Preview;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_print_operation_preview_get_type");

   -------------
   -- Methods --
   -------------

   procedure End_Preview (Preview : Gtk_Print_Operation_Preview);
   pragma Import (C, End_Preview, "gtk_print_operation_preview_end_preview");
   --  Ends a preview.
   --  This function must be called to finish a custom print preview.
   --  Since: gtk+ 2.10

   function Is_Selected
      (Preview : Gtk_Print_Operation_Preview;
       Page_Nr : Glib.Gint) return Boolean;
   --  Returns whether the given page is included in the set of pages that
   --  have been selected for printing.
   --  Since: gtk+ 2.10
   --  "page_nr": a page number

   procedure Render_Page
      (Preview : Gtk_Print_Operation_Preview;
       Page_Nr : Glib.Gint);
   pragma Import (C, Render_Page, "gtk_print_operation_preview_render_page");
   --  Renders a page to the preview, using the print context that was passed
   --  to the Gtk.Print_Operation.Gtk_Print_Operation::preview handler together
   --  with Preview.
   --  A custom iprint preview should use this function in its ::expose
   --  handler to render the currently selected page.
   --  Note that this function requires a suitable cairo context to be
   --  associated with the print context.
   --  Since: gtk+ 2.10
   --  "page_nr": the page to render

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void is not null access procedure
     (Self       : Gtk_Print_Operation_Preview;
      Context    : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class;
      Page_Setup : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class);

   type Cb_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void is not null access procedure
     (Self       : access Glib.Object.GObject_Record'Class;
      Context    : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class;
      Page_Setup : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class);

   Signal_Got_Page_Size : constant Glib.Signal_Name := "got-page-size";
   procedure On_Got_Page_Size
      (Self  : Gtk_Print_Operation_Preview;
       Call  : Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Gtk_Page_Setup_Void;
       After : Boolean := False);
   procedure On_Got_Page_Size
      (Self  : Gtk_Print_Operation_Preview;
       Call  : Cb_GObject_Gtk_Print_Context_Gtk_Page_Setup_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::got-page-size signal is emitted once for each page that gets
   --  rendered to the preview.
   --
   --  A handler for this signal should update the Context according to
   --  Page_Setup and set up a suitable cairo context, using
   --  Gtk.Print_Context.Set_Cairo_Context.
   -- 
   --  Callback parameters:
   --    --  "context": the current Gtk.Print_Context.Gtk_Print_Context
   --    --  "page_setup": the Gtk.Page_Setup.Gtk_Page_Setup for the current page

   type Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void is not null access procedure
     (Self    : Gtk_Print_Operation_Preview;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class);

   type Cb_GObject_Gtk_Print_Context_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class);

   Signal_Ready : constant Glib.Signal_Name := "ready";
   procedure On_Ready
      (Self  : Gtk_Print_Operation_Preview;
       Call  : Cb_Gtk_Print_Operation_Preview_Gtk_Print_Context_Void;
       After : Boolean := False);
   procedure On_Ready
      (Self  : Gtk_Print_Operation_Preview;
       Call  : Cb_GObject_Gtk_Print_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::ready signal gets emitted once per preview operation, before the
   --  first page is rendered.
   --
   --  A handler for this signal can be used for setup tasks.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Print_Operation_Preview"

   function "+" (W : Gtk_Print_Operation_Preview) return Gtk_Print_Operation_Preview;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_End_Preview is access procedure (Preview : Gtk_Print_Operation_Preview);
   pragma Convention (C, Virtual_End_Preview);
   --  Ends a preview.
   --  This function must be called to finish a custom print preview.
   --  Since: gtk+ 2.10

   type Virtual_Got_Page_Size is access procedure
     (Preview    : Gtk_Print_Operation_Preview;
      Context    : System.Address;
      Page_Setup : System.Address);
   pragma Convention (C, Virtual_Got_Page_Size);

   type Virtual_Is_Selected is access function
     (Preview : Gtk_Print_Operation_Preview;
      Page_Nr : Glib.Gint) return Glib.Gboolean;
   pragma Convention (C, Virtual_Is_Selected);
   --  Returns whether the given page is included in the set of pages that
   --  have been selected for printing.
   --  Since: gtk+ 2.10
   --  "page_nr": a page number

   type Virtual_Ready is access procedure
     (Preview : Gtk_Print_Operation_Preview;
      Context : System.Address);
   pragma Convention (C, Virtual_Ready);

   type Virtual_Render_Page is access procedure
     (Preview : Gtk_Print_Operation_Preview;
      Page_Nr : Glib.Gint);
   pragma Convention (C, Virtual_Render_Page);
   --  Renders a page to the preview, using the print context that was passed
   --  to the Gtk.Print_Operation.Gtk_Print_Operation::preview handler together
   --  with Preview.
   --  A custom iprint preview should use this function in its ::expose
   --  handler to render the currently selected page.
   --  Note that this function requires a suitable cairo context to be
   --  associated with the print context.
   --  Since: gtk+ 2.10
   --  "page_nr": the page to render

   subtype Print_Operation_Preview_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_End_Preview
     (Self    : Print_Operation_Preview_Interface_Descr;
      Handler : Virtual_End_Preview);
   pragma Import (C, Set_End_Preview, "gtkada_Print_Operation_Preview_set_end_preview");

   procedure Set_Got_Page_Size
     (Self    : Print_Operation_Preview_Interface_Descr;
      Handler : Virtual_Got_Page_Size);
   pragma Import (C, Set_Got_Page_Size, "gtkada_Print_Operation_Preview_set_got_page_size");

   procedure Set_Is_Selected
     (Self    : Print_Operation_Preview_Interface_Descr;
      Handler : Virtual_Is_Selected);
   pragma Import (C, Set_Is_Selected, "gtkada_Print_Operation_Preview_set_is_selected");

   procedure Set_Ready
     (Self    : Print_Operation_Preview_Interface_Descr;
      Handler : Virtual_Ready);
   pragma Import (C, Set_Ready, "gtkada_Print_Operation_Preview_set_ready");

   procedure Set_Render_Page
     (Self    : Print_Operation_Preview_Interface_Descr;
      Handler : Virtual_Render_Page);
   pragma Import (C, Set_Render_Page, "gtkada_Print_Operation_Preview_set_render_page");
   --  See Glib.Object.Add_Interface

private

Null_Gtk_Print_Operation_Preview : constant Gtk_Print_Operation_Preview :=
   Gtk_Print_Operation_Preview (Glib.Types.Null_Interface);
end Gtk.Print_Operation_Preview;
