------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

package body Gtk.Print_Operation_Preview is

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
      (Preview : Gtk_Print_Operation_Preview;
       Page_Nr : Gint) return Boolean
   is
      function Internal
         (Preview : Gtk_Print_Operation_Preview;
          Page_Nr : Gint) return Integer;
      pragma Import (C, Internal, "gtk_print_operation_preview_is_selected");
   begin
      return Boolean'Val (Internal (Preview, Page_Nr));
   end Is_Selected;

   ----------------------
   -- On_Got_Page_Size --
   ----------------------

   procedure On_Got_Page_Size
      (Self : Gtk_Print_Operation_Preview;
       Call : not null access procedure
         (Self       : Gtk_Print_Operation_Preview;
          Context    : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class;
          Page_Setup : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Got_Page_Size;

   ----------------------
   -- On_Got_Page_Size --
   ----------------------

   procedure On_Got_Page_Size
      (Self : Gtk_Print_Operation_Preview;
       Call : not null access procedure
         (Self       : access Glib.Object.GObject_Record'Class;
          Context    : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class;
          Page_Setup : not null access Gtk.Page_Setup.Gtk_Page_Setup_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Got_Page_Size;

   --------------
   -- On_Ready --
   --------------

   procedure On_Ready
      (Self : Gtk_Print_Operation_Preview;
       Call : not null access procedure
         (Self    : Gtk_Print_Operation_Preview;
          Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Ready;

   --------------
   -- On_Ready --
   --------------

   procedure On_Ready
      (Self : Gtk_Print_Operation_Preview;
       Call : not null access procedure
         (Self    : access Glib.Object.GObject_Record'Class;
          Context : not null access Gtk.Print_Context.Gtk_Print_Context_Record'Class);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Ready;

   function "+" (W : Gtk_Print_Operation_Preview) return Gtk_Print_Operation_Preview is
   begin
      return W;
   end "+";

end Gtk.Print_Operation_Preview;
