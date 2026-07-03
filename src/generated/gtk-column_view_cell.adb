------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

package body Gtk.Column_View_Cell is

   package Type_Conversion_Gtk_Column_View_Cell is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Column_View_Cell_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Column_View_Cell);

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : not null access Gtk_Column_View_Cell_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_column_view_cell_get_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Child;

   -------------------
   -- Get_Focusable --
   -------------------

   function Get_Focusable
      (Self : not null access Gtk_Column_View_Cell_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_column_view_cell_get_focusable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Focusable;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self : not null access Gtk_Column_View_Cell_Record)
       return System.Address
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_column_view_cell_get_item");
   begin
      return Internal (Get_Object (Self));
   end Get_Item;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Self : not null access Gtk_Column_View_Cell_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_column_view_cell_get_position");
   begin
      return Internal (Get_Object (Self));
   end Get_Position;

   ------------------
   -- Get_Selected --
   ------------------

   function Get_Selected
      (Self : not null access Gtk_Column_View_Cell_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_column_view_cell_get_selected");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Selected;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
      (Self  : not null access Gtk_Column_View_Cell_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_column_view_cell_set_child");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Set_Child;

   -------------------
   -- Set_Focusable --
   -------------------

   procedure Set_Focusable
      (Self      : not null access Gtk_Column_View_Cell_Record;
       Focusable : Boolean)
   is
      procedure Internal (Self : System.Address; Focusable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_column_view_cell_set_focusable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Focusable));
   end Set_Focusable;

end Gtk.Column_View_Cell;
