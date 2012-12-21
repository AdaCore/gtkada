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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Cell_Renderer_Accel is

   package Type_Conversion_Gtk_Cell_Renderer_Accel is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Renderer_Accel_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Cell_Renderer_Accel);

   ---------------------------------
   -- Gtk_Cell_Renderer_Accel_New --
   ---------------------------------

   function Gtk_Cell_Renderer_Accel_New return Gtk_Cell_Renderer_Accel is
      Self : constant Gtk_Cell_Renderer_Accel := new Gtk_Cell_Renderer_Accel_Record;
   begin
      Gtk.Cell_Renderer_Accel.Initialize (Self);
      return Self;
   end Gtk_Cell_Renderer_Accel_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Accel) is
   begin
      Self := new Gtk_Cell_Renderer_Accel_Record;
      Gtk.Cell_Renderer_Accel.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Accel_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_renderer_accel_new");
   begin
      Set_Object (Self, Internal);
   end Initialize;

   ----------------------
   -- On_Accel_Cleared --
   ----------------------

   procedure On_Accel_Cleared
      (Self : not null access Gtk_Cell_Renderer_Accel_Record;
       Call : not null access procedure
         (Self        : access Gtk_Cell_Renderer_Accel_Record'Class;
          Path_String : UTF8_String))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Accel_Cleared;

   ----------------------
   -- On_Accel_Cleared --
   ----------------------

   procedure On_Accel_Cleared
      (Self : not null access Gtk_Cell_Renderer_Accel_Record;
       Call : not null access procedure
         (Self        : access Glib.Object.GObject_Record'Class;
          Path_String : UTF8_String);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Accel_Cleared;

   ---------------------
   -- On_Accel_Edited --
   ---------------------

   procedure On_Accel_Edited
      (Self : not null access Gtk_Cell_Renderer_Accel_Record;
       Call : not null access procedure
         (Self             : access Gtk_Cell_Renderer_Accel_Record'Class;
          Path_String      : UTF8_String;
          Accel_Key        : Guint;
          Accel_Mods       : Gdk.Types.Gdk_Modifier_Type;
          Hardware_Keycode : Guint))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Accel_Edited;

   ---------------------
   -- On_Accel_Edited --
   ---------------------

   procedure On_Accel_Edited
      (Self : not null access Gtk_Cell_Renderer_Accel_Record;
       Call : not null access procedure
         (Self             : access Glib.Object.GObject_Record'Class;
          Path_String      : UTF8_String;
          Accel_Key        : Guint;
          Accel_Mods       : Gdk.Types.Gdk_Modifier_Type;
          Hardware_Keycode : Guint);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Accel_Edited;

end Gtk.Cell_Renderer_Accel;
