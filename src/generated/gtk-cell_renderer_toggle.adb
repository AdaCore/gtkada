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

package body Gtk.Cell_Renderer_Toggle is

   package Type_Conversion_Gtk_Cell_Renderer_Toggle is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Renderer_Toggle_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Cell_Renderer_Toggle);

   ----------------------------------
   -- Gtk_Cell_Renderer_Toggle_New --
   ----------------------------------

   function Gtk_Cell_Renderer_Toggle_New return Gtk_Cell_Renderer_Toggle is
      Self : constant Gtk_Cell_Renderer_Toggle := new Gtk_Cell_Renderer_Toggle_Record;
   begin
      Gtk.Cell_Renderer_Toggle.Initialize (Self);
      return Self;
   end Gtk_Cell_Renderer_Toggle_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Toggle) is
   begin
      Self := new Gtk_Cell_Renderer_Toggle_Record;
      Gtk.Cell_Renderer_Toggle.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_new");
   begin
      Set_Object (Self, Internal);
   end Initialize;

   ---------------------
   -- Get_Activatable --
   ---------------------

   function Get_Activatable
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record)
       return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_get_activatable");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Activatable;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record)
       return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_get_active");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Active;

   ---------------
   -- Get_Radio --
   ---------------

   function Get_Radio
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record)
       return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_get_radio");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Radio;

   ---------------------
   -- Set_Activatable --
   ---------------------

   procedure Set_Activatable
      (Self    : not null access Gtk_Cell_Renderer_Toggle_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Integer);
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_set_activatable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Activatable;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
      (Self    : not null access Gtk_Cell_Renderer_Toggle_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Integer);
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_set_active");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Active;

   ---------------
   -- Set_Radio --
   ---------------

   procedure Set_Radio
      (Self  : not null access Gtk_Cell_Renderer_Toggle_Record;
       Radio : Boolean)
   is
      procedure Internal (Self : System.Address; Radio : Integer);
      pragma Import (C, Internal, "gtk_cell_renderer_toggle_set_radio");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Radio));
   end Set_Radio;

   ----------------
   -- On_Toggled --
   ----------------

   procedure On_Toggled
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record;
       Call : not null access procedure
         (Self : access Gtk_Cell_Renderer_Toggle_Record'Class;
          Path : UTF8_String))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Toggled;

   ----------------
   -- On_Toggled --
   ----------------

   procedure On_Toggled
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record;
       Call : not null access procedure
         (Self : access Glib.Object.GObject_Record'Class;
          Path : UTF8_String);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Toggled;

end Gtk.Cell_Renderer_Toggle;
