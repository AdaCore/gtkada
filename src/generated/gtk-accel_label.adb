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
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Accel_Label is

   package Type_Conversion_Gtk_Accel_Label is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Accel_Label_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Accel_Label);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Accel_Label; String : UTF8_String) is
   begin
      Self := new Gtk_Accel_Label_Record;
      Gtk.Accel_Label.Initialize (Self, String);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Accel_Label_Record'Class;
       String : UTF8_String)
   is
      function Internal
         (String : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_accel_label_new");
      Tmp_String : Interfaces.C.Strings.chars_ptr := New_String (String);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_String);
      Free (Tmp_String);
      Set_Object (Self, Tmp_Return);
   end Initialize;

   ----------------------
   -- Get_Accel_Widget --
   ----------------------

   function Get_Accel_Widget
      (Self : not null access Gtk_Accel_Label_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accel_label_get_accel_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Accel_Widget;

   ---------------------
   -- Get_Accel_Width --
   ---------------------

   function Get_Accel_Width
      (Self : not null access Gtk_Accel_Label_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_accel_label_get_accel_width");
   begin
      return Internal (Get_Object (Self));
   end Get_Accel_Width;

   -------------
   -- Refetch --
   -------------

   function Refetch
      (Self : not null access Gtk_Accel_Label_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_accel_label_refetch");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Refetch;

   -----------------------
   -- Set_Accel_Closure --
   -----------------------

   procedure Set_Accel_Closure
      (Self          : not null access Gtk_Accel_Label_Record;
       Accel_Closure : System.Address)
   is
      procedure Internal
         (Self          : System.Address;
          Accel_Closure : System.Address);
      pragma Import (C, Internal, "gtk_accel_label_set_accel_closure");
   begin
      Internal (Get_Object (Self), Accel_Closure);
   end Set_Accel_Closure;

   ----------------------
   -- Set_Accel_Widget --
   ----------------------

   procedure Set_Accel_Widget
      (Self         : not null access Gtk_Accel_Label_Record;
       Accel_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Self         : System.Address;
          Accel_Widget : System.Address);
      pragma Import (C, Internal, "gtk_accel_label_set_accel_widget");
   begin
      Internal (Get_Object (Self), Get_Object (Accel_Widget));
   end Set_Accel_Widget;

end Gtk.Accel_Label;
