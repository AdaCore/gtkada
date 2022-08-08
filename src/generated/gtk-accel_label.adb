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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Accel_Label is

   package Type_Conversion_Gtk_Accel_Label is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Accel_Label_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Accel_Label);

   -------------------------
   -- Gtk_Accel_Label_New --
   -------------------------

   function Gtk_Accel_Label_New
      (String : UTF8_String) return Gtk_Accel_Label
   is
      Accel_Label : constant Gtk_Accel_Label := new Gtk_Accel_Label_Record;
   begin
      Gtk.Accel_Label.Initialize (Accel_Label, String);
      return Accel_Label;
   end Gtk_Accel_Label_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Accel_Label : out Gtk_Accel_Label;
       String      : UTF8_String)
   is
   begin
      Accel_Label := new Gtk_Accel_Label_Record;
      Gtk.Accel_Label.Initialize (Accel_Label, String);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Accel_Label : not null access Gtk_Accel_Label_Record'Class;
       String      : UTF8_String)
   is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_accel_label_new");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
   begin
      if not Accel_Label.Is_Created then
         Tmp_Return := Internal (Tmp_String);
         Free (Tmp_String);
         Set_Object (Accel_Label, Tmp_Return);
      end if;
   end Initialize;

   ---------------
   -- Get_Accel --
   ---------------

   procedure Get_Accel
      (Accel_Label      : not null access Gtk_Accel_Label_Record;
       Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
         (Accel_Label      : System.Address;
          Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
          Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_accel_label_get_accel");
   begin
      Internal (Get_Object (Accel_Label), Accelerator_Key, Accelerator_Mods);
   end Get_Accel;

   ----------------------
   -- Get_Accel_Widget --
   ----------------------

   function Get_Accel_Widget
      (Accel_Label : not null access Gtk_Accel_Label_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Accel_Label : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accel_label_get_accel_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Accel_Label)), Stub_Gtk_Widget));
   end Get_Accel_Widget;

   ---------------------
   -- Get_Accel_Width --
   ---------------------

   function Get_Accel_Width
      (Accel_Label : not null access Gtk_Accel_Label_Record) return Guint
   is
      function Internal (Accel_Label : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_accel_label_get_accel_width");
   begin
      return Internal (Get_Object (Accel_Label));
   end Get_Accel_Width;

   -------------
   -- Refetch --
   -------------

   function Refetch
      (Accel_Label : not null access Gtk_Accel_Label_Record) return Boolean
   is
      function Internal (Accel_Label : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accel_label_refetch");
   begin
      return Internal (Get_Object (Accel_Label)) /= 0;
   end Refetch;

   ---------------
   -- Set_Accel --
   ---------------

   procedure Set_Accel
      (Accel_Label      : not null access Gtk_Accel_Label_Record;
       Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
         (Accel_Label      : System.Address;
          Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
          Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_accel_label_set_accel");
   begin
      Internal (Get_Object (Accel_Label), Accelerator_Key, Accelerator_Mods);
   end Set_Accel;

   -----------------------
   -- Set_Accel_Closure --
   -----------------------

   procedure Set_Accel_Closure
      (Accel_Label   : not null access Gtk_Accel_Label_Record;
       Accel_Closure : System.Address)
   is
      procedure Internal
         (Accel_Label   : System.Address;
          Accel_Closure : System.Address);
      pragma Import (C, Internal, "gtk_accel_label_set_accel_closure");
   begin
      Internal (Get_Object (Accel_Label), Accel_Closure);
   end Set_Accel_Closure;

   ----------------------
   -- Set_Accel_Widget --
   ----------------------

   procedure Set_Accel_Widget
      (Accel_Label  : not null access Gtk_Accel_Label_Record;
       Accel_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Accel_Label  : System.Address;
          Accel_Widget : System.Address);
      pragma Import (C, Internal, "gtk_accel_label_set_accel_widget");
   begin
      Internal (Get_Object (Accel_Label), Get_Object_Or_Null (GObject (Accel_Widget)));
   end Set_Accel_Widget;

end Gtk.Accel_Label;
