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
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Frame is

   package Type_Conversion_Gtk_Frame is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Frame_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Frame);

   -------------------
   -- Gtk_Frame_New --
   -------------------

   function Gtk_Frame_New (Label : UTF8_String := "") return Gtk_Frame is
      Frame : constant Gtk_Frame := new Gtk_Frame_Record;
   begin
      Gtk.Frame.Initialize (Frame, Label);
      return Frame;
   end Gtk_Frame_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Frame : out Gtk_Frame; Label : UTF8_String := "") is
   begin
      Frame := new Gtk_Frame_Record;
      Gtk.Frame.Initialize (Frame, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Frame : not null access Gtk_Frame_Record'Class;
       Label : UTF8_String := "")
   is
      function Internal
         (Label : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_frame_new");
      Tmp_Label  : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Frame.Is_Created then
         if Label = "" then
            Tmp_Label := Gtkada.Types.Null_Ptr;
         else
            Tmp_Label := New_String (Label);
         end if;
         Tmp_Return := Internal (Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Frame, Tmp_Return);
      end if;
   end Initialize;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
      (Frame : not null access Gtk_Frame_Record) return UTF8_String
   is
      function Internal
         (Frame : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_frame_get_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Frame)));
   end Get_Label;

   ---------------------
   -- Get_Label_Align --
   ---------------------

   procedure Get_Label_Align
      (Frame  : not null access Gtk_Frame_Record;
       Xalign : out Gfloat;
       Yalign : out Gfloat)
   is
      procedure Internal
         (Frame  : System.Address;
          Xalign : out Gfloat;
          Yalign : out Gfloat);
      pragma Import (C, Internal, "gtk_frame_get_label_align");
   begin
      Internal (Get_Object (Frame), Xalign, Yalign);
   end Get_Label_Align;

   ----------------------
   -- Get_Label_Widget --
   ----------------------

   function Get_Label_Widget
      (Frame : not null access Gtk_Frame_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Frame : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_frame_get_label_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Frame)), Stub_Gtk_Widget));
   end Get_Label_Widget;

   ---------------------
   -- Get_Shadow_Type --
   ---------------------

   function Get_Shadow_Type
      (Frame : not null access Gtk_Frame_Record)
       return Gtk.Enums.Gtk_Shadow_Type
   is
      function Internal
         (Frame : System.Address) return Gtk.Enums.Gtk_Shadow_Type;
      pragma Import (C, Internal, "gtk_frame_get_shadow_type");
   begin
      return Internal (Get_Object (Frame));
   end Get_Shadow_Type;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Frame : not null access Gtk_Frame_Record;
       Label : UTF8_String := "")
   is
      procedure Internal
         (Frame : System.Address;
          Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_frame_set_label");
      Tmp_Label : Gtkada.Types.Chars_Ptr;
   begin
      if Label = "" then
         Tmp_Label := Gtkada.Types.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Internal (Get_Object (Frame), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   ---------------------
   -- Set_Label_Align --
   ---------------------

   procedure Set_Label_Align
      (Frame  : not null access Gtk_Frame_Record;
       Xalign : Gfloat;
       Yalign : Gfloat)
   is
      procedure Internal
         (Frame  : System.Address;
          Xalign : Gfloat;
          Yalign : Gfloat);
      pragma Import (C, Internal, "gtk_frame_set_label_align");
   begin
      Internal (Get_Object (Frame), Xalign, Yalign);
   end Set_Label_Align;

   ----------------------
   -- Set_Label_Widget --
   ----------------------

   procedure Set_Label_Widget
      (Frame        : not null access Gtk_Frame_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Frame        : System.Address;
          Label_Widget : System.Address);
      pragma Import (C, Internal, "gtk_frame_set_label_widget");
   begin
      Internal (Get_Object (Frame), Get_Object_Or_Null (GObject (Label_Widget)));
   end Set_Label_Widget;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Frame    : not null access Gtk_Frame_Record;
       The_Type : Gtk.Enums.Gtk_Shadow_Type)
   is
      procedure Internal
         (Frame    : System.Address;
          The_Type : Gtk.Enums.Gtk_Shadow_Type);
      pragma Import (C, Internal, "gtk_frame_set_shadow_type");
   begin
      Internal (Get_Object (Frame), The_Type);
   end Set_Shadow_Type;

end Gtk.Frame;
