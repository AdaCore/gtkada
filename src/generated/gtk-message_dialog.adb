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

package body Gtk.Message_Dialog is

   package Type_Conversion_Gtk_Message_Dialog is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Message_Dialog_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Message_Dialog);

   ----------------------------
   -- Gtk_Message_Dialog_New --
   ----------------------------

   function Gtk_Message_Dialog_New
      (Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "") return Gtk_Message_Dialog
   is
      Dialog : constant Gtk_Message_Dialog := new Gtk_Message_Dialog_Record;
   begin
      Gtk.Message_Dialog.Initialize (Dialog, Parent, Flags, The_Type, Buttons, Message);
      return Dialog;
   end Gtk_Message_Dialog_New;

   ----------------------------------------
   -- Gtk_Message_Dialog_New_With_Markup --
   ----------------------------------------

   function Gtk_Message_Dialog_New_With_Markup
      (Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "") return Gtk_Message_Dialog
   is
      Dialog : constant Gtk_Message_Dialog := new Gtk_Message_Dialog_Record;
   begin
      Gtk.Message_Dialog.Initialize_With_Markup (Dialog, Parent, Flags, The_Type, Buttons, Message);
      return Dialog;
   end Gtk_Message_Dialog_New_With_Markup;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Dialog   : out Gtk_Message_Dialog;
       Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "")
   is
   begin
      Dialog := new Gtk_Message_Dialog_Record;
      Gtk.Message_Dialog.Initialize (Dialog, Parent, Flags, The_Type, Buttons, Message);
   end Gtk_New;

   -------------------------
   -- Gtk_New_With_Markup --
   -------------------------

   procedure Gtk_New_With_Markup
      (Dialog   : out Gtk_Message_Dialog;
       Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "")
   is
   begin
      Dialog := new Gtk_Message_Dialog_Record;
      Gtk.Message_Dialog.Initialize_With_Markup (Dialog, Parent, Flags, The_Type, Buttons, Message);
   end Gtk_New_With_Markup;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Dialog   : not null access Gtk_Message_Dialog_Record'Class;
       Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "")
   is
      function Internal
         (Parent   : System.Address;
          Flags    : Gtk_Dialog_Flags;
          The_Type : Gtk_Message_Type;
          Buttons  : Gtk_Buttons_Type;
          Message  : Gtkada.Types.Chars_Ptr;
          Varargs  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_message_dialog_new");
      Tmp_Message : Gtkada.Types.Chars_Ptr;
      Tmp_Return  : System.Address;
   begin
      if not Dialog.Is_Created then
         if Message = "" then
            Tmp_Message := Gtkada.Types.Null_Ptr;
         else
            Tmp_Message := New_String (Message);
         end if;
         Tmp_Return := Internal (Get_Object_Or_Null (GObject (Parent)), Flags, The_Type, Buttons, Tmp_Message, System.Null_Address);
         Free (Tmp_Message);
         Set_Object (Dialog, Tmp_Return);
      end if;
   end Initialize;

   ----------------------------
   -- Initialize_With_Markup --
   ----------------------------

   procedure Initialize_With_Markup
      (Dialog   : not null access Gtk_Message_Dialog_Record'Class;
       Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "")
   is
      function Internal
         (Parent   : System.Address;
          Flags    : Gtk_Dialog_Flags;
          The_Type : Gtk_Message_Type;
          Buttons  : Gtk_Buttons_Type;
          Message  : Gtkada.Types.Chars_Ptr;
          Varargs  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_message_dialog_new_with_markup");
      Tmp_Message : Gtkada.Types.Chars_Ptr;
      Tmp_Return  : System.Address;
   begin
      if not Dialog.Is_Created then
         if Message = "" then
            Tmp_Message := Gtkada.Types.Null_Ptr;
         else
            Tmp_Message := New_String (Message);
         end if;
         Tmp_Return := Internal (Get_Object_Or_Null (GObject (Parent)), Flags, The_Type, Buttons, Tmp_Message, System.Null_Address);
         Free (Tmp_Message);
         Set_Object (Dialog, Tmp_Return);
      end if;
   end Initialize_With_Markup;

   -----------------------------
   -- Format_Secondary_Markup --
   -----------------------------

   procedure Format_Secondary_Markup
      (Dialog  : not null access Gtk_Message_Dialog_Record;
       Message : UTF8_String := "")
   is
      procedure Internal
         (Dialog  : System.Address;
          Message : Gtkada.Types.Chars_Ptr;
          Varargs : System.Address);
      pragma Import (C, Internal, "gtk_message_dialog_format_secondary_markup");
      Tmp_Message : Gtkada.Types.Chars_Ptr := New_String (Message);
   begin
      Internal (Get_Object (Dialog), Tmp_Message, System.Null_Address);
      Free (Tmp_Message);
   end Format_Secondary_Markup;

   ---------------
   -- Get_Image --
   ---------------

   function Get_Image
      (Dialog : not null access Gtk_Message_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_message_dialog_get_image");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Dialog)), Stub_Gtk_Widget));
   end Get_Image;

   ----------------------
   -- Get_Message_Area --
   ----------------------

   function Get_Message_Area
      (Dialog : not null access Gtk_Message_Dialog_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_message_dialog_get_message_area");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Dialog)), Stub_Gtk_Widget));
   end Get_Message_Area;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
      (Dialog : not null access Gtk_Message_Dialog_Record;
       Image  : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Dialog : System.Address; Image : System.Address);
      pragma Import (C, Internal, "gtk_message_dialog_set_image");
   begin
      Internal (Get_Object (Dialog), Get_Object (Image));
   end Set_Image;

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup
      (Dialog : not null access Gtk_Message_Dialog_Record;
       Str    : UTF8_String)
   is
      procedure Internal
         (Dialog : System.Address;
          Str    : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_message_dialog_set_markup");
      Tmp_Str : Gtkada.Types.Chars_Ptr := New_String (Str);
   begin
      Internal (Get_Object (Dialog), Tmp_Str);
      Free (Tmp_Str);
   end Set_Markup;

end Gtk.Message_Dialog;
