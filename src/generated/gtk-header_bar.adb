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

package body Gtk.Header_Bar is

   package Type_Conversion_Gtk_Header_Bar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Header_Bar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Header_Bar);

   ------------------------
   -- Gtk_Header_Bar_New --
   ------------------------

   function Gtk_Header_Bar_New return Gtk_Header_Bar is
      Self : constant Gtk_Header_Bar := new Gtk_Header_Bar_Record;
   begin
      Gtk.Header_Bar.Initialize (Self);
      return Self;
   end Gtk_Header_Bar_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Header_Bar) is
   begin
      Self := new Gtk_Header_Bar_Record;
      Gtk.Header_Bar.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Header_Bar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_header_bar_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------------
   -- Get_Custom_Title --
   ----------------------

   function Get_Custom_Title
      (Self : not null access Gtk_Header_Bar_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_header_bar_get_custom_title");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Custom_Title;

   ---------------------------
   -- Get_Decoration_Layout --
   ---------------------------

   function Get_Decoration_Layout
      (Self : not null access Gtk_Header_Bar_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_header_bar_get_decoration_layout");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Decoration_Layout;

   ----------------------
   -- Get_Has_Subtitle --
   ----------------------

   function Get_Has_Subtitle
      (Self : not null access Gtk_Header_Bar_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_header_bar_get_has_subtitle");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Subtitle;

   ---------------------------
   -- Get_Show_Close_Button --
   ---------------------------

   function Get_Show_Close_Button
      (Self : not null access Gtk_Header_Bar_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_header_bar_get_show_close_button");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Show_Close_Button;

   ------------------
   -- Get_Subtitle --
   ------------------

   function Get_Subtitle
      (Self : not null access Gtk_Header_Bar_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_header_bar_get_subtitle");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Subtitle;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Self : not null access Gtk_Header_Bar_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_header_bar_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Title;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Self  : not null access Gtk_Header_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_header_bar_pack_end");
   begin
      Internal (Get_Object (Self), Get_Object (Child));
   end Pack_End;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
      (Self  : not null access Gtk_Header_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_header_bar_pack_start");
   begin
      Internal (Get_Object (Self), Get_Object (Child));
   end Pack_Start;

   ----------------------
   -- Set_Custom_Title --
   ----------------------

   procedure Set_Custom_Title
      (Self         : not null access Gtk_Header_Bar_Record;
       Title_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Self         : System.Address;
          Title_Widget : System.Address);
      pragma Import (C, Internal, "gtk_header_bar_set_custom_title");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Title_Widget)));
   end Set_Custom_Title;

   ---------------------------
   -- Set_Decoration_Layout --
   ---------------------------

   procedure Set_Decoration_Layout
      (Self   : not null access Gtk_Header_Bar_Record;
       Layout : UTF8_String := "")
   is
      procedure Internal
         (Self   : System.Address;
          Layout : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_header_bar_set_decoration_layout");
      Tmp_Layout : Gtkada.Types.Chars_Ptr;
   begin
      if Layout = "" then
         Tmp_Layout := Gtkada.Types.Null_Ptr;
      else
         Tmp_Layout := New_String (Layout);
      end if;
      Internal (Get_Object (Self), Tmp_Layout);
      Free (Tmp_Layout);
   end Set_Decoration_Layout;

   ----------------------
   -- Set_Has_Subtitle --
   ----------------------

   procedure Set_Has_Subtitle
      (Self    : not null access Gtk_Header_Bar_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_header_bar_set_has_subtitle");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Has_Subtitle;

   ---------------------------
   -- Set_Show_Close_Button --
   ---------------------------

   procedure Set_Show_Close_Button
      (Self    : not null access Gtk_Header_Bar_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_header_bar_set_show_close_button");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Show_Close_Button;

   ------------------
   -- Set_Subtitle --
   ------------------

   procedure Set_Subtitle
      (Self     : not null access Gtk_Header_Bar_Record;
       Subtitle : UTF8_String := "")
   is
      procedure Internal
         (Self     : System.Address;
          Subtitle : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_header_bar_set_subtitle");
      Tmp_Subtitle : Gtkada.Types.Chars_Ptr;
   begin
      if Subtitle = "" then
         Tmp_Subtitle := Gtkada.Types.Null_Ptr;
      else
         Tmp_Subtitle := New_String (Subtitle);
      end if;
      Internal (Get_Object (Self), Tmp_Subtitle);
      Free (Tmp_Subtitle);
   end Set_Subtitle;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Self  : not null access Gtk_Header_Bar_Record;
       Title : UTF8_String := "")
   is
      procedure Internal
         (Self  : System.Address;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_header_bar_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr;
   begin
      if Title = "" then
         Tmp_Title := Gtkada.Types.Null_Ptr;
      else
         Tmp_Title := New_String (Title);
      end if;
      Internal (Get_Object (Self), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

end Gtk.Header_Bar;
