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
with Gtkada.Bindings; use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Icon_Source is

   function From_Object_Free
     (B : access Gtk_Icon_Source'Class) return Gtk_Icon_Source
   is
      Result : constant Gtk_Icon_Source := Gtk_Icon_Source (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Icon_Source is
      S : Gtk_Icon_Source;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -------------------------
   -- Gtk_Icon_Source_New --
   -------------------------

   function Gtk_Icon_Source_New return Gtk_Icon_Source is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_source_new");
      Self : Gtk_Icon_Source;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gtk_Icon_Source_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Icon_Source) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_source_new");
   begin
      Self.Set_Object (Internal);
   end Gtk_New;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Gtk_Icon_Source) return Gtk_Icon_Source is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_source_copy");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Self : Gtk_Icon_Source) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_icon_source_free");
   begin
      Internal (Get_Object (Self));
   end Free;

   -------------------
   -- Get_Direction --
   -------------------

   function Get_Direction
      (Self : Gtk_Icon_Source) return Gtk.Enums.Gtk_Text_Direction
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Text_Direction;
      pragma Import (C, Internal, "gtk_icon_source_get_direction");
   begin
      return Internal (Get_Object (Self));
   end Get_Direction;

   ------------------------------
   -- Get_Direction_Wildcarded --
   ------------------------------

   function Get_Direction_Wildcarded (Self : Gtk_Icon_Source) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_source_get_direction_wildcarded");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Direction_Wildcarded;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Self : Gtk_Icon_Source) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_icon_source_get_filename");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Filename;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name (Self : Gtk_Icon_Source) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_icon_source_get_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Icon_Name;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf (Self : Gtk_Icon_Source) return Gdk.Pixbuf.Gdk_Pixbuf is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_source_get_pixbuf");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Pixbuf));
   end Get_Pixbuf;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (Self : Gtk_Icon_Source) return Gtk.Enums.Gtk_Icon_Size is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Icon_Size;
      pragma Import (C, Internal, "gtk_icon_source_get_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Size;

   -------------------------
   -- Get_Size_Wildcarded --
   -------------------------

   function Get_Size_Wildcarded (Self : Gtk_Icon_Source) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_source_get_size_wildcarded");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Size_Wildcarded;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
      (Self : Gtk_Icon_Source) return Gtk.Enums.Gtk_State_Type
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_State_Type;
      pragma Import (C, Internal, "gtk_icon_source_get_state");
   begin
      return Internal (Get_Object (Self));
   end Get_State;

   --------------------------
   -- Get_State_Wildcarded --
   --------------------------

   function Get_State_Wildcarded (Self : Gtk_Icon_Source) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_source_get_state_wildcarded");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_State_Wildcarded;

   -------------------
   -- Set_Direction --
   -------------------

   procedure Set_Direction
      (Self      : Gtk_Icon_Source;
       Direction : Gtk.Enums.Gtk_Text_Direction)
   is
      procedure Internal
         (Self      : System.Address;
          Direction : Gtk.Enums.Gtk_Text_Direction);
      pragma Import (C, Internal, "gtk_icon_source_set_direction");
   begin
      Internal (Get_Object (Self), Direction);
   end Set_Direction;

   ------------------------------
   -- Set_Direction_Wildcarded --
   ------------------------------

   procedure Set_Direction_Wildcarded
      (Self    : Gtk_Icon_Source;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_icon_source_set_direction_wildcarded");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Direction_Wildcarded;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename (Self : Gtk_Icon_Source; Filename : UTF8_String) is
      procedure Internal
         (Self     : System.Address;
          Filename : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_icon_source_set_filename");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
   begin
      Internal (Get_Object (Self), Tmp_Filename);
      Free (Tmp_Filename);
   end Set_Filename;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
      (Self      : Gtk_Icon_Source;
       Icon_Name : UTF8_String := "")
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_icon_source_set_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Icon_Name = "" then
         Tmp_Icon_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Icon_Name := New_String (Icon_Name);
      end if;
      Internal (Get_Object (Self), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_Icon_Name;

   ----------------
   -- Set_Pixbuf --
   ----------------

   procedure Set_Pixbuf
      (Self   : Gtk_Icon_Source;
       Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal (Self : System.Address; Pixbuf : System.Address);
      pragma Import (C, Internal, "gtk_icon_source_set_pixbuf");
   begin
      Internal (Get_Object (Self), Get_Object (Pixbuf));
   end Set_Pixbuf;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
      (Self : Gtk_Icon_Source;
       Size : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Self : System.Address;
          Size : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_icon_source_set_size");
   begin
      Internal (Get_Object (Self), Size);
   end Set_Size;

   -------------------------
   -- Set_Size_Wildcarded --
   -------------------------

   procedure Set_Size_Wildcarded (Self : Gtk_Icon_Source; Setting : Boolean) is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_icon_source_set_size_wildcarded");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Size_Wildcarded;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
      (Self  : Gtk_Icon_Source;
       State : Gtk.Enums.Gtk_State_Type)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Enums.Gtk_State_Type);
      pragma Import (C, Internal, "gtk_icon_source_set_state");
   begin
      Internal (Get_Object (Self), State);
   end Set_State;

   --------------------------
   -- Set_State_Wildcarded --
   --------------------------

   procedure Set_State_Wildcarded
      (Self    : Gtk_Icon_Source;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_icon_source_set_state_wildcarded");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_State_Wildcarded;

end Gtk.Icon_Source;
