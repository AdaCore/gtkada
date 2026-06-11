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

package body Gtk.Scroll_Info is

   function From_Object_Free
     (B : access Gtk_Scroll_Info'Class) return Gtk_Scroll_Info
   is
      Result : constant Gtk_Scroll_Info := Gtk_Scroll_Info (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Scroll_Info is
      S : Gtk_Scroll_Info;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Scroll_Info) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_scroll_info_new");
   begin
      Self.Set_Object (Internal);
   end Gtk_New;

   -------------------------
   -- Gtk_Scroll_Info_New --
   -------------------------

   function Gtk_Scroll_Info_New return Gtk_Scroll_Info is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_scroll_info_new");
      Self : Gtk_Scroll_Info;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gtk_Scroll_Info_New;

   ---------------------------
   -- Get_Enable_Horizontal --
   ---------------------------

   function Get_Enable_Horizontal (Self : Gtk_Scroll_Info) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scroll_info_get_enable_horizontal");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Enable_Horizontal;

   -------------------------
   -- Get_Enable_Vertical --
   -------------------------

   function Get_Enable_Vertical (Self : Gtk_Scroll_Info) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scroll_info_get_enable_vertical");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Enable_Vertical;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gtk_Scroll_Info) return Gtk_Scroll_Info is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scroll_info_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   ---------------------------
   -- Set_Enable_Horizontal --
   ---------------------------

   procedure Set_Enable_Horizontal
      (Self       : Gtk_Scroll_Info;
       Horizontal : Boolean)
   is
      procedure Internal (Self : System.Address; Horizontal : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_scroll_info_set_enable_horizontal");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Horizontal));
   end Set_Enable_Horizontal;

   -------------------------
   -- Set_Enable_Vertical --
   -------------------------

   procedure Set_Enable_Vertical
      (Self     : Gtk_Scroll_Info;
       Vertical : Boolean)
   is
      procedure Internal (Self : System.Address; Vertical : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_scroll_info_set_enable_vertical");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Vertical));
   end Set_Enable_Vertical;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gtk_Scroll_Info) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_scroll_info_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gtk.Scroll_Info;
