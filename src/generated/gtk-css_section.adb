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

package body Gtk.Css_Section is

   function From_Object_Free
     (B : access Gtk_Css_Section'Class) return Gtk_Css_Section
   is
      Result : constant Gtk_Css_Section := Gtk_Css_Section (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Css_Section is
      S : Gtk_Css_Section;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   ------------------
   -- Get_End_Line --
   ------------------

   function Get_End_Line (Self : Gtk_Css_Section) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_css_section_get_end_line");
   begin
      return Internal (Get_Object (Self));
   end Get_End_Line;

   ----------------------
   -- Get_End_Position --
   ----------------------

   function Get_End_Position (Self : Gtk_Css_Section) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_css_section_get_end_position");
   begin
      return Internal (Get_Object (Self));
   end Get_End_Position;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Self : Gtk_Css_Section) return Gtk_Css_Section is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_css_section_get_parent");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Parent;

   ----------------------
   -- Get_Section_Type --
   ----------------------

   function Get_Section_Type
      (Self : Gtk_Css_Section) return Gtk_Css_Section_Type
   is
      function Internal (Self : System.Address) return Gtk_Css_Section_Type;
      pragma Import (C, Internal, "gtk_css_section_get_section_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Section_Type;

   --------------------
   -- Get_Start_Line --
   --------------------

   function Get_Start_Line (Self : Gtk_Css_Section) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_css_section_get_start_line");
   begin
      return Internal (Get_Object (Self));
   end Get_Start_Line;

   ------------------------
   -- Get_Start_Position --
   ------------------------

   function Get_Start_Position (Self : Gtk_Css_Section) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_css_section_get_start_position");
   begin
      return Internal (Get_Object (Self));
   end Get_Start_Position;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gtk_Css_Section) return Gtk_Css_Section is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_css_section_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gtk_Css_Section) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_css_section_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gtk.Css_Section;
