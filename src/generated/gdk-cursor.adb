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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types; use Gtkada.Types;
pragma Warnings(On);

package body Gdk.Cursor is

   --------------------
   -- Gdk_Cursor_New --
   --------------------

   function Gdk_Cursor_New (Cursor_Type : Gdk_Cursor_Type) return Gdk_Cursor is
      function Internal (Cursor_Type : Gdk_Cursor_Type) return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new");
      Self : Gdk_Cursor;
   begin
      Self := Internal (Cursor_Type);
      return Self;
   end Gdk_Cursor_New;

   --------------------------------
   -- Gdk_Cursor_New_For_Display --
   --------------------------------

   function Gdk_Cursor_New_For_Display
      (Display     : not null access Gdk.Display.Gdk_Display_Record'Class;
       Cursor_Type : Gdk_Cursor_Type) return Gdk_Cursor
   is
      function Internal
         (Display     : System.Address;
          Cursor_Type : Gdk_Cursor_Type) return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new_for_display");
      Self : Gdk_Cursor;
   begin
      Self := Internal (Get_Object (Display), Cursor_Type);
      return Self;
   end Gdk_Cursor_New_For_Display;

   ------------------------------
   -- Gdk_Cursor_New_From_Name --
   ------------------------------

   function Gdk_Cursor_New_From_Name
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       Name    : UTF8_String) return Gdk_Cursor
   is
      function Internal
         (Display : System.Address;
          Name    : Gtkada.Types.Chars_Ptr) return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new_from_name");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : Gdk_Cursor;
      Self       : Gdk_Cursor;
   begin
      Tmp_Return := Internal (Get_Object (Display), Tmp_Name);
      Free (Tmp_Name);
      Self := Tmp_Return;
      return Self;
   end Gdk_Cursor_New_From_Name;

   ---------------------------------
   -- Gdk_Cursor_New_From_Surface --
   ---------------------------------

   function Gdk_Cursor_New_From_Surface
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       Surface : Cairo.Cairo_Surface;
       X       : Gdouble;
       Y       : Gdouble) return Gdk_Cursor
   is
      function Internal
         (Display : System.Address;
          Surface : Cairo.Cairo_Surface;
          X       : Gdouble;
          Y       : Gdouble) return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new_from_surface");
      Self : Gdk_Cursor;
   begin
      Self := Internal (Get_Object (Display), Surface, X, Y);
      return Self;
   end Gdk_Cursor_New_From_Surface;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Self : out Gdk_Cursor; Cursor_Type : Gdk_Cursor_Type) is
      function Internal (Cursor_Type : Gdk_Cursor_Type) return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new");
   begin
      Self := Internal (Cursor_Type);
   end Gdk_New;

   -------------------------
   -- Gdk_New_For_Display --
   -------------------------

   procedure Gdk_New_For_Display
      (Self        : out Gdk_Cursor;
       Display     : not null access Gdk.Display.Gdk_Display_Record'Class;
       Cursor_Type : Gdk_Cursor_Type)
   is
      function Internal
         (Display     : System.Address;
          Cursor_Type : Gdk_Cursor_Type) return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new_for_display");
   begin
      Self := Internal (Get_Object (Display), Cursor_Type);
   end Gdk_New_For_Display;

   -----------------------
   -- Gdk_New_From_Name --
   -----------------------

   procedure Gdk_New_From_Name
      (Self    : out Gdk_Cursor;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       Name    : UTF8_String)
   is
      function Internal
         (Display : System.Address;
          Name    : Gtkada.Types.Chars_Ptr) return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new_from_name");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : Gdk_Cursor;
   begin
      Tmp_Return := Internal (Get_Object (Display), Tmp_Name);
      Free (Tmp_Name);
      Self := Tmp_Return;
   end Gdk_New_From_Name;

   --------------------------
   -- Gdk_New_From_Surface --
   --------------------------

   procedure Gdk_New_From_Surface
      (Self    : out Gdk_Cursor;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       Surface : Cairo.Cairo_Surface;
       X       : Gdouble;
       Y       : Gdouble)
   is
      function Internal
         (Display : System.Address;
          Surface : Cairo.Cairo_Surface;
          X       : Gdouble;
          Y       : Gdouble) return Gdk_Cursor;
      pragma Import (C, Internal, "gdk_cursor_new_from_surface");
   begin
      Self := Internal (Get_Object (Display), Surface, X, Y);
   end Gdk_New_From_Surface;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : Gdk.Gdk_Cursor) return Gdk.Display.Gdk_Display
   is
      function Internal (Self : Gdk.Gdk_Cursor) return System.Address;
      pragma Import (C, Internal, "gdk_cursor_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display (Get_User_Data (Internal (Self), Stub_Gdk_Display));
   end Get_Display;

   -----------------
   -- Get_Surface --
   -----------------

   procedure Get_Surface
      (Self    : Gdk.Gdk_Cursor;
       X_Hot   : out Gdouble;
       Y_Hot   : out Gdouble;
       Surface : out Cairo.Cairo_Surface)
   is
      function Internal
         (Self      : Gdk.Gdk_Cursor;
          Acc_X_Hot : access Gdouble;
          Acc_Y_Hot : access Gdouble) return Cairo.Cairo_Surface;
      pragma Import (C, Internal, "gdk_cursor_get_surface");
      Acc_X_Hot  : aliased Gdouble;
      Acc_Y_Hot  : aliased Gdouble;
      Tmp_Return : Cairo.Cairo_Surface;
   begin
      Tmp_Return := Internal (Self, Acc_X_Hot'Access, Acc_Y_Hot'Access);
      X_Hot := Acc_X_Hot;
      Y_Hot := Acc_Y_Hot;
      Surface := Tmp_Return;
   end Get_Surface;

end Gdk.Cursor;
