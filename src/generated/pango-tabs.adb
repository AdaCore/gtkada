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

package body Pango.Tabs is

   function From_Object_Free
     (B : access Pango_Tab_Array'Class) return Pango_Tab_Array
   is
      Result : constant Pango_Tab_Array := Pango_Tab_Array (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Pango_Tab_Array is
      S : Pango_Tab_Array;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
      (Self                : out Pango_Tab_Array;
       Initial_Size        : Glib.Gint;
       Positions_In_Pixels : Boolean)
   is
      function Internal
         (Initial_Size        : Glib.Gint;
          Positions_In_Pixels : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "pango_tab_array_new");
   begin
      Self.Set_Object (Internal (Initial_Size, Boolean'Pos (Positions_In_Pixels)));
   end Gdk_New;

   -------------------------
   -- Pango_Tab_Array_New --
   -------------------------

   function Pango_Tab_Array_New
      (Initial_Size        : Glib.Gint;
       Positions_In_Pixels : Boolean) return Pango_Tab_Array
   is
      function Internal
         (Initial_Size        : Glib.Gint;
          Positions_In_Pixels : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "pango_tab_array_new");
      Self : Pango_Tab_Array;
   begin
      Self.Set_Object (Internal (Initial_Size, Boolean'Pos (Positions_In_Pixels)));
      return Self;
   end Pango_Tab_Array_New;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Pango_Tab_Array) return Pango_Tab_Array is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_tab_array_copy");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Self : Pango_Tab_Array) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "pango_tab_array_free");
   begin
      Internal (Get_Object (Self));
   end Free;

   -----------------------------
   -- Get_Positions_In_Pixels --
   -----------------------------

   function Get_Positions_In_Pixels (Self : Pango_Tab_Array) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_tab_array_get_positions_in_pixels");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Positions_In_Pixels;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (Self : Pango_Tab_Array) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_tab_array_get_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Size;

   -------------
   -- Get_Tab --
   -------------

   procedure Get_Tab
      (Self      : Pango_Tab_Array;
       Tab_Index : Glib.Gint;
       Alignment : out Pango_Tab_Align;
       Location  : out Glib.Gint)
   is
      procedure Internal
         (Self      : System.Address;
          Tab_Index : Glib.Gint;
          Alignment : out Pango_Tab_Align;
          Location  : out Glib.Gint);
      pragma Import (C, Internal, "pango_tab_array_get_tab");
   begin
      Internal (Get_Object (Self), Tab_Index, Alignment, Location);
   end Get_Tab;

   ------------
   -- Resize --
   ------------

   procedure Resize (Self : Pango_Tab_Array; New_Size : Glib.Gint) is
      procedure Internal (Self : System.Address; New_Size : Glib.Gint);
      pragma Import (C, Internal, "pango_tab_array_resize");
   begin
      Internal (Get_Object (Self), New_Size);
   end Resize;

   -------------
   -- Set_Tab --
   -------------

   procedure Set_Tab
      (Self      : Pango_Tab_Array;
       Tab_Index : Glib.Gint;
       Alignment : Pango_Tab_Align;
       Location  : Glib.Gint)
   is
      procedure Internal
         (Self      : System.Address;
          Tab_Index : Glib.Gint;
          Alignment : Pango_Tab_Align;
          Location  : Glib.Gint);
      pragma Import (C, Internal, "pango_tab_array_set_tab");
   begin
      Internal (Get_Object (Self), Tab_Index, Alignment, Location);
   end Set_Tab;

end Pango.Tabs;
