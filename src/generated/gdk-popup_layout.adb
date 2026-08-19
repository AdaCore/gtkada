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

package body Gdk.Popup_Layout is

   function From_Object_Free
     (B : access Gdk_Popup_Layout'Class) return Gdk_Popup_Layout
   is
      Result : constant Gdk_Popup_Layout := Gdk_Popup_Layout (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gdk_Popup_Layout is
      S : Gdk_Popup_Layout;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   ---------------------
   -- Get_Anchor_Rect --
   ---------------------

   function Get_Anchor_Rect
     (Self : Gdk_Popup_Layout) return access constant Gdk.Rectangle.Gdk_Rectangle
   is
      function Internal
        (Self : System.Address) return access constant Gdk.Rectangle.Gdk_Rectangle;
      pragma Import (C, Internal, "gdk_popup_layout_get_anchor_rect");
   begin
      return Internal (Get_Object (Self));
   end Get_Anchor_Rect;

   ---------------------
   -- Set_Anchor_Rect --
   ---------------------

   procedure Set_Anchor_Rect
     (Self        : Gdk_Popup_Layout;
      Anchor_Rect : not null access Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
        (Self        : System.Address;
         Anchor_Rect : access Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gdk_popup_layout_set_anchor_rect");
   begin
      Internal (Get_Object (Self), Anchor_Rect);
   end Set_Anchor_Rect;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
      (Self           : out Gdk_Popup_Layout;
       Anchor_Rect    : Gdk.Rectangle.Gdk_Rectangle;
       Rect_Anchor    : Gdk.Enums.Gdk_Gravity;
       Surface_Anchor : Gdk.Enums.Gdk_Gravity)
   is
      function Internal
         (Anchor_Rect    : Gdk.Rectangle.Gdk_Rectangle;
          Rect_Anchor    : Gdk.Enums.Gdk_Gravity;
          Surface_Anchor : Gdk.Enums.Gdk_Gravity) return System.Address;
      pragma Import (C, Internal, "gdk_popup_layout_new");
   begin
      Self.Set_Object (Internal (Anchor_Rect, Rect_Anchor, Surface_Anchor));
   end Gdk_New;

   --------------------------
   -- Gdk_Popup_Layout_New --
   --------------------------

   function Gdk_Popup_Layout_New
      (Anchor_Rect    : Gdk.Rectangle.Gdk_Rectangle;
       Rect_Anchor    : Gdk.Enums.Gdk_Gravity;
       Surface_Anchor : Gdk.Enums.Gdk_Gravity) return Gdk_Popup_Layout
   is
      function Internal
         (Anchor_Rect    : Gdk.Rectangle.Gdk_Rectangle;
          Rect_Anchor    : Gdk.Enums.Gdk_Gravity;
          Surface_Anchor : Gdk.Enums.Gdk_Gravity) return System.Address;
      pragma Import (C, Internal, "gdk_popup_layout_new");
      Self : Gdk_Popup_Layout;
   begin
      Self.Set_Object (Internal (Anchor_Rect, Rect_Anchor, Surface_Anchor));
      return Self;
   end Gdk_Popup_Layout_New;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Gdk_Popup_Layout) return Gdk_Popup_Layout is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_popup_layout_copy");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Copy;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self  : Gdk_Popup_Layout;
       Other : Gdk_Popup_Layout) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Other : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_popup_layout_equal");
   begin
      return Internal (Get_Object (Self), Get_Object (Other)) /= 0;
   end Equal;

   ----------------------
   -- Get_Anchor_Hints --
   ----------------------

   function Get_Anchor_Hints
      (Self : Gdk_Popup_Layout) return Gdk.Enums.Gdk_Anchor_Hints
   is
      function Internal
         (Self : System.Address) return Gdk.Enums.Gdk_Anchor_Hints;
      pragma Import (C, Internal, "gdk_popup_layout_get_anchor_hints");
   begin
      return Internal (Get_Object (Self));
   end Get_Anchor_Hints;

   ----------------
   -- Get_Offset --
   ----------------

   procedure Get_Offset
      (Self : Gdk_Popup_Layout;
       Dx   : out Glib.Gint;
       Dy   : out Glib.Gint)
   is
      procedure Internal
         (Self : System.Address;
          Dx   : out Glib.Gint;
          Dy   : out Glib.Gint);
      pragma Import (C, Internal, "gdk_popup_layout_get_offset");
   begin
      Internal (Get_Object (Self), Dx, Dy);
   end Get_Offset;

   ---------------------
   -- Get_Rect_Anchor --
   ---------------------

   function Get_Rect_Anchor
      (Self : Gdk_Popup_Layout) return Gdk.Enums.Gdk_Gravity
   is
      function Internal (Self : System.Address) return Gdk.Enums.Gdk_Gravity;
      pragma Import (C, Internal, "gdk_popup_layout_get_rect_anchor");
   begin
      return Internal (Get_Object (Self));
   end Get_Rect_Anchor;

   ----------------------
   -- Get_Shadow_Width --
   ----------------------

   procedure Get_Shadow_Width
      (Self   : Gdk_Popup_Layout;
       Left   : out Glib.Gint;
       Right  : out Glib.Gint;
       Top    : out Glib.Gint;
       Bottom : out Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Left   : out Glib.Gint;
          Right  : out Glib.Gint;
          Top    : out Glib.Gint;
          Bottom : out Glib.Gint);
      pragma Import (C, Internal, "gdk_popup_layout_get_shadow_width");
   begin
      Internal (Get_Object (Self), Left, Right, Top, Bottom);
   end Get_Shadow_Width;

   ------------------------
   -- Get_Surface_Anchor --
   ------------------------

   function Get_Surface_Anchor
      (Self : Gdk_Popup_Layout) return Gdk.Enums.Gdk_Gravity
   is
      function Internal (Self : System.Address) return Gdk.Enums.Gdk_Gravity;
      pragma Import (C, Internal, "gdk_popup_layout_get_surface_anchor");
   begin
      return Internal (Get_Object (Self));
   end Get_Surface_Anchor;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gdk_Popup_Layout) return Gdk_Popup_Layout is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_popup_layout_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   ----------------------
   -- Set_Anchor_Hints --
   ----------------------

   procedure Set_Anchor_Hints
      (Self         : Gdk_Popup_Layout;
       Anchor_Hints : Gdk.Enums.Gdk_Anchor_Hints)
   is
      procedure Internal
         (Self         : System.Address;
          Anchor_Hints : Gdk.Enums.Gdk_Anchor_Hints);
      pragma Import (C, Internal, "gdk_popup_layout_set_anchor_hints");
   begin
      Internal (Get_Object (Self), Anchor_Hints);
   end Set_Anchor_Hints;

   ----------------
   -- Set_Offset --
   ----------------

   procedure Set_Offset
      (Self : Gdk_Popup_Layout;
       Dx   : Glib.Gint;
       Dy   : Glib.Gint)
   is
      procedure Internal
         (Self : System.Address;
          Dx   : Glib.Gint;
          Dy   : Glib.Gint);
      pragma Import (C, Internal, "gdk_popup_layout_set_offset");
   begin
      Internal (Get_Object (Self), Dx, Dy);
   end Set_Offset;

   ---------------------
   -- Set_Rect_Anchor --
   ---------------------

   procedure Set_Rect_Anchor
      (Self   : Gdk_Popup_Layout;
       Anchor : Gdk.Enums.Gdk_Gravity)
   is
      procedure Internal
         (Self   : System.Address;
          Anchor : Gdk.Enums.Gdk_Gravity);
      pragma Import (C, Internal, "gdk_popup_layout_set_rect_anchor");
   begin
      Internal (Get_Object (Self), Anchor);
   end Set_Rect_Anchor;

   ----------------------
   -- Set_Shadow_Width --
   ----------------------

   procedure Set_Shadow_Width
      (Self   : Gdk_Popup_Layout;
       Left   : Glib.Gint;
       Right  : Glib.Gint;
       Top    : Glib.Gint;
       Bottom : Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Left   : Glib.Gint;
          Right  : Glib.Gint;
          Top    : Glib.Gint;
          Bottom : Glib.Gint);
      pragma Import (C, Internal, "gdk_popup_layout_set_shadow_width");
   begin
      Internal (Get_Object (Self), Left, Right, Top, Bottom);
   end Set_Shadow_Width;

   ------------------------
   -- Set_Surface_Anchor --
   ------------------------

   procedure Set_Surface_Anchor
      (Self   : Gdk_Popup_Layout;
       Anchor : Gdk.Enums.Gdk_Gravity)
   is
      procedure Internal
         (Self   : System.Address;
          Anchor : Gdk.Enums.Gdk_Gravity);
      pragma Import (C, Internal, "gdk_popup_layout_set_surface_anchor");
   begin
      Internal (Get_Object (Self), Anchor);
   end Set_Surface_Anchor;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gdk_Popup_Layout) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_popup_layout_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gdk.Popup_Layout;
