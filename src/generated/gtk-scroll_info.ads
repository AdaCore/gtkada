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

--  Provides detailed information on how a scroll operation should be
--  performed.
--
--  Scrolling functions usually allow passing a `NULL` scroll info which will
--  cause the default values to be used and just scroll the element into view.

pragma Warnings (Off, "*is already use-visible*");
with Glib; use Glib;

package Gtk.Scroll_Info is

   type Gtk_Scroll_Info is new Glib.C_Boxed with null record;
   Null_Gtk_Scroll_Info : constant Gtk_Scroll_Info;

   function From_Object (Object : System.Address) return Gtk_Scroll_Info;
   function From_Object_Free (B : access Gtk_Scroll_Info'Class) return Gtk_Scroll_Info;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Scroll_Info);
   --  Creates a new scroll info for scrolling an element into view.
   --  Since: gtk+ 4.12

   function Gtk_Scroll_Info_New return Gtk_Scroll_Info;
   --  Creates a new scroll info for scrolling an element into view.
   --  Since: gtk+ 4.12

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_scroll_info_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Enable_Horizontal (Self : Gtk_Scroll_Info) return Boolean;
   --  Checks if horizontal scrolling is enabled.
   --  Since: gtk+ 4.12
   --  @return True if horizontal scrolling is enabled.

   procedure Set_Enable_Horizontal
      (Self       : Gtk_Scroll_Info;
       Horizontal : Boolean);
   --  Turns horizontal scrolling on or off.
   --  Since: gtk+ 4.12
   --  @param Horizontal if scrolling in the horizontal direction should
   --  happen

   function Get_Enable_Vertical (Self : Gtk_Scroll_Info) return Boolean;
   --  Checks if vertical scrolling is enabled.
   --  Since: gtk+ 4.12
   --  @return True if vertical scrolling is enabled.

   procedure Set_Enable_Vertical
      (Self     : Gtk_Scroll_Info;
       Vertical : Boolean);
   --  Turns vertical scrolling on or off.
   --  Since: gtk+ 4.12
   --  @param Vertical if scrolling in the vertical direction should happen

   function Ref (Self : Gtk_Scroll_Info) return Gtk_Scroll_Info;
   --  Increases the reference count of a `GtkScrollInfo` by one.
   --  Since: gtk+ 4.12
   --  @return the passed in `GtkScrollInfo`.

   procedure Unref (Self : Gtk_Scroll_Info);
   --  Decreases the reference count of a `GtkScrollInfo` by one.
   --  If the resulting reference count is zero, frees the self.
   --  Since: gtk+ 4.12

private

   Null_Gtk_Scroll_Info : constant Gtk_Scroll_Info := (Glib.C_Boxed with null record);

end Gtk.Scroll_Info;
