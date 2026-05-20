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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Overlay_Layout is

   package Type_Conversion_Gtk_Overlay_Layout is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Overlay_Layout_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Overlay_Layout);

   package Type_Conversion_Gtk_Overlay_Layout_Child is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Overlay_Layout_Child_Get_Type'Access, Gtk_Overlay_Layout_Child_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Overlay_Layout_Child);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Overlay_Layout) is
   begin
      Self := new Gtk_Overlay_Layout_Record;
      Gtk.Overlay_Layout.Initialize (Self);
   end Gtk_New;

   ----------------------------
   -- Gtk_Overlay_Layout_New --
   ----------------------------

   function Gtk_Overlay_Layout_New return Gtk_Overlay_Layout is
      Self : constant Gtk_Overlay_Layout := new Gtk_Overlay_Layout_Record;
   begin
      Gtk.Overlay_Layout.Initialize (Self);
      return Self;
   end Gtk_Overlay_Layout_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Overlay_Layout_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_overlay_layout_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------------
   -- Get_Clip_Overlay --
   ----------------------

   function Get_Clip_Overlay
      (Child : not null access Gtk_Overlay_Layout_Child_Record)
       return Boolean
   is
      function Internal (Child : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_overlay_layout_child_get_clip_overlay");
   begin
      return Internal (Get_Object (Child)) /= 0;
   end Get_Clip_Overlay;

   -----------------
   -- Get_Measure --
   -----------------

   function Get_Measure
      (Child : not null access Gtk_Overlay_Layout_Child_Record)
       return Boolean
   is
      function Internal (Child : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_overlay_layout_child_get_measure");
   begin
      return Internal (Get_Object (Child)) /= 0;
   end Get_Measure;

   ----------------------
   -- Set_Clip_Overlay --
   ----------------------

   procedure Set_Clip_Overlay
      (Child        : not null access Gtk_Overlay_Layout_Child_Record;
       Clip_Overlay : Boolean)
   is
      procedure Internal
         (Child        : System.Address;
          Clip_Overlay : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_overlay_layout_child_set_clip_overlay");
   begin
      Internal (Get_Object (Child), Boolean'Pos (Clip_Overlay));
   end Set_Clip_Overlay;

   -----------------
   -- Set_Measure --
   -----------------

   procedure Set_Measure
      (Child   : not null access Gtk_Overlay_Layout_Child_Record;
       Measure : Boolean)
   is
      procedure Internal (Child : System.Address; Measure : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_overlay_layout_child_set_measure");
   begin
      Internal (Get_Object (Child), Boolean'Pos (Measure));
   end Set_Measure;

end Gtk.Overlay_Layout;
