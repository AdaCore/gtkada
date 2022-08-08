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

package body Gtk.Scrollbar is

   package Type_Conversion_Gtk_Scrollbar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Scrollbar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Scrollbar);

   ------------------------
   -- Gtk_Hscrollbar_New --
   ------------------------

   function Gtk_Hscrollbar_New
      (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Hscrollbar
   is
      Scrollbar : constant Gtk_Hscrollbar := new Gtk_Hscrollbar_Record;
   begin
      Gtk.Scrollbar.Initialize_Hscrollbar (Scrollbar, Adjustment);
      return Scrollbar;
   end Gtk_Hscrollbar_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Scrollbar   : out Gtk_Scrollbar;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Scrollbar := new Gtk_Scrollbar_Record;
      Gtk.Scrollbar.Initialize (Scrollbar, Orientation, Adjustment);
   end Gtk_New;

   ------------------------
   -- Gtk_New_Hscrollbar --
   ------------------------

   procedure Gtk_New_Hscrollbar
      (Scrollbar  : out Gtk_Hscrollbar;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Scrollbar := new Gtk_Hscrollbar_Record;
      Gtk.Scrollbar.Initialize_Hscrollbar (Scrollbar, Adjustment);
   end Gtk_New_Hscrollbar;

   ------------------------
   -- Gtk_New_Vscrollbar --
   ------------------------

   procedure Gtk_New_Vscrollbar
      (Scrollbar  : out Gtk_Vscrollbar;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Scrollbar := new Gtk_Vscrollbar_Record;
      Gtk.Scrollbar.Initialize_Vscrollbar (Scrollbar, Adjustment);
   end Gtk_New_Vscrollbar;

   -----------------------
   -- Gtk_Scrollbar_New --
   -----------------------

   function Gtk_Scrollbar_New
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Scrollbar
   is
      Scrollbar : constant Gtk_Scrollbar := new Gtk_Scrollbar_Record;
   begin
      Gtk.Scrollbar.Initialize (Scrollbar, Orientation, Adjustment);
      return Scrollbar;
   end Gtk_Scrollbar_New;

   ------------------------
   -- Gtk_Vscrollbar_New --
   ------------------------

   function Gtk_Vscrollbar_New
      (Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Vscrollbar
   is
      Scrollbar : constant Gtk_Vscrollbar := new Gtk_Vscrollbar_Record;
   begin
      Gtk.Scrollbar.Initialize_Vscrollbar (Scrollbar, Adjustment);
      return Scrollbar;
   end Gtk_Vscrollbar_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Scrollbar   : not null access Gtk_Scrollbar_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      function Internal
         (Orientation : Gtk.Enums.Gtk_Orientation;
          Adjustment  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrollbar_new");
   begin
      if not Scrollbar.Is_Created then
         Set_Object (Scrollbar, Internal (Orientation, Get_Object_Or_Null (GObject (Adjustment))));
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_Hscrollbar --
   ---------------------------

   procedure Initialize_Hscrollbar
      (Scrollbar  : not null access Gtk_Hscrollbar_Record'Class;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      function Internal (Adjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_hscrollbar_new");
   begin
      if not Scrollbar.Is_Created then
         Set_Object (Scrollbar, Internal (Get_Object_Or_Null (GObject (Adjustment))));
      end if;
   end Initialize_Hscrollbar;

   ---------------------------
   -- Initialize_Vscrollbar --
   ---------------------------

   procedure Initialize_Vscrollbar
      (Scrollbar  : not null access Gtk_Vscrollbar_Record'Class;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      function Internal (Adjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_vscrollbar_new");
   begin
      if not Scrollbar.Is_Created then
         Set_Object (Scrollbar, Internal (Get_Object_Or_Null (GObject (Adjustment))));
      end if;
   end Initialize_Vscrollbar;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Scrollbar_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Scrollbar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

end Gtk.Scrollbar;
