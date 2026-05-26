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

package body Gtk.Center_Layout is

   package Type_Conversion_Gtk_Center_Layout is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Center_Layout_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Center_Layout);

   ---------------------------
   -- Gtk_Center_Layout_New --
   ---------------------------

   function Gtk_Center_Layout_New return Gtk_Center_Layout is
      Center_Layout : constant Gtk_Center_Layout := new Gtk_Center_Layout_Record;
   begin
      Gtk.Center_Layout.Initialize (Center_Layout);
      return Center_Layout;
   end Gtk_Center_Layout_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Center_Layout : out Gtk_Center_Layout) is
   begin
      Center_Layout := new Gtk_Center_Layout_Record;
      Gtk.Center_Layout.Initialize (Center_Layout);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Center_Layout : not null access Gtk_Center_Layout_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_center_layout_new");
   begin
      if not Center_Layout.Is_Created then
         Set_Object (Center_Layout, Internal);
      end if;
   end Initialize;

   ---------------------------
   -- Get_Baseline_Position --
   ---------------------------

   function Get_Baseline_Position
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Gtk.Enums.Gtk_Baseline_Position
   is
      function Internal
         (Center_Layout : System.Address)
          return Gtk.Enums.Gtk_Baseline_Position;
      pragma Import (C, Internal, "gtk_center_layout_get_baseline_position");
   begin
      return Internal (Get_Object (Center_Layout));
   end Get_Baseline_Position;

   -----------------------
   -- Get_Center_Widget --
   -----------------------

   function Get_Center_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Center_Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_center_layout_get_center_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Center_Layout)), Stub_Gtk_Widget));
   end Get_Center_Widget;

   --------------------
   -- Get_End_Widget --
   --------------------

   function Get_End_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Center_Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_center_layout_get_end_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Center_Layout)), Stub_Gtk_Widget));
   end Get_End_Widget;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Center_Layout : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_center_layout_get_orientation");
   begin
      return Internal (Get_Object (Center_Layout));
   end Get_Orientation;

   ----------------------------
   -- Get_Shrink_Center_Last --
   ----------------------------

   function Get_Shrink_Center_Last
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Boolean
   is
      function Internal
         (Center_Layout : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_center_layout_get_shrink_center_last");
   begin
      return Internal (Get_Object (Center_Layout)) /= 0;
   end Get_Shrink_Center_Last;

   ----------------------
   -- Get_Start_Widget --
   ----------------------

   function Get_Start_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Center_Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_center_layout_get_start_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Center_Layout)), Stub_Gtk_Widget));
   end Get_Start_Widget;

   ---------------------------
   -- Set_Baseline_Position --
   ---------------------------

   procedure Set_Baseline_Position
      (Center_Layout     : not null access Gtk_Center_Layout_Record;
       Baseline_Position : Gtk.Enums.Gtk_Baseline_Position)
   is
      procedure Internal
         (Center_Layout     : System.Address;
          Baseline_Position : Gtk.Enums.Gtk_Baseline_Position);
      pragma Import (C, Internal, "gtk_center_layout_set_baseline_position");
   begin
      Internal (Get_Object (Center_Layout), Baseline_Position);
   end Set_Baseline_Position;

   -----------------------
   -- Set_Center_Widget --
   -----------------------

   procedure Set_Center_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Center_Layout : System.Address;
          Widget        : System.Address);
      pragma Import (C, Internal, "gtk_center_layout_set_center_widget");
   begin
      Internal (Get_Object (Center_Layout), Get_Object_Or_Null (GObject (Widget)));
   end Set_Center_Widget;

   --------------------
   -- Set_End_Widget --
   --------------------

   procedure Set_End_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Center_Layout : System.Address;
          Widget        : System.Address);
      pragma Import (C, Internal, "gtk_center_layout_set_end_widget");
   begin
      Internal (Get_Object (Center_Layout), Get_Object_Or_Null (GObject (Widget)));
   end Set_End_Widget;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Center_Layout : not null access Gtk_Center_Layout_Record;
       Orientation   : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Center_Layout : System.Address;
          Orientation   : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_center_layout_set_orientation");
   begin
      Internal (Get_Object (Center_Layout), Orientation);
   end Set_Orientation;

   ----------------------------
   -- Set_Shrink_Center_Last --
   ----------------------------

   procedure Set_Shrink_Center_Last
      (Center_Layout      : not null access Gtk_Center_Layout_Record;
       Shrink_Center_Last : Boolean)
   is
      procedure Internal
         (Center_Layout      : System.Address;
          Shrink_Center_Last : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_center_layout_set_shrink_center_last");
   begin
      Internal (Get_Object (Center_Layout), Boolean'Pos (Shrink_Center_Last));
   end Set_Shrink_Center_Last;

   ----------------------
   -- Set_Start_Widget --
   ----------------------

   procedure Set_Start_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Center_Layout : System.Address;
          Widget        : System.Address);
      pragma Import (C, Internal, "gtk_center_layout_set_start_widget");
   begin
      Internal (Get_Object (Center_Layout), Get_Object_Or_Null (GObject (Widget)));
   end Set_Start_Widget;

end Gtk.Center_Layout;
