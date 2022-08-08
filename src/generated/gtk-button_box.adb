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

package body Gtk.Button_Box is

   package Type_Conversion_Gtk_Button_Box is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Button_Box_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Button_Box);

   ------------------------
   -- Gtk_Button_Box_New --
   ------------------------

   function Gtk_Button_Box_New
      (Orientation : Gtk.Enums.Gtk_Orientation) return Gtk_Button_Box
   is
      Widget : constant Gtk_Button_Box := new Gtk_Button_Box_Record;
   begin
      Gtk.Button_Box.Initialize (Widget, Orientation);
      return Widget;
   end Gtk_Button_Box_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget      : out Gtk_Button_Box;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
   begin
      Widget := new Gtk_Button_Box_Record;
      Gtk.Button_Box.Initialize (Widget, Orientation);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Widget      : not null access Gtk_Button_Box_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      function Internal
         (Orientation : Gtk.Enums.Gtk_Orientation) return System.Address;
      pragma Import (C, Internal, "gtk_button_box_new");
   begin
      if not Widget.Is_Created then
         Set_Object (Widget, Internal (Orientation));
      end if;
   end Initialize;

   -------------------------------
   -- Get_Child_Non_Homogeneous --
   -------------------------------

   function Get_Child_Non_Homogeneous
      (Widget : not null access Gtk_Button_Box_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Widget : System.Address;
          Child  : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_button_box_get_child_non_homogeneous");
   begin
      return Internal (Get_Object (Widget), Get_Object (Child)) /= 0;
   end Get_Child_Non_Homogeneous;

   -------------------------
   -- Get_Child_Secondary --
   -------------------------

   function Get_Child_Secondary
      (Widget : not null access Gtk_Button_Box_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Widget : System.Address;
          Child  : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_button_box_get_child_secondary");
   begin
      return Internal (Get_Object (Widget), Get_Object (Child)) /= 0;
   end Get_Child_Secondary;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout
      (Widget : not null access Gtk_Button_Box_Record)
       return Gtk.Enums.Gtk_Button_Box_Style
   is
      function Internal
         (Widget : System.Address) return Gtk.Enums.Gtk_Button_Box_Style;
      pragma Import (C, Internal, "gtk_button_box_get_layout");
   begin
      return Internal (Get_Object (Widget));
   end Get_Layout;

   -------------------------------
   -- Set_Child_Non_Homogeneous --
   -------------------------------

   procedure Set_Child_Non_Homogeneous
      (Widget          : not null access Gtk_Button_Box_Record;
       Child           : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Non_Homogeneous : Boolean)
   is
      procedure Internal
         (Widget          : System.Address;
          Child           : System.Address;
          Non_Homogeneous : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_button_box_set_child_non_homogeneous");
   begin
      Internal (Get_Object (Widget), Get_Object (Child), Boolean'Pos (Non_Homogeneous));
   end Set_Child_Non_Homogeneous;

   -------------------------
   -- Set_Child_Secondary --
   -------------------------

   procedure Set_Child_Secondary
      (Widget       : not null access Gtk_Button_Box_Record;
       Child        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Is_Secondary : Boolean)
   is
      procedure Internal
         (Widget       : System.Address;
          Child        : System.Address;
          Is_Secondary : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_button_box_set_child_secondary");
   begin
      Internal (Get_Object (Widget), Get_Object (Child), Boolean'Pos (Is_Secondary));
   end Set_Child_Secondary;

   ----------------
   -- Set_Layout --
   ----------------

   procedure Set_Layout
      (Widget       : not null access Gtk_Button_Box_Record;
       Layout_Style : Gtk.Enums.Gtk_Button_Box_Style)
   is
      procedure Internal
         (Widget       : System.Address;
          Layout_Style : Gtk.Enums.Gtk_Button_Box_Style);
      pragma Import (C, Internal, "gtk_button_box_set_layout");
   begin
      Internal (Get_Object (Widget), Layout_Style);
   end Set_Layout;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Button_Box_Record)
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
      (Self        : not null access Gtk_Button_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

end Gtk.Button_Box;
