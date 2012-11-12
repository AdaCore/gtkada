------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

package body Gtk.Paned is

   package Type_Conversion_Gtk_Paned is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Paned_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Paned);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Paned       : out Gtk_Paned;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
   begin
      Paned := new Gtk_Paned_Record;
      Gtk.Paned.Initialize (Paned, Orientation);
   end Gtk_New;

   --------------------
   -- Gtk_New_Hpaned --
   --------------------

   procedure Gtk_New_Hpaned (Paned : out Gtk_Hpaned) is
   begin
      Paned := new Gtk_Hpaned_Record;
      Gtk.Paned.Initialize_Hpaned (Paned);
   end Gtk_New_Hpaned;

   --------------------
   -- Gtk_New_Vpaned --
   --------------------

   procedure Gtk_New_Vpaned (Paned : out Gtk_Vpaned) is
   begin
      Paned := new Gtk_Vpaned_Record;
      Gtk.Paned.Initialize_Vpaned (Paned);
   end Gtk_New_Vpaned;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Paned       : not null access Gtk_Paned_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      function Internal
         (Orientation : Gtk.Enums.Gtk_Orientation) return System.Address;
      pragma Import (C, Internal, "gtk_paned_new");
   begin
      Set_Object (Paned, Internal (Orientation));
   end Initialize;

   -----------------------
   -- Initialize_Hpaned --
   -----------------------

   procedure Initialize_Hpaned
      (Paned : not null access Gtk_Hpaned_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hpaned_new");
   begin
      Set_Object (Paned, Internal);
   end Initialize_Hpaned;

   -----------------------
   -- Initialize_Vpaned --
   -----------------------

   procedure Initialize_Vpaned
      (Paned : not null access Gtk_Vpaned_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vpaned_new");
   begin
      Set_Object (Paned, Internal);
   end Initialize_Vpaned;

   ----------
   -- Add1 --
   ----------

   procedure Add1
      (Paned : not null access Gtk_Paned_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Paned : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_paned_add1");
   begin
      Internal (Get_Object (Paned), Get_Object (Child));
   end Add1;

   ----------
   -- Add2 --
   ----------

   procedure Add2
      (Paned : not null access Gtk_Paned_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Paned : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_paned_add2");
   begin
      Internal (Get_Object (Paned), Get_Object (Child));
   end Add2;

   ----------------
   -- Get_Child1 --
   ----------------

   function Get_Child1
      (Paned : not null access Gtk_Paned_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Paned : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paned_get_child1");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Paned)), Stub_Gtk_Widget));
   end Get_Child1;

   ----------------
   -- Get_Child2 --
   ----------------

   function Get_Child2
      (Paned : not null access Gtk_Paned_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Paned : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_paned_get_child2");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Paned)), Stub_Gtk_Widget));
   end Get_Child2;

   -----------------------
   -- Get_Handle_Window --
   -----------------------

   function Get_Handle_Window
      (Paned : not null access Gtk_Paned_Record) return Gdk.Gdk_Window
   is
      function Internal (Paned : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gtk_paned_get_handle_window");
   begin
      return Internal (Get_Object (Paned));
   end Get_Handle_Window;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Paned : not null access Gtk_Paned_Record) return Gint
   is
      function Internal (Paned : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_paned_get_position");
   begin
      return Internal (Get_Object (Paned));
   end Get_Position;

   -----------
   -- Pack1 --
   -----------

   procedure Pack1
      (Paned  : not null access Gtk_Paned_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Resize : Boolean := False;
       Shrink : Boolean := True)
   is
      procedure Internal
         (Paned  : System.Address;
          Child  : System.Address;
          Resize : Integer;
          Shrink : Integer);
      pragma Import (C, Internal, "gtk_paned_pack1");
   begin
      Internal (Get_Object (Paned), Get_Object (Child), Boolean'Pos (Resize), Boolean'Pos (Shrink));
   end Pack1;

   -----------
   -- Pack2 --
   -----------

   procedure Pack2
      (Paned  : not null access Gtk_Paned_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Resize : Boolean := False;
       Shrink : Boolean := False)
   is
      procedure Internal
         (Paned  : System.Address;
          Child  : System.Address;
          Resize : Integer;
          Shrink : Integer);
      pragma Import (C, Internal, "gtk_paned_pack2");
   begin
      Internal (Get_Object (Paned), Get_Object (Child), Boolean'Pos (Resize), Boolean'Pos (Shrink));
   end Pack2;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Paned    : not null access Gtk_Paned_Record;
       Position : Gint)
   is
      procedure Internal (Paned : System.Address; Position : Gint);
      pragma Import (C, Internal, "gtk_paned_set_position");
   begin
      Internal (Get_Object (Paned), Position);
   end Set_Position;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Paned_Record)
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
      (Self        : not null access Gtk_Paned_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

end Gtk.Paned;
