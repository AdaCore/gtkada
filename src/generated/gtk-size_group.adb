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

package body Gtk.Size_Group is

   package Type_Conversion_Gtk_Size_Group is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Size_Group_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Size_Group);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Size_Group : out Gtk_Size_Group;
       Mode       : Size_Group_Mode := Both)
   is
   begin
      Size_Group := new Gtk_Size_Group_Record;
      Gtk.Size_Group.Initialize (Size_Group, Mode);
   end Gtk_New;

   ------------------------
   -- Gtk_Size_Group_New --
   ------------------------

   function Gtk_Size_Group_New
      (Mode : Size_Group_Mode := Both) return Gtk_Size_Group
   is
      Size_Group : constant Gtk_Size_Group := new Gtk_Size_Group_Record;
   begin
      Gtk.Size_Group.Initialize (Size_Group, Mode);
      return Size_Group;
   end Gtk_Size_Group_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Size_Group : not null access Gtk_Size_Group_Record'Class;
       Mode       : Size_Group_Mode := Both)
   is
      function Internal (Mode : Size_Group_Mode) return System.Address;
      pragma Import (C, Internal, "gtk_size_group_new");
   begin
      if not Size_Group.Is_Created then
         Set_Object (Size_Group, Internal (Mode));
      end if;
   end Initialize;

   ----------------
   -- Add_Widget --
   ----------------

   procedure Add_Widget
      (Size_Group : not null access Gtk_Size_Group_Record;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Size_Group : System.Address;
          Widget     : System.Address);
      pragma Import (C, Internal, "gtk_size_group_add_widget");
   begin
      Internal (Get_Object (Size_Group), Get_Object (Widget));
   end Add_Widget;

   -----------------------
   -- Get_Ignore_Hidden --
   -----------------------

   function Get_Ignore_Hidden
      (Size_Group : not null access Gtk_Size_Group_Record) return Boolean
   is
      function Internal (Size_Group : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_size_group_get_ignore_hidden");
   begin
      return Internal (Get_Object (Size_Group)) /= 0;
   end Get_Ignore_Hidden;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode
      (Size_Group : not null access Gtk_Size_Group_Record)
       return Size_Group_Mode
   is
      function Internal (Size_Group : System.Address) return Size_Group_Mode;
      pragma Import (C, Internal, "gtk_size_group_get_mode");
   begin
      return Internal (Get_Object (Size_Group));
   end Get_Mode;

   -----------------
   -- Get_Widgets --
   -----------------

   function Get_Widgets
      (Size_Group : not null access Gtk_Size_Group_Record)
       return Gtk.Widget.Widget_SList.GSlist
   is
      function Internal (Size_Group : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_size_group_get_widgets");
      Tmp_Return : Gtk.Widget.Widget_SList.GSlist;
   begin
      Gtk.Widget.Widget_SList.Set_Object (Tmp_Return, Internal (Get_Object (Size_Group)));
      return Tmp_Return;
   end Get_Widgets;

   -------------------
   -- Remove_Widget --
   -------------------

   procedure Remove_Widget
      (Size_Group : not null access Gtk_Size_Group_Record;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Size_Group : System.Address;
          Widget     : System.Address);
      pragma Import (C, Internal, "gtk_size_group_remove_widget");
   begin
      Internal (Get_Object (Size_Group), Get_Object (Widget));
   end Remove_Widget;

   -----------------------
   -- Set_Ignore_Hidden --
   -----------------------

   procedure Set_Ignore_Hidden
      (Size_Group    : not null access Gtk_Size_Group_Record;
       Ignore_Hidden : Boolean)
   is
      procedure Internal
         (Size_Group    : System.Address;
          Ignore_Hidden : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_size_group_set_ignore_hidden");
   begin
      Internal (Get_Object (Size_Group), Boolean'Pos (Ignore_Hidden));
   end Set_Ignore_Hidden;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
      (Size_Group : not null access Gtk_Size_Group_Record;
       Mode       : Size_Group_Mode)
   is
      procedure Internal
         (Size_Group : System.Address;
          Mode       : Size_Group_Mode);
      pragma Import (C, Internal, "gtk_size_group_set_mode");
   begin
      Internal (Get_Object (Size_Group), Mode);
   end Set_Mode;

end Gtk.Size_Group;
