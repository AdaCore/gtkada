-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2000-2003 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Interfaces.C.Strings;

package body Gtk.Selection is

   ------------------------
   -- Get_Data_As_String --
   ------------------------

   function Get_Data_As_String
     (Selection : Selection_Data) return String
   is
      function Internal
        (Selection : Selection_Data) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gtk_dnd_get_data");

   begin
      return Interfaces.C.Strings.Value (Internal (Selection));
   end Get_Data_As_String;

   ------------------------
   -- Selection_Data_Set --
   ------------------------

   procedure Selection_Data_Set
     (Selection : Selection_Data;
      The_Type  : Gdk.Types.Gdk_Atom;
      Format    : Gint;
      Data      : String) is
   begin
      Selection_Data_Set
        (Selection, The_Type, Format, Data'Address, Data'Length);
   end Selection_Data_Set;

   ---------------------
   -- Target_List_New --
   ---------------------

   function Target_List_New
     (Targets : Target_Entry_Array) return Target_List
   is
      function Internal
        (Targets : System.Address; N_Targets : Guint) return Target_List;
      pragma Import (C, Internal, "gtk_target_list_new");

   begin
      return Internal (Targets'Address, Targets'Length);
   end Target_List_New;

   ---------------------------
   -- Target_List_Add_Table --
   ---------------------------

   procedure Target_List_Add_Table
     (List    : Target_List;
      Targets : Target_Entry_Array)
   is
      procedure Internal
        (List      : Target_List;
         Targets   : System.Address;
         N_Targets : Guint);
      pragma Import (C, Internal, "gtk_target_list_add_table");

   begin
      Internal (List, Targets'Address, Targets'Length);
   end Target_List_Add_Table;

   ----------------------
   -- Target_List_Find --
   ----------------------

   procedure Target_List_Find
     (List   : Target_List;
      Target : Gdk.Types.Gdk_Atom;
      Info   : out Guint;
      Found  : out Boolean)
   is
      function Internal
        (List   : Target_List;
         Target : Gdk.Types.Gdk_Atom;
         Info   : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_target_list_find");

      J : aliased Guint;

   begin
      Found := Boolean'Val (Internal (List, Target, J'Address));
      Info := J;
   end Target_List_Find;

   ---------------
   -- Owner_Set --
   ---------------

   function Owner_Set
     (Widget    : Gtk.Widget.Gtk_Widget;
      Selection : Gdk_Selection := Selection_Primary;
      Time      : Guint32 := 0) return Boolean
   is
      function Internal
        (Widget    : System.Address;
         Selection : Gdk_Selection;
         Time      : Guint32) return Gint;
      pragma Import (C, Internal, "gtk_selection_owner_set");

   begin
      return Boolean'Val (Internal (Get_Object (Widget), Selection, Time));
   end Owner_Set;

   ----------------
   -- Add_Target --
   ----------------

   procedure Add_Target
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection;
      Target    : Gdk.Types.Gdk_Atom;
      Info      : Guint)
   is
      procedure Internal
        (Widget    : System.Address;
         Selection : Gdk_Selection;
         Target    : Gdk.Types.Gdk_Atom;
         Info      : Guint);
      pragma Import (C, Internal, "gtk_selection_add_target");

   begin
      Internal (Get_Object (Widget), Selection, Target, Info);
   end Add_Target;

   -----------------
   -- Add_Targets --
   -----------------

   procedure Add_Targets
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection;
      Targets   : Target_Entry_Array)
   is
      procedure Internal
        (Widget    : System.Address;
         Selection : Gdk_Selection;
         Targets   : System.Address;
         N_Targets : Guint);
      pragma Import (C, Internal, "gtk_selection_add_targets");

   begin
      Internal (Get_Object (Widget), Selection, Targets'Address,
                Targets'Length);
   end Add_Targets;

   -------------
   -- Convert --
   -------------

   function Convert
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection := Selection_Primary;
      Target    : Gdk.Types.Gdk_Atom;
      Time      : Guint32 := 0) return Boolean
   is
      function Internal
        (Widget    : System.Address;
         Selection : Gdk_Selection;
         Target    : Gdk.Types.Gdk_Atom;
         Time      : Guint32) return Gint;
      pragma Import (C, Internal, "gtk_selection_convert");

   begin
      return Boolean'Val (Internal (Get_Object (Widget), Selection,
                                    Target, Time));
   end Convert;

   -------------------
   -- Clear_Targets --
   -------------------

   procedure Clear_Targets
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk_Selection)
   is
      procedure Internal
        (Widget    : System.Address;
         Selection : Gdk_Selection);
      pragma Import (C, Internal, "gtk_selection_clear_targets");

   begin
      Internal (Get_Object (Widget), Selection);
   end Clear_Targets;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_selection_remove_all");

   begin
      Internal (Get_Object (Widget));
   end Remove_All;

end Gtk.Selection;
