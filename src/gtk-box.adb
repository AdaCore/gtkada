-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with System;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Widget;         use Gtk.Widget;

package body Gtk.Box is

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
     (Box : access Gtk_Box_Record;
      Num : in Gint) return Gtk_Widget
   is
      function Internal
        (Box : in System.Address; Num : in Gint) return System.Address;
      pragma Import (C, Internal, "ada_box_get_child");

      use type System.Address;
      Stub : Gtk_Widget_Record;
      S    : System.Address := Internal (Get_Object (Box), Num);

   begin
      if S /= System.Null_Address then
         return Gtk_Widget (Get_User_Data (S, Stub));
      else
         return null;
      end if;
   end Get_Child;

   ------------------
   -- Gtk_New_Vbox --
   ------------------

   procedure Gtk_New_Vbox (Box         : in out Gtk_Box;
                           Homogeneous : in  Boolean := False;
                           Spacing     : in  Gint := 0)
   is
   begin
      Box := new Gtk_Box_Record;
      Initialize_Vbox (Box, Homogeneous, Spacing);
   end Gtk_New_Vbox;

   ------------------
   -- Gtk_New_Hbox --
   ------------------

   procedure Gtk_New_Hbox (Box         : in out Gtk_Box;
                           Homogeneous : in  Boolean := False;
                           Spacing     : in  Gint := 0)
   is
   begin
      Box := new Gtk_Box_Record;
      Initialize_Hbox (Box, Homogeneous, Spacing);
   end Gtk_New_Hbox;

   ---------------------
   -- Initialize_Hbox --
   ---------------------

   procedure Initialize_Hbox (Box         : access Gtk_Box_Record'Class;
                              Homogeneous : in  Boolean := False;
                              Spacing     : in  Gint := 0) is
      function Internal (Homogeneous : Gint;
                         Spacing     : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_hbox_new");
   begin
      Set_Object (Box, Internal (Boolean'Pos (Homogeneous), Spacing));
      Initialize_User_Data (Box);
   end Initialize_Hbox;

   ---------------------
   -- Initialize_Vbox --
   ---------------------

   procedure Initialize_Vbox (Box         : access Gtk_Box_Record'Class;
                              Homogeneous : in  Boolean := False;
                              Spacing     : in  Gint := 0) is
      function Internal (Homogeneous : Gint;
                         Spacing     : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_vbox_new");
   begin
      Set_Object (Box, Internal (Boolean'Pos (Homogeneous), Spacing));
      Initialize_User_Data (Box);
   end Initialize_Vbox;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in Gint    := 0)
   is
      procedure Internal (In_Box  : System.Address;
                          Child   : System.Address;
                          Expand  : Gint;
                          Fill    : Gint;
                          Padding : Gint);
      pragma Import (C, Internal, "gtk_box_pack_start");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Pack_Start;

   -------------------------
   -- Pack_Start_Defaults --
   -------------------------

   procedure Pack_Start_Defaults
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class) is
      procedure Internal (In_Box  : System.Address;
                          Child   : System.Address);
      pragma Import (C, Internal, "gtk_box_pack_start_defaults");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child));
   end Pack_Start_Defaults;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in Gint    := 0)
   is
      procedure Internal (In_Box  : in System.Address;
                          Child   : in System.Address;
                          Expand  : in Gint;
                          Fill    : in Gint;
                          Padding : in Gint);
      pragma Import (C, Internal, "gtk_box_pack_end");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Pack_End;

   -----------------------
   -- Pack_End_Defaults --
   -----------------------

   procedure Pack_End_Defaults
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class) is
      procedure Internal (In_Box  : System.Address;
                          Child   : System.Address);
      pragma Import (C, Internal, "gtk_box_pack_end_defaults");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child));
   end Pack_End_Defaults;

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous (In_Box : access Gtk_Box_Record;
                              Homogeneous : in Boolean)
   is
      procedure Internal (In_Box : in System.Address; Homogeneous : in Gint);
      pragma Import (C, Internal, "gtk_box_set_homogeneous");
   begin
      Internal (Get_Object (In_Box), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing (In_Box  : access Gtk_Box_Record;
                          Spacing : in Gint)
   is
      procedure Internal (In_Box  : in System.Address; Spacing : in Gint);
      pragma Import (C, Internal, "gtk_box_set_spacing");
   begin
      Internal (Get_Object (In_Box), Spacing);
   end Set_Spacing;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
     (In_Box : access Gtk_Box_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Pos    : in Guint)
   is
      procedure Internal (In_Box : in System.Address;
                          Child  : in System.Address;
                          Pos    : in Guint);
      pragma Import (C, Internal, "gtk_box_reorder_child");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child), Pos);
   end Reorder_Child;

   -------------------------
   -- Query_Child_Packing --
   -------------------------

   procedure Query_Child_Packing
     (In_Box   : access Gtk_Box_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand   : out Boolean;
      Fill     : out Boolean;
      Padding  : out Gint;
      PackType : out Gtk.Enums.Gtk_Pack_Type)
   is
      procedure Internal (In_Box   : in System.Address;
                          Child    : in System.Address;
                          Expand   : out Gint;
                          Fill     : out Gint;
                          Padding  : out Gint;
                          PackType : out Gint);
      pragma Import (C, Internal, "gtk_box_query_child_packing");

      Expand_Ptr  : Gint;
      Fill_Ptr    : Gint;
      PackT_Ptr   : Gint;
   begin
      Internal (Get_Object (In_Box), Get_Object (Child),
                Expand_Ptr, Fill_Ptr, Padding, PackT_Ptr);
      Expand   := Expand_Ptr /= 0;
      Fill     := Fill_Ptr /= 0;
      PackType := Gtk_Pack_Type'Val (PackT_Ptr);
   end Query_Child_Packing;

   -----------------------
   -- Set_Child_Packing --
   -----------------------

   procedure Set_Child_Packing
     (In_Box    : access Gtk_Box_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand    : in Boolean;
      Fill      : in Boolean;
      Padding   : in Gint;
      PackType  : in Gtk_Pack_Type)
   is
      procedure Internal (In_Box   : in System.Address;
                          Child    : in System.Address;
                          Expand   : in Gint;
                          Fill     : in Gint;
                          Padding  : in Gint;
                          PackType : in Gint);
      pragma Import (C, Internal, "gtk_box_set_child_packing");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child),
                Boolean'Pos (Expand), Boolean'Pos (Fill), Padding,
                Gtk_Pack_Type'Pos (PackType));
   end Set_Child_Packing;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      use Container;

      Child_Name : constant Node_Ptr := Find_Tag (N.Child, "child_name");
      Class      : constant String_Ptr := Get_Field (N, "class");
      Id         : constant Gtk_Type := Get_Type;
      pragma Warnings (Off, Id);

   begin
      if Child_Name = null then
         if not N.Specific_Data.Created then
            Gen_New (N, "Box", Get_Field (N, "homogeneous").all,
              Get_Field (N, "spacing").all,
              Class (Class'First + 3) & "box", File);
         end if;

      else
         Gen_Child (N, Child_Name, File);
      end if;

      Container.Generate (N, File);

      if Child_Name /= null then
         Gen_Set (N, "Box", "homogeneous", File);
         Gen_Set (N, "Box", "spacing", File);
      end if;
   end Generate;

end Gtk.Box;
