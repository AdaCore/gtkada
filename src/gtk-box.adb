-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with System;
with Gdk; use Gdk;

package body Gtk.Box is

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Box : in Gtk_Box;
                       Num : in Gint)
                       return   Gtk.Widget.Gtk_Widget is
      function Internal (Box : in System.Address;
                         Num : in Gint)
                         return  System.Address;
      pragma Import (C, Internal, "ada_box_get_child");
      W : Gtk.Widget.Gtk_Widget;
   begin
      Set_Object (W, Internal (Get_Object (Box), Num));
      return W;
   end Get_Child;

   ------------------
   -- Gtk_New_Vbox --
   ------------------

   procedure Gtk_New_Vbox (Widget      : out Gtk_Box;
                           Homogeneous : in  Boolean;
                           Spacing     : in  Gint)
   is
      function Internal (Homogeneous : Gint;
                         Spacing     : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_vbox_new");
   begin
      Set_Object (Widget, Internal (Boolean'Pos (Homogeneous),
                                    Spacing));
   end Gtk_New_Vbox;

   ------------------
   -- Gtk_New_Hbox --
   ------------------

   procedure Gtk_New_Hbox (Widget      : out Gtk_Box;
                           Homogeneous : in  Boolean;
                           Spacing     : in  Gint)
   is
      function Internal (Homogeneous : Gint;
                         Spacing     : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_hbox_new");
   begin
      Set_Object (Widget, Internal (Boolean'Pos (Homogeneous),
                                    Spacing));
   end Gtk_New_Hbox;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
     (In_Box  : in Gtk_Box'Class;
      Child   : in Gtk.Widget.Gtk_Widget'Class;
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

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
     (In_Box  : in Gtk_Box'Class;
      Child   : in Gtk.Widget.Gtk_Widget'Class;
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

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous (In_Box      : in Gtk_Box'Class;
                              Homogeneous : in Boolean)
   is
      procedure Internal (In_Box      : in System.Address;
                          Homogeneous : in Gint);
      pragma Import (C, Internal, "gtk_box_set_homogeneous");
   begin
      Internal (Get_Object (In_Box), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing (In_Box  : in Gtk_Box'Class;
                          Spacing : in Gint)
   is
      procedure Internal (In_Box  : in System.Address;
                          Spacing : in Gint);
      pragma Import (C, Internal, "gtk_box_set_spacing");
   begin
      Internal (Get_Object (In_Box), Spacing);
   end Set_Spacing;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
     (In_Box : in Gtk_Box'Class;
      Child  : in Gtk.Widget.Gtk_Widget'Class;
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
     (In_Box   : in  Gtk_Box'Class;
      Child    : in  Gtk.Widget.Gtk_Widget'Class;
      Expand   : out Boolean;
      Fill     : out Boolean;
      Padding  : out Gint;
      PackType : out Gtk_Pack_Type)
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
     (In_Box    : in Gtk_Box'Class;
      Child     : in Gtk.Widget.Gtk_Widget'Class;
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


end Gtk.Box;
