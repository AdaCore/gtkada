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
with Gtk.Util; use Gtk.Util;

package body Gtk.Layout is

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment (Layout : access Gtk_Layout_Record)
     return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Layout : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_layout_get_hadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Layout)), Stub));
   end Get_Hadjustment;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment (Layout : access Gtk_Layout_Record)
     return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Layout : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_layout_get_vadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Layout)), Stub));
   end Get_Vadjustment;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Layout      : out Gtk_Layout;
      Hadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment) is
   begin
      Layout := new Gtk_Layout_Record;
      Initialize (Layout, Hadjustment, Vadjustment);
   end Gtk_New;

   -----------------
   -- Initialize  --
   -----------------

   procedure Initialize
     (Layout : access Gtk_Layout_Record'Class;
      Hadjustment : Gtk.Adjustment.Gtk_Adjustment;
      Vadjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal
        (Hadjustment : in System.Address;
         Vadjustment : in System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_layout_new");

      Hadj, Vadj : System.Address;

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Hadjustment = null then
         Hadj := System.Null_Address;
      else
         Hadj := Get_Object (Hadjustment);
      end if;

      if Vadjustment = null then
         Vadj := System.Null_Address;
      else
         Vadj := Get_Object (Vadjustment);
      end if;

      Set_Object (Layout, Internal (Hadj, Vadj));
      Initialize_User_Data (Layout);
   end Initialize;

   ----------
   -- Move --
   ----------

   procedure Move
     (Layout : access Gtk_Layout_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : in     Gint;
      Y      : in     Gint)
   is
      procedure Internal
        (Layout : in System.Address;
         Widget : in System.Address;
         X      : in Gint;
         Y      : in Gint);
      pragma Import (C, Internal, "gtk_layout_move");

   begin
      Internal (Get_Object (Layout), Get_Object (Widget), X, Y);
   end Move;

   ---------
   -- Put --
   ---------

   procedure Put
     (Layout : access Gtk_Layout_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : in     Gint;
      Y      : in     Gint)
   is
      procedure Internal
        (Layout : in System.Address;
         Widget : in System.Address;
         X      : in Gint;
         Y      : in Gint);
      pragma Import (C, Internal, "gtk_layout_put");

   begin
      Internal (Get_Object (Layout), Get_Object (Widget), X, Y);
   end Put;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
     (Layout     : access Gtk_Layout_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal (Layout     : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_layout_set_hadjustment");
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Adjustment = null then
         Internal (Get_Object (Layout), System.Null_Address);
      else
         Internal (Get_Object (Layout), Get_Object (Adjustment));
      end if;
   end Set_Hadjustment;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (Layout : access Gtk_Layout_Record;
                       Width  : in     Guint;
                       Height : in     Guint)
   is
      procedure Internal (Layout : in System.Address;
                          Width  : in Guint;
                          Height : in Guint);
      pragma Import (C, Internal, "gtk_layout_set_size");
   begin
      Internal (Get_Object (Layout), Width, Height);
   end Set_Size;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
     (Layout     : access Gtk_Layout_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal (Layout     : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_layout_set_vadjustment");
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Adjustment = null then
         Internal (Get_Object (Layout), System.Null_Address);
      else
         Internal (Get_Object (Layout), Get_Object (Adjustment));
      end if;
   end Set_Vadjustment;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      Cur : constant String_Ptr := Get_Field (N, "name");
      Top : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");

   begin
      Gen_New (N, "Layout", File => File);
      Container.Generate (N, File);

      Gen_Set (N, "Layout", "Size", "area_width", "area_height", "", "",
        File => File);
      Add_Package ("Adjustment");
      Put_Line (File, "   Set_Step_Increment (Get_Hadjustment (" &
        To_Ada (Top.all) & "." & To_Ada (Cur.all) & "), " &
        To_Float (Get_Field (N, "hstep").all) & ");");
      Put_Line (File, "   Set_Step_Increment (Get_Vadjustment (" &
        To_Ada (Top.all) & "." & To_Ada (Cur.all) & "), " &
        To_Float (Get_Field (N, "vstep").all) & ");");
   end Generate;

   procedure Generate (Layout : in out Object.Gtk_Object; N : in Node_Ptr) is
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Layout (Layout));
         Set_Object (Get_Field (N, "name"), Layout);
         N.Specific_Data.Created := True;
      end if;

      Container.Generate (Layout, N);

      Set_Size (Gtk_Layout (Layout),
        Guint'Value (Get_Field (N, "area_width").all),
        Guint'Value (Get_Field (N, "area_height").all));
      Adjustment.Set_Step_Increment
        (Get_Hadjustment (Gtk_Layout (Layout)),
         Gfloat'Value (Get_Field (N, "hstep").all));
      Adjustment.Set_Step_Increment
        (Get_Vadjustment (Gtk_Layout (Layout)),
         Gfloat'Value (Get_Field (N, "vstep").all));
   end Generate;

end Gtk.Layout;
