-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                 Copyright (C) 2000-2002 ACT-Europe                --
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
with Gtk.Container; use Gtk.Container;

package body Gtk.Paned is

   ----------
   -- Add1 --
   ----------

   procedure Add1
     (Paned : access Gtk_Paned_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Paned : System.Address;
         Child : System.Address);
      pragma Import (C, Internal, "gtk_paned_add1");

   begin
      Internal (Get_Object (Paned), Get_Object (Child));
   end Add1;

   ----------
   -- Add2 --
   ----------

   procedure Add2
     (Paned : access Gtk_Paned_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Paned : System.Address;
         Child : System.Address);
      pragma Import (C, Internal, "gtk_paned_add2");

   begin
      Internal (Get_Object (Paned), Get_Object (Child));
   end Add2;

   ----------------
   -- Get_Child1 --
   ----------------

   function Get_Child1
     (Paned : access Gtk_Paned_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Paned : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_paned_get_child1");

   begin
      return Convert (Internal (Get_Object (Paned)));
   end Get_Child1;

   ----------------
   -- Get_Child2 --
   ----------------

   function Get_Child2
     (Paned : access Gtk_Paned_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Paned : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_paned_get_child2");

   begin
      return Convert (Internal (Get_Object (Paned)));
   end Get_Child2;

   -----------------------
   -- Get_Child1_Resize --
   -----------------------

   function Get_Child1_Resize
     (Paned : access Gtk_Paned_Record) return Boolean
   is
      function Internal (Paned : System.Address) return Gboolean;
      pragma Import (C, Internal, "ada_paned_get_child1_resize");

   begin
      return To_Boolean (Internal (Get_Object (Paned)));
   end Get_Child1_Resize;

   -----------------------
   -- Get_Child2_Resize --
   -----------------------

   function Get_Child2_Resize
     (Paned : access Gtk_Paned_Record) return Boolean
   is
      function Internal (Paned : System.Address) return Gboolean;
      pragma Import (C, Internal, "ada_paned_get_child2_resize");

   begin
      return To_Boolean (Internal (Get_Object (Paned)));
   end Get_Child2_Resize;

   -----------------------
   -- Get_Child1_Shrink --
   -----------------------

   function Get_Child1_Shrink
     (Paned : access Gtk_Paned_Record) return Boolean
   is
      function Internal (Paned : System.Address) return Gboolean;
      pragma Import (C, Internal, "ada_paned_get_child1_shrink");

   begin
      return To_Boolean (Internal (Get_Object (Paned)));
   end Get_Child1_Shrink;

   -----------------------
   -- Get_Child2_Shrink --
   -----------------------

   function Get_Child2_Shrink
     (Paned : access Gtk_Paned_Record) return Boolean
   is
      function Internal (Paned : System.Address) return Gboolean;
      pragma Import (C, Internal, "ada_paned_get_child2_shrink");

   begin
      return To_Boolean (Internal (Get_Object (Paned)));
   end Get_Child2_Shrink;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position (Paned : access Gtk_Paned_Record) return Gint is
      function Internal (Paned : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_paned_get_position");

   begin
      return Internal (Get_Object (Paned));
   end Get_Position;

   --------------------
   -- Gtk_New_Hpaned --
   --------------------

   procedure Gtk_New_Hpaned (Widget : out Gtk_Paned) is
   begin
      Widget := new Gtk_Paned_Record;
      Initialize_Hpaned (Widget);
   end Gtk_New_Hpaned;

   --------------------
   -- Gtk_New_Vpaned --
   --------------------

   procedure Gtk_New_Vpaned (Widget : out Gtk_Paned) is
   begin
      Widget := new Gtk_Paned_Record;
      Initialize_Vpaned (Widget);
   end Gtk_New_Vpaned;

   -----------------------
   -- Initialize_Hpaned --
   -----------------------

   procedure Initialize_Hpaned (Widget : access Gtk_Paned_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hpaned_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize_Hpaned;

   -----------------------
   -- Initialize_Vpaned --
   -----------------------

   procedure Initialize_Vpaned (Widget : access Gtk_Paned_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vpaned_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize_Vpaned;

   -----------
   -- Pack1 --
   -----------

   procedure Pack1
     (Paned  : access Gtk_Paned_Record;
      Child  : access Gtk_Widget_Record'Class;
      Resize : Boolean := False;
      Shrink : Boolean := True)
   is
      procedure Internal
        (Paned  : System.Address;
         Child  : System.Address;
         Resize : Gint;
         Shrink : Gint);
      pragma Import (C, Internal, "gtk_paned_pack1");

   begin
      Internal (Get_Object (Paned), Get_Object (Child),
                Boolean'Pos (Resize), Boolean'Pos (Shrink));
   end Pack1;

   -----------
   -- Pack2 --
   -----------

   procedure Pack2
     (Paned  : access Gtk_Paned_Record;
      Child  : access Gtk_Widget_Record'Class;
      Resize : Boolean := False;
      Shrink : Boolean := False)
   is
      procedure Internal
        (Paned  : System.Address;
         Child  : System.Address;
         Resize : Gint;
         Shrink : Gint);
      pragma Import (C, Internal, "gtk_paned_pack2");

   begin
      Internal (Get_Object (Paned), Get_Object (Child),
                Boolean'Pos (Resize), Boolean'Pos (Shrink));
   end Pack2;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Paned : access Gtk_Paned_Record; Position : Gint) is
      procedure Internal (Paned : System.Address; Position : Gint);
      pragma Import (C, Internal, "gtk_paned_set_position");

   begin
      Internal (Get_Object (Paned), Position);
   end Set_Position;

   ---------------------
   -- Set_Handle_Size --
   ---------------------

   procedure Set_Handle_Size
     (Paned : access Gtk_Paned_Record;
      Size  : Guint16)
   is
      pragma Unreferenced (Paned, Size);
   begin
      null;
   end Set_Handle_Size;

   ---------------------
   -- Get_Handle_Size --
   ---------------------

   function Get_Handle_Size (Paned : access Gtk_Paned_Record) return Guint16 is
      pragma Unreferenced (Paned);
   begin
      return 0;
   end Get_Handle_Size;

   ---------------------
   -- Set_Gutter_Size --
   ---------------------

   procedure Set_Gutter_Size
     (Paned : access Gtk_Paned_Record;
      Size  : Guint16)
   is
      pragma Unreferenced (Paned, Size);
   begin
      null;
   end Set_Gutter_Size;

   ---------------------
   -- Get_Gutter_Size --
   ---------------------

   function Get_Gutter_Size (Paned : access Gtk_Paned_Record) return Guint16 is
      pragma Unreferenced (Paned);
   begin
      return 0;
   end Get_Gutter_Size;

end Gtk.Paned;
