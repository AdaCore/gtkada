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
with Gdk; use Gdk;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Object; use Gtk.Object;

package body Gtk.Container is

   ---------
   -- Add --
   ---------

   procedure Add (Container : access Gtk_Container_Record;
                  Widget    : access Gtk.Widget.Gtk_Widget_Record'Class) is
      procedure Internal (Container : System.Address;
                          Widget    : System.Address);
      pragma Import (C, Internal, "gtk_container_add");
   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Add;

   ------------------
   -- Check_Resize --
   ------------------

   procedure Check_Resize (Container : access Gtk_Container_Record) is
      procedure Internal (Container : System.Address);
      pragma Import (C, Internal, "gtk_container_check_resize");
   begin
      Internal (Get_Object (Container));
   end Check_Resize;

   --------------
   -- Children --
   --------------

   function Children (Container : access Gtk_Container_Record)
                      return Gtk.Widget.Widget_List.Glist is
      function Internal (Container : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_container_children");
      List : Gtk.Widget.Widget_List.Glist;
   begin
      Gtk.Widget.Widget_List.Set_Object (List,
                                         Internal (Get_Object (Container)));
      return List;
   end Children;

   ----------------
   -- Child_Type --
   ----------------

   function Child_Type (Container : access Gtk_Container_Record)
                       return Gtk.Gtk_Type
   is
      function Internal (Container : System.Address) return Gtk.Gtk_Type;
      pragma Import (C, Internal, "gtk_container_child_type");
   begin
      return Internal (Get_Object (Container));
   end Child_Type;

   -------------------
   -- Get_Toplevels --
   -------------------

   function Get_Toplevels return Gtk.Widget.Widget_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_container_get_toplevels");
      List : Gtk.Widget.Widget_List.Glist;
   begin
      Gtk.Widget.Widget_List.Set_Object (List, Internal);
      return List;
   end Get_Toplevels;

   ------------
   -- Remove --
   ------------

   procedure Remove (Container : access Gtk_Container_Record;
                     Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      procedure Internal (Container : System.Address;
                          Widget : System.Address);
      pragma Import (C, Internal, "gtk_container_remove");
   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Remove;

   ----------------------
   -- Set_Border_Width --
   ----------------------

   procedure Set_Border_Width (Container : access Gtk_Container_Record;
                               Border_Width : in Gint) is
      procedure Internal (Container  : System.Address;
                          Border_Widget : Gint);
      pragma Import (C, Internal, "gtk_container_set_border_width");
   begin
      Internal (Get_Object (Container), Border_Width);
   end Set_Border_Width;

   ---------------------
   -- Set_Focus_Child --
   ---------------------

   procedure Set_Focus_Child
     (Container : access Gtk_Container_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Container : System.Address;
                          Child     : System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_child");
   begin
      Internal (Get_Object (Container), Get_Object (Child));
   end Set_Focus_Child;

   ---------------------------
   -- Set_Focus_Hadjustment --
   ---------------------------

   procedure Set_Focus_Hadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal (Container : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_hadjustment");
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Adjustment = null then
         Internal (Get_Object (Container), System.Null_Address);
      else
         Internal (Get_Object (Container), Get_Object (Adjustment));
      end if;
   end Set_Focus_Hadjustment;

   ---------------------------
   -- Set_Focus_Vadjustment --
   ---------------------------

   procedure Set_Focus_Vadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal (Container : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_vadjustment");
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Adjustment = null then
         Internal (Get_Object (Container), System.Null_Address);
      else
         Internal (Get_Object (Container), Get_Object (Adjustment));
      end if;
   end Set_Focus_Vadjustment;

   ----------------------------
   -- Set_Reallocate_Redraws --
   ----------------------------

   procedure Set_Reallocate_Redraws (Container : access Gtk_Container_Record;
                                     Needs_Redraws : Boolean := False)
   is
      procedure Internal (Container : in System.Address;
                          Needs_Redraws : in Gint);
      pragma Import (C, Internal, "gtk_container_set_reallocate_redraws");
   begin
      Internal (Get_Object (Container), Boolean'Pos (Needs_Redraws));
   end Set_Reallocate_Redraws;

   ---------------------
   -- Set_Resize_Mode --
   ---------------------

   procedure Set_Resize_Mode (Container   : access Gtk_Container_Record;
                              Resize_Mode : in Gtk_Resize_Mode) is
      procedure Internal (Container : in System.Address;
                          Mode      : in Integer);
      pragma Import (C, Internal, "gtk_container_set_resize_mode");
   begin
      Internal (Get_Object (Container), Gtk_Resize_Mode'Pos (Resize_Mode));
   end Set_Resize_Mode;

   -----------
   -- Focus --
   -----------

   function Focus (Container : access Gtk_Container_Record;
                   Direction : Gtk_Direction_Type)
                  return Boolean
   is
      function Internal (Container : System.Address;
                         Direction : Gtk_Direction_Type)
                        return Gint;
      pragma Import (C, Internal, "gtk_container_focus");
   begin
      return Boolean'Val (Internal (Get_Object (Container), Direction));
   end Focus;

   -----------------------
   -- Register_Toplevel --
   -----------------------

   procedure Register_Toplevel (Container : access Gtk_Container_Record) is
      procedure Internal (Container : System.Address);
      pragma Import (C, Internal, "gtk_container_register_toplevel");
   begin
      Internal (Get_Object (Container));
   end Register_Toplevel;

   -------------------------
   -- Unregister_Toplevel --
   -------------------------

   procedure Unregister_Toplevel (Container : access Gtk_Container_Record) is
      procedure Internal (Container : System.Address);
      pragma Import (C, Internal, "gtk_container_unregister_toplevel");
   begin
      Internal (Get_Object (Container));
   end Unregister_Toplevel;

   ----------------
   -- Forall_Pkg --
   ----------------

   package body Forall_Pkg is

      type Internal_Data is record
         Data : Data_Type;
         Func : Forall_Function;
      end record;

      type Internal_Data_Access is access all Internal_Data;

      procedure Internal_Func (Widget : System.Address;
                               Data   : Internal_Data_Access);

      -------------------
      -- Internal_Func --
      -------------------

      procedure Internal_Func (Widget : System.Address;
                               Data   : Internal_Data_Access)
      is
         Stub : Gtk_Widget_Record;
      begin
         Data.Func (Gtk_Widget (Get_User_Data (Widget, Stub)), Data.Data);
      end Internal_Func;

      ------------
      -- Forall --
      ------------

      procedure Forall (Container : access Gtk_Container_Record;
                        Func      : Forall_Function;
                        Data      : Data_Type)
      is
         procedure Internal (Container : System.Address;
                             Func      : System.Address;
                             Data      : System.Address);
         pragma Import (C, Internal, "gtk_container_forall");
         D : aliased Internal_Data := (Data, Func);
      begin
         Internal (Get_Object (Container), Internal_Func'Address, D'Address);
      end Forall;

   end Forall_Pkg;

   ------------
   -- Forall --
   ------------

   procedure Forall (Container : access Gtk_Container_Record;
                     Func      : Forall_Function)
   is
      procedure Internal_Func (Widget : System.Address;
                               Data   : Forall_Function);

      -------------------
      -- Internal_Func --
      -------------------

      procedure Internal_Func (Widget : System.Address;
                               Data   : Forall_Function)
      is
         Stub : Gtk_Widget_Record;
      begin
         Data (Gtk_Widget (Get_User_Data (Widget, Stub)));
      end Internal_Func;

      procedure Internal (Container : System.Address;
                          Func      : System.Address;
                          Data      : Forall_Function);
      pragma Import (C, Internal, "gtk_container_forall");
   begin
      Internal (Get_Object (Container), Internal_Func'Address, Func);
   end Forall;

   --------------
   -- Generate --
   --------------

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type) is
   begin
      Widget.Generate (N, File);
      Gen_Set (N, "Container", "border_width", File);
      Gen_Set (N, "Container", "resize_mode", File);
   end Generate;

   procedure Generate (Container : in out Gtk_Object; N : in Node_Ptr) is
      S : String_Ptr;
   begin
      Widget.Generate (Container, N);
      S := Get_Field (N, "border_width");

      if S /= null then
         Set_Border_Width (Gtk_Container (Container), Gint'Value (S.all));
      end if;

      S := Get_Field (N, "resize_mode");

      if S /= null then
         Set_Resize_Mode
           (Gtk_Container (Container),
            Gtk_Resize_Mode'Value (S (S'First + 4 .. S'Last)));
      end if;
   end Generate;

end Gtk.Container;
