-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

with Glib;          use Glib;
with Gdk.Bitmap;    use Gdk.Bitmap;
with Gdk.Color;     use Gdk.Color;
with Gdk.Event;     use Gdk.Event;
with Gdk.Pixmap;    use Gdk.Pixmap;
with Gdk.Types;     use Gdk.Types;
with Gdk.Window;    use Gdk.Window;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Enums;     use Gtk.Enums;

package body Gtk.Dnd is

   --------------
   -- Dest_Set --
   --------------

   procedure Dest_Set
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags   : Dest_Defaults := Dest_No_Default;
      Targets : Target_Entry_Array := Any_Target_Entry;
      Actions : Drag_Action := Action_Any)
   is
      procedure Internal
        (Widget    : System.Address;
         Flags     : Dest_Defaults;
         Targets   : System.Address;
         N_Targets : Gint;
         Actions   : Drag_Action);
      pragma Import (C, Internal, "gtk_drag_dest_set");

   begin
      if Targets'Length = 0 then
         Internal
           (Get_Object (Widget), Flags, System.Null_Address, 0, Actions);
      else
         Internal
           (Get_Object (Widget), Flags,
            Targets'Address, Targets'Length, Actions);
      end if;
   end Dest_Set;

   --------------------
   -- Dest_Set_Proxy --
   --------------------

   procedure Dest_Set_Proxy
     (Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
      Proxy_Window    : Gdk.Window.Gdk_Window;
      Protocol        : Drag_Protocol;
      Use_Coordinates : Boolean)
   is
      procedure Internal
        (Widget          : System.Address;
         Proxy_Window    : Gdk.Window.Gdk_Window;
         Protocol        : Drag_Protocol;
         Use_Coordinates : Gint);
      pragma Import (C, Internal, "gtk_drag_dest_set_proxy");

   begin
      Internal (Get_Object (Widget), Proxy_Window, Protocol,
                Boolean'Pos (Use_Coordinates));
   end Dest_Set_Proxy;

   ----------------
   -- Dest_Unset --
   ----------------

   procedure Dest_Unset
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_dest_unset");

   begin
      Internal (Get_Object (Widget));
   end Dest_Unset;

   ----------------
   -- Source_Set --
   ----------------

   procedure Source_Set
     (Widget            : access Gtk.Widget.Gtk_Widget_Record'Class;
      Start_Button_Mask : in Gdk.Types.Gdk_Modifier_Type;
      Targets           : in Target_Entry_Array;
      Actions           : in Drag_Action)
   is
      procedure Internal
        (Widget            : System.Address;
         Start_Button_Mask : Gdk.Types.Gdk_Modifier_Type;
         Targets           : System.Address;
         N_Targets         : Gint;
         Actions           : Drag_Action);
      pragma Import (C, Internal, "gtk_drag_source_set");

   begin
      Internal (Get_Object (Widget), Start_Button_Mask,
                Targets (Targets'First)'Address,
                Targets'Length,
                Actions);
   end Source_Set;

   ------------------
   -- Source_Unset --
   ------------------

   procedure Source_Unset
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_unset");

   begin
      Internal (Get_Object (Widget));
   end Source_Unset;

   ------------
   -- Finish --
   ------------

   procedure Finish
     (Context : Drag_Context;
      Success : Boolean;
      Del     : Boolean;
      Time    : Guint32 := 0)
   is
      procedure Internal
        (Context  : Drag_Context;
         Succcess : Gint;
         Del      : Gint;
         Time     : Guint32);
      pragma Import (C, Internal, "gtk_drag_finish");

   begin
      Internal (Context, Boolean'Pos (Success), Boolean'Pos (Del), Time);
   end Finish;

   --------------
   -- Get_Data --
   --------------

   procedure Get_Data
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Context : Drag_Context;
      Target  : Gdk.Types.Gdk_Atom;
      Time    : Guint32 := 0)
   is
      procedure Internal
        (Widget   : System.Address;
         Context  : Drag_Context;
         Target   : Gdk.Types.Gdk_Atom;
         Time     : Guint32);
      pragma Import (C, Internal, "gtk_drag_get_data");

   begin
      Internal (Get_Object (Widget), Context, Target, Time);
   end Get_Data;

   -----------------------
   -- Get_Source_Widget --
   -----------------------

   function Get_Source_Widget
     (Context : in Drag_Context) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Context : Drag_Context) return System.Address;
      pragma Import (C, Internal, "gtk_drag_get_source_widget");

   begin
      return Convert (Internal (Context));
   end Get_Source_Widget;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_highlight");

   begin
      Internal (Get_Object (Widget));
   end Highlight;

   -----------------
   -- Unhighlight --
   -----------------

   procedure Unhighlight
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_unhighlight");

   begin
      Internal (Get_Object (Widget));
   end Unhighlight;

   ----------------
   -- Drag_Begin --
   ----------------

   function Drag_Begin
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Targets : Target_List;
      Actions : Drag_Action;
      Button  : Gint;
      Event   : Gdk.Event.Gdk_Event) return Drag_Context
   is
      function Internal
        (Widget  : System.Address;
         Targets : Target_List;
         Actions : Drag_Action;
         Button  : Gint;
         Event   : System.Address) return Drag_Context;
      pragma Import (C, Internal, "gtk_drag_begin");

   begin
      return Internal (Get_Object (Widget), Targets, Actions, Button,
                       To_Address (Event));
   end Drag_Begin;

   ---------------------
   -- Set_Icon_Widget --
   ---------------------

   procedure Set_Icon_Widget
     (Context : Drag_Context;
      Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Hot_X   : Gint;
      Hot_Y   : Gint)
   is
      procedure Internal
        (Context : Drag_Context;
         Widget  : System.Address;
         Hot_X   : Gint;
         Hot_Y   : Gint);
      pragma Import (C, Internal, "gtk_drag_set_icon_widget");

   begin
      Internal (Context, Get_Object (Widget), Hot_X, Hot_Y);
   end Set_Icon_Widget;

   ---------------------
   -- Source_Set_Icon --
   ---------------------

   procedure Source_Set_Icon
     (Widget   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Colormap : Gdk.Color.Gdk_Colormap;
      Pixmap   : Gdk.Pixmap.Gdk_Pixmap;
      Mask     : Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
        (Widget : System.Address;
         Colormap : Gdk_Colormap;
         Pixmap   : Gdk.Pixmap.Gdk_Pixmap;
         Mask     : Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_drag_source_set_icon");

   begin
      Internal (Get_Object (Widget), Colormap, Pixmap, Mask);
   end Source_Set_Icon;

   -----------------
   -- Get_Targets --
   -----------------

   function Get_Targets (Context : Drag_Context) return Guint_List.Glist is
      function Internal (Context : Drag_Context) return System.Address;
      pragma Import (C, Internal, "ada_gtk_dnd_context_get_targets");

      List : Guint_List.Glist;

   begin
      Guint_List.Set_Object (List, Internal (Context));
      return List;
   end Get_Targets;

end Gtk.Dnd;
