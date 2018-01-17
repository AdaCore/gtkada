------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Gdk.Event;  use Gdk.Event;
with Gdk.Types;  use Gdk.Types;
with Gdk.Window; use Gdk.Window;
with Gtk.Widget; use Gtk.Widget;

package body Gtk.Dnd is

   ----------------------
   -- Set_Icon_Default --
   ----------------------

   procedure Set_Icon_Default (Context : Drag_Context) is
      procedure Internal
        (Context : System.Address);
      pragma Import (C, Internal, "gtk_drag_set_icon_default");

   begin
      Internal (Get_Object (Context));
   end Set_Icon_Default;

   ---------------------
   -- Set_Icon_Pixbuf --
   ---------------------

   procedure Set_Icon_Pixbuf
     (Context : Drag_Context;
      Pixbuf  : Gdk.Pixbuf.Gdk_Pixbuf;
      Hot_X   : Gint;
      Hot_Y   : Gint)
   is
      procedure Internal
        (Context : System.Address;
         Pixbuf  : System.Address;
         Hot_X   : Gint;
         Hot_Y   : Gint);
      pragma Import (C, Internal, "gtk_drag_set_icon_pixbuf");

   begin
      Internal (Get_Object (Context), Get_Object (Pixbuf), Hot_X, Hot_Y);
   end Set_Icon_Pixbuf;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name
     (Context   : Drag_Context;
      Icon_Name : String;
      Hot_X     : Gint;
      Hot_Y     : Gint)
   is
      procedure Internal
        (Context   : System.Address;
         Icon_Name : String;
         Hot_X     : Gint;
         Hot_Y     : Gint);
      pragma Import (C, Internal, "gtk_drag_set_icon_name");
   begin
      Internal (Get_Object (Context), Icon_Name & ASCII.NUL, Hot_X, Hot_Y);
   end Set_Icon_Name;

   --------------------
   -- Set_Icon_Stock --
   --------------------

   procedure Set_Icon_Stock
     (Context  : Drag_Context;
      Stock_Id : String;
      Hot_X    : Gint;
      Hot_Y    : Gint)
   is
      procedure Internal
        (Context  : System.Address;
         Stock_Id : String;
         Hot_X    : Gint;
         Hot_Y    : Gint);
      pragma Import (C, Internal, "gtk_drag_set_icon_stock");
   begin
      Internal (Get_Object (Context),
                Stock_Id & ASCII.NUL,
                Hot_X,
                Hot_Y);
   end Set_Icon_Stock;

   ----------------------------
   -- Source_Set_Icon_Pixbuf --
   ----------------------------

   procedure Source_Set_Icon_Pixbuf
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal
        (Widget : System.Address;
         Pixbuf : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_set_icon_pixbuf");

   begin
      Internal (Get_Object (Widget), Get_Object (Pixbuf));
   end Source_Set_Icon_Pixbuf;

   ---------------------------
   -- Source_Set_Icon_Stock --
   ---------------------------

   procedure Source_Set_Icon_Stock
     (Widget   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Stock_Id : String)
   is
      procedure Internal
        (Widget   : System.Address;
         Stock_Id : String);
      pragma Import (C, Internal, "gtk_drag_source_set_icon_stock");
   begin
      Internal (Get_Object (Widget),
                Stock_Id & ASCII.NUL);
   end Source_Set_Icon_Stock;

   --------------------------
   -- Source_Set_Icon_Name --
   --------------------------

   procedure Source_Set_Icon_Name
     (Widget    : access Gtk_Widget_Record'Class;
      Icon_Name : String)
   is
      procedure Internal (Widget : System.Address; Icon_Name : String);
      pragma Import (C, Internal, "gtk_drag_source_set_icon_name");
   begin
      Internal (Get_Object (Widget), Icon_Name & ASCII.NUL);
   end Source_Set_Icon_Name;

   ---------------------
   -- Check_Threshold --
   ---------------------

   function Check_Threshold
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Start_X   : Gint;
      Start_Y   : Gint;
      Current_X : Gint;
      Current_Y : Gint)
      return Boolean
   is
      function Internal
        (Widget    : System.Address;
         Start_X   : Gint;
         Start_Y   : Gint;
         Current_X : Gint;
         Current_Y : Gint)
         return Gint;
      pragma Import (C, Internal, "gtk_drag_check_threshold");
   begin
      return Boolean'Val (Internal (Get_Object (Widget),
                                    Start_X,
                                    Start_Y,
                                    Current_X,
                                    Current_Y));
   end Check_Threshold;

   --------------------------
   -- Dest_Set_Target_List --
   --------------------------

   procedure Dest_Set_Target_List
     (Widget      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Target_List : Gtk.Target_List.Gtk_Target_List)
   is
      procedure Internal
        (Widget      : System.Address;
         Target_List : System.Address);
      pragma Import (C, Internal, "gtk_drag_dest_set_target_list");
   begin
      Internal (Get_Object (Widget), Target_List.Get_Object);
   end Dest_Set_Target_List;

   --------------------------
   -- Dest_Get_Target_List --
   --------------------------

   function Dest_Get_Target_List
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk_Target_List
   is
      function Internal (Widget : System.Address)
                        return System.Address;
      pragma Import (C, Internal, "gtk_drag_dest_get_target_list");
      T : Gtk_Target_List;
   begin
      T.Set_Object (Internal (Get_Object (Widget)));
      return T;
   end Dest_Get_Target_List;

   ----------------------------
   -- Source_Set_Target_List --
   ----------------------------

   procedure Source_Set_Target_List
     (Widget      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Target_List : Gtk.Target_List.Gtk_Target_List)
   is
      procedure Internal
        (Widget      : System.Address;
         Target_List : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_set_target_list");
   begin
      Internal (Get_Object (Widget), Target_List.Get_Object);
   end Source_Set_Target_List;

   ----------------------------
   -- Source_Get_Target_List --
   ----------------------------

   function Source_Get_Target_List
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk_Target_List
   is
      function Internal (Widget : System.Address)
                        return System.Address;
      pragma Import (C, Internal, "gtk_drag_source_get_target_list");

      T : Gtk_Target_List;
   begin
      T.Set_Object (Internal (Get_Object (Widget)));
      return T;
   end Source_Get_Target_List;

   ----------------------
   -- Dest_Find_Target --
   ----------------------

   function Dest_Find_Target
     (Widget      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Context     : Gdk.Drag_Contexts.Drag_Context;
      Target_List : Gtk.Target_List.Gtk_Target_List) return Gdk.Types.Gdk_Atom
   is
      function Internal
        (Widget      : System.Address;
         Context     : System.Address;
         Target_List : System.Address)
         return Gdk_Atom;
      pragma Import (C, Internal, "gtk_drag_dest_find_target");
   begin
      return Internal
        (Get_Object (Widget), Get_Object (Context), Target_List.Get_Object);
   end Dest_Find_Target;

   ---------------------------
   -- Dest_Get_Track_Motion --
   ---------------------------

   function Dest_Get_Track_Motion
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Boolean
   is
      function Internal (Widget : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_drag_dest_get_track_motion");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Dest_Get_Track_Motion;

   ---------------------------
   -- Dest_Set_Track_Motion --
   ---------------------------

   procedure Dest_Set_Track_Motion
     (Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Track_Motion : Boolean)
   is
      procedure Internal
        (Widget       : System.Address;
         Track_Motion : Gboolean);
      pragma Import (C, Internal, "gtk_drag_dest_set_track_motion");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Track_Motion));
   end Dest_Set_Track_Motion;

   --------------
   -- Dest_Set --
   --------------

   procedure Dest_Set
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags   : Dest_Defaults := Dest_No_Default;
      Targets : Target_Entry_Array := No_Target_Entry;
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
      Proxy_Window    : Gdk.Gdk_Window;
      Protocol        : Drag_Protocol;
      Use_Coordinates : Boolean)
   is
      procedure Internal
        (Widget          : System.Address;
         Proxy_Window    : Gdk.Gdk_Window;
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
      Start_Button_Mask : Gdk.Types.Gdk_Modifier_Type;
      Targets           : Target_Entry_Array := No_Target_Entry;
      Actions           : Drag_Action)
   is
      procedure Internal
        (Widget            : System.Address;
         Start_Button_Mask : Gdk.Types.Gdk_Modifier_Type;
         Targets           : System.Address;
         N_Targets         : Gint;
         Actions           : Drag_Action);
      pragma Import (C, Internal, "gtk_drag_source_set");

   begin
      if Targets'Length = 0 then
         Internal (Get_Object (Widget), Start_Button_Mask,
                   System.Null_Address, 0, Actions);
      else
         Internal (Get_Object (Widget), Start_Button_Mask,
                   Targets (Targets'First)'Address,
                   Targets'Length,
                   Actions);
      end if;
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
        (Context  : System.Address;
         Succcess : Gint;
         Del      : Gint;
         Time     : Guint32);
      pragma Import (C, Internal, "gtk_drag_finish");

   begin
      Internal
        (Get_Object (Context), Boolean'Pos (Success), Boolean'Pos (Del), Time);
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
         Context  : System.Address;
         Target   : Gdk.Types.Gdk_Atom;
         Time     : Guint32);
      pragma Import (C, Internal, "gtk_drag_get_data");

   begin
      Internal (Get_Object (Widget), Get_Object (Context), Target, Time);
   end Get_Data;

   -----------------------
   -- Get_Source_Widget --
   -----------------------

   function Get_Source_Widget
     (Context : Drag_Context) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drag_get_source_widget");

   begin
      return Convert (Internal (Get_Object (Context)));
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
      Targets : Gtk_Target_List;
      Actions : Drag_Action;
      Button  : Gint;
      Event   : Gdk.Event.Gdk_Event) return Drag_Context
   is
      function Internal
        (Widget  : System.Address;
         Targets : System.Address;
         Actions : Drag_Action;
         Button  : Gint;
         Event   : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_drag_begin");
      Stub : Gdk.Drag_Contexts.Drag_Context_Record;
   begin
      return Drag_Context
        (Get_User_Data
           (Internal
              (Get_Object (Widget),
               Get_Object (Targets),
               Actions, Button, To_Address (Event)),
            Stub));
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
        (Context : System.Address;
         Widget  : System.Address;
         Hot_X   : Gint;
         Hot_Y   : Gint);
      pragma Import (C, Internal, "gtk_drag_set_icon_widget");

   begin
      Internal (Get_Object (Context), Get_Object (Widget), Hot_X, Hot_Y);
   end Set_Icon_Widget;

   ----------------------------
   -- Dest_Add_Image_Targets --
   ----------------------------

   procedure Dest_Add_Image_Targets
     (Widget : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_dest_add_image_targets");
   begin
      Internal (Get_Object (Widget));
   end Dest_Add_Image_Targets;

   ---------------------------
   -- Dest_Add_Text_Targets --
   ---------------------------

   procedure Dest_Add_Text_Targets
     (Widget : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_dest_add_text_targets");
   begin
      Internal (Get_Object (Widget));
   end Dest_Add_Text_Targets;

   --------------------------
   -- Dest_Add_Uri_Targets --
   --------------------------

   procedure Dest_Add_Uri_Targets
     (Widget : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_dest_add_uri_targets");
   begin
      Internal (Get_Object (Widget));
   end Dest_Add_Uri_Targets;

   ------------------------------
   -- Source_Add_Image_Targets --
   ------------------------------

   procedure Source_Add_Image_Targets
     (Widget : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_add_image_targets");
   begin
      Internal (Get_Object (Widget));
   end Source_Add_Image_Targets;

   -----------------------------
   -- Source_Add_Text_Targets --
   -----------------------------

   procedure Source_Add_Text_Targets
     (Widget : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_add_text_targets");
   begin
      Internal (Get_Object (Widget));
   end Source_Add_Text_Targets;

   ----------------------------
   -- Source_Add_Uri_Targets --
   ----------------------------

   procedure Source_Add_Uri_Targets
     (Widget : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_add_uri_targets");
   begin
      Internal (Get_Object (Widget));
   end Source_Add_Uri_Targets;

end Gtk.Dnd;
