-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

--  The widget is the base of the tree for displayable objects.
--  (A displayable object is one which takes up some amount
--  of screen real estate). It provides a common base and interface
--  which actual widgets must adhere to.

with Gdk.Color;       use Gdk.Color;
with Gdk.Event;
with Gdk.Rectangle;
with Gdk.Types;
with Gdk.Visual;      use Gdk.Visual;
with Gdk.Window;
with Gtk.Accel_Group;
with Gtk.Enums;
with Gtk.Object;
with Glib.Glist;
with Glib.GSlist;
pragma Elaborate_All (Glib.GSlist);

with System;

package Gtk.Widget is

   --  Flags used by Widget on top of Object
   Toplevel         : constant := 2 ** 4;
   No_Window        : constant := 2 ** 5;
   Realized         : constant := 2 ** 6;
   Mapped           : constant := 2 ** 7;
   Visible          : constant := 2 ** 8;
   Sensitive        : constant := 2 ** 9;
   Parent_Sensitive : constant := 2 ** 10;
   Can_Focus        : constant := 2 ** 11;
   Has_Focus        : constant := 2 ** 12;
   Can_Default      : constant := 2 ** 13;
   Has_Default      : constant := 2 ** 14;
   Has_Grab         : constant := 2 ** 15;
   Rc_Style         : constant := 2 ** 16;
   Composite_Child  : constant := 2 ** 17;
   No_Reparent      : constant := 2 ** 18;
   App_Paintable    : constant := 2 ** 19;
   Receives_Default : constant := 2 ** 20;

   type Gtk_Widget_Record is new Object.Gtk_Object_Record with null record;
   type Gtk_Widget is access all Gtk_Widget_Record'Class;

   --  This is the desired amount of space when you create new widgets.
   --  See the examples/base_widget directory for an example how to use this

   type Gtk_Requisition is record
      Width  : Gint16;
      Height : Gint16;
   end record;
   pragma Pack (Gtk_Requisition);

   --  This is a size and position for a new widget. See examples/base_widget

   type Gtk_Allocation is record
      X      : Gint16;
      Y      : Gint16;
      Width  : Guint16;
      Height : Guint16;
   end record;
   pragma Pack (Gtk_Allocation);

   function Get_Window (Widget : access Gtk_Widget_Record)
                        return Gdk.Window.Gdk_Window;

   procedure Activate (Widget : access Gtk_Widget_Record);

   procedure Destroy (Widget : access Gtk_Widget_Record);

   procedure Draw
     (Widget : access Gtk_Widget_Record;
      Area   : in Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area);

   procedure Set_Name (Widget : access Gtk_Widget_Record; Name : in String);

   procedure Set_Sensitive (Widget    : access Gtk_Widget_Record;
                            Sensitive : in Boolean := True);

   procedure Set_UPosition (Widget : access Gtk_Widget_Record; X, Y : in Gint);

   procedure Set_USize (Widget : access Gtk_Widget_Record;
                        Width, Height : in Gint);

   procedure Show (Widget : access Gtk_Widget_Record);

   procedure Show_All (Widget : access Gtk_Widget_Record);

   procedure Hide (Widget : access Gtk_Widget_Record);

   procedure Map (Widget : access Gtk_Widget_Record);

   procedure Unmap (Widget : access Gtk_Widget_Record);

   procedure Realize (Widget : access Gtk_Widget_Record);

   procedure Unrealize (Widget : access Gtk_Widget_Record);

   procedure Reparent (Widget : access Gtk_Widget_Record;
                       New_Parent : access Gtk_Widget_Record'Class);

   function Get_Parent (Widget : access Gtk_Widget_Record)
                        return Gtk_Widget;

   procedure Popup (Widget : access Gtk_Widget_Record; X, Y : in Gint);

   procedure Grab_Default (Widget : access Gtk_Widget_Record);

   procedure Grab_Focus (Widget : access Gtk_Widget_Record);

   procedure Set_Parent (Widget : access Gtk_Widget_Record;
                         Parent : in Gtk_Widget);

   function Get_Toplevel (Widget : access Gtk_Widget_Record)
                          return Gtk_Widget;

   function Get_Events (Widget : access Gtk_Widget_Record) return Gint;

   procedure Set_Events (Widget : access Gtk_Widget_Record;
                         Events : in     Gdk.Types.Gdk_Event_Mask);

   procedure Set_Extension_Events
     (Widget : access Gtk_Widget_Record;
      Mode   : in     Gdk.Types.Gdk_Extension_Mode);

   procedure Set_State (Widget : access Gtk_Widget_Record;
                        State : in Enums.Gtk_State_Type);

   --  (See also gtk-style for functions dealing with styles)

   --  The following functions deal with Visuals

   function Get_Visual (Widget : access Gtk_Widget_Record) return Gdk_Visual;
   function Get_Default_Visual return Gdk_Visual;
   procedure Set_Visual (Widget : access Gtk_Widget_Record;
                         Visual : Gdk_Visual);
   procedure Set_Default_Visual (Visual : Gdk_Visual);

   --  The following functions deal with Colormaps

   function Get_Colormap (Widget : access Gtk_Widget_Record)
                          return Gdk_Colormap;
   function Get_Default_Colormap return Gdk_Colormap;
   procedure Set_Colormap (Widget : access Gtk_Widget_Record;
                           Cmap : Gdk_Colormap);
   procedure Set_Default_Colormap (Cmap : Gdk_Colormap);

   --  The following four functions get the size and position of the widget

   function Get_Allocation_Width (Widget : access Gtk_Widget_Record)
                                  return Guint;
   function Get_Allocation_Height (Widget : access Gtk_Widget_Record)
                                   return Guint;
   function Get_Allocation_X (Widget : access Gtk_Widget_Record) return Gint;
   function Get_Allocation_Y (Widget : access Gtk_Widget_Record) return Gint;

   -------------
   --  Events --
   -------------

   procedure Event (Widget : access Gtk_Widget_Record'Class;
                    Event  : Gdk.Event.Gdk_Event);

   -------------------
   --  Accelerators --
   -------------------

   procedure Add_Accelerator
     (Widget       : access Gtk_Widget_Record;
      Accel_Signal : in String;
      Accel_Group  : in Gtk.Accel_Group.Gtk_Accel_Group;
      Accel_Key    : in Gdk.Types.Gdk_Key_Type;
      Accel_Mods   : in Gdk.Types.Gdk_Modifier_Type;
      Accel_Flags  : in Gtk.Accel_Group.Gtk_Accel_Flags);

   procedure Remove_Accelerator
     (Widget       : access Gtk_Widget_Record;
      Accel_Group  : in Gtk.Accel_Group.Gtk_Accel_Group;
      Accel_Key    : in Gdk.Types.Gdk_Key_Type;
      Accel_Mods   : in Gdk.Types.Gdk_Modifier_Type);

   procedure Remove_Accelerators
     (Widget       : access Gtk_Widget_Record;
      Accel_Signal : in String;
      Visible_Only : in Boolean);

   function Accelerator_Signal
     (Widget       : access Gtk_Widget_Record;
      Accel_Group  : in Gtk.Accel_Group.Gtk_Accel_Group;
      Accel_Key    : in Gdk.Types.Gdk_Key_Type;
      Accel_Mods   : in Gdk.Types.Gdk_Modifier_Type)
     return Guint;

   procedure Lock_Accelerators
     (Widget       : access Gtk_Widget_Record);

   procedure Unlock_Accelerators
     (Widget       : access Gtk_Widget_Record);

   --------------------
   --  Widget flags  --
   --------------------

   function Toplevel_Is_Set (Widget : access Gtk_Widget_Record'Class)
                             return Boolean;
   function No_Window_Is_Set (Widget : access Gtk_Widget_Record'Class)
                              return Boolean;
   function Realized_Is_Set (Widget : access Gtk_Widget_Record'Class)
                             return Boolean;
   function Mapped_Is_Set (Widget : access Gtk_Widget_Record'Class)
                           return Boolean;
   function Visible_Is_Set (Widget : access Gtk_Widget_Record'Class)
                            return Boolean;
   function Drawable_Is_Set (Widget : access Gtk_Widget_Record'Class)
                             return Boolean;
   function Sensitive_Is_Set (Widget : access Gtk_Widget_Record'Class)
                              return Boolean;
   function Parent_Sensitive_Is_Set (Widget : access Gtk_Widget_Record'Class)
                                     return Boolean;
   function Is_Sensitive_Is_Set (Widget : access Gtk_Widget_Record'Class)
                                 return Boolean;
   function Can_Focus_Is_Set (Widget : access Gtk_Widget_Record'Class)
                              return Boolean;
   function Has_Focus_Is_Set (Widget : access Gtk_Widget_Record'Class)
                              return Boolean;
   function Has_Default_Is_Set (Widget : access Gtk_Widget_Record'Class)
                                return Boolean;
   function Has_Grab_Is_Set (Widget : access Gtk_Widget_Record'Class)
                             return Boolean;
   function Rc_Style_Is_Set (Widget : access Gtk_Widget_Record'Class)
                             return Boolean;

   -----------------------
   --  Default callbacks
   --  These methods are available for use with Gtk.Signal.C_Unsafe_Connect
   -----------------------

   function Get_Default_Motion_Notify_Event (Widget : access Gtk_Widget_Record)
                                             return System.Address;

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);

   procedure Generate (Widget : in out Object.Gtk_Object; N : in Node_Ptr);

   ------------------------
   --  Definitions for lists of widgets
   ------------------------

   function Convert (W : in Gtk_Widget) return System.Address;
   function Convert (W : System.Address) return Gtk_Widget;
   package Widget_List is new Glib.Glist.Generic_List (Gtk_Widget);
   package Widget_SList is new Glib.GSlist.Generic_SList (Gtk_Widget);

end Gtk.Widget;
