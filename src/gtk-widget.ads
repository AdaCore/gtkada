-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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
with Gtk.Enums;
with Gtk.Object;
with Glib.Glist;
with Glib.GSlist;
with System;

package Gtk.Widget is

   --  Flags used by Widget on top of Object
   TopLevel         : constant := 2 ** 4;
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
   Basic            : constant := 2 ** 16;
   Reserved_3       : constant := 2 ** 17;
   Rc_Style         : constant := 2 ** 18;

   type Gtk_Widget is new Object.Gtk_Object with null record;
   type Gtk_Widget_Access is access all Gtk_Widget'Class;

   function Get_Window (Widget : in Gtk_Widget)
                        return Gdk.Window.Gdk_Window;

   procedure Activate (Widget : in out Gtk_Widget);

   procedure Destroy (Widget : in out Gtk_Widget);

   procedure Destroyed (Dummy  : in out Gtk_Widget;
                        Widget : in out Gtk_Widget_Access);
   --  Destroyed sets Widget to NULL

   procedure Draw
     (Widget : in Gtk_Widget;
      Area   : in Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area);

   procedure Set_Name (Widget : in out Gtk_Widget; Name : in String);

   procedure Set_Sensitive (Widget    : in out Gtk_Widget;
                            Sensitive : in Boolean := True);

   procedure Set_UPosition (Widget : in out Gtk_Widget; X, Y : in Gint);

   procedure Set_USize (Widget : in out Gtk_Widget; Width, Height : in Gint);

   procedure Show (Widget : in out Gtk_Widget);

   procedure Show_All (Widget : in out Gtk_Widget);

   procedure Hide (Widget : in out Gtk_Widget);

   procedure Map (Widget : in out Gtk_Widget);

   procedure Unmap (Widget : in out Gtk_Widget);

   procedure Realize (Widget : in out Gtk_Widget);

   procedure Unrealize (Widget : in out Gtk_Widget);

   procedure Reparent (Widget : in out Gtk_Widget;
                       New_Parent : in Gtk_Widget'Class);

   function Get_Parent (Widget : in Gtk_Widget) return Gtk_Widget'Class;

   procedure Popup (Widget : in out Gtk_Widget; X, Y : in Gint);

   procedure Grab_Default (Widget : in out Gtk_Widget);

   procedure Grab_Focus (Widget : in out Gtk_Widget);

   procedure Set_Parent (Widget : in out Gtk_Widget;
                         Parent : in Gtk_Widget'Class);

   function Get_Toplevel (Widget : in Gtk_Widget) return Gtk_Widget'Class;

   function Get_Events (Widget : in Gtk_Widget) return Gint;

   procedure Set_Events (Widget : in out Gtk_Widget;
                         Events : in     Gdk.Types.Gdk_Event_Mask);

   procedure Set_State (Widget : in out Gtk_Widget;
                        State : in Enums.Gtk_State_Type);


   --  (See also gtk-style for functions dealing with styles)

   --  The following functions deal with Visuals

   function Get_Visual (Widget : Gtk_Widget) return Gdk_Visual;
   function Get_Default_Visual return Gdk_Visual;
   procedure Set_Default_Visual (Widget : Gtk_Widget; Visual : Gdk_Visual);

   --  The following functions deal with Colormaps

   function Get_Colormap (Widget : Gtk_Widget) return Gdk_Colormap;
   function Get_Default_Colormap return Gdk_Colormap;
   procedure Set_Default_Colormap (Widget : Gtk_Widget; Cmap : Gdk_Colormap);

   --  The following four functions get the size and position of the widget

   function Get_Allocation_Width (Widget : in Gtk_Widget) return Guint;
   function Get_Allocation_Height (Widget : in Gtk_Widget) return Guint;
   function Get_Allocation_X (Widget : in Gtk_Widget) return Gint;
   function Get_Allocation_Y (Widget : in Gtk_Widget) return Gint;

   -------------
   --  Events --
   -------------

   procedure Event (Widget : Gtk.Widget.Gtk_Widget'Class;
                    Event  : Gdk.Event.Gdk_Event);

   --------------------
   --  Widget flags  --
   --------------------

   function Toplevel_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function No_Window_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Realized_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Mapped_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Visible_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Drawable_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Sensitive_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Parent_Sensitive_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Is_Sensitive_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Can_Focus_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Has_Focus_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Has_Default_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Has_Grab_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Basic_Is_Set (Widget : Gtk_Widget'Class) return Boolean;
   function Rc_Style_Is_Set (Widget : Gtk_Widget'Class) return Boolean;

   -----------------------
   --  Default callbacks
   --  These methods are available for use with Gtk.Signal.C_Unsafe_Connect
   -----------------------

   function Get_Default_Motion_Notify_Event (Widget : in Gtk_Widget)
                                             return System.Address;

   ------------------------
   --  Definitions for lists of widgets
   ------------------------

   function Convert (W : Gtk.Widget.Gtk_Widget'Class) return System.Address;
   function Convert (W : System.Address) return Gtk.Widget.Gtk_Widget'Class;
   package Widget_List is new Glib.Glist.Generic_List
     (Gtk.Widget.Gtk_Widget'Class);
   package Widget_SList is new Glib.GSlist.Generic_SList
     (Gtk.Widget.Gtk_Widget'Class);

end Gtk.Widget;
