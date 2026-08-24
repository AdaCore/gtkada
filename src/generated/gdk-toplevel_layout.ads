------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Contains information that is necessary to present a sovereign window on
--  screen.
--
--  The `GdkToplevelLayout` struct is necessary for using
--  [methodGdk.Toplevel.present].
--
--  Toplevel surfaces are sovereign windows that can be presented to the user
--  in various states (maximized, on all workspaces, etc).

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Monitor; use Gdk.Monitor;
with Glib;        use Glib;
with System;

package Gdk.Toplevel_Layout is

   type Gdk_Toplevel_Layout is new Glib.C_Boxed with null record;
   Null_Gdk_Toplevel_Layout : constant Gdk_Toplevel_Layout;

   function From_Object (Object : System.Address) return Gdk_Toplevel_Layout;
   function From_Object_Free (B : access Gdk_Toplevel_Layout'Class) return Gdk_Toplevel_Layout;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New (Self : out Gdk_Toplevel_Layout);
   --  Create a toplevel layout description.
   --  Used together with [methodGdk.Toplevel.present] to describe how a
   --  toplevel surface should be placed and behave on-screen.
   --  The size is in "application pixels", not "device pixels" (see
   --  [methodGdk.Surface.get_scale]).

   function Gdk_Toplevel_Layout_New return Gdk_Toplevel_Layout;
   --  Create a toplevel layout description.
   --  Used together with [methodGdk.Toplevel.present] to describe how a
   --  toplevel surface should be placed and behave on-screen.
   --  The size is in "application pixels", not "device pixels" (see
   --  [methodGdk.Surface.get_scale]).

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_toplevel_layout_get_type");

   -------------
   -- Methods --
   -------------

   function Copy (Self : Gdk_Toplevel_Layout) return Gdk_Toplevel_Layout;
   --  Create a new `GdkToplevelLayout` and copy the contents of Layout into
   --  it.
   --  @return a copy of Layout.

   function Equal
      (Self  : Gdk_Toplevel_Layout;
       Other : Gdk_Toplevel_Layout) return Boolean;
   --  Check whether Layout and Other has identical layout properties.
   --  @param Other another toplevel layout
   --  @return true if Layout and Other have identical layout properties,
   --  otherwise false.

   function Get_Fullscreen
      (Self       : Gdk_Toplevel_Layout;
       Fullscreen : out Boolean) return Boolean;
   --  If the layout specifies whether to the toplevel should go fullscreen,
   --  the value pointed to by Fullscreen is set to true if it should go
   --  fullscreen, or false, if it should go unfullscreen.
   --  @param Fullscreen location to store whether the toplevel should be
   --  fullscreen
   --  @return whether the Layout specifies the fullscreen state for the
   --  toplevel

   procedure Set_Fullscreen
      (Self       : Gdk_Toplevel_Layout;
       Fullscreen : Boolean;
       Monitor    : access Gdk.Monitor.Gdk_Monitor_Record'Class);
   --  Sets whether the layout should cause the surface to be fullscreen when
   --  presented.
   --  @param Fullscreen true to fullscreen the surface
   --  @param Monitor the monitor to fullscreen on

   function Get_Fullscreen_Monitor
      (Self : Gdk_Toplevel_Layout) return Gdk.Monitor.Gdk_Monitor;
   --  Returns the monitor that the layout is fullscreening the surface on.
   --  @return the monitor on which Layout fullscreens
   --  Return has transfer-ownership='none'

   function Get_Maximized
      (Self      : Gdk_Toplevel_Layout;
       Maximized : out Boolean) return Boolean;
   --  If the layout specifies whether to the toplevel should go maximized,
   --  the value pointed to by Maximized is set to true if it should go
   --  maximized, or false, if it should go unmaximized.
   --  @param Maximized set to true if the toplevel should be maximized
   --  @return whether the Layout specifies the maximized state for the
   --  toplevel

   procedure Set_Maximized (Self : Gdk_Toplevel_Layout; Maximized : Boolean);
   --  Sets whether the layout should cause the surface to be maximized when
   --  presented.
   --  @param Maximized true to maximize

   function Get_Resizable (Self : Gdk_Toplevel_Layout) return Boolean;
   --  Returns whether the layout should allow the user to resize the surface.
   --  @return true if the layout is resizable

   procedure Set_Resizable (Self : Gdk_Toplevel_Layout; Resizable : Boolean);
   --  Sets whether the layout should allow the user to resize the surface
   --  after it has been presented.
   --  @param Resizable true to allow resizing

   function Ref (Self : Gdk_Toplevel_Layout) return Gdk_Toplevel_Layout;
   --  Increases the reference count of Layout.
   --  @return the same Layout

   procedure Unref (Self : Gdk_Toplevel_Layout);
   --  Decreases the reference count of Layout.

private
   Null_Gdk_Toplevel_Layout : constant Gdk_Toplevel_Layout :=
      (Glib.C_Boxed with null record);

end Gdk.Toplevel_Layout;
