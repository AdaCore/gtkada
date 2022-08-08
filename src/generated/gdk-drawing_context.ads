------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  Gdk.Drawing_Context.Gdk_Drawing_Context is an object that represents the
--  current drawing state of a Gdk.Gdk_Window.
--
--  It's possible to use a Gdk.Drawing_Context.Gdk_Drawing_Context to draw on
--  a Gdk.Gdk_Window via rendering API like Cairo or OpenGL.
--
--  A Gdk.Drawing_Context.Gdk_Drawing_Context can only be created by calling
--  Gdk.Window.Begin_Draw_Frame and will be valid until a call to
--  Gdk.Window.End_Draw_Frame.
--
--  Gdk.Drawing_Context.Gdk_Drawing_Context is available since GDK 3.22
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;           use Cairo;
with Cairo.Region;    use Cairo.Region;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gdk.Drawing_Context is

   type Gdk_Drawing_Context_Record is new GObject_Record with null record;
   type Gdk_Drawing_Context is access all Gdk_Drawing_Context_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_drawing_context_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Cairo_Context
      (Self : not null access Gdk_Drawing_Context_Record)
       return Cairo.Cairo_Context;
   --  Retrieves a Cairo context to be used to draw on the Gdk.Gdk_Window that
   --  created the Gdk.Drawing_Context.Gdk_Drawing_Context.
   --  The returned context is guaranteed to be valid as long as the
   --  Gdk.Drawing_Context.Gdk_Drawing_Context is valid, that is between a call
   --  to Gdk.Window.Begin_Draw_Frame and Gdk.Window.End_Draw_Frame.
   --  Since: gtk+ 3.22

   function Get_Clip
      (Self : not null access Gdk_Drawing_Context_Record)
       return Cairo.Region.Cairo_Region;
   --  Retrieves a copy of the clip region used when creating the Context.
   --  Since: gtk+ 3.22

   function Get_Window
      (Self : not null access Gdk_Drawing_Context_Record)
       return Gdk.Gdk_Window;
   --  Retrieves the window that created the drawing Context.
   --  Since: gtk+ 3.22

   function Is_Valid
      (Self : not null access Gdk_Drawing_Context_Record) return Boolean;
   --  Checks whether the given Gdk.Drawing_Context.Gdk_Drawing_Context is
   --  valid.
   --  Since: gtk+ 3.22

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Clip_Property : constant Glib.Properties.Property_Boxed;
   --  Type: cairo.Region
   --  The clip region applied to the drawing context.

   Window_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Window.Gtk_Window
   --  The Gdk.Gdk_Window that created the drawing context.

private
   Window_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("window");
   Clip_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("clip");
end Gdk.Drawing_Context;
