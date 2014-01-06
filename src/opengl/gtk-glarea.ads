------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2014, AdaCore                     --
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
--
--  This widget is derived from Gtk_Drawing_Area and provides an area where
--  it is possible to use the openGL API.
--
--  </description>
--  <c_version>gtkglarea 1.2.2</c_version>
--  <group>Drawing</group>

with Gtk.Drawing_Area;
with Gdk.GL; use Gdk.GL;

package Gtk.GLArea is

   type Gtk_GLArea_Record is
     new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type Gtk_GLArea is access all Gtk_GLArea_Record'Class;

   type Attributes_Array is array (Natural range <>) of GL_Configs;
   --  Note: as opposed to what exists in C, you don't need to have
   --  the last element in the array be GDK_GL_NONE. This is done
   --  transparently by GtkAda itself.

   procedure Gtk_New (Widget    : out Gtk_GLArea;
                      Attr_List : Attributes_Array);
   --  Make an OpenGL widget, Attr_List is passed to glXChooseVisual GLX call.
   --  Attr_List specifies a list of Boolean attributes and enum/integer
   --  attribute/value pairs.
   --  See glXChooseVisual man page for more explanation on Attr_List.
   --  Widget is created with visual and colormap of the
   --  requested type and GLX context is created for this widget. You
   --  can't do opengl calls on widget until it has X window. X window
   --  is not created until widget is realized.

   procedure Initialize (Widget    : access Gtk_GLArea_Record;
                         Attr_List : Attributes_Array);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gtk_New
     (Widget    : out Gtk_GLArea;
      Attr_List : Attributes_Array;
      Share     : access Gtk_GLArea_Record'Class);
   --  Same as above.
   --  Share specifies the widget with which to share display lists and
   --  texture objects. A non initialized value indicates that no sharing is
   --  to take place.

   procedure Initialize
     (Widget    : access Gtk_GLArea_Record;
      Attr_List : Attributes_Array;
      Share     : access Gtk_GLArea_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_GLArea.

   function Make_Current
     (Glarea : access Gtk_GLArea_Record'Class)
     return Boolean;
   --  Must be called before rendering into OpenGL widgets.
   --  Return True if rendering to widget is possible. Rendering is not
   --  possible if widget is not Gtk_GLArea widget or widget is not realized.

   procedure Swap_Buffers (Glarea : access Gtk_GLArea_Record'Class);
   --  Promote contents of back buffer of Glarea to front buffer.
   --  The contents of front buffer become undefined.

private
   type Gtk_GLArea_Record is
     new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with null record;

   pragma Import (C, Get_Type, "gtk_gl_area_get_type");
end Gtk.GLArea;
