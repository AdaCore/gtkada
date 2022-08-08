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
--  The Gtk.Aspect_Frame.Gtk_Aspect_Frame is useful when you want pack a
--  widget so that it can resize but always retains the same aspect ratio. For
--  instance, one might be drawing a small preview of a larger image.
--  Gtk.Aspect_Frame.Gtk_Aspect_Frame derives from Gtk.Frame.Gtk_Frame, so it
--  can draw a label and a frame around the child. The frame will be
--  "shrink-wrapped" to the size of the child.
--
--  # CSS nodes
--
--  GtkAspectFrame uses a CSS node with name frame.
--
--  </description>
--  <group>Layout Containers</group>
--  <testgtk>create_frame.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Frame;       use Gtk.Frame;

package Gtk.Aspect_Frame is

   type Gtk_Aspect_Frame_Record is new Gtk_Frame_Record with null record;
   type Gtk_Aspect_Frame is access all Gtk_Aspect_Frame_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Aspect_Frame : out Gtk_Aspect_Frame;
       Label        : UTF8_String := "";
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean);
   procedure Initialize
      (Aspect_Frame : not null access Gtk_Aspect_Frame_Record'Class;
       Label        : UTF8_String := "";
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean);
   --  Create a new Gtk.Aspect_Frame.Gtk_Aspect_Frame.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "label": Label text.
   --  "xalign": Horizontal alignment of the child within the allocation of
   --  the Gtk.Aspect_Frame.Gtk_Aspect_Frame. This ranges from 0.0 (left
   --  aligned) to 1.0 (right aligned)
   --  "yalign": Vertical alignment of the child within the allocation of the
   --  Gtk.Aspect_Frame.Gtk_Aspect_Frame. This ranges from 0.0 (top aligned) to
   --  1.0 (bottom aligned)
   --  "ratio": The desired aspect ratio.
   --  "obey_child": If True, Ratio is ignored, and the aspect ratio is taken
   --  from the requistion of the child.

   function Gtk_Aspect_Frame_New
      (Label      : UTF8_String := "";
       Xalign     : Gfloat;
       Yalign     : Gfloat;
       Ratio      : Gfloat;
       Obey_Child : Boolean) return Gtk_Aspect_Frame;
   --  Create a new Gtk.Aspect_Frame.Gtk_Aspect_Frame.
   --  "label": Label text.
   --  "xalign": Horizontal alignment of the child within the allocation of
   --  the Gtk.Aspect_Frame.Gtk_Aspect_Frame. This ranges from 0.0 (left
   --  aligned) to 1.0 (right aligned)
   --  "yalign": Vertical alignment of the child within the allocation of the
   --  Gtk.Aspect_Frame.Gtk_Aspect_Frame. This ranges from 0.0 (top aligned) to
   --  1.0 (bottom aligned)
   --  "ratio": The desired aspect ratio.
   --  "obey_child": If True, Ratio is ignored, and the aspect ratio is taken
   --  from the requistion of the child.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_aspect_frame_get_type");

   -------------
   -- Methods --
   -------------

   procedure Set
      (Aspect_Frame : not null access Gtk_Aspect_Frame_Record;
       Xalign       : Gfloat;
       Yalign       : Gfloat;
       Ratio        : Gfloat;
       Obey_Child   : Boolean);
   --  Set parameters for an existing Gtk.Aspect_Frame.Gtk_Aspect_Frame.
   --  "xalign": Horizontal alignment of the child within the allocation of
   --  the Gtk.Aspect_Frame.Gtk_Aspect_Frame. This ranges from 0.0 (left
   --  aligned) to 1.0 (right aligned)
   --  "yalign": Vertical alignment of the child within the allocation of the
   --  Gtk.Aspect_Frame.Gtk_Aspect_Frame. This ranges from 0.0 (top aligned) to
   --  1.0 (bottom aligned)
   --  "ratio": The desired aspect ratio.
   --  "obey_child": If True, Ratio is ignored, and the aspect ratio is taken
   --  from the requistion of the child.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Obey_Child_Property : constant Glib.Properties.Property_Boolean;

   Ratio_Property : constant Glib.Properties.Property_Float;

   Xalign_Property : constant Glib.Properties.Property_Float;

   Yalign_Property : constant Glib.Properties.Property_Float;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Aspect_Frame_Record, Gtk_Aspect_Frame);
   function "+"
     (Widget : access Gtk_Aspect_Frame_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Aspect_Frame
   renames Implements_Gtk_Buildable.To_Object;

private
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Ratio_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("ratio");
   Obey_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("obey-child");
end Gtk.Aspect_Frame;
