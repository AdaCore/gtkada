-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

--  <description>
--
--  A Gtk_Aspect_Frame is the same type of widget as a frame, but it
--  constrains its child to a specific aspect ratio between its width
--  and its height.
--
--  This ratio can either be given explicitly by the user, or chosen from the
--  widget's initial size request (might be different from the one if was
--  actually given).
--
--  </description>
--  <c_version>1.3.11</c_version>

with Glib.Properties;
with Gtk.Frame;

package Gtk.Aspect_Frame is

   type Gtk_Aspect_Frame_Record is new Gtk.Frame.Gtk_Frame_Record with private;
   type Gtk_Aspect_Frame is access all Gtk_Aspect_Frame_Record'Class;

   procedure Gtk_New
     (Aspect_Frame : out Gtk_Aspect_Frame;
      Label        : String;
      Xalign       : Gfloat;
      Yalign       : Gfloat;
      Ratio        : Gfloat;
      Obey_Child   : Boolean);
   --  Create a new Aspect_Frame.
   --  If Label is the empty string, then the frame won't have any title.
   --  Xalign and Yalign are constrained to the range 0.0 .. 1.0 and specify
   --  the alignment of the child inside the frame (0.0 means either left or
   --  top aligned, 1.0 means right or bottom aligned).
   --  Ratio is the ratio width/height for the child of the frame.
   --  If Obey_Child is True, then Ratio is ignored and the effective ratio
   --  is taken from the child's requisition (ie the ideal size it asked
   --  for at creation time).

   procedure Initialize
     (Aspect_Frame : access Gtk_Aspect_Frame_Record'Class;
      Label        : String;
      Xalign       : Gfloat;
      Yalign       : Gfloat;
      Ratio        : Gfloat;
      Obey_Child   : Boolean);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Aspect_Frame.

   procedure Set
     (Aspect_Frame : access Gtk_Aspect_Frame_Record;
      Xalign       : Gfloat;
      Yalign       : Gfloat;
      Ratio        : Gfloat;
      Obey_Child   : Boolean);
   --  Modify the frame's parameters (see the description of these parameters
   --  for Gtk_New.

   function Get_Ratio
     (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat;
   --  Return the current ratio for the frame (width / height)

   function Get_Xalign
     (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat;
   --  Return the current X alignment for the frame.
   --  0.0 means the child is left aligned, 1.0 that it is right aligned.

   function Get_Yalign
     (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat;
   --  Return the current Y alignment for the frame.
   --  1.0 means the child is top aligned, 1.0 that it is bottom aligned.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Xalign_Property
   --    Type:  Gfloat
   --    Flags: read-write
   --    Descr: X alignment of the child
   --    See also: Set and Get_Xalign
   --
   --  - Name:  Yalign_Property
   --    Type:  Gfloat
   --    Flags: read-write
   --    Descr: Y alignment of the child
   --    See also: Set and Get_Xalign
   --
   --  - Name:  Ratio_Property
   --    Type:  Gfloat
   --    Flags: read-write
   --    Descr: Aspect ratio if obey_child is FALSE
   --    See also: Set and Get_Ratio
   --
   --  - Name:  Obey_Child_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Force aspect ratio to match that of the frame's child
   --    See also: Set and Get_Ratio
   --
   --  </properties>

   Xalign_Property     : constant Glib.Properties.Property_Float;
   Yalign_Property     : constant Glib.Properties.Property_Float;
   Radio_Property      : constant Glib.Properties.Property_Float;
   Obey_Child_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Aspect_Frame_Record is new Gtk.Frame.Gtk_Frame_Record
     with null record;

   Xalign_Property     : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Yalign_Property     : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Radio_Property      : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("ratio");
   Obey_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("obey_child");

   pragma Import (C, Get_Type, "gtk_aspect_frame_get_type");
end Gtk.Aspect_Frame;
