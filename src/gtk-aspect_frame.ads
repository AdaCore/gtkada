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

--  <description>
--
--  A Gtk_Aspect_Frame is the same type of widget as a frame, but it
--  constrains its child to a specific aspect ratio between its width
--  and its height.
--
--  This ratio can either be given explictly by the user, or chosen from the
--  widget's initial size request (might be different from the one if was
--  actually given).
--
--  </description>
--  <c_version>1.2.6</c_version>

with Gtk.Object;
with Gtk.Frame;

package Gtk.Aspect_Frame is

   type Gtk_Aspect_Frame_Record is new Gtk.Frame.Gtk_Frame_Record with private;
   type Gtk_Aspect_Frame is access all Gtk_Aspect_Frame_Record'Class;

   procedure Gtk_New (Aspect_Frame : out Gtk_Aspect_Frame;
                      Label        : in String;
                      Xalign       : in Gfloat;
                      Yalign       : in Gfloat;
                      Ratio        : in Gfloat;
                      Obey_Child   : in Boolean);
   --  Creates a new Aspect_Frame.
   --  If LABEL is the empty string, then the frame won't have any title.
   --  XALIGN and YALIGN are constrained to the range 0.0 .. 1.0 and specify
   --  the alignment of the child inside the frame (0.0 means either left or
   --  top aligned, 1.0 means right or bottom aligned).
   --  RATIO is the ratio width/height for the child of the frame.
   --  If OBEY_CHILD is True, then RATIO is ignored and the effective ratio
   --  is taken from the child's requisition (ie the ideal size it asked
   --  for at creation time).

   procedure Initialize (Aspect_Frame : access Gtk_Aspect_Frame_Record'Class;
                         Label        : in String;
                         Xalign       : in Gfloat;
                         Yalign       : in Gfloat;
                         Ratio        : in Gfloat;
                         Obey_Child   : in Boolean);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Returns the internal value associated with a Gtk_Aspect_Frame
   --  internally.

   procedure Set (Aspect_Frame : access Gtk_Aspect_Frame_Record;
                  Xalign       : in Gfloat;
                  Yalign       : in Gfloat;
                  Ratio        : in Gfloat;
                  Obey_Child   : in Boolean);
   --  Modifies the frame's parameters (see the description of these parameters
   --  for Gtk_New.

   function Get_Ratio (Aspect_Frame : access Gtk_Aspect_Frame_Record)
                      return Gfloat;
   --  Returns the current ratio for the frame (width / height)

   function Get_Xalign (Aspect_Frame : access Gtk_Aspect_Frame_Record)
                       return Gfloat;
   --  Returns the current X alignment for the frame.
   --  0.0 means the child is left aligned, 1.0 that it is right aligned.

   function Get_Yalign (Aspect_Frame : access Gtk_Aspect_Frame_Record)
                       return Gfloat;
   --  Returns the current Y alignment for the frame.
   --  1.0 means the child is top aligned, 1.0 that it is bottom aligned.

   ----------------------------
   -- Support for GATE/DGATE --
   ----------------------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);
   --  Gate internal function

   procedure Generate (Aspect_Frame : in out Gtk.Object.Gtk_Object;
                       N            : in Node_Ptr);
   --  Dgate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Aspect_Frame_Record is new Gtk.Frame.Gtk_Frame_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_aspect_frame_get_type");
end Gtk.Aspect_Frame;
