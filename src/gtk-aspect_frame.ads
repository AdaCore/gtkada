-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------


with Gtk.Frame;

package Gtk.Aspect_Frame is

   type Gtk_Aspect_Frame is new Gtk.Frame.Gtk_Frame with private;

   function Get_Ratio (Widget : in Gtk_Aspect_Frame'Class)
                       return      Gfloat;
   function Get_Xalign (Widget : in Gtk_Aspect_Frame'Class)
                        return      Gfloat;
   function Get_Yalign (Widget : in Gtk_Aspect_Frame'Class)
                        return      Gfloat;
   procedure Gtk_New
      (Widget     : out Gtk_Aspect_Frame;
       Label      : in String;
       Xalign     : in Gfloat;
       Yalign     : in Gfloat;
       Ratio      : in Gfloat;
       Obey_Child : in Gint);
   procedure Set
      (Aspect_Frame : in Gtk_Aspect_Frame'Class;
       Xalign       : in Gfloat;
       Yalign       : in Gfloat;
       Ratio        : in Gfloat;
       Obey_Child   : in Gint);

private
   type Gtk_Aspect_Frame is new Gtk.Frame.Gtk_Frame with null record;

   --  mapping: Get_Ratio gtkaspectframe.h GtkAspectFrame->ratio
   --  mapping: NOT_IMPLEMENTED gtkaspectframe.h gtk_aspect_frame_get_type
   --  mapping: Get_Xalign gtkaspectframe.h GtkAspectFrame->xalign
   --  mapping: Get_Yalign gtkaspectframe.h GtkAspectFrame->yalign
   --  mapping: Gtk_New gtkaspectframe.h gtk_aspect_frame_new
   --  mapping: Set gtkaspectframe.h gtk_aspect_frame_set
end Gtk.Aspect_Frame;
