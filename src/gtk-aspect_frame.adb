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

with System;
with Gdk; use Gdk;

package body Gtk.Aspect_Frame is

   ---------------
   -- Get_Ratio --
   ---------------

   function Get_Ratio (Widget : in Gtk_Aspect_Frame) return Gfloat is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_ratio");
   begin
      return Internal (Get_Object (Widget));
   end Get_Ratio;

   ----------------
   -- Get_Xalign --
   ----------------

   function Get_Xalign (Widget : in Gtk_Aspect_Frame) return Gfloat is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_xalign");
   begin
      return Internal (Get_Object (Widget));
   end Get_Xalign;

   ----------------
   -- Get_Yalign --
   ----------------

   function Get_Yalign (Widget : in Gtk_Aspect_Frame) return Gfloat is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_yalign");
   begin
      return Internal (Get_Object (Widget));
   end Get_Yalign;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget     : out Gtk_Aspect_Frame;
       Label      : in String;
       Xalign     : in Gfloat;
       Yalign     : in Gfloat;
       Ratio      : in Gfloat;
       Obey_Child : in Gint)
   is
      function Internal
         (Label      : in String;
          Xalign     : in Gfloat;
          Yalign     : in Gfloat;
          Ratio      : in Gfloat;
          Obey_Child : in Gint)
          return          System.Address;
      pragma Import (C, Internal, "gtk_aspect_frame_new");
   begin
      Set_Object (Widget, Internal (Label & Ascii.NUL,
                                    Xalign,
                                    Yalign,
                                    Ratio,
                                    Obey_Child));
   end Gtk_New;

   ---------
   -- Set --
   ---------

   procedure Set
      (Aspect_Frame : in Gtk_Aspect_Frame;
       Xalign       : in Gfloat;
       Yalign       : in Gfloat;
       Ratio        : in Gfloat;
       Obey_Child   : in Gint)
   is
      procedure Internal
         (Aspect_Frame : in System.Address;
          Xalign       : in Gfloat;
          Yalign       : in Gfloat;
          Ratio        : in Gfloat;
          Obey_Child   : in Gint);
      pragma Import (C, Internal, "gtk_aspect_frame_set");
   begin
      Internal (Get_Object (Aspect_Frame),
                Xalign,
                Yalign,
                Ratio,
                Obey_Child);
   end Set;

end Gtk.Aspect_Frame;
