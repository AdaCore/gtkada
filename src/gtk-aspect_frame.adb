-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

package body Gtk.Aspect_Frame is

   ---------------
   -- Get_Ratio --
   ---------------

   function Get_Ratio
     (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat
   is
      function Internal (Widget : System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_ratio");

   begin
      return Internal (Get_Object (Aspect_Frame));
   end Get_Ratio;

   ----------------
   -- Get_Xalign --
   ----------------

   function Get_Xalign
     (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat
   is
      function Internal (Widget : System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_xalign");

   begin
      return Internal (Get_Object (Aspect_Frame));
   end Get_Xalign;

   ----------------
   -- Get_Yalign --
   ----------------

   function Get_Yalign
     (Aspect_Frame : access Gtk_Aspect_Frame_Record) return Gfloat
   is
      function Internal (Widget : System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_yalign");

   begin
      return Internal (Get_Object (Aspect_Frame));
   end Get_Yalign;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Aspect_Frame : out Gtk_Aspect_Frame;
      Label        : String;
      Xalign       : Gfloat;
      Yalign       : Gfloat;
      Ratio        : Gfloat;
      Obey_Child   : Boolean) is
   begin
      Aspect_Frame := new Gtk_Aspect_Frame_Record;
      Initialize (Aspect_Frame, Label, Xalign, Yalign, Ratio, Obey_Child);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Aspect_Frame : access Gtk_Aspect_Frame_Record'Class;
      Label        : String;
      Xalign       : Gfloat;
      Yalign       : Gfloat;
      Ratio        : Gfloat;
      Obey_Child   : Boolean)
   is
      function Internal
        (Label      : System.Address;
         Xalign     : Gfloat;
         Yalign     : Gfloat;
         Ratio      : Gfloat;
         Obey_Child : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_aspect_frame_new");

      S : aliased constant String := Label & ASCII.NUL;
      Sa : System.Address := System.Null_Address;
   begin
      if Label /= "" then
         Sa := S'Address;
      end if;

      Set_Object
        (Aspect_Frame,
         Internal
           (Sa, Xalign, Yalign, Ratio, Boolean'Pos (Obey_Child)));
      Initialize_User_Data (Aspect_Frame);
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
     (Aspect_Frame : access Gtk_Aspect_Frame_Record;
      Xalign       : Gfloat;
      Yalign       : Gfloat;
      Ratio        : Gfloat;
      Obey_Child   : Boolean)
   is
      procedure Internal
        (Aspect_Frame : System.Address;
         Xalign       : Gfloat;
         Yalign       : Gfloat;
         Ratio        : Gfloat;
         Obey_Child   : Gint);
      pragma Import (C, Internal, "gtk_aspect_frame_set");

   begin
      Internal
        (Get_Object (Aspect_Frame), Xalign, Yalign, Ratio,
         Boolean'Pos (Obey_Child));
   end Set;

end Gtk.Aspect_Frame;
