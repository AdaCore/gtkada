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

package body Gtk.Alignment is

   ----------------
   -- Get_Xalign --
   ----------------

   function Get_Xalign (Widget : access Gtk_Alignment_Record) return Gfloat is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_alignment_get_xalign");
   begin
      return Internal (Get_Object (Widget));
   end Get_Xalign;

   ----------------
   -- Get_Xscale --
   ----------------

   function Get_Xscale (Widget : access Gtk_Alignment_Record) return Gfloat is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_alignment_get_xscale");
   begin
      return Internal (Get_Object (Widget));
   end Get_Xscale;

   ----------------
   -- Get_Yalign --
   ----------------

   function Get_Yalign (Widget : access Gtk_Alignment_Record) return Gfloat is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_alignment_get_yalign");
   begin
      return Internal (Get_Object (Widget));
   end Get_Yalign;

   ----------------
   -- Get_Yscale --
   ----------------

   function Get_Yscale (Widget : access Gtk_Alignment_Record) return Gfloat is
      function Internal (Widget : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_alignment_get_yscale");
   begin
      return Internal (Get_Object (Widget));
   end Get_Yscale;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget : out Gtk_Alignment;
       Xalign : in Gfloat;
       Yalign : in Gfloat;
       Xscale : in Gfloat;
       Yscale : in Gfloat)
   is
   begin
      Widget := new Gtk_Alignment_Record;
      Initialize (Widget, Xalign, Yalign, Xscale, Yscale);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Widget : access Gtk_Alignment_Record;
       Xalign : in Gfloat;
       Yalign : in Gfloat;
       Xscale : in Gfloat;
       Yscale : in Gfloat)
   is
      function Internal
         (Xalign : in Gfloat;
          Yalign : in Gfloat;
          Xscale : in Gfloat;
          Yscale : in Gfloat)
          return      System.Address;
      pragma Import (C, Internal, "gtk_alignment_new");
   begin
      Set_Object (Widget, Internal (Xalign, Yalign, Xscale, Yscale));
      Initialize_User_Data (Widget);
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set
      (Alignment : access Gtk_Alignment_Record;
       Xalign    : in Gfloat;
       Yalign    : in Gfloat;
       Xscale    : in Gfloat;
       Yscale    : in Gfloat)
   is
      procedure Internal
         (Alignment : in System.Address;
          Xalign    : in Gfloat;
          Yalign    : in Gfloat;
          Xscale    : in Gfloat;
          Yscale    : in Gfloat);
      pragma Import (C, Internal, "gtk_alignment_set");
   begin
      Internal (Get_Object (Alignment), Xalign, Yalign, Xscale, Yscale);
   end Set;

end Gtk.Alignment;
