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

with System;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtk.Frame is

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Frame : access Gtk_Frame_Record) return String is
      function Internal (Frame : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_frame_get_label");

   begin
      return Value (Internal (Get_Object (Frame)));
   end Get_Label;

   ---------------------
   -- Get_Label_Align --
   ---------------------

   procedure Get_Label_Align
     (Frame  : access Gtk_Frame_Record;
      Xalign : out Gfloat;
      Yalign : out Gfloat)
   is
      procedure Internal
        (Frame  : System.Address;
         Xalign : out Gfloat;
         Yalign : out Gfloat);
      pragma Import (C, Internal, "gtk_frame_get_label_align");

   begin
      Internal (Get_Object (Frame), Xalign, Yalign);
   end Get_Label_Align;

   ---------------------
   -- Get_Shadow_Type --
   ---------------------

   function Get_Shadow_Type
     (Frame : access Gtk_Frame_Record) return Gtk_Shadow_Type
   is
      function Internal (Frame : System.Address) return Gtk_Shadow_Type;
      pragma Import (C, Internal, "gtk_frame_get_shadow_type");

   begin
      return Internal (Get_Object (Frame));
   end Get_Shadow_Type;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Frame : out Gtk_Frame; Label : String := "") is
   begin
      Frame := new Gtk_Frame_Record;
      Initialize (Frame, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Frame : access Gtk_Frame_Record'Class; Label : String := "")
   is
      function Internal (Label : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_frame_new");

   begin
      if Label = "" then
         Set_Object (Frame, Internal (System.Null_Address));
      else
         declare
            S : aliased constant String := Label & ASCII.NUL;
         begin
            Set_Object (Frame, Internal (S'Address));
         end;
      end if;
   end Initialize;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (Frame : access Gtk_Frame_Record;
      Label : String := "")
   is
      procedure Internal (Frame : System.Address; Label : System.Address);
      pragma Import (C, Internal, "gtk_frame_set_label");

      S : aliased constant String := Label & ASCII.NUL;

   begin
      if Label = "" then
         Internal (Get_Object (Frame), System.Null_Address);
      else
         Internal (Get_Object (Frame), S'Address);
      end if;
   end Set_Label;

   ---------------------
   -- Set_Label_Align --
   ---------------------

   procedure Set_Label_Align
     (Frame  : access Gtk_Frame_Record;
      Xalign : Gfloat := 0.0;
      Yalign : Gfloat := 0.0)
   is
      procedure Internal
        (Frame : System.Address; Xalign : Gfloat; Yalign : Gfloat);
      pragma Import (C, Internal, "gtk_frame_set_label_align");

   begin
      Internal (Get_Object (Frame), Xalign, Yalign);
   end Set_Label_Align;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
     (Frame    : access Gtk_Frame_Record;
      The_Type : Gtk_Shadow_Type)
   is
      procedure Internal (Frame : System.Address; The_Type : Gtk_Shadow_Type);
      pragma Import (C, Internal, "gtk_frame_set_shadow_type");

   begin
      Internal (Get_Object (Frame), The_Type);
   end Set_Shadow_Type;

   ----------------------
   -- Set_Widget_Label --
   ----------------------

   procedure Set_Label_Widget
     (Frame        : access Gtk_Frame_Record;
      Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Frame, Widget : System.Address);
      pragma Import (C, Internal, "gtk_frame_set_label_widget");

   begin
      Internal (Get_Object (Frame), Get_Object (Label_Widget));
   end Set_Label_Widget;

end Gtk.Frame;
