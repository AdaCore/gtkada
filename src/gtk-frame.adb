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

with System;

package body Gtk.Frame is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Frame : out Gtk_Frame;
      Label : in String := "") is
   begin
      Frame := new Gtk_Frame_Record;
      Initialize (Frame, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Frame : access Gtk_Frame_Record'Class;
      Label : in String := "")
   is
      function Internal (Label  : in System.Address) return System.Address;
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

      Initialize_User_Data (Frame);
   end Initialize;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (Frame : access Gtk_Frame_Record;
      Label : in String := "")
   is
      procedure Internal
        (Frame : in System.Address;
         Label : in System.Address);
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
      Xalign : in Gfloat := 0.0;
      Yalign : in Gfloat := 0.0)
   is
      procedure Internal
        (Frame  : in System.Address;
         Xalign : in Gfloat;
         Yalign : in Gfloat);
      pragma Import (C, Internal, "gtk_frame_set_label_align");

   begin
      Internal (Get_Object (Frame), Xalign, Yalign);
   end Set_Label_Align;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
     (Frame    : access Gtk_Frame_Record;
      The_Type : in Gtk_Shadow_Type)
   is
      procedure Internal
        (Frame    : in System.Address;
         The_Type : in Gint);
      pragma Import (C, Internal, "gtk_frame_set_shadow_type");
   begin
      Internal (Get_Object (Frame), Gtk_Shadow_Type'Pos (The_Type));
   end Set_Shadow_Type;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      S  : String_Ptr;
      Id : constant Gtk_Type := Get_Type;
      pragma Warnings (Off, Id);

   begin
      S := Get_Field (N, "label");

      if S /= null then
         if Gettext_Support (N) then
            Gen_New (N, "Frame", S.all,
              File => File, Prefix => "-""", Postfix => """");
         else
            Gen_New (N, "Frame", S.all,
              File => File, Prefix => """", Postfix => """");
         end if;
      else
         Gen_New (N, "Frame", File => File);
      end if;

      Bin.Generate (N, File);
      Gen_Set
        (N, "Frame", "Label_Align",
         "label_xalign", "label_yalign", "", "", File,
         Is_Float => True);
      Gen_Set (N, "Frame", "shadow_type", File);
   end Generate;

end Gtk.Frame;
