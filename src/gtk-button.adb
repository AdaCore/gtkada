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
with Gtk.Util; use Gtk.Util;
with Gtk.Container; use Gtk.Container;
with Gtk.File_Selection; use Gtk.File_Selection;

package body Gtk.Button is

   -------------
   -- Clicked --
   -------------

   procedure Clicked (Button : in Gtk_Button) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_clicked");
   begin
      Internal (Get_Object (Button));
   end Clicked;

   -----------
   -- Enter --
   -----------

   procedure Enter (Button : in Gtk_Button) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_enter");
   begin
      Internal (Get_Object (Button));
   end Enter;

   ----------------
   -- Get_Relief --
   ----------------

   function Get_Relief (Button : in Gtk_Button)
                        return Gtk.Enums.Gtk_Relief_Style is
      function Internal (Button : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_button_get_relief");
   begin
      return Gtk.Enums.Gtk_Relief_Style'Val (Internal (Get_Object (Button)));
   end Get_Relief;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Button : out Gtk_Button) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_button_new");
   begin
      Set_Object (Button, Internal);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Button : out Gtk_Button; Label  : in String) is
      function Internal (S : String) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_with_label");
   begin
      Set_Object (Button, Internal (Label & Ascii.NUL));
   end Gtk_New;

   -----------
   -- Leave --
   -----------

   procedure Leave (Button : in Gtk_Button) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_enter");
   begin
      Internal (Get_Object (Button));
   end Leave;

   -------------
   -- Pressed --
   -------------

   procedure Pressed (Button : in Gtk_Button) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_pressed");
   begin
      Internal (Get_Object (Button));
   end Pressed;

   --------------
   -- Released --
   --------------

   procedure Released (Button : in Gtk_Button) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_released");
   begin
      Internal (Get_Object (Button));
   end Released;

   ----------------
   -- Set_Relief --
   ----------------

   procedure Set_Relief (Button   : in out Gtk_Button;
                         NewStyle : in Gtk.Enums.Gtk_Relief_Style) is
      procedure Internal (Button : in System.Address;
                          NewStyle : in Gint);
      pragma Import (C, Internal, "gtk_button_set_relief");
   begin
      Internal (Get_Object (Button),
                Gtk.Enums.Gtk_Relief_Style'Pos (NewStyle));
   end Set_Relief;

   --------------
   -- Generate --
   --------------

   procedure Generate (Button : in Gtk_Button;
                       N      : in Node_Ptr;
                       File   : in File_Type) is
      use Container;

      Child_Name : Node_Ptr   := Find_Tag (N.Child, "child_name");
      Label      : String_Ptr := Get_Field (N, "label");

   begin
      if Child_Name = null then
         if Label = null then
            Gen_New (N, "Button", File => File);
         else
            Gen_New (N, "Button", Label.all, File => File, Delim => '"');
         end if;
      else
         Gen_Child (N, Child_Name, File);
      end if;

      Generate (Gtk_Container (Button), N, File);
      Gen_Set (N, "Button", "relief", File);

      if Child_Name = null then
         Gen_Call_Child (N, null, "Container", "Add", File => File);
      end if;
   end Generate;

   procedure Generate (Button : in out Gtk_Button;
                       N      : in Node_Ptr) is
      use Container;

      Child_Name     : String_Ptr := Get_Field (N, "child_name");
      S              : String_Ptr := Get_Field (N, "label");
      File_Selection : Gtk_File_Selection;

   begin
      if not N.Specific_Data.Created then
         if Child_Name = null then
            if S = null then
               Gtk_New (Button);
            else
               Gtk_New (Button, S.all);
            end if;
         else
            File_Selection :=
              Gtk_File_Selection (Get_Object (Find_Tag
                (Find_Parent (N.Parent, Get_Part (Child_Name.all, 1)),
                 "name").Value).all);

            if Get_Part (Child_Name.all, 2) = "ok_button" then
               Button := Get_Ok_Button (File_Selection);
            elsif Get_Part (Child_Name.all, 2) = "cancel_button" then
               Button := Get_Cancel_Button (File_Selection);
            elsif Get_Part (Child_Name.all, 2) = "help_button" then
               Button := Get_Help_Button (File_Selection);
            end if;
         end if;

         Set_Object (Get_Field (N, "name"), Button'Unchecked_Access);
         N.Specific_Data.Created := True;
      end if;

      Generate (Gtk_Container (Button), N);

      S := Get_Field (N, "relief");

      if S /= null then
         Set_Relief (Button, Gtk.Enums.Gtk_Relief_Style'Value (S.all));
      end if;

      if Child_Name = null then
         Container.Add (Gtk_Container (Get_Object
           (Get_Field (N.Parent, "name")).all), Button);
      end if;
   end Generate;

end Gtk.Button;
