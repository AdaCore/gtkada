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
with Gtk.Util; use Gtk.Util;
with Gtk.Object; use Gtk.Object;
with Gtk.Container; use Gtk.Container;
with Gtk.File_Selection; use Gtk.File_Selection;
with Gtk.Color_Selection_Dialog; use Gtk.Color_Selection_Dialog;
with Gtk.Font_Selection_Dialog; use Gtk.Font_Selection_Dialog;
with Gtk.Type_Conversion_Hooks;
pragma Elaborate_All (Gtk.Type_Conversion_Hooks);

package body Gtk.Button is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Type_Conversion (Type_Name : String) return Root_Type_Access;
   --  This function is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   -------------
   -- Clicked --
   -------------

   procedure Clicked (Button : access Gtk_Button_Record) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_clicked");
   begin
      Internal (Get_Object (Button));
   end Clicked;

   -----------
   -- Enter --
   -----------

   procedure Enter (Button : access Gtk_Button_Record) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_enter");
   begin
      Internal (Get_Object (Button));
   end Enter;

   ----------------
   -- Get_Relief --
   ----------------

   function Get_Relief (Button : access Gtk_Button_Record)
                        return Gtk.Enums.Gtk_Relief_Style is
      function Internal (Button : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_button_get_relief");
   begin
      return Gtk.Enums.Gtk_Relief_Style'Val (Internal (Get_Object (Button)));
   end Get_Relief;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Button : out Gtk_Button; Label : in String := "") is
   begin
      Button := new Gtk_Button_Record;
      Initialize (Button, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Button : access Gtk_Button_Record'Class;
                         Label : in String)
   is
      function Internal (S : String) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_with_label");
      function Internal2 return System.Address;
      pragma Import (C, Internal2, "gtk_button_new");
   begin
      if Label = "" then
         Set_Object (Button, Internal2);
      else
         Set_Object (Button, Internal (Label & ASCII.Nul));
      end if;
      Initialize_User_Data (Button);
   end Initialize;

   -----------
   -- Leave --
   -----------

   procedure Leave (Button : access Gtk_Button_Record) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_enter");
   begin
      Internal (Get_Object (Button));
   end Leave;

   -------------
   -- Pressed --
   -------------

   procedure Pressed (Button : access Gtk_Button_Record) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_pressed");
   begin
      Internal (Get_Object (Button));
   end Pressed;

   --------------
   -- Released --
   --------------

   procedure Released (Button : access Gtk_Button_Record) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_released");
   begin
      Internal (Get_Object (Button));
   end Released;

   ----------------
   -- Set_Relief --
   ----------------

   procedure Set_Relief (Button   : access Gtk_Button_Record;
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

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type) is
      Child_Name : Node_Ptr   := Find_Tag (N.Child, "child_name");
      Label      : String_Ptr := Get_Field (N, "label");

   begin
      if N.Specific_Data.Initialized then
         return;
      end if;

      if not N.Specific_Data.Created then
         if Child_Name = null then
            if Label = null then
               Gen_New (N, "Button", File => File);
            else
               if Gettext_Support (N) then
                  Gen_New (N, "Button", Label.all, File => File,
                    Prefix => "-""", Postfix => """");
               else
                  Gen_New (N, "Button", Label.all, File => File,
                    Prefix => """", Postfix => """");
               end if;

            end if;
         else
            Gen_Child (N, Child_Name, File);
         end if;
      end if;

      Container.Generate (N, File);
      Gen_Set (N, "Button", "relief", File);
   end Generate;

   procedure Generate (Button : in out Gtk_Object;
                       N      : in Node_Ptr) is
      Child_Name      : String_Ptr := Get_Field (N, "child_name");
      S               : String_Ptr := Get_Field (N, "label");
      File_Selection  : Gtk_File_Selection;
      Color_Selection : Gtk_Color_Selection_Dialog;
      Font_Selection  : Gtk_Font_Selection_Dialog;

   begin
      if N.Specific_Data.Initialized then
         return;
      end if;

      if not N.Specific_Data.Created then
         if Child_Name = null then
            if S = null then
               Gtk_New (Gtk_Button (Button));
            else
               Gtk_New (Gtk_Button (Button), S.all);
            end if;
         else
            declare
               Selection : Gtk_Object :=
                 Get_Object (Find_Tag
                   (Find_Parent (N.Parent, Get_Part (Child_Name.all, 1)),
                    "name").Value);

            begin
               if Get_Part (Child_Name.all, 1) = "FileSel" then
                  File_Selection := Gtk_File_Selection (Selection);

                  if Get_Part (Child_Name.all, 2) = "ok_button" then
                     Button := Gtk_Object (Get_Ok_Button (File_Selection));
                  elsif Get_Part (Child_Name.all, 2) = "cancel_button" then
                     Button := Gtk_Object (Get_Cancel_Button (File_Selection));
                  elsif Get_Part (Child_Name.all, 2) = "help_button" then
                     Button := Gtk_Object (Get_Help_Button (File_Selection));
                  end if;

               elsif Get_Part (Child_Name.all, 1) = "ColorSel" then
                  Color_Selection := Gtk_Color_Selection_Dialog (Selection);

                  if Get_Part (Child_Name.all, 2) = "ok_button" then
                     Button := Gtk_Object (Get_OK_Button (Color_Selection));
                  elsif Get_Part (Child_Name.all, 2) = "cancel_button" then
                     Button :=
                       Gtk_Object (Get_Cancel_Button (Color_Selection));
                  elsif Get_Part (Child_Name.all, 2) = "help_button" then
                     Button := Gtk_Object (Get_Help_Button (Color_Selection));
                  end if;

               elsif Get_Part (Child_Name.all, 1) = "FontSel" then
                  Font_Selection := Gtk_Font_Selection_Dialog (Selection);

                  if Get_Part (Child_Name.all, 2) = "ok_button" then
                     Button := Gtk_Object (Get_OK_Button (Font_Selection));
                  elsif Get_Part (Child_Name.all, 2) = "cancel_button" then
                     Button :=
                       Gtk_Object (Get_Cancel_Button (Font_Selection));
                  elsif Get_Part (Child_Name.all, 2) = "apply_button" then
                     Button := Gtk_Object (Get_Apply_Button (Font_Selection));
                  end if;
               end if;
            end;
         end if;

         Set_Object (Get_Field (N, "name"), Button);
         N.Specific_Data.Created := True;
      end if;

      Container.Generate (Button, N);

      S := Get_Field (N, "relief");

      if S /= null then
         Set_Relief
           (Gtk_Button (Button),
            Gtk.Enums.Gtk_Relief_Style'Value (S (S'First + 4 .. S'Last)));
      end if;
   end Generate;

   ---------------------
   -- Type_Conversion --
   ---------------------

   function Type_Conversion (Type_Name : String) return Root_Type_Access is
   begin
      if Type_Name = "GtkButton" then
         return new Gtk_Button_Record;
      else
         return null;
      end if;
   end Type_Conversion;

begin
   Gtk.Type_Conversion_Hooks.Add_Hook (Type_Conversion'Access);
end Gtk.Button;
