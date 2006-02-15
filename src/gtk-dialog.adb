-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
with Gdk.Event;  use Gdk.Event;
with Gtk.Window; use Gtk.Window;

package body Gtk.Dialog is

   ---------------------
   -- Get_Action_Area --
   ---------------------

   function Get_Action_Area
     (Dialog : access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_dialog_get_action_area");

      Stub : Gtk.Box.Gtk_Box_Record;

   begin
      return Gtk.Box.Gtk_Box
        (Get_User_Data (Internal (Get_Object (Dialog)), Stub));
   end Get_Action_Area;

   --------------
   -- Get_Vbox --
   --------------

   function Get_Vbox
     (Dialog : access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_dialog_get_vbox");

      Stub : Gtk.Box.Gtk_Box_Record;

   begin
      return Gtk.Box.Gtk_Box
        (Get_User_Data (Internal (Get_Object (Dialog)), Stub));
   end Get_Vbox;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Dialog : out Gtk_Dialog) is
   begin
      Dialog := new Gtk_Dialog_Record;
      Gtk.Dialog.Initialize (Dialog);
   end Gtk_New;

   procedure Gtk_New
     (Dialog : out Gtk_Dialog;
      Title  : UTF8_String;
      Parent : Gtk.Window.Gtk_Window;
      Flags  : Gtk_Dialog_Flags) is
   begin
      Dialog := new Gtk_Dialog_Record;
      Gtk.Dialog.Initialize (Dialog, Title, Parent, Flags);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Dialog : access Gtk_Dialog_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_dialog_new");

   begin
      Set_Object (Dialog, Internal);

      if Get_Follow_Events then
         Add_Events (Dialog, Pointer_Motion_Mask);
      end if;
   end Initialize;

   procedure Initialize
     (Dialog : access Gtk_Dialog_Record'Class;
      Title  : UTF8_String;
      Parent : Gtk.Window.Gtk_Window;
      Flags  : Gtk_Dialog_Flags)
   is
      function Internal
        (Title     : UTF8_String;
         Parent    : System.Address;
         Flags     : Gtk_Dialog_Flags) return System.Address;
      pragma Import (C, Internal, "ada_gtk_dialog_new_with_buttons");

   begin
      if Parent = null then
         Set_Object
           (Dialog, Internal (Title & ASCII.NUL, System.Null_Address, Flags));
      else
         Set_Object
           (Dialog, Internal (Title & ASCII.NUL, Get_Object (Parent), Flags));
      end if;

      if Get_Follow_Events then
         Add_Events (Dialog, Pointer_Motion_Mask);
      end if;
   end Initialize;

   -----------------------
   -- Add_Action_Widget --
   -----------------------

   procedure Add_Action_Widget
     (Dialog      : access Gtk_Dialog_Record;
      Child       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Response_Id : Gtk_Response_Type)
   is
      procedure Internal
        (Dialog, Child : System.Address; Response_Id : Gtk_Response_Type);
      pragma Import (C, Internal, "gtk_dialog_add_action_widget");

   begin
      Internal (Get_Object (Dialog), Get_Object (Child), Response_Id);
   end Add_Action_Widget;

   ----------------
   -- Add_Button --
   ----------------

   function Add_Button
     (Dialog      : access Gtk_Dialog_Record;
      Text        : UTF8_String;
      Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Dialog : System.Address; Text : UTF8_String; Id : Gtk_Response_Type)
         return System.Address;
      pragma Import (C, Internal, "gtk_dialog_add_button");

   begin
      return Gtk.Widget.Convert
        (Internal (Get_Object (Dialog), Text & ASCII.NUL, Response_Id));
   end Add_Button;

   ----------------------------
   -- Set_Response_Sensitive --
   ----------------------------

   procedure Set_Response_Sensitive
     (Dialog      : access Gtk_Dialog_Record;
      Response_Id : Gtk_Response_Type;
      Setting     : Boolean)
   is
      procedure Internal
        (Dialog     : System.Address;
         Reponse_Id : Gtk_Response_Type;
         Setting    : Gint);
      pragma Import (C, Internal, "gtk_dialog_set_response_sensitive");

   begin
      Internal (Get_Object (Dialog), Response_Id, Boolean'Pos (Setting));
   end Set_Response_Sensitive;

   --------------------------
   -- Set_Default_Response --
   --------------------------

   procedure Set_Default_Response
     (Dialog : access Gtk_Dialog_Record; Response_Id : Gtk_Response_Type)
   is
      procedure Internal
        (Dialog : System.Address; Response_Id : Gtk_Response_Type);
      pragma Import (C, Internal, "gtk_dialog_set_default_response");

   begin
      Internal (Get_Object (Dialog), Response_Id);
   end Set_Default_Response;

   -----------------------
   -- Set_Has_Separator --
   -----------------------

   procedure Set_Has_Separator
     (Dialog : access Gtk_Dialog_Record; Setting : Boolean)
   is
      procedure Internal (Dialog : System.Address; Setting : Gint);
      pragma Import (C, Internal, "gtk_dialog_set_has_separator");
   begin
      Internal (Get_Object (Dialog), Boolean'Pos (Setting));
   end Set_Has_Separator;

   -----------------------
   -- Get_Has_Separator --
   -----------------------

   function Get_Has_Separator
     (Dialog : access Gtk_Dialog_Record) return Boolean
   is
      function Internal (Dialog : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_dialog_get_has_separator");
   begin
      return Internal (Get_Object (Dialog)) /= 0;
   end Get_Has_Separator;

   ---------
   -- Run --
   ---------

   function Run (Dialog : access Gtk_Dialog_Record) return Gtk_Response_Type is
      function Internal (Dialog : System.Address) return Gtk_Response_Type;
      pragma Import (C, Internal, "gtk_dialog_run");
   begin
      return Internal (Get_Object (Dialog));
   end Run;

   --------------
   -- Response --
   --------------

   procedure Response
     (Dialog : access Gtk_Dialog_Record; Response_Id : Gtk_Response_Type)
   is
      procedure Internal (Dialog : System.Address; Id : Gtk_Response_Type);
      pragma Import (C, Internal, "gtk_dialog_response");
   begin
      Internal (Get_Object (Dialog), Response_Id);
   end Response;

end Gtk.Dialog;
