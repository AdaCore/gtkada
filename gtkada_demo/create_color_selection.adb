------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Gdk;                         use Gdk;
with Gdk.Color;                   use Gdk.Color;
with Gdk.RGBA;                    use Gdk.RGBA;
with Gtk;                         use Gtk;
with Glib;                        use Glib;
with Glib.Properties;
with Gtk.Color_Selection;         use Gtk.Color_Selection;
with Gtk.Color_Selection_Dialog;  use Gtk.Color_Selection_Dialog;
with Gtk.Button;                  use Gtk.Button;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Settings;                use Gtk.Settings;
with Common;                      use Common;
with Ada.Text_IO;                 use Ada.Text_IO;
with Gtk.Widget;                  use Gtk.Widget;
with System;

package body Create_Color_Selection is

   type Gtk_Color_Dialog_Access is access all Gtk_Color_Selection_Dialog;
   package Destroy_Dialog_Cb is new Handlers.User_Callback
     (Gtk_Color_Selection_Dialog_Record, Gtk_Color_Dialog_Access);

   Dialog : aliased Gtk_Color_Selection_Dialog;

   package Color_Sel_Cb is new Handlers.Callback
     (Gtk_Color_Selection_Dialog_Record);

   procedure Destroy_Dialog
     (Win : access Gtk_Color_Selection_Dialog_Record'Class;
      Ptr : Gtk_Color_Dialog_Access);
   --  Called when the dialog is destroyed

   procedure On_Palette_Changed
     (Screen : System.Address;
      Colors : Gdk.Color.Gdk_Color_Unconstrained_Array;
      N_Colors : Gint);
   pragma Convention (C, On_Palette_Changed);
   --  Called when the palette is changed

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This widget provides an easy way to select new colors."
        & " This is a very specific widget, and most applications won't"
        & " need it. There are two versions, one with a dialog, and one"
        & " without.";
   end Help;

   --------------------
   -- Destroy_Dialog --
   --------------------

   procedure Destroy_Dialog
     (Win : access Gtk_Color_Selection_Dialog_Record'Class;
      Ptr : Gtk_Color_Dialog_Access)
   is
      pragma Warnings (Off, Win);
   begin
      Ptr.all := null;
   end Destroy_Dialog;

   ------------------
   -- Close_Window --
   ------------------

   procedure Close_Window (Win : access Gtk_Widget_Record'Class) is
   begin
      Destroy (Win);
   end Close_Window;

   ------------------------
   -- On_Palette_Changed --
   ------------------------

   procedure On_Palette_Changed
     (Screen : System.Address;
      Colors : Gdk.Color.Gdk_Color_Unconstrained_Array;
      N_Colors : Gint)
   is
      pragma Unreferenced (Screen);
      Palette : constant Gdk_Color_Array := To_Array (Colors, N_Colors);
      Str : constant String := Palette_To_String (Palette);
   begin
      Put_Line ("Palette has changed, and became " & Str);
      Glib.Properties.Set_Property
         (Get_Default, Gtk_Color_Palette_Property, Str);
   end On_Palette_Changed;

   --------------
   -- Color_Ok --
   --------------

   procedure Color_Ok
     (Dialog : access Gtk_Color_Selection_Dialog_Record'Class)
   is
      Color : Gdk_RGBA;
   begin
      Get_Current_Rgba (Get_Color_Selection (Dialog), Color);
      Put_Line ("Selected color is: ");
      Put ("Red=" & Color.Red'Img);
      Put (" Green=" & Color.Green'Img);
      Put (" Blue=" & Color.Blue'Img);
      Put_Line (" Alpha=" & Color.Alpha'Img);
   end Color_Ok;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk_Frame_Record'Class) is
      pragma Warnings (Off, Frame);
   begin
      if Dialog = null then
         Gtk_New (Dialog, Title => "Color Selection Dialog");
         Set_Position (Dialog, Enums.Win_Pos_Mouse);

         Set_Has_Palette (Get_Color_Selection (Dialog), True);
         Set_Has_Opacity_Control (Get_Color_Selection (Dialog), True);
         Set_Change_Palette_With_Screen_Hook (On_Palette_Changed'Access);

         Destroy_Dialog_Cb.Connect
           (Dialog, "destroy",
            Destroy_Dialog_Cb.To_Marshaller (Destroy_Dialog'Access),
            Dialog'Access);

         Color_Sel_Cb.Object_Connect
           (Gtk_Button
              (Glib.Properties.Get_Property (Dialog, Ok_Button_Property)),
            "clicked",
            Color_Sel_Cb.To_Marshaller (Color_Ok'Access),
            Slot_Object => Dialog);
         Widget_Handler.Object_Connect
           (Gtk_Button
              (Glib.Properties.Get_Property (Dialog, Cancel_Button_Property)),
            "clicked",
            Close_Window'Access,
            Slot_Object => Dialog);
         Show (Dialog);
      else
         Destroy (Dialog);
      end if;
   end Run;

end Create_Color_Selection;
