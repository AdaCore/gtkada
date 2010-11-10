-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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
with Ada.Numerics;
with Ada.Text_IO;         use Ada.Text_IO;

with Glib;                use Glib;
with Glib.Object;         use Glib.Object;

with Gtk.Box;             use Gtk.Box;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Page_Setup;      use Gtk.Page_Setup;
with Gtk.Paper_Size;      use Gtk.Paper_Size;
with Gtk.Print_Context;   use Gtk.Print_Context;
with Gtk.Print_Operation; use Gtk.Print_Operation;
with Gtk.Window;          use Gtk.Window;

with Gtkada.Printing;     use Gtkada.Printing;

with Cairo;               use Cairo;

package body Create_Print is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This is a very simple demo to demonstrate GtkAda's high-level,"
        & " portable printing API.  On some platforms, Gtkada_Print_Operation"
        & " uses the native print dialog.  On platforms which do not provide"
        & " a native print dialog, GtkAda uses GTK+'s own.";
   end Help;

   ---------------
   -- Draw_Page --
   ---------------

   procedure Draw_Page
     (Print_Operation : Gtkada_Print_Operation;
      Context         : Gtk_Print_Context;
      Page_Num        : Gint)
   is
      pragma Unreferenced (Print_Operation);
      pragma Unreferenced (Page_Num);
      Cr : Cairo_Context;
   begin
      Cr := Get_Cairo_Context (Context);

      --  Draw a red rectangle, as wide as the paper (inside the margins)
      Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);
      Rectangle (Cr, 0.0, 0.0, Get_Width (Context), 1.5);
      Cairo.Fill (Cr);

      --  Draw some lines
      Move_To (Cr, 1.0, 0.5);
      Line_To (Cr, 2.0, 1.0);
      Arc (Cr, 3.0, 3.0, 1.0, 0.0, Ada.Numerics.Pi * 1.5);
      Line_To (Cr, 4.0, 1.0);

      Set_Source_Rgb (Cr, 0.0, 0.0, 1.0);
      Set_Line_Width (Cr, 1.0/32.0);
      Set_Line_Cap (Cr, Cairo_Line_Cap_Round);
      Set_Line_Join (Cr, Cairo_Line_Join_Round);
      Cairo.Stroke (Cr);
   end Draw_Page;

   ------------------------
   -- Request_Page_Setup --
   ------------------------

   procedure Request_Page_Setup
     (Print_Operation : Gtkada_Print_Operation;
      Context         : Gtk_Print_Context;
      Page_Num        : Gint;
      Setup           : Gtk_Page_Setup;
      User_Data       : System.Address)
   is
      pragma Unreferenced (Print_Operation);
      pragma Unreferenced (Context);
      pragma Unreferenced (User_Data);

      A5_Size : Gtk_Paper_Size;
   begin
      --  Make the second page landscape mode A5
      if Page_Num = 1 then
         Gtk_New (A5_Size, Gtk_Paper_Name_A5);
         Set_Orientation (Setup, Page_Orientation_Landscape);
         Set_Paper_Size (Setup, A5_Size);
         Free (A5_Size);
      end if;
   end Request_Page_Setup;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box      : Gtk_Box;
      Print_Op : Gtkada_Print_Operation;
      Result   : Gtk_Print_Operation_Result;
   begin
      Set_Label (Frame, "Printing");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      --  Set up print operation basics
      Gtk_New (Print_Op);
      Set_Current_Page (Print_Op, 1);
      Set_N_Pages (Print_Op, 2);
      Set_Unit (Print_Op, Inch);

      --  Connect signals
      Install_Draw_Page_Handler (Print_Op, Draw_Page'Access);
      Install_Request_Page_Setup_Handler (Print_Op, Request_Page_Setup'Access);

      --  Call up the print operation dialog
      Result := Connect_And_Run
        (Print_Op, Action_Print_Dialog, Gtk_Window (Get_Toplevel (Frame)));
      Put_Line ("Result is " & Result'Img);
      Put_Line ("Print status: " & Get_Status (Print_Op));

      Show_All (Frame);
   end Run;

end Create_Print;
