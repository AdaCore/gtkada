------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2010-2026, AdaCore                     --
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

with Ada.Numerics;
with Ada.Unchecked_Deallocation;

with Glib;                use Glib;
with Glib.Error;          use Glib.Error;
with Glib.Object;         use Glib.Object;

with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Label;           use Gtk.Label;
with Gtk.Print_Settings;  use Gtk.Print_Settings;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Page_Setup;      use Gtk.Page_Setup;
with Gtk.Paper_Size;      use Gtk.Paper_Size;
with Gtk.Print_Context;   use Gtk.Print_Context;
with Gtk.Print_Operation; use Gtk.Print_Operation;
with Gtk.Window;          use Gtk.Window;

with Cairo;               use Cairo;

package body Create_Print is

   type Print_State is record
      Settings : Gtk_Print_Settings;
      Setup    : Gtk_Page_Setup;
      Status   : Gtk_Label;
      Filename : Gtk_Entry;
   end record;
   type Print_State_Access is access Print_State;
   package Print_Data is new Glib.Object.User_Data (Print_State_Access);
   procedure Free_State is new Ada.Unchecked_Deallocation
     (Print_State, Print_State_Access);

   procedure Request_Page_Setup
     (Print_Operation : access Gtk_Print_Operation_Record'Class;
      Context         : not null access Gtk_Print_Context_Record'Class;
      Page_Num        : Gint;
      Setup           : not null access Gtk_Page_Setup_Record'Class);
   --  Callback to set up page layout when printing.

   procedure Draw_Page
     (Print_Operation : access Gtk_Print_Operation_Record'Class;
      Context         : not null access Gtk_Print_Context_Record'Class;
      Page_Num        : Gint);
   --  Callback to draw the pages as they are being printed.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "Print a two-page Cairo drawing or export it as a PDF."
        & " Page Setup selects the layout for the first page; the second"
        & " page demonstrates request-page-setup with A5 landscape paper."
        & " Print settings are remembered while this demo is open."
        & " Enter a PDF filename before choosing Export PDF.";
   end Help;

   ---------------
   -- Draw_Page --
   ---------------

   procedure Draw_Page
     (Print_Operation : access Gtk_Print_Operation_Record'Class;
      Context         : not null access Gtk_Print_Context_Record'Class;
      Page_Num        : Gint)
   is
      pragma Unreferenced (Print_Operation);
      pragma Unreferenced (Page_Num);
      Cr      : Cairo_Context;
      X, Y, Z : Gdouble;
   begin
      Cr := Get_Cairo_Context (Context);

      --  Draw a thin light gray bar, as wide as the paper, inside the margins
      Set_Source_Rgb (Cr, 0.8, 0.8, 0.8);
      Rectangle
        (Cr     => Cr,
         X      => 0.0,
         Y      => 0.0,
         Width  => Get_Width (Context),
         Height => 0.2);
      Cairo.Fill (Cr);

      --  Circles
      for Xi in 1 .. 10 loop
         X := Gdouble (Xi) / 10.0;
         for Yi in 1 .. 10 loop
            Y := Gdouble (Yi) / 10.0;
            for Zi in 1 .. 10 loop
               Z := Gdouble (Zi) / 10.0;

               Set_Source_Rgb (Cr, X, Y, Z);
               Arc
                 (Cr     => Cr,
                  Xc     => 2.5 + X * 3.0,
                  Yc     => 1.0 + Y * 3.0,
                  Radius => 0.2,
                  Angle1 => 2.0 * Ada.Numerics.Pi * (Z - 0.1),
                  Angle2 => 2.0 * Ada.Numerics.Pi * Z);
               Set_Line_Width (Cr, 1.0 / 32.0);
               Cairo.Stroke (Cr);

            end loop;
         end loop;
      end loop;

      --  Draw some lines
      Move_To (Cr, 2.0, 0.5);
      Curve_To (Cr, 2.0, 1.5, 6.0, -0.5, 6.2, 0.5);
      Line_To (Cr, 6.2, 4.75);
      Curve_To (Cr, 6.2, 3.75, 2.0, 5.75, 2.0, 4.75);
      Close_Path (Cr);

      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Set_Line_Width (Cr, 1.0 / 16.0);
      Set_Line_Cap (Cr, Cairo_Line_Cap_Round);
      Set_Line_Join (Cr, Cairo_Line_Join_Round);
      Cairo.Stroke (Cr);
   end Draw_Page;

   ------------------------
   -- Request_Page_Setup --
   ------------------------

   procedure Request_Page_Setup
     (Print_Operation : access Gtk_Print_Operation_Record'Class;
      Context         : not null access Gtk_Print_Context_Record'Class;
      Page_Num        : Gint;
      Setup           : not null access Gtk_Page_Setup_Record'Class)
   is
      pragma Unreferenced (Print_Operation);
      pragma Unreferenced (Context);

      A5_Size : Gtk_Paper_Size;
   begin
      --  Make the second page landscape mode A5
      if Page_Num = 1 then
         Gtk_New (A5_Size, Paper_Name_A5);
         Set_Orientation (Setup, Page_Orientation_Landscape);
         Set_Paper_Size_And_Default_Margins (Setup, A5_Size);
         Free (A5_Size);
      end if;
   end Request_Page_Setup;

   procedure Release_State (State : Print_State_Access) is
      Owned : Print_State_Access := State;
   begin
      State.Settings.Unref;
      State.Setup.Unref;
      Free_State (Owned);
   end Release_State;

   procedure Page_Setup (Object : access GObject_Record'Class) is
      State : constant Print_State_Access := Print_Data.Get (Object);
      Setup : Gtk_Page_Setup;
   begin
      --  Keep the state alive across the dialog's recursive main loop.
      Object.Ref;
      Setup := Run_Page_Setup_Dialog
        (Gtk_Window (Gtk_Box (Object).Get_Ancestor (Gtk.Window.Get_Type)),
         State.Setup, State.Settings);
      State.Setup.Unref;
      State.Setup := Setup;
      Object.Unref;
   end Page_Setup;

   procedure Do_Print
     (Object : access GObject_Record'Class;
      Action : Gtk_Print_Operation_Action)
   is
      State    : constant Print_State_Access := Print_Data.Get (Object);
      Print_Op : Gtk_Print_Operation;
      Result   : Gtk_Print_Operation_Result;
      Error    : GError;
   begin
      Object.Ref;
      Gtk_New (Print_Op);
      Print_Op.Set_Current_Page (0);
      Print_Op.Set_N_Pages (2);
      Print_Op.Set_Unit (Inch);
      Print_Op.Set_Job_Name ("GtkAda printing demo");
      Print_Op.Set_Allow_Async (False);
      Print_Op.Set_Default_Page_Setup (State.Setup);
      Print_Op.Set_Print_Settings (State.Settings);
      Print_Op.Set_Embed_Page_Setup (True);
      Print_Op.On_Request_Page_Setup (Request_Page_Setup'Access);
      Print_Op.On_Draw_Page (Draw_Page'Access);
      if Action = Print_Operation_Action_Export then
         Print_Op.Set_Export_Filename (State.Filename.Get_Text);
      end if;

      Result := Print_Op.Run
        (Action,
         Gtk_Window (Gtk_Box (Object).Get_Ancestor (Gtk.Window.Get_Type)),
         Error);
      case Result is
         when Print_Operation_Result_Error =>
            if Error /= null then
               State.Status.Set_Text ("Printing failed: " & Get_Message (Error));
               Error_Free (Error);
            else
               State.Status.Set_Text ("Printing failed.");
            end if;
         when Print_Operation_Result_Apply =>
            --  Get_Print_Settings returns a borrowed reference.
            declare
               Settings : constant Gtk_Print_Settings :=
                 Print_Op.Get_Print_Settings.Copy;
            begin
               State.Settings.Unref;
               State.Settings := Settings;
            end;
            if Action = Print_Operation_Action_Export then
               State.Status.Set_Text
                 ("PDF exported to " & State.Filename.Get_Text);
            else
               State.Status.Set_Text (Print_Op.Get_Status_String);
            end if;
         when Print_Operation_Result_Cancel =>
            State.Status.Set_Text ("Printing cancelled.");
         when Print_Operation_Result_In_Progress =>
            State.Status.Set_Text (Print_Op.Get_Status_String);
      end case;
      Print_Op.Unref;
      Object.Unref;
   end Do_Print;

   procedure Print (Object : access GObject_Record'Class) is
   begin
      Do_Print (Object, Print_Operation_Action_Print_Dialog);
   end Print;

   procedure Export_Pdf (Object : access GObject_Record'Class) is
   begin
      Do_Print (Object, Print_Operation_Action_Export);
   end Export_Pdf;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box     : Gtk_Box;
      Buttons : Gtk_Box;
      Button  : Gtk_Button;
      State   : constant Print_State_Access := new Print_State;
      Paper   : Gtk_Paper_Size;
   begin
      Frame.Set_Label ("Printing");
      Gtk_New (Box, Orientation_Vertical, 12);
      Box.Set_Margin_Start (12);
      Box.Set_Margin_End (12);
      Box.Set_Margin_Top (12);
      Box.Set_Margin_Bottom (12);
      Frame.Set_Child (Box);
      Box.Append (Gtk_Label_New
        ("Two pages of Cairo artwork: first A4, then A5 landscape."));

      Gtk_New (State.Settings);
      Gtk_New (State.Setup);
      Gtk_New (Paper, Paper_Name_A4);
      State.Setup.Set_Paper_Size_And_Default_Margins (Paper);
      Paper.Free;
      Gtk_New (State.Filename);
      State.Filename.Set_Text ("gtkada-print.pdf");
      Box.Append (Gtk_Label_New ("PDF output filename:"));
      Box.Append (State.Filename);
      Gtk_New (State.Status, "Choose Page Setup, Print, or Export PDF.");
      State.Status.Set_Wrap (True);
      Box.Append (State.Status);
      Print_Data.Set (Box, State, On_Destroyed => Release_State'Access);

      Gtk_New (Buttons, Orientation_Horizontal, 6);
      Box.Append (Buttons);
      Gtk_New (Button, "Page Setup");
      Button.On_Clicked (Page_Setup'Access, Slot => Box);
      Buttons.Append (Button);
      Gtk_New (Button, "Print");
      Button.On_Clicked (Print'Access, Slot => Box);
      Buttons.Append (Button);
      Gtk_New (Button, "Export PDF");
      Button.On_Clicked (Export_Pdf'Access, Slot => Box);
      Buttons.Append (Button);
   end Run;

end Create_Print;
