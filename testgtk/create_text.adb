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

with Glib;                use Glib;
with Gdk.Color;           use Gdk.Color;
with Gdk.Font;            use Gdk.Font;
with Gtk.Box;             use Gtk.Box;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Hbutton_Box;     use Gtk.Hbutton_Box;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text;            use Gtk.Text;
with Gtk.Toggle_Button;   use Gtk.Toggle_Button;
with Gtk;                 use Gtk;
with Ada.Text_IO;         use Ada.Text_IO;

package body Create_Text is

   package Text_Cb is new Handlers.User_Callback
     (Gtk_Toggle_Button_Record, Gtk_Text);

   type String_Access is access String;
   type Text_Colors_Type is record
      Red, Green, Blue : Gushort;
      Name  : String_Access;
   end record;
   type Text_Colors_Type_Array is array (Natural range <>) of
     Text_Colors_Type;

   Text_Colors : constant Text_Colors_Type_Array :=
     ((16#0#, 16#0#, 16#0#, new String'("black")),
      (16#FFFF#, 16#FFFF#, 16#FFFF#, new String'("white")),
      (16#FFFF#, 16#0#,    16#0#,    new String'("red")),
      (16#0#,    16#FFFF#, 16#0#,    new String'("green")),
      (16#0#,    16#0#,    16#FFFF#, new String'("blue")),
      (16#0#,    16#FFFF#, 16#FFFF#, new String'("cyan")),
      (16#FFFF#, 16#0#,    16#FFFF#, new String'("magenta")),
      (16#FFFF#, 16#FFFF#, 16#0#,    new String'("yellow")));

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "The @bGtk_Text@B widget allows you to display easily any kind of"
        & " text in your applications. As you can see in this demo, this"
        & " widget supports colors, text wrapping, images in the"
        & " background,..."
        & ASCII.LF
        & "One important thing to note is that the text widget does not"
        & " provide any scrolling in itself. Instead, you have to put it in a"
        & " @bGtk_Scrolled_Window@B if you want some scrolling.";
   end Help;

   ---------------------
   -- Toggle_Editable --
   ---------------------

   procedure Toggle_Editable (Toggle : access Gtk_Toggle_Button_Record'Class;
                              Text   : in Gtk_Text)
   is
   begin
      Set_Editable (Text, Get_Active (Toggle));
   end Toggle_Editable;

   ---------------
   -- Word_Wrap --
   ---------------

   procedure Word_Wrap (Toggle : access Gtk_Toggle_Button_Record'Class;
                        Text   : in Gtk_Text)
   is
   begin
      Set_Word_Wrap (Text, Get_Active (Toggle));
   end Word_Wrap;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1,
        Box2       : Gtk_Box;
      Hbox         : Gtk_HButton_Box;
      Text         : Gtk_Text;
      Font         : Gdk_Font;
      Check        : Gtk_Check_Button;
      Scrolled     : Gtk_Scrolled_Window;
      Color_I, Color_J : Gdk_Color;

   begin
      Set_Label (Frame, "Text");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, True, True, 0);
      Show (Box2);

      Gtk_New (Scrolled);
      Pack_Start (Box2, Scrolled, True, True, 0);
      Set_Policy (Scrolled, Policy_Never, Policy_Always);
      Show (Scrolled);

      Gtk_New (Text);
      Set_Editable (Text, True);
      Add (Scrolled, Text);
      Grab_Focus (Text);

      Freeze (Text);

      Load (Font, "-adobe-courier-medium-r-normal--*-120-*-*-*-*-*-*");

      for I in Text_Colors'Range loop
         Insert (Text, Font,
                 White (Get_Colormap (Text)), Null_Color,
                 Text_Colors (I).Name.all & Ascii.HT, -1);
         Set_Rgb (Color_I, Text_Colors (I).Red, Text_Colors (I).Green,
                  Text_Colors (I).Blue);
         for J in Text_Colors'Range loop
            Set_Rgb (Color_J, Text_Colors (J).Red, Text_Colors (J).Green,
                     Text_Colors (J).Blue);
            Insert (Text, Font, Color_J, Color_I, "XYZ", -1);
         end loop;
         Insert (Text, Null_Font, Null_Color, Null_Color,
                 "" & Ascii.LF, -1);
      end loop;

      declare
         Buffer : String (1 .. 1024);
         Last   : Natural;
         Infile       : File_Type;
      begin
         Open (Infile, In_File, "create_text.adb");
         while not End_Of_File (Infile) loop
            Get_Line (Infile, Buffer, Last);

            Insert (Text, Null_Font, White (Get_Colormap (Text)),
                    Null_Color,
                    Buffer (1 .. Last) & Ascii.LF, Gint (Last) + 1);
         end loop;
         Close (Infile);
      exception
         when Name_Error =>
            Put_Line ("File create_text.adb not found....");
      end;

      Thaw (Text);

      Gtk_New (Hbox);
      Pack_Start (Box2, Hbox, False, False, 0);

      Gtk_New (Check, "Editable");
      Pack_Start (Hbox, Check, False, False, 0);
      Text_Cb.Connect (Check, "toggled",
                       Text_Cb.To_Marshaller (Toggle_Editable'Access),
                       Text);
      Set_Active (Check, True);

      Gtk_New (Check, "Wrap Words");
      Pack_Start (Hbox, Check, False, False, 0);
      Text_Cb.Connect (Check, "toggled",
                       Text_Cb.To_Marshaller (Word_Wrap'Access), Text);
      Set_Active (Check, False);

      Show_All (Frame);
   end Run;

end Create_Text;

