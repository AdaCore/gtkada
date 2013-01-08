------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2013, AdaCore                     --
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

with Cairo;            use Cairo;
with Glib;             use Glib;
with Glib.Error;       use Glib.Error;
with Glib.Main;        use Glib.Main;
with Gdk.Cairo;        use Gdk.Cairo;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.Image;        use Gtk.Image;
with Gtk.Label;        use Gtk.Label;
with Gtk.Widget;       use Gtk.Widget;
with Gtkada.Handlers;  use Gtkada.Handlers;

with Ada.Numerics.Generic_Elementary_Functions;

package body Create_Pixbuf is

   package Gdouble_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Gdouble_Functions;

   type String_Access is access String;
   type String_Array is array (Positive range <>) of String_Access;

   type Pixbuf_Array is array (Positive range <>) of Gdk_Pixbuf;

   Image_Names : constant String_Array :=
     (1 => new String'("apple-red.png"),
      2 => new String'("gnome-applets.png"),
      3 => new String'("gnome-calendar.png"),
      4 => new String'("gnome-foot.png"),
      5 => new String'("gnome-gmush.png"),
      6 => new String'("gnome-gimp.png"),
      7 => new String'("gnome-gsame.png"),
      8 => new String'("gnu-keys.png"));

   Background_Name : constant String := "background.jpg";
   Gif_Image       : constant String := "dancing-penguin.gif";

   Frame_Delay : constant Guint := 40;
   Cycle_Len   : constant := 40;

   Frame : Gdk_Pixbuf;
   Frame_Num : Gint;
   --  The current frame

   Background : Gdk_Pixbuf;
   Back_Width, Back_Height : Gint;
   --  The background image and its size

   Da : Gtk_Drawing_Area;

   Timeout_Id : G_Source_Id := 0;

   Images : Pixbuf_Array (Image_Names'Range);

   function Load_Pixbufs return Boolean;
   --  Load the images for the demo.
   --  Return False if one of the pixmaps could not be loaded

   function On_Draw
     (Widget : access Gtk_Widget_Record'Class;
      Cr     : Cairo_Context) return Boolean;
   --  Expose callback for the drawing area

   function Timeout_Handler return Boolean;
   --  Timeout handler to regenerate the frame

   procedure Destroy_Cb (Widget : access Gtk_Widget_Record'Class);
   --  Callback when the widget is destroyed

   ------------------
   -- Load_Pixbufs --
   ------------------

   function Load_Pixbufs return Boolean is
      Error : Glib.Error.GError;
   begin
      Gdk_New_From_File (Background, Background_Name, Error);

      if Background = null then
         return False;
      end if;

      Back_Width  := Get_Width (Background);
      Back_Height := Get_Height (Background);

      for J in Images'Range loop
         Gdk_New_From_File (Images (J), Image_Names (J).all, Error);
         if Images (J) = null then
            return False;
         end if;
      end loop;
      return True;
   end Load_Pixbufs;

   -------------
   -- On_Draw --
   -------------

   function On_Draw
     (Widget : access Gtk_Widget_Record'Class;
      Cr     : Cairo_Context) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      Set_Source_Pixbuf (Cr,
                         Pixbuf => Frame,
                         Pixbuf_X => 0.0,
                         Pixbuf_Y => 0.0);
      Cairo.Paint (Cr);
      return True;
   end On_Draw;

   ---------------------
   -- Timeout_Handler --
   ---------------------

   function Timeout_Handler return Boolean is
      Pi : constant Gdouble := 3.1415926535;
      F, Xmid, Ymid, Radius : Gdouble;
      Ang, Iw, Ih, R, K, C, S : Gdouble;
      Xpos, Ypos : Gdouble;
      R1, R2, Dest : Gdk_Rectangle;
      Inter : Boolean;
      Alpha : Gint;
   begin
      --  Restore the background
      Copy_Area
        (Src_Pixbuf  => Background,
         Src_X       => 0,
         Src_Y       => 0,
         Width       => Back_Width,
         Height      => Back_Height,
         Dest_Pixbuf => Frame,
         Dest_X      => 0,
         Dest_Y      => 0);

      R2 := (X      => 0,
             Y      => 0,
             Width  => Back_Width,
             Height => Back_Height);

      F      := Gdouble (Frame_Num mod Cycle_Len) / Gdouble (Cycle_Len);
      Xmid   := Gdouble (Back_Width) * 0.5;
      Ymid   := Gdouble (Back_Height) * 0.5;
      Radius := Gdouble'Min (Xmid, Ymid) * 0.5;
      S      := Sin (F * 2.0 * Pi);
      C      := Cos (F * 2.0 * Pi);
      R      := Radius + Radius / 3.0 * S;

      --  Then draw each of the images

      for J in Images'Range loop
         Ang  := 2.0 * Pi * (Gdouble (J) / Gdouble (Images'Length) - F);
         Iw   := Gdouble (Get_Width (Images (J)));
         Ih   := Gdouble (Get_Height (Images (J)));
         Xpos := Xmid + R * Cos (Ang) - Iw * 0.5 + 0.5;
         Ypos := Ymid + R * Sin (Ang) - Ih * 0.5 + 0.5;

         if J mod 2 = 1 then
            K := S;
         else
            K := C;
         end if;
         K := Gdouble'Max (0.25, 2.0 * K * K);

         R1 := (X      => Gint (Xpos),
                Y      => Gint (Ypos),
                Width  => Gint (Iw * K),
                Height => Gint (Ih * K));

         Intersect (R1, R2, Dest, Inter);
         if Inter then
            --  Play with the transparency, so that the animation is smoother
            if J mod 2 = 1 then
               Alpha := Gint'Max (50, abs (Gint (255.0 * S)));
            else
               Alpha := Gint'Max (50, abs (Gint (255.0 * C)));
            end if;

            Composite
              (Src           => Images (J),
               Dest          => Frame,
               Dest_X        => Dest.X,
               Dest_Y        => Dest.Y,
               Dest_Width    => Dest.Width,
               Dest_Height   => Dest.Height,
               Offset_X      => Xpos,
               Offset_Y      => Ypos,
               Scale_X       => K,
               Scale_Y       => K,
               Inter_Type    => Interp_Nearest,
               Overall_Alpha => Alpha_Range (Alpha));
         end if;
      end loop;

      Queue_Draw (Da);

      Frame_Num := Frame_Num + 1;
      return True;
   end Timeout_Handler;

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb (Widget : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Widget);
   begin
      Remove (Timeout_Id);
      Timeout_Id := 0;
   end Destroy_Cb;

   ---------
   -- Run --
   ---------

   procedure Run (F : access Gtk_Frame_Record'Class) is
      Label : Gtk_Label;
   begin
      if not Load_Pixbufs then
         Gtk_New (Label, "Images not found");
         Add (F, Label);
         return;
      end if;

      Gtk_New (Da);
      Add (F, Da);

      Frame := Gdk.Pixbuf.Gdk_New
        (Colorspace      => Colorspace_RGB,
         Has_Alpha       => False,
         Bits_Per_Sample => 8,
         Width           => Back_Width,
         Height          => Back_Height);

      Widget_Callback.Connect (Da, "destroy", Destroy_Cb'Access);
      Return_Callback.Connect
        (Da, Signal_Draw,
         Return_Callback.To_Marshaller (On_Draw'Access));

      Timeout_Id := Timeout_Add (Frame_Delay, Timeout_Handler'Access);

      Show_All (F);
   end Run;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows how one can animate several images on the"
        & " screen. All the images are loaded from the disk (and thus you"
        & " should start testgtk from the directory that contains the images."
        & ASCII.LF
        & "Note that nothing is precomputed in this demo, and all the drawing,"
        & " scaling and transparency is done in real-time."
        & ASCII.LF
        & "This demo uses some timeout callback to do the animation. It is"
        & " on several @bGdk_Pixbuf@B images.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run_Gif (F : access Gtk_Frame_Record'Class) is
      Image : Gtk_Image;
   begin
      Gtk_New (Image, Filename => Gif_Image);
      Add (F, Image);
      Show_All (F);
   end Run_Gif;

   --------------
   -- Help_Gif --
   --------------

   function Help_Gif return String is
   begin
      return "This file show how a @bGtk_Image@B can be used to display"
        & " an animated GIF image. The whole code for this demo is only"
        & " three lines in Ada!"
        & ASCII.LF
        & "This demo must be run from the testgtk directory itself so that"
        & " the image can be loaded from the disk.";
   end Help_Gif;

end Create_Pixbuf;
