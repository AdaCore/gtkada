------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Ada.Text_IO;       use Ada.Text_IO;
with Glib;              use Glib;
with Glib.Error;        use Glib.Error;
with Gdk.Event;         use Gdk.Event;
with Gdk.Rgb;           use Gdk.Rgb;
with Gtk.Arguments;     use Gtk.Arguments;
with Gtk.Drawing_Area;  use Gtk.Drawing_Area;
with Gtk.Handlers;      use Gtk.Handlers;
with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Gtk.Style;         use Gtk.Style;
with Gtk.Widget;        use Gtk.Widget;

package body Power_GNU is

   Default_Width  : constant := 800;
   Default_Height : constant := 600;

   ------------------------
   -- Callbacks packages --
   ------------------------

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Image_Drawing_Record, Boolean);

   package Destroy_Cb is new Gtk.Handlers.Callback
     (Image_Drawing_Record);

   package Image_Callback is new Gtk.Handlers.Callback
     (Image_Drawing_Record);

   ------------------------
   -- Internal Callbacks --
   ------------------------

   procedure Destroy (Draw : access Image_Drawing_Record'Class);

   procedure Size_Request
     (Draw        : access Image_Drawing_Record'Class;
      Args        : Gtk_Args);

   procedure Size_Allocate
     (Draw       : access Image_Drawing_Record'Class;
      Args       : Gtk_Args);

   function Expose
     (Draw  : access Image_Drawing_Record'Class;
      Event : Gdk_Event) return Boolean;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Draw : access Image_Drawing_Record'Class) is
   begin
      if Draw.Orig = Null_Pixbuf then
         return;
      end if;

      --  Destroy the associated image
      Unref (Draw.Orig);
      Unref (Draw.Pix);
   end Destroy;

   ------------
   -- Expose --
   ------------

   function Expose
     (Draw  : access Image_Drawing_Record'Class;
      Event : Gdk_Event) return Boolean is
      pragma Unreferenced (Event);
   begin
      if Draw.Pix = Null_Pixbuf then
         return False;
      end if;

      Render_To_Drawable
        (Draw.Pix,
         Get_Window (Draw),
         Gtk.Style.Get_Black_GC (Get_Style (Draw)),
         0, 0,
         0, 0,
         Get_Width (Draw.Pix), Get_Height (Draw.Pix),
         Dither_Normal,
         0, 0);
      return False;
   end Expose;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Draw : out Image_Drawing) is
   begin
      --  The drawing area MUST be created with Gdk.Rgb colormap,
      --  otherwise the image can not be rendered correctly.

      Gtk.Widget.Push_Colormap (Gdk.Rgb.Get_Cmap);

      Draw := new Image_Drawing_Record;
      Power_GNU.Initialize (Draw);
      Gtk.Widget.Pop_Colormap;
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Draw : access Image_Drawing_Record'Class) is
   begin
      Gtk.Drawing_Area.Initialize (Draw);

      --  Set up the appropriate callbacks to redraw, ...
      Event_Cb.Connect
        (Draw, "expose_event", Event_Cb.To_Marshaller (Expose'Access), True);
      Image_Callback.Connect (Draw, "size_request", Size_Request'Access);
      Image_Callback.Connect (Draw, "size_allocate", Size_Allocate'Access);
      Destroy_Cb.Connect
        (Draw, "destroy",
         Destroy_Cb.To_Marshaller (Destroy'Access));
   end Initialize;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
     (Draw  : Image_Drawing;
      Image : String)
   is
      Error : GError;
   begin
      if Draw.Orig /= Null_Pixbuf then
         Unref (Draw.Orig);
         Unref (Draw.Pix);
      end if;

      Gdk_New_From_File (Draw.Orig, Image, Error);

      if Error /= null then
         Put_Line ("Error: " & Get_Message (Error));
         Error_Free (Error);
         return;
      end if;

      Draw.Pix := Scale_Simple
        (Draw.Orig,
         Gint (Get_Allocation_Width (Draw)),
         Gint (Get_Allocation_Height (Draw)));
      Gtk.Widget.Draw (Gtk_Widget (Draw));
   end Set_Image;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate
     (Draw       : access Image_Drawing_Record'Class;
      Args       : Gtk_Args)
   is
      Allocation : Gtk_Allocation_Access := To_Allocation (Args, 1);
   begin
      if Draw.Pix = Null_Pixbuf then
         return;
      end if;

      Unref (Draw.Pix);
      Draw.Pix := Scale_Simple
        (Draw.Orig,
         Gint (Allocation.Width),
         Gint (Allocation.Height));
      Gtk.Handlers.Emit_Stop_By_Name (Draw, "size_allocate");
   end Size_Allocate;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Draw        : access Image_Drawing_Record'Class;
      Args        : Gtk_Args)
   is
      Requisition : Gtk_Requisition_Access := To_Requisition (Args, 1);
   begin
      Requisition.Width := Default_Width;
      Requisition.Height := Default_Height;

      --  Stop the signal from being propagated to the parent's default
      --  size_request function
      Gtk.Handlers.Emit_Stop_By_Name (Draw, "size_request");
   end Size_Request;

end Power_GNU;
