--  Exercise Pango.Cairo's default font backend selection and layout drawing.
--  No matching Pango.Cairo tests are present in testsuite/c_tests/.

with Ada.Command_Line;

with Cairo;
with Cairo.Image_Surface;
with Cairo.Surface;
with Glib.Object;
with Glib.Test; use Glib.Test;
with Gtk.Label; use Gtk.Label;
with Gtk.Main;
with Pango.Cairo;
with Pango.Context;
with Pango.Layout;
with System;
with System.Address_To_Access_Conversions;

use type Cairo.Cairo_Status;
use type Cairo.Cairo_Font_Type;
use type Cairo.Image_Surface.Byte;
use type Glib.Gint;
use type Glib.Object.GObject;
use type Pango.Context.Pango_Context;
use type Pango.Layout.Pango_Layout;
use type System.Address;

procedure Main is

   --  The test inspects Cairo's image buffer after drawing. The fixed-size
   --  overlay matches the 256 by 64 ARGB32 image used below.
   type Pixel_Buffer is array
     (Natural range 0 .. 256 * 64 - 1) of Cairo.Image_Surface.ARGB32_Data;
   package Pixel_Pointers is new
     System.Address_To_Access_Conversions (Pixel_Buffer);
   use type Pixel_Pointers.Object_Pointer;

   function Get_Default_Font_Map return System.Address;
   pragma Import (C, Get_Default_Font_Map, "pango_cairo_font_map_get_default");

   --  Pango.Cairo does not expose its backend getter in Ada. This read-only C
   --  query checks the effect of the public setter under test.
   function Get_Font_Type
     (Font_Map : System.Address) return Cairo.Cairo_Font_Type;
   pragma Import (C, Get_Font_Type, "pango_cairo_font_map_get_font_type");

   function Ref_Object (Object : System.Address) return System.Address;
   pragma Import (C, Ref_Object, "g_object_ref");

   procedure Unref_Object (Object : System.Address);
   pragma Import (C, Unref_Object, "g_object_unref");

   procedure Test_Show_Layout with Convention => C;
   procedure Test_Set_Default_Font_Type with Convention => C;

   ----------------------
   -- Test_Show_Layout --
   ----------------------

   --  Render ordinary text to a transparent image and confirm the layout
   --  changed its pixels, as well as leaving Cairo in a successful state.
   procedure Test_Show_Layout is
      Label   : constant Gtk_Label := Gtk_Label_New ("");
      Context : constant Pango.Context.Pango_Context :=
        Label.Get_Pango_Context;
      Layout  : constant Pango.Layout.Pango_Layout :=
        Pango.Layout.Pango_Layout_New (Context);
      Surface : constant Cairo.Cairo_Surface :=
        Cairo.Image_Surface.Create
          (Cairo.Image_Surface.Cairo_Format_ARGB32, 256, 64);
      Cr      : constant Cairo.Cairo_Context := Cairo.Create (Surface);
      Data    : Pixel_Pointers.Object_Pointer;
      Has_Ink : Boolean := False;
   begin
      --  Keep an owned widget reference while its borrowed Pango context is in
      --  use. The test runner initializes GTK on its private X display.
      Glib.Object.Ref_Sink (Glib.Object.GObject (Label));
      Layout.Set_Text ("Pango Cairo layout");
      Cairo.Move_To (Cr, 8.0, 8.0);
      Pango.Cairo.Show_Layout (Cr, Layout);
      Cairo.Surface.Flush (Surface);

      Assert_True (Cairo.Status (Cr) = Cairo.Cairo_Status_Success);
      Assert_Cmpint_Eq (Cairo.Image_Surface.Get_Stride (Surface), 256 * 4);
      Data := Pixel_Pointers.To_Pointer
        (Cairo.Image_Surface.Get_Data_Generic (Surface));
      Assert_True (Data /= null);
      for Pixel of Data.all loop
         if Pixel.Alpha /= 0 then
            Has_Ink := True;
            exit;
         end if;
      end loop;
      Assert_True (Has_Ink);

      --  Release the Cairo handles and the owned GObject references after
      --  checking the output; the context is borrowed from the label.
      Cairo.Destroy (Cr);
      Cairo.Surface.Destroy (Surface);
      Glib.Object.Unref (Glib.Object.GObject (Layout));
      Glib.Object.Unref (Glib.Object.GObject (Label));
   end Test_Show_Layout;

   ----------------------------------
   -- Test_Set_Default_Font_Type --
   ----------------------------------

   --  Use the backend already selected by this platform; other Cairo font
   --  backends are not necessarily available to Pango on every system. Keep
   --  the old map alive so the test can verify the setter replaces it.
   procedure Test_Set_Default_Font_Type is
      Old_Map  : System.Address;
      New_Map  : System.Address;
      Font_Type : Cairo.Cairo_Font_Type;
   begin
      Old_Map := Get_Default_Font_Map;
      Assert_True (Old_Map /= System.Null_Address);
      Old_Map := Ref_Object (Old_Map);
      Font_Type := Get_Font_Type (Old_Map);
      Pango.Cairo.Set_Default_Font_Type (Font_Type);
      New_Map := Get_Default_Font_Map;
      Assert_True (New_Map /= System.Null_Address);
      Assert_True (New_Map /= Old_Map);
      Assert_True (Get_Font_Type (New_Map) = Font_Type);
      Unref_Object (Old_Map);
   end Test_Set_Default_Font_Type;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/pango-cairo/show-layout", Test_Show_Layout'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-cairo/default-font-type",
      Test_Set_Default_Font_Type'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
