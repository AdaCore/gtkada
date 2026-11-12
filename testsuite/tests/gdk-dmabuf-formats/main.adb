--  Exercise Gdk.Dmabuf_Formats through the formats advertised by the display.
--  There is no matching C testsuite case for this boxed type. The headless
--  display may advertise an empty list, so indexed reads are conditional.

with Ada.Command_Line;
with Ada.Environment_Variables;
with Gdk.Display;
with Gdk.Dmabuf_Formats;
with Glib;
with Glib.Test;
with Gtk.Main;

procedure Main is
   use Glib.Test;
   use Gdk.Dmabuf_Formats;
   use type Glib.Gsize;
   use type Glib.Guint32;
   use type Glib.Guint64;

   procedure Test_Formats with Convention => C;

   --  Check the borrowed display value, the boxed reference helpers, and
   --  every format pair that this backend reports.
   procedure Test_Formats is
      Display : constant Gdk.Gdk_Display := Gdk.Display.Get_Default;
      Formats : constant Gdk_Dmabuf_Formats :=
        Gdk.Display.Get_Dmabuf_Formats (Display);
      Count : Glib.Gsize;
      Retained : Gdk_Dmabuf_Formats;
      Fourcc : Glib.Guint32;
      Modifier : Glib.Guint64;
   begin
      --  A display owns a formats object even when the backend has no
      --  dma-buf formats to advertise.
      Assert_True (not Glib.Is_Null (Formats));
      Count := Get_N_Formats (Formats);
      Assert_True (Equal (Formats, Formats));

      --  Ref returns an owned reference; releasing it must leave the
      --  display's borrowed reference usable.
      Retained := Ref (Formats);
      Assert_True (Equal (Formats, Retained));
      Assert_True (Get_N_Formats (Retained) = Count);
      Unref (Retained);
      Assert_True (Get_N_Formats (Formats) = Count);

      --  Read every valid index and verify the returned pair is contained.
      --  A zero count is valid on backends without dma-buf support.
      if Count > 0 then
         for Index in 0 .. Count - 1 loop
            Get_Format (Formats, Index, Fourcc, Modifier);
            Assert_True (Contains (Formats, Fourcc, Modifier));
         end loop;
      end if;

      --  Check a format code outside the DRM fourcc namespace. It remains
      --  unsupported even when the display's list is empty.
      Assert_False (Contains (Formats, 0, 0));
   end Test_Formats;

begin
   Glib.Test.Init;
   --  The getter may initialize GL or Vulkan. The Xvfb test display has no
   --  graphics stack, so disable those optional providers before GDK starts.
   Ada.Environment_Variables.Set ("GDK_DISABLE", "gl,vulkan");
   Gtk.Main.Init;
   Glib.Test.Add_Func ("/gdk/dmabuf-formats", Test_Formats'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
