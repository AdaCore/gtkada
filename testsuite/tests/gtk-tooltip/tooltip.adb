--  Exercises Gtk.Tooltip's setters from the GtkWidget::query-tooltip signal.
--  GTK's C testsuite has no direct GtkTooltip test to port.

with Ada.Command_Line;
with Gdk;
with Gdk.Display;
with Gdk.Paintable;
with Gdk.Pixbuf;          use Gdk.Pixbuf;
with Gdk.Rectangle;       use Gdk.Rectangle;
with Gdk.Texture;         use Gdk.Texture;
with Glib;
with Glib.G_Icon;
with Glib.Main;           use Glib.Main;
with Glib.Object;         use Glib.Object;
with Glib.Test;           use Glib.Test;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;
with Gtk.Native;
with Gtk.Tooltip;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Window;          use Gtk.Window;
with Gtkada.Types;        use Gtkada.Types;
with Interfaces.C;
with System;

procedure Tooltip is
   use type Gtk.Widget.Gtk_Widget;
   use type Gdk.Paintable.Gdk_Paintable;
   use type Glib.G_Icon.G_Icon;
   use type System.Address;

   --  GTK creates GtkTooltip internally and passes it only to this callback.
   --  Keep the source and contents alive while the callback configures it.
   Expected_Widget : Gtk_Widget;
   Custom_Widget   : Gtk_Widget;
   Paintable       : Gdk.Paintable.Gdk_Paintable;
   Icon            : Glib.G_Icon.G_Icon;
   Query_Calls     : Natural := 0;
   Wait_Done       : Boolean := False;
   pragma Volatile (Wait_Done);

   procedure G_Object_Unref (Object : Glib.G_Icon.G_Icon);
   pragma Import (C, G_Object_Unref, "g_object_unref");

   --  The test driver uses Xvfb. Warping to the native surface's local
   --  coordinates makes the tooltip query deterministic without real input.
   function X11_Display (Display : System.Address) return System.Address;
   pragma Import (C, X11_Display, "gdk_x11_display_get_xdisplay");
   function X11_Surface (Surface : System.Address)
     return Interfaces.C.unsigned_long;
   pragma Import (C, X11_Surface, "gdk_x11_surface_get_xid");
   function X_Warp_Pointer
     (Display              : System.Address;
      Source_Window        : Interfaces.C.unsigned_long;
      Destination_Window   : Interfaces.C.unsigned_long;
      Source_X, Source_Y   : Interfaces.C.int;
      Source_Width         : Interfaces.C.unsigned;
      Source_Height        : Interfaces.C.unsigned;
      Destination_X        : Interfaces.C.int;
      Destination_Y        : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, X_Warp_Pointer, "XWarpPointer");
   function X_Flush (Display : System.Address) return Interfaces.C.int;
   pragma Import (C, X_Flush, "XFlush");

   function Themed_Icon_New
     (Name : Chars_Ptr) return Glib.G_Icon.G_Icon;
   pragma Import (C, Themed_Icon_New, "g_themed_icon_new");

   function Stop_Waiting return Boolean;
   function On_Query_Tooltip
     (Self          : access Gtk_Widget_Record'Class;
      X, Y          : Glib.Gint;
      Keyboard_Mode : Boolean;
      Tooltip       : not null access Glib.Object.GObject_Record'Class)
      return Boolean;
   procedure Test_Setters with Convention => C;

   function Stop_Waiting return Boolean is
   begin
      Wait_Done := True;
      return False;
   end Stop_Waiting;

   --  GtkTooltip has no public getters. Check the real callback arguments and
   --  custom-widget parent changes, then exercise each documented setter.
   function On_Query_Tooltip
     (Self          : access Gtk_Widget_Record'Class;
      X, Y          : Glib.Gint;
      Keyboard_Mode : Boolean;
      Tooltip       : not null access Glib.Object.GObject_Record'Class)
      return Boolean
   is
      pragma Unreferenced (X, Y, Keyboard_Mode);
      Tip : constant Gtk.Tooltip.Gtk_Tooltip :=
        Gtk.Tooltip.Gtk_Tooltip (Tooltip);
   begin
      Assert_True (Get_Object (Self) = Get_Object (Expected_Widget));
      Assert_Nonnull (Get_Object (Tooltip));

      --  Custom content can be installed and removed without destroying it.
      Gtk.Tooltip.Set_Custom (Tip, Custom_Widget);
      Assert_Nonnull (Get_Object (Get_Parent (Custom_Widget)));
      Gtk.Tooltip.Set_Custom (Tip, null);
      Assert_True (Get_Parent (Custom_Widget) = null);

      --  Paintable and GIcon forms accept both real values and null to hide
      --  the image; named icons also exercise the empty-name clearing path.
      Gtk.Tooltip.Set_Icon (Tip, Paintable);
      Gtk.Tooltip.Set_Icon (Tip, Gdk.Paintable.Null_Gdk_Paintable);
      Gtk.Tooltip.Set_Icon_From_Gicon (Tip, Icon);
      Gtk.Tooltip.Set_Icon_From_Gicon (Tip, null);
      Gtk.Tooltip.Set_Icon_From_Icon_Name (Tip, "image-missing");
      Gtk.Tooltip.Set_Icon_From_Icon_Name (Tip, "");

      --  Non-empty and empty strings cover setting and hiding both label
      --  forms; Set_Tip_Area records the widget-coordinate hit region.
      Gtk.Tooltip.Set_Markup (Tip, "<b>tooltip markup</b>");
      Gtk.Tooltip.Set_Markup (Tip, "");
      Gtk.Tooltip.Set_Text (Tip, "tooltip text");
      Gtk.Tooltip.Set_Text (Tip, "");
      Gtk.Tooltip.Set_Tip_Area
        (Tip, (X => 0, Y => 0, Width => 40, Height => 20));

      Query_Calls := Query_Calls + 1;
      return True;
   end On_Query_Tooltip;

   procedure Test_Setters is
      Window : constant Gtk_Window := Gtk_Window_New;
      Source : constant Gtk_Label := Gtk_Label_New ("tooltip source");
      Pixbuf : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 2, 2);
      Texture : constant Gdk_Texture := Gdk_Texture_New_For_Pixbuf (Pixbuf);
      Name : Chars_Ptr := New_String ("image-missing");
   begin
      --  Create a valid paintable and GIcon for the tooltip setters. The test
      --  keeps its own references through the query callback.
      Pixbuf.Fill (16#336699FF#);
      Paintable := +Texture;
      Icon := Themed_Icon_New (Name);
      Free (Name);
      Assert_True (Icon /= null);

      Custom_Widget := Gtk_Widget (Gtk_Label_New ("custom tooltip content"));
      Ref_Sink (Custom_Widget);
      Expected_Widget := Gtk_Widget (Source);
      Source.Set_Has_Tooltip (True);
      Source.On_Query_Tooltip (On_Query_Tooltip'Unrestricted_Access);
      Window.Set_Child (Source);
      Window.Set_Default_Size (180, 60);
      Window.Present;

      --  A tooltip query requires a mapped source widget. The timeout keeps a
      --  failed map or query from hanging the unattended test run.
      Wait_Done := False;
      declare
         Timeout : constant G_Source_Id := Timeout_Add
           (5000, Stop_Waiting'Unrestricted_Access);
         pragma Unreferenced (Timeout);
      begin
         while not Get_Mapped (Source) and not Wait_Done loop
            declare
               Dispatched : constant Boolean := Main_Context_Iteration (null, True);
               pragma Unreferenced (Dispatched);
            begin
               null;
            end;
         end loop;
      end;
      Assert_True (Get_Mapped (Source));

      --  GTK only emits query-tooltip for the widget under the pointer. Move
      --  the Xvfb pointer into the window's surface before requesting a query.
      declare
         Display : constant Gdk.Display.Gdk_Display := Source.Get_Display;
         Native  : constant Gtk.Native.Gtk_Native := Get_Native (Source);
         Surface : constant Gdk.Gdk_Surface := Gtk.Native.Get_Surface (Native);
         Xdisplay : constant System.Address := X11_Display (Get_Object (Display));
         Xid      : constant Interfaces.C.unsigned_long :=
           X11_Surface (Get_Object (Surface));
         Warped   : constant Interfaces.C.int :=
           X_Warp_Pointer
             (Xdisplay, 0, Xid, 0, 0, 0, 0, 5, 5);
         Flushed  : constant Interfaces.C.int := X_Flush (Xdisplay);
         pragma Unreferenced (Warped, Flushed);
      begin
         null;
      end;

      --  GTK creates GtkTooltip internally and emits query-tooltip for the
      --  pointer over the mapped source widget.
      Source.Trigger_Tooltip_Query;
      Wait_Done := False;
      declare
         Timeout : constant G_Source_Id := Timeout_Add
           (5000, Stop_Waiting'Unrestricted_Access);
         pragma Unreferenced (Timeout);
      begin
         while Query_Calls = 0 and not Wait_Done loop
            declare
               Dispatched : constant Boolean := Main_Context_Iteration (null, True);
               pragma Unreferenced (Dispatched);
            begin
               null;
            end;
         end loop;
      end;
      Assert_Cmpuint_Eq (Glib.Guint (Query_Calls), 1);

      Window.Destroy;
      Unref (Custom_Widget);
      G_Object_Unref (Icon);
      Unref (Texture);
      Unref (Pixbuf);
   end Test_Setters;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gtk-tooltip/setters", Test_Setters'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Tooltip;
