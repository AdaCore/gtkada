with Glib;          use Glib;
with Gdk;           use Gdk;
with Gdk.Color;     use Gdk.Color;
with Gdk.Drawable;  use Gdk.Drawable;
with Gdk.Event;     use Gdk.Event;
with Gdk.Rectangle; use Gdk.Rectangle;
with Gdk.Types;     use Gdk.Types;
with Gtk.Widget;    use Gtk.Widget;
with Gdk.Window;    use Gdk.Window;
with Gtk.Object;    use Gtk.Object;
with Gtk.Signal;    use Gtk.Signal;

with System;

package body My_Widget is

   --  Warning: Creating a widget from scratch is a difficult matter.
   --  You should not try that unless you already know quite well how
   --  gtk works. There are a lot of issues to consider for handling
   --  signals, requesting a size, drawing the widget, ...
   --  Please have a look at the tutorial for gtk+ itself to see a
   --  brief summary of all the things to take care of.


   --  This pointer will keep a pointer to the C 'class record' for
   --  gtk. To avoid allocating memory for each widget, this may be done
   --  only once, and reused
   Class_Record : System.Address := System.Null_Address;

   --  Array of the signals created for this widget
   Signals      : Gtk.Object.Signal_Array (0 .. 1) :=
     (0 => new String'("bullseye" & Ascii.NUL),
      1 => new String'("missed" & Ascii.NUL));

   --  Note: To create a real-life widget, you would have to do more
   --  things than are done in this simple example:
   --    * The "expose_event" signal actually passes the Area to
   --      redraw, which can speed things up a lot if used correctly
   --    * You should probably use some double-buffers to avoid the
   --      flickering that is visible in this simple example
   --    * Connect a function to the "destroy" callback to take care of
   --      the finalization of the object.

   package Draw_Cb is new Void_Callback (Target_Widget_Record);
   package Size_Cb is new Record_Callback
     (Target_Widget_Record, Gtk.Widget.Gtk_Requisition);
   package Allocation_Cb is new Record_Callback
     (Target_Widget_Record, Gtk.Widget.Gtk_Allocation);
   package Button_Cb is new Two_Callback
     (Target_Widget_Record, Integer, Gdk.Event.Gdk_Event_Button);

   ---------------
   -- Draw_Target --
   ---------------


   procedure Draw_Target (Widget  : access Target_Widget_Record)
     --  This function is called when we need to redraw the widget (for
     --  instance whenever part of it has been cleared
   is
      Width, Height : Gint;
      Win  : Gdk.Window.Gdk_Window := Get_Window (Widget);
   begin
      if not Is_Created (Widget.GC_In) then
         declare
            Color : Gdk_Color;
         begin
            Color := Gdk.Color.Parse ("Red");
            Gdk.Color.Alloc (Gtk.Widget.Get_Default_Colormap, Color);

            Gdk.Gc.Gdk_New (Widget.GC_In, Win);
            Gdk.Gc.Set_Foreground (Widget.GC_In, Color);

            Color := Gdk.Color.Parse ("Blue");
            Gdk.Color.Alloc (Gtk.Widget.Get_Default_Colormap, Color);

            Gdk.Gc.Gdk_New (Widget.GC_Out, Win);
            Gdk.Gc.Set_Foreground (Widget.GC_Out, Color);
         end;
      end if;

      Gdk.Window.Get_Size (Win, Width, Height);
      Gdk.Drawable.Draw_Arc
        (Win, Widget.Gc_Out,
         Filled => True,
         X      => (Width - Widget.Radius) / 2,
         Y      => (Height - Widget.Radius) / 2,
         Width  => Widget.Radius,
         Height => Widget.Radius,
         Angle1 => 0,
         Angle2 => 360*64);
      Gdk.Drawable.Draw_Arc
        (Win, Widget.Gc_In,
         Filled => True,
         X      => (Width - Widget.Radius / 2) / 2,
         Y      => (Height - Widget.Radius / 2) / 2,
         Width  => Widget.Radius / 2,
         Height => Widget.Radius / 2,
         Angle1 => 0,
         Angle2 => 360*64);
   end Draw_Target;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request (Widget      : access Target_Widget_Record;
                           Requisition : in out Gtk.Widget.Gtk_Requisition)
     --  This function is called by gtk+ when the widget is realized.
     --  It should modify Requisition to ask for a appropriate size for
     --  the widget. Note that the widget will not necessary have that size,
     --  it could be bigger, depending on what it is contained into.
     --  See the signal "size_allocate" too.
   is
   begin
      Requisition.Width := Widget.Radius;
      Requisition.Height := Widget.Radius;

      --  Stop the signal from being propagated to the parent's default
      --  size_request function
      Gtk.Signal.Emit_Stop_By_Name (Widget, "size_request");
   end Size_Request;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate (Widget     : access Target_Widget_Record;
                            Allocation : in out Gtk.Widget.Gtk_Allocation)
     --  This function is called once gtk has decided what size and position
     --  the widget will actually have, or everytime the widget is resized.
     --  This would be a good time for instance for resizing the component
     --  sub-widgets.
     --  Note that we have to move and resize the widget ourselves, and that
     --  for such a simple case, we could simply rely on the ancestor's
     --  size_allocation function.
   is
   begin
      if Realized_Is_Set (Widget) then
         Widget.Radius
           := Gint (Guint'Min (Allocation.Width, Allocation.Height));
         Gdk.Window.Move_Resize (Get_Window (Widget),
                                 Allocation.X, Allocation.Y,
                                 Gint (Allocation.Width),
                                 Gint (Allocation.Height));
      end if;
      Gtk.Signal.Emit_Stop_By_Name (Widget, "size_allocate");
   end Size_Allocate;

   -------------
   -- Clicked --
   -------------

   procedure Clicked (Widget : access Target_Widget_Record;
                      Event  : in Gdk_Event_Button;
                      Dummy  : Integer)
     -- called when the mouse is clicked within the widget
   is
      Tmp_X, Tmp_Y : Gint;
      Width, Height : Gint;
   begin
      Gdk.Window.Get_Size (Get_Window (Widget), Width, Height);
      Tmp_X := Gint (Get_X (Event)) - Width / 2;
      Tmp_Y := Gint (Get_Y (Event)) - Height / 2;

      --  The signal emitted depends on where the user has clicked.
      if Tmp_X * Tmp_X + Tmp_Y * Tmp_Y
        <= Widget.Radius * Widget.Radius / 4
      then
         Gtk.Signal.Emit_By_Name (Widget, "bullseye");
      else
         Gtk.Signal.Emit_By_Name (Widget, "missed");
      end if;
   end Clicked;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Target_Widget) is
      --  Used to create a new widget
   begin
      Widget := new Target_Widget_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Target_Widget_Record) is
      Id : Guint;
   begin
      --  We need to call the ancestor's Initialize function to create
      --  the underlying C object.
      Gtk.Drawing_Area.Initialize
        (Gtk.Drawing_Area.Gtk_Drawing_Area_Record (Widget.all)'Access);

      --  The following call is required to initialize the class record,
      --  and the new signals created for this widget.
      --  Note that for now you can only create basic signals (whose
      --  callbacks do not have any parameters), and that you just have
      --  to put their name in a table.
      --  Note also that we keep Class_Record, so that the memory allocation
      --  is done only once.
      Gtk.Object.Initialize_Class_Record (Widget, Signals, Class_Record);

      --  Note: We can not create the GC here, since the widget is not
      --  realized yet, and thus has no window available. This could be
      --  done be using the "realize" signal, if we really want to be clean,
      --  here we just do it in the redraw function (a little bit slower).

      Widget.Radius := 60;

      --  We want to get Button_Release events
      Set_Events (Widget, Exposure_Mask or Button_Release_Mask);

      --  Set up the appropriate callbacks to redraw, ...
      Id := Draw_Cb.Connect (Widget, "expose_event", Draw_Target'Access, True);
      Id := Size_Cb.Connect (Widget, "size_request", Size_Request'Access);
      Id := Button_Cb.Connect (Widget, "button_release_event",
                               Clicked'Access, 0);
      Id := Allocation_Cb.Connect (Widget, "size_allocate",
                                   Size_Allocate'Access);
   end Initialize;


end My_Widget;
