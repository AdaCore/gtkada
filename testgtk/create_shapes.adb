with Glib; use Glib;
with Gdk.Bitmap; use Gdk.Bitmap;
with Gdk.Pixmap; use Gdk.Pixmap;
with Gdk.Window; use Gdk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Fixed; use Gtk.Fixed;
with Gtk.Hseparator; use Gtk.Hseparator;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Table; use Gtk.Table;
with Gtk.Vbox; use Gtk.Vbox;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

package body Create_Shapes is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Win_Cb is new Signal.Two_Callback (Gtk_Window, Gint,
                                              Gdk_Event_Button);
   package Win_Cb2 is new Signal.Void_Callback (Gtk_Window);
   package Win_Cb3 is new Signal.Two_Callback (Gtk_Window, Gint,
                                               Gdk_Event_Motion);

   Modeller : Gtk_Window;
   Sheets   : Gtk_Window;
   Rings    : Gtk_Window;
   Root_Win : Gdk_Window;

   type Cursor_Offset is
      record
         X : Gint;
         Y : Gint;
      end record;
   type Cursor_Access is access all Cursor_Offset;
   Global_Cursor : Cursor_Offset;
   package User is new User_Data (Cursor_Access);

   procedure Shape_Pressed (Window : in out Gtk_Window'Class;
                            Event  : in out Gdk_Event_Button;
                            Dummy  : in out Gint)
   is
      Cursor : Cursor_Access;
   begin
      --  ignore double and triple click
      if (Get_Type (Event) /= Button_Press) then
         return;
      end if;

      Cursor := User.Get (Window);
      Cursor.X := Get_X (Event);
      Cursor.Y := Get_Y (Event);

      Grab_Add (Window);
      Pointer_Grab (Get_Window (Window), True,
                    Button_Release_Mask + Button_Motion_Mask
                    + Pointer_Motion_Hint_Mask,
                    null, null, 0);
   end Shape_Pressed;

   procedure Shape_Released (Window : in out Gtk_Window'Class) is
   begin
      Grab_Remove (Widget);
      Pointer_Ungrab (0);
   end Shape_Released;

   procedure Shape_Motion (Window : in out Gtk_Window'Class;
                           Event  : in out Gdk_Event_Motion)
   is
      Cursor : Cursor_Access;
      Xp, Yp : Gint;
      Mask   : Gdk_Modifier_Type;
   begin
      Cursor := User.Get (Window);
      --  We can't use Event->x and Event->Y since we need absolute
      --  coordinates
      Get_Pointer (Root_Win, Xp, Yp, Mask);
      Set_Uposition (Window, Xp - Cursor.X, Yp - Cursor.Y);
   end Shape_Motion;

   function Shape_Create_Icon (Xpm_File    : in String;
                               X           : in Gint;
                               Y           : in Gint;
                               Px          : in Gint;
                               Py          : in Gint;
                               Window_Type : in Gtk_Window_Type)
                               return Gtk_Window
   is
      Window    : Gtk_Window;
      Style     : Gtk_Style;
      GC        : Gdk_GC;
      Fixed     : Gtk_Fixed;
      GdkPixmap : Gdk_Pixmap;
      Pixmap    : Gtk_Pixmap;
      Mask      : Gdk_Bitmap;
   begin
      Style := Get_Default_Style;
      GC    := Get_Black_GC (Style);

      Gtk_New (Window, Window_Type);

      Gtk_New (Fixed);
      Set_Usize (Fixed, 100, 100);
      Add (Window, Fixed);
      Show (Fixed);

      Set_Events (Window, Get_Events (Window)
                  + Button_Motion_Mask
                  + Pointer_Motion_Hint_Mask
                  + Button_Press_Mask);
      Realize (Window);

      Create_From_Xpm (GdkPixmap, Get_Window (Window), Mask,
                       Get_Bg (Style, State_Normal), Xpm_File);
      Gtk_New (Pixmap, GdkPixmap, Mask);
      Put (Fixed, Pixmap, Px, Py);
      Show (Pixmap);

      Shape_Combine_Mask (Window, Mask, Px, Py);
      Id := Win_Cb.Connect (Window, "button_press_event",
                            Shape_Pressed'Access);
      Id := Win_Cb2.Connect (Window, "button_realease_event",
                             Shape_Released'Access);
      Id := Win_Cb3.Connect (Window, "motion_notify_event",
                             Shape_Motion'Access);
      User.Set (Window, Global_Cursor'Access);
      Set_Uposition (Window, X, Y);
      Show (Window);
      return Window;
   end Shape_Create_Icon;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id      : Guint;
   begin

      if not Is_Created (Modeller) then
         Modeller := Shape_Create_Icon ("Modeller.xpm", 440, 140, 0, 0,
                                        Popup);
         Id := Widget_Cb.Connect (Modeller, "destroy", Destroy'Access,
                                  Modeller);
      else
         Destroy (Modeller);
      end if;

      if not Is_Created (Sheets) then
         Modeller := Shape_Create_Icon ("FilesQueue.xpm", 440, 140, 0, 0,
                                        Popup);
         Id := Widget_Cb.Connect (Sheets, "destroy", Destroy'Access,
                                  Sheets);
      else
         Destroy (Sheets);
      end if;

      if not Is_Created (Rings) then
         Rings := Shape_Create_Icon ("3DRings.xpm", 440, 140, 0, 0,
                                     Popup);
         Id := Widget_Cb.Connect (Rings, "destroy", Destroy'Access,
                                  Rings);
      else
         Destroy (Rings);
      end if;
   end Run;

end Create_Shapes;

