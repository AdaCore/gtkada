with Glib; use Glib;
with Gtk; use Gtk;
with Gtk.Box;
with Gtk.Toggle_Button;
with Gtk.Separator;
with Gtk.Signal;
with Gtk.Widget;
with Gtk.Window;use Gtk.Window;

package body Create_Toggle_Buttons is

   package Exit_Cb is new Signal.Object_Callback
     (Widget_Type => Gtk.Widget.Gtk_Widget);
   --  Must be instanciated at library level !


   New_Window : Window.Gtk_Window;


   ----------------------------------------------------------------------

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Box1, Box2 : Box.Gtk_Box;
      A_Button : Button.Gtk_Button;
      A_Toggle_Button : Toggle_Button.Gtk_Toggle_Button;
      A_Separator : Separator.Gtk_Separator;
      Cb_Id : Guint;
   begin

      if not Is_Created (New_Window) then
         Window.Gtk_New (Window => New_Window,
                         The_Type => Enums.Window_Toplevel);
         Cb_Id := Exit_Cb.Connect (Obj => New_Window,
                                   Name => "destroy",
                                   Func => Gtk.Widget.Destroy'Access,
                                   Slot_Object => New_Window);
         Window.Set_Title (Window => New_Window,
                           Title => "Toggle buttons");
         Container.Border_Width (Container => New_Window,
                                 Border_Width => 0);

         Box.Gtk_New_Vbox (Widget => Box1, Homogeneous => False, Spacing => 0);
         Container.Add (Container => New_Window, Widget => Box1);
         Gtk.Widget.Show (Box1);

         Box.Gtk_New_Vbox (Widget => Box2, Homogeneous => False, Spacing => 10);
         Container.Border_Width (Container => Box2, Border_Width => 10);
         Box.Pack_Start (In_Box => Box1, Child => Box2);
         Gtk.Widget.Show (Box2);

         Toggle_Button.Gtk_New (A_Toggle_Button, "button1");
         Box.Pack_Start (In_Box => Box2, Child => A_Toggle_Button);
         Gtk.Widget.Show (A_Toggle_Button);

         Toggle_Button.Gtk_New (A_Toggle_Button, "button2");
         Box.Pack_Start (In_Box => Box2, Child => A_Toggle_Button);
         Gtk.Widget.Show (A_Toggle_Button);

         Toggle_Button.Gtk_New (A_Toggle_Button, "button3");
         Box.Pack_Start (In_Box => Box2, Child => A_Toggle_Button);
         Gtk.Widget.Show (A_Toggle_Button);

         Gtk.Separator.Gtk_New_Hseparator (A_Separator);
         Box.Pack_Start (In_Box => Box1, Child => A_Separator,
                         Expand => False);
         Gtk.Widget.Show (A_Separator);

         Box.Gtk_New_Vbox (Widget => Box2, Homogeneous => False, Spacing => 10);
         Container.Border_Width (Container => Box2, Border_Width => 10);
         Box.Pack_Start (In_Box => Box1, Child => Box2, Expand => False);
         Gtk.Widget.Show (Box2);

         Button.Gtk_New (Widget => A_Button, Label => "close");
         Cb_Id := Exit_Cb.Connect (Obj => A_Button,
                                   Name => "clicked",
                                   Func => Gtk.Widget.Destroy'Access,
                                   Slot_Object => New_Window);
         Box.Pack_Start (In_Box => Box2, Child => A_Button);
         Object.Set_Flags (Object => A_Button,
                           Flags => Gtk.Widget.Can_Default);
         Gtk.Widget.Grab_Default (A_Button);
         Gtk.Widget.Show (A_Button);

      end if;

      if not Gtk.Widget.Visible_Is_Set (New_Window) then
         Gtk.Widget.Show (New_Window);
      else
         Gtk.Widget.Destroy (New_Window);
      end if;

   end Run;

end Create_Toggle_Buttons;
