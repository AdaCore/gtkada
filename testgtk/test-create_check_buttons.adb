with Gtk.Check_Button;

separate (Test)

procedure Create_Check_Buttons (Widget : in out Button.Gtk_Button'Class) is
   New_Window : Window.Gtk_Window renames Check_Buttons_Window;
   Box1, Box2 : Vbox.Gtk_Vbox;
   A_Button : Button.Gtk_Button;
   A_Check_Button : Check_Button.Gtk_Check_Button;
   A_Separator : HSeparator.Gtk_HSeparator;
   Cb_Id : Guint;
begin
   if not Is_Created (New_Window) then
      Window.Gtk_New (Window => New_Window,
                      The_Type => Enums.Window_Toplevel);
      Cb_Id := Do_Exit_Callback.Connect (Obj => New_Window,
                                         Name => "destroy",
                                         Func => Do_Exit'Access,
                                         Func_Data => New_Window);
      Window.Set_Title (Window => New_Window,
                        Title => "check buttons");
      Container.Border_Width (Container => New_Window,
                              Border_Width => 0);

      Vbox.Gtk_New (Widget => Box1, Homogeneous => False, Spacing => 0);
      Container.Add (Container => New_Window, Widget => Box1);
      Gtk.Widget.Show (Box1);

      Vbox.Gtk_New (Widget => Box2, Homogeneous => False, Spacing => 10);
      Container.Border_Width (Container => Box2, Border_Width => 10);
      Box.Pack_Start (In_Box => Box1, Child => Box2);
      Gtk.Widget.Show (Box2);

      Check_Button.Gtk_New (Widget => A_Check_Button, With_Label => "button1");
      Box.Pack_Start (In_Box => Box2, Child => A_Check_Button);
      Gtk.Widget.Show (A_Check_Button);

      Check_Button.Gtk_New (Widget => A_Check_Button, With_Label => "button2");
      Box.Pack_Start (In_Box => Box2, Child => A_Check_Button);
      Gtk.Widget.Show (A_Check_Button);

      Check_Button.Gtk_New (Widget => A_Check_Button, With_Label => "button3");
      Box.Pack_Start (In_Box => Box2, Child => A_Check_Button);
      Gtk.Widget.Show (A_Check_Button);

      HSeparator.Gtk_New (A_Separator);
      Box.Pack_Start (In_Box => Box1, Child => A_Separator, Expand => False);
      Gtk.Widget.Show (A_Separator);

      Vbox.Gtk_New (Widget => Box2, Homogeneous => False, Spacing => 10);
      Container.Border_Width (Container => Box2, Border_Width => 10);
      Box.Pack_Start (In_Box => Box1, Child => Box2, Expand => False);
      Gtk.Widget.Show (Box2);

      Button.Gtk_New (Widget => A_Button, Label => "close");
      Cb_Id := Do_Exit_Callback.Connect (Obj => A_Button,
                                         Name => "clicked",
                                         Func => Do_Exit'Access,
                                         Func_Data => New_Window);
      Box.Pack_Start (In_Box => Box2, Child => A_Button);
      Object.Set_Flags (Object => A_Button, Flags => Gtk.Widget.Can_Default);
      Gtk.Widget.Grab_Default (A_Button);
      Gtk.Widget.Show (A_Button);

   end if;

   if not Gtk.Widget.Visible_Is_Set (New_Window) then
      Gtk.Widget.Show (New_Window);
   else
      Gtk.Widget.Destroy (New_Window);
   end if;

end Create_Check_Buttons;
