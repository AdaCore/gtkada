with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Object; use Gtk.Object;
with Gtk.Option_Menu; use Gtk.Option_Menu;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

package body Create_Menu is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);

   Window : Gtk_Window;

   function Create_Menu (Depth : Integer) return Gtk_Menu is
      Menu      : Gtk_Menu;
      Group     : Widget_SList.GSlist;
      Menu_Item : Gtk_Radio_Menu_Item;
   begin
      if Depth < 1 then
         return Menu;
      end if;

      Gtk_New (Menu);

      for I in 0 .. 5 loop
         Gtk_New (Menu_Item, Group, "Item" & Integer'Image (Depth)
                  & " -" & Integer'Image (I + 1));
         Group := Gtk.Radio_Menu_Item.Group (Menu_Item);
         if Depth mod 2 /= 0 then
            Set_Show_Toggle (Menu_Item, True);
         end if;
         Append (Menu, Menu_Item);
         Show (Menu_Item);
         if I = 3 then
            Set_Sensitive (Menu_Item, False);
         end if;
         Set_Submenu (Menu_Item, Create_Menu (Depth - 1));
      end loop;
      return Menu;
   end Create_Menu;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id   : Guint;
      Box1 : Gtk_Box;
      Box2 : Gtk_Box;
      Menu_Bar  : Gtk_Menu_Bar;
      Menu : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
      Option_Menu : Gtk_Option_Menu;
      Separator   : Gtk_Separator;
      Button : Gtk_Button;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Set_Title (Window, "Menus");
         Border_Width (Window, 0);
         Id := Widget_Cb.Connect (Window, "destroy", Destroy'Access, Window);
         Id := Widget_Cb.Connect (Window, "delete_event", Destroy'Access,
                                  Window);
         -- FIXME : the previous line should use Gtk_True

         Gtk_New_Vbox (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New (Menu_Bar);
         Pack_Start (Box1, Menu_Bar, False, True, 0);
         Show (Menu_Bar);

         Menu := Create_Menu (2);

         Gtk_New (Menu_Item, "test" & Ascii.LF & "line2");
         Set_Submenu (Menu_Item, Menu);
         Append (Menu_Bar, Menu_Item);
         Show (Menu_Item);

         Gtk_New (Menu_Item, "foo");
         Set_Submenu (Menu_Item, Create_Menu (3));
         Append (Menu_Bar, Menu_Item);
         Show (Menu_Item);

         Gtk_New (Menu_Item, "bar");
         Set_Submenu (Menu_Item, Create_Menu (4));
         Append (Menu_Bar, Menu_Item);
         Show (Menu_Item);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (Option_Menu);
         Set_Menu (Option_Menu, Create_Menu (1));
         Set_History (Option_Menu, 4);
         Pack_Start (Box2, Option_Menu, True, True, 0);
         Show (Option_Menu);

         Gtk_New_Hseparator (Separator);
         Pack_Start (Box1, Separator, True, True, 0);
         Show (Separator);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, False, True, 0);
         Show (Box2);

         Gtk_New (Button, "Close");
         Id := Widget_Cb.Connect (Button, "clicked", Destroy'Access, Window);
         Pack_Start (Box2, Button, True, True, 0);
         Set_Flags (Button, Can_Default);
         Grab_Default (Button);
         Show (Button);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Menu;

