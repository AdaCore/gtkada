with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Hseparator; use Gtk.Hseparator;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Object; use Gtk.Object;
with Gtk.Vbox; use Gtk.Vbox;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Interfaces.C.Strings;

package body Create_Entry is

   package Entry_Cb is new Signal.Callback (Gtk_Entry, Gtk_Check_Button);
   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);

   Window : Gtk_Window;

   procedure Toggle_Editable (Button : in out Gtk_Check_Button'Class;
                              The_Entry : in out Gtk_Entry)
   is
   begin
      Set_Editable (The_Entry, Is_Active (Button));
   end Toggle_Editable;



   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      package ICS renames Interfaces.C.Strings;
      use Gtk.Combo.String_List;

      List      : Glist;
      Id        : Guint;
      Box1,
        Box2    : Gtk_Vbox;
      The_Entry : Gtk_Entry;
      Combo     : Gtk_Combo;
      Check     : Gtk_Check_Button;
      Separator : Gtk_HSeparator;
      Button    : Gtk_Button;
   begin
      if not Is_Created (Window) then

         Append (List, ICS.New_String ("item0"));
         Append (List, ICS.New_String ("item1 item1"));
         Append (List, ICS.New_String ("item2 item2 item2"));
         Append (List, ICS.New_String ("item3 item3 item3 item3"));
         Append (List, ICS.New_String ("item4 item4 item4 item4 item4"));
         Append (List, ICS.New_String ("item5 item5 item5 item5 item5 item5"));
         Append (List, ICS.New_String ("item6 item6 item6 item6 item6"));
         Append (List, ICS.New_String ("item7 item7 item7 item7"));
         Append (List, ICS.New_String ("item8 item8 item8"));
         Append (List, ICS.New_String ("item9 item9"));

         Gtk_New (Window, Window_Toplevel);
         Id := Widget_Cb.Connect (Window, "destroy", Destroy'Access, Window);
         Set_Title (Window, "entry");
         Border_Width (Window, 0);

         Gtk_New (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (The_Entry);
         Set_Text (The_Entry, "Hello world");
         Gtk.GEntry.Select_Region (The_Entry, 0, -1);
         Pack_Start (Box2, The_Entry, True, True, 0);
         Show (The_Entry);

         Gtk_New (Combo);
         Set_Popdown_Strings (Combo, List);
         Set_Text (Get_Entry (Combo), "hello world");
         Gtk.GEntry.Select_Region (Get_Entry (Combo), 0, -1);
         Pack_Start (Box2, Combo, True, True, 0);
         Show (Combo);

         Gtk_New (Check, "Editable");
         Pack_Start (Box2, Check, False, True, 0);
         Id := Entry_Cb.Connect (Check, "toggled", Toggle_Editable'Access,
                                 The_Entry);
         Set_State (Check, True);
         Show (Check);

         Gtk_New (Separator);
         Pack_Start (Box1, Separator, False, True, 0);
         Show (Separator);

         Gtk_New (Box2, False, 10);
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

      if not Visible_Is_Set (Window) then
         Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;
   end Run;

end Create_Entry;

