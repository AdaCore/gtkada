with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Hbox; use Gtk.Hbox;
with Gtk.Hseparator; use Gtk.Hseparator;
with Gtk.Label; use Gtk.Label;
with Gtk.Object; use Gtk.Object;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Vbox; use Gtk.Vbox;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

with Ada.Text_IO;

package body Create_Reparent is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Box_Cb is new Signal.Callback (Gtk_Button, Gtk_Box'Class);
   package Int_Cb is new Signal.Two_Callback (Gtk_Label, Gint, Gtk_Widget);
   package Label_User is new User_Data (Gtk_Label);

   Window : Gtk.Window.Gtk_Window;

   procedure Set_Parent_Signal (Child      : in out Gtk_Label'Class;
                                Old_Parent : in out Gtk_Widget;
                                Data       : in out Gint)
   is
   begin
      Ada.Text_IO.Put ("Set_Parent for ");
      if Is_Created (Child) then
         Ada.Text_IO.Put (Type_Name (Get_Type (Child))
                          & " : new parent : ");
         if Is_Created (Get_Parent (Child)) then
            Ada.Text_IO.Put (Type_Name (Get_Type (Get_Parent (Child))));
         else
            Ada.Text_IO.Put ("NULL");
         end if;
      else
         Ada.Text_IO.Put ("NULL ");
      end if;
      Ada.Text_IO.Put ("  old parent : ");
      if Is_Created (Old_Parent) then
         Ada.Text_IO.Put (Type_Name (Get_Type (Old_Parent)));
      else
         Ada.Text_IO.Put ("NULL");
      end if;
      Ada.Text_IO.Put_Line (" data = " & Gint'Image (Data));
   end Set_Parent_Signal;

   procedure Reparent_Label (Widget     : in out Gtk_Button'Class;
                             New_Parent : in out Gtk_Box'Class)
   is
      Label : Gtk_Label;
   begin
      Label := Label_User.Get (Widget);
      Reparent (Label, New_Parent);
   end Reparent_Label;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id        : Guint;
      Box1      : Gtk_Vbox;
      Box2      : Gtk_Hbox;
      Box3      : Gtk_Vbox;
      Label     : Gtk_Label;
      Frame     : Gtk_Frame;
      Button    : Gtk_Button;
      Separator : Gtk_HSeparator;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget_Cb.Connect (Window, "destroy", Destroy'Access, Window);
         Set_Title (Window, "reparent");
         Border_Width (Window, Border_Width => 0);

         Gtk_New (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New (Box2, False, 5);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (Label, "hello world");

         Gtk_New (Frame, "Frame 1");
         Pack_Start (Box2, Frame, True, True, 0);
         Show (Frame);
         Gtk_New (Box3, False, 5);
         Border_Width (Box3, 5);
         Add (Frame, Box3);
         Show (Box3);
         Gtk_New (Button, "switch");
         Id := Box_Cb.Connect (Button, "clicked", Reparent_Label'Access, Box3);
         Label_User.Set (Button, Label);
         Pack_Start (Box3, Button, False, True, 0);
         Show (Button);

         Pack_Start (Box3, Label, False, True, 0);
         Id := Int_Cb.Connect (Label, "parent_set", Set_Parent_Signal'Access,
                               42);
         Show (Label);

         Gtk_New (Frame, "Frame 2");
         Pack_Start (Box2, Frame, True, True, 0);
         Show (Frame);
         Gtk_New (Box3, False, 5);
         Border_Width (Box3, 5);
         Add (Frame, Box3);
         Show (Box3);
         Gtk_New (Button, "switch");
         Id := Box_Cb.Connect (Button, "clicked", Reparent_Label'Access, Box3);
         Label_User.Set (Button, Label);
         Pack_Start (Box3, Button, False, True, 0);
         Show (Button);

         Gtk_New (Separator);
         Pack_Start (Box1, Separator, False, True, 0);
         Show (Separator);

         Gtk_New (Box3, False, 10);
         Border_Width (Box3, 10);
         Pack_Start (Box1, Box3, False, True, 0);
         Show (Box3);

         Gtk_New (Button, "close");
         Id := Widget_Cb.Connect (Button, "clicked", Destroy'Access, Window);
         Pack_Start (Box3, Button, True, True, 0);
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

end Create_Reparent;

