with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.File_Selection; use Gtk.File_Selection;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

with Ada.Text_IO;

package body Create_File_Selection is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Files_Cb is new Signal.Object_Callback (Gtk_File_Selection);

   Window : Gtk_File_Selection;

   procedure Ok (Files : in out Gtk_File_Selection'Class) is
   begin
      Ada.Text_IO.Put_Line ("Selected " & Get_Filename (Files));
      Gtk.Widget.Destroy (Files);
   end Ok;


   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id     : Guint;
      Button : Gtk_Button;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Title => "File Selection Dialog");
         Hide_Fileop_Buttons (Window);
         Position (Window, Win_Pos_Mouse);
         Id := Widget_Cb.Connect (Window, "destroy", Gtk.Widget.Destroy'Access,
                                  Window);
         Id := Files_Cb.Connect (Get_Ok_Button (Window), "clicked",
                                 Ok'Access, Window);
         Id := Widget_Cb.Connect (Get_Cancel_Button (Window), "clicked",
                                  Gtk.Widget.Destroy'Access, Window);

         Gtk_New (Button, Label => "Hide Fileops");
         Id := Files_Cb.Connect (Button, "clicked", Hide_Fileop_Buttons'Access,
                                 Window);
         Pack_Start (Get_Action_Area (Window), Button, False, False, 0);
         Show (Button);

         Gtk_New (Button, Label => "Show Fileops");
         Id := Files_Cb.Connect (Button, "clicked", Show_Fileop_Buttons'Access,
                                 Window);
         Pack_Start (Get_Action_Area (Window), Button, False, False, 0);
         Show (Button);

      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_File_Selection;

