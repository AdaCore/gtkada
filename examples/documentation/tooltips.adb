--  This example demonstrates how you can change the color scheme used
--  for tooltips.
--  This is of course done through styles. However, you can not directly
--  associate a Gtk_Tooltips with a style, so you have to do the following.

--  Note also that this choice should probably left to the user, who can
--  modify it through a RC file that contains the following:
--      style "postie"
--      {
--         bg[NORMAL]={1.0, 0.93, 0.22}
--      }
--      widget "gtk-tooltips*" style "postie"

with Gtk.Tooltips, Gtk.Style, Gtk.Enums, Gdk.Color;
use Gtk.Tooltips, Gtk.Style, Gtk.Enums, Gdk.Color;

procedure Tooltips is
   Style : Gtk_Style;
   Tips  : Gtk_Tooltips;
   Color : Gdk_Color;
begin
   Gtk_New (Style);

   --  blue foreground
   Set_Rgb (Color, 255, 255, 65535);
   Alloc (Gdk.Color.Get_System, Color);
   Set_Foreground (Style, State_Normal, Color);

   --  green background
   Set_Rgb (Color, 255, 65535, 255);
   Alloc (Gdk.Color.Get_System, Color);
   Set_Background (Style, State_Normal, Color);

   --  temporarily change the default style while creating the tooltips.
   Push_Style (Style);
   Gtk_New (tips);
   Force_Window (Tips);
   Pop_Style;

end Tooltips;
