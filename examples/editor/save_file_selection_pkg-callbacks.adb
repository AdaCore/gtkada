with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with File_Utils; use File_Utils;

package body Save_File_Selection_Pkg.Callbacks is

   use Gtk.Arguments;

   ----------------------------------
   -- On_Save_Filesel_Delete_Event --
   ----------------------------------

   procedure On_Save_Filesel_Delete_Event
     (Object : access Gtk_File_Selection_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      Hide (Get_Toplevel (Object));
   end On_Save_Filesel_Delete_Event;

   ---------------------------------------
   -- On_Save_Filesel_Ok_Button_Clicked --
   ---------------------------------------

   procedure On_Save_Filesel_Ok_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Hide (Save_File_Selection);
      Free (Current_Filename);
      Current_Filename := new String' (Get_Filename (Save_File_Selection));
      Real_Open_File (Current_Filename.all);
   end On_Save_Filesel_Ok_Button_Clicked;

   -------------------------------------------
   -- On_Save_Filesel_Cancel_Button_Clicked --
   -------------------------------------------

   procedure On_Save_Filesel_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Hide (Get_Toplevel (Gtk_Widget (Object)));
   end On_Save_Filesel_Cancel_Button_Clicked;

end Save_File_Selection_Pkg.Callbacks;
