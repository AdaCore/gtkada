with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Open_File_Selection_Pkg.Callbacks is

   use Gtk.Arguments;

   ----------------------------------
   -- On_Open_Filesel_Delete_Event --
   ----------------------------------

   procedure On_Open_Filesel_Delete_Event
     (Object : access Gtk_File_Selection_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      null;
   end On_Open_Filesel_Delete_Event;

   ---------------------------------------
   -- On_Open_Filesel_Ok_Button_Clicked --
   ---------------------------------------

   procedure On_Open_Filesel_Ok_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Open_Filesel_Ok_Button_Clicked;

   -------------------------------------------
   -- On_Open_Filesel_Cancel_Button_Clicked --
   -------------------------------------------

   procedure On_Open_Filesel_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Open_Filesel_Cancel_Button_Clicked;

end Open_File_Selection_Pkg.Callbacks;
