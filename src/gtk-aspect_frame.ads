
with Gtk.Frame;

package Gtk.Aspect_Frame is

   type Gtk_Aspect_Frame is new Gtk.Frame.Gtk_Frame with private;

   function Get_Ratio (Widget : in Gtk_Aspect_Frame'Class)
                       return      Gfloat;
   function Get_Xalign (Widget : in Gtk_Aspect_Frame'Class)
                        return      Gfloat;
   function Get_Yalign (Widget : in Gtk_Aspect_Frame'Class)
                        return      Gfloat;
   procedure Gtk_New
      (Widget     : out Gtk_Aspect_Frame;
       Label      : in String;
       Xalign     : in Gfloat;
       Yalign     : in Gfloat;
       Ratio      : in Gfloat;
       Obey_Child : in Gint);
   procedure Set
      (Aspect_Frame : in Gtk_Aspect_Frame'Class;
       Xalign       : in Gfloat;
       Yalign       : in Gfloat;
       Ratio        : in Gfloat;
       Obey_Child   : in Gint);

private
   type Gtk_Aspect_Frame is new Gtk.Frame.Gtk_Frame with null record;

   --  mapping: Get_Ratio gtkaspectframe.h GtkAspectFrame->ratio
   --  mapping: NOT_IMPLEMENTED gtkaspectframe.h gtk_aspect_frame_get_type
   --  mapping: Get_Xalign gtkaspectframe.h GtkAspectFrame->xalign
   --  mapping: Get_Yalign gtkaspectframe.h GtkAspectFrame->yalign
   --  mapping: Gtk_New gtkaspectframe.h gtk_aspect_frame_new
   --  mapping: Set gtkaspectframe.h gtk_aspect_frame_set
end Gtk.Aspect_Frame;
