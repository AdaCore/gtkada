
with Gtk.Bin;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Frame is

   type Gtk_Frame is new Gtk.Bin.Gtk_Bin with private;

   procedure Gtk_New (Widget : out Gtk_Frame;
                      Label  : in String := "");
   procedure Set_Label
      (Frame : in Gtk_Frame'Class;
       Label : in String);
   procedure Set_Label_Align
      (Frame  : in Gtk_Frame'Class;
       Xalign : in Gfloat;
       Yalign : in Gfloat);
   procedure Set_Shadow_Type
      (Frame    : in Gtk_Frame'Class;
       The_Type : in Gtk_Shadow_Type);

private
   type Gtk_Frame is new Gtk.Bin.Gtk_Bin with null record;

   --  mapping: NOT_IMPLEMENTED gtkframe.h gtk_frame_get_type
   --  mapping: Gtk_New gtkframe.h gtk_frame_new
   --  mapping: Set_Label gtkframe.h gtk_frame_set_label
   --  mapping: Set_Label_Align gtkframe.h gtk_frame_set_label_align
   --  mapping: Set_Shadow_Type gtkframe.h gtk_frame_set_shadow_type
end Gtk.Frame;
