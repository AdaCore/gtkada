with Gtk.Bin;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Window is

   type Gtk_Window is new Bin.Gtk_Bin with private;

   procedure Gtk_New (Window   : out Gtk_Window;
                      The_Type : in  Gtk_Window_Type);
   --  mapping: New gtkwindow.h gtk_window_new

   procedure Set_Title (Window : in out Gtk_Window;
                        Title  : in String);
   --  mapping: Set_Title gtkwindow.h gtk_window_set_title

   procedure Set_Focus (Window : in out Gtk_Window'Class);
   --  mapping: Set_Focus gtkwindow.h gtk_window_set_focus

   procedure Set_Default (Window   : in out Gtk_Window'Class;
                          Defaultw : in     Widget.Gtk_Widget'Class);
   --  mapping: Set_Default gtkwindow.h gtk_window_set_default

   procedure Set_Policy (Window       : in out Gtk_Window'Class;
                         Allow_Shrink : in     Boolean;
                         Allow_Grow   : in     Boolean;
                         Auto_Shrink  : in     Boolean);
   --  mapping: Set_Policy gtkwindow.h gtk_window_set_policy

   procedure Position (Window   : in out Gtk_Window'Class;
                       Position : in     Enums.Gtk_Window_Position);
   --  mapping: Position gtkwindow.h gtk_window_position

   function Activate_Focus (Window : in Gtk_Window'Class) return Boolean;
   --  mapping: Activate_Focus gtkwindow.h gtk_window_activate_focus

   function Activate_Default (Window : in Gtk_Window'Class) return Boolean;
   --  mapping: NOT_IMPLEMENTED gtkwindow.h gtk_window_activate_default

private

   type Gtk_Window is new Bin.Gtk_Bin with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkwindow.h gtk_window_get_type
   --  mapping: NOT_IMPLEMENTED gtkwindow.h gtk_window_set_wmclass

   --  FIXME  --  need Gtk_Accelerator_Table
   --  mapping: NOT_IMPLEMENTED gtkwindow.h gtk_window_add_accelerator_table
   --  mapping: NOT_IMPLEMENTED gtkwindow.h gtk_window_remove_accelerator_table

end Gtk.Window;
