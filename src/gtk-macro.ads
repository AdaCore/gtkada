with Glib;
with Gdk.Event;
with Gtk.Widget;
with Gdk.Types;
with Ada.Text_IO;

package Gtk.Macro is

private

   type Macro_Item;
   type Macro_Item_Access is access all Macro_Item'Class;

   --  These types have to be in the specs so that the operations below
   --  are dispatching


   type Macro_Item (Widget_Name_Length : Natural) is abstract tagged
      record
         Widget_Name   : String (1 .. Widget_Name_Length);
         Event_Type    : Types.Gdk_Event_Type;
         Next          : Macro_Item_Access;
         Widget_Depth  : Natural := 0;
         X             : Gint;
         Y             : Gint;
      end record;
   --  WIDGET_DEPTH is 0 if the event was actually sent to the widget
   --  whose name is WIDGET_NAME, and not to one of its children.
   --  (X, Y) are used to find the child in case WIDGET_DEPTH is not null.
   --  This might not be relevant for some events, in which case
   --  WIDGET_DEPTH should absolutely be 0.
   --  The search for the actual widget is done in WIDGET_NAME and its children
   --  no deeper than WIDGET_DEPTH, and while (X, Y) is in a child.

   function Create_Event (Item : Macro_Item;
                          Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
                         return Gdk.Event.Gdk_Event;
   --  Creates the matching event. The event is considered to have been
   --  send to Widget.

   procedure Save_To_Disk (File : Ada.Text_IO.File_Type;
                           Item : Macro_Item);
   --  Saves the item to the disk

   procedure Load_From_Disk (File : Ada.Text_IO.File_Type;
                             Item : out Macro_Item);
   --  Load an item from the disk


   type Macro_Item_Mouse_Press is new Macro_Item with
      record
         Button : Guint;
         State  : Gdk.Types.Gdk_Modifier_Type;
      end record;
   type Macro_Item_Mouse_Press_Access is
     access all Macro_Item_Mouse_Press'Class;
   function Create_Event (Item : Macro_Item_Mouse_Press;
                          Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
                         return Gdk.Event.Gdk_Event;
   procedure Save_To_Disk (File : Ada.Text_IO.File_Type;
                           Item : Macro_Item_Mouse_Press);
   procedure Load_From_Disk (File : Ada.Text_IO.File_Type;
                             Item : out Macro_Item_Mouse_Press);


   type Macro_Item_Crossing is new Macro_Item with
      record
         Mode   : Gdk.Types.Gdk_Crossing_Mode;
      end record;
   type Macro_Item_Crossing_Access is
     access all Macro_Item_Crossing'Class;
   function Create_Event (Item : Macro_Item_Crossing;
                          Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
                         return Gdk.Event.Gdk_Event;
   procedure Save_To_Disk (File : Ada.Text_IO.File_Type;
                           Item : Macro_Item_Crossing);
   procedure Load_From_Disk (File : Ada.Text_IO.File_Type;
                             Item : out Macro_Item_Crossing);

end Gtk.Macro;
