with Gtk.Hbox;

package Gtk.Status_Bar is

   type Gtk_Status_Bar is new Gtk.Hbox.Gtk_Hbox with private;
   type Context_Id is new Guint;
   type Message_Id is new Guint;

   procedure Gtk_New (Widget : out Gtk_Status_Bar);
   --  mapping: Gtk_New gtkstatusbar.h gtk_statusbar_new

   function Get_Context_Id (Statusbar           : in Gtk_Status_Bar'Class;
                            Context_Description : in String)
                            return Context_Id;
   --  mapping: Statusbar_Get_Context_Id gtkstatusbar.h \
   --  mapping: gtk_statusbar_get_context_id

   function Push
     (Statusbar : in Gtk_Status_Bar'Class;
      Context   : in Context_Id;
      Text      : in String)
      return Message_Id;
   --  mapping: Push gtkstatusbar.h gtk_statusbar_push

   procedure Pop
     (Statusbar : in Gtk_Status_Bar'Class;
      Context   : in Context_Id);
   --  mapping: Pop gtkstatusbar.h gtk_statusbar_pop

   procedure Remove (Statusbar  : in Gtk_Status_Bar'Class;
                     Context    : in Context_Id;
                     Message    : in Message_Id);
   --  mapping: Remove gtkstatusbar.h gtk_statusbar_remove


   --  mapping: NOT_IMPLEMENTED gtkstatusbar.h gtk_statusbar_get_type

private

   type Gtk_Status_Bar is new Gtk.Hbox.Gtk_Hbox with null record;

end Gtk.Status_Bar;
