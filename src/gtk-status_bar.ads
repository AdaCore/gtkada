with Gtk.Box;
with Interfaces.C.Strings;
with Glib.GSlist;

package Gtk.Status_Bar is

   type Gtk_Status_Bar is new Gtk.Box.Gtk_Box with private;
   type Context_Id is new Guint;
   type Message_Id is new Guint;

   type Status_Bar_Msg is
      record
         Text    : Interfaces.C.Strings.chars_ptr;
         Context : Context_Id;
         Message : Message_Id;
      end record;
   function Convert (Msg : Status_Bar_Msg) return System.Address;
   function Convert (Msg : System.Address) return Status_Bar_Msg;
   package Messages_List is new Glib.GSlist.Generic_SList (Status_Bar_Msg);

   procedure Gtk_New (Widget : out Gtk_Status_Bar);
   --  mapping: Gtk_New gtkstatusbar.h gtk_statusbar_new

   function Get_Context_Id (Statusbar           : in Gtk_Status_Bar'Class;
                            Context_Description : in String)
                            return Context_Id;
   --  mapping: Statusbar_Get_Context_Id gtkstatusbar.h \
   --  mapping: gtk_statusbar_get_context_id

   function Get_Messages (Statusbar : in Gtk_Status_Bar'Class)
                          return Messages_List.GSlist;

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

   type Gtk_Status_Bar is new Gtk.Box.Gtk_Box with null record;

end Gtk.Status_Bar;
