
with Gtk.Editable;

package Gtk.Gtk_Entry is

   type Gtk_Entry is new Gtk.Editable.Gtk_Editable with private;

   procedure Append_Text
     (The_Entry : in Gtk_Entry'Class;
      Text      : in String);
   function Get_Text (The_Entry : in Gtk_Entry'Class)
                      return      String;
   procedure Gtk_New (Widget : out Gtk_Entry;
                      Max    : in Guint16);
   procedure Gtk_New (Widget : out Gtk_Entry);
   procedure Prepend_Text
      (The_Entry : in Gtk_Entry'Class;
       Text      : in String);
   procedure Select_Region
      (The_Entry : in Gtk_Entry'Class;
       Start     : in Gint;
       The_End   : in Gint);
   procedure Set_Editable
      (The_Entry : in Gtk_Entry'Class;
       Editable  : in Boolean);
   procedure Set_Max_Length
      (The_Entry : in Gtk_Entry'Class;
       Max       : in Guint16);
   procedure Set_Position
      (The_Entry : in Gtk_Entry'Class;
       Position  : in Gint);
   procedure Set_Text
      (The_Entry : in Gtk_Entry'Class;
       Text      : in String);
   procedure Set_Visibility
      (The_Entry : in Gtk_Entry'Class;
       Visible   : in Boolean);

private
   type Gtk_Entry is new Gtk.Editable.Gtk_Editable with null record;

   --  mapping: Append_Text gtkentry.h gtk_entry_append_text
   --  mapping: Get_Text gtkentry.h gtk_entry_get_text
   --  mapping: NOT_IMPLEMENTED gtkentry.h gtk_entry_get_type
   --  mapping: Gtk_New gtkentry.h gtk_entry_new_with_max_length
   --  mapping: Gtk_New gtkentry.h gtk_entry_new
   --  mapping: Prepend_Text gtkentry.h gtk_entry_prepend_text
   --  mapping: Select_Region gtkentry.h gtk_entry_select_region
   --  mapping: Set_Editable gtkentry.h gtk_entry_set_editable
   --  mapping: Set_Max_Length gtkentry.h gtk_entry_set_max_length
   --  mapping: Set_Position gtkentry.h gtk_entry_set_position
   --  mapping: Set_Text gtkentry.h gtk_entry_set_text
   --  mapping: Set_Visibility gtkentry.h gtk_entry_set_visibility
end Gtk.Gtk_Entry;
