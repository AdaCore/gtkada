
with Gtk.Widget;

package Gtk.Editable is

   type Gtk_Editable is new Gtk.Widget.Gtk_Widget with private;

   procedure Changed (Editable : in Gtk_Editable'Class);
   procedure Claim_Selection
      (Editable : in Gtk_Editable'Class;
       Claim    : in Boolean;
       Time     : in Guint32);
   procedure Copy_Clipboard
      (Editable : in Gtk_Editable'Class;
       Time     : in Guint32);
   procedure Cut_Clipboard
      (Editable : in Gtk_Editable'Class;
       Time     : in Guint32);
   procedure Delete_Selection (Editable : in Gtk_Editable'Class);
   procedure Delete_Text
      (Editable  : in Gtk_Editable'Class;
       Start_Pos : in Gint;
       End_Pos   : in Gint);
   function Get_Chars
      (Editable  : in Gtk_Editable'Class;
       Start_Pos : in Gint;
       End_Pos   : in Gint)
       return         String;
   function Get_Clipboard_Text (Widget : in Gtk_Editable'Class)
                                return      String;
   function Get_Current_Pos (Widget : in Gtk_Editable'Class)
                             return      Guint;
   function Get_Editable (Widget : in Gtk_Editable'Class)
                          return      Boolean;
   function Get_Has_Selection (Widget : in Gtk_Editable'Class)
                               return      Boolean;
   function Get_Selection_End_Pos (Widget : in Gtk_Editable'Class)
                                   return      Guint;
   function Get_Selection_Start_Pos (Widget : in Gtk_Editable'Class)
                                     return      Guint;
   procedure Insert_Text
      (Editable        : in Gtk_Editable'Class;
       New_Text        : in String;
       New_Text_Length : in Gint;
       Position        : in out Gint);
   procedure Paste_Clipboard
      (Editable : in Gtk_Editable'Class;
       Time     : in Guint32);
   procedure Select_Region
      (Editable : in Gtk_Editable'Class;
       Start    : in Gint;
       The_End  : in Gint);

private
   type Gtk_Editable is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: Changed gtkeditable.h gtk_editable_changed
   --  mapping: Claim_Selection gtkeditable.h gtk_editable_claim_selection
   --  mapping: Copy_Clipboard gtkeditable.h gtk_editable_copy_clipboard
   --  mapping: Cut_Clipboard gtkeditable.h gtk_editable_cut_clipboard
   --  mapping: Delete_Selection gtkeditable.h gtk_editable_delete_selection
   --  mapping: Delete_Text gtkeditable.h gtk_editable_delete_text
   --  mapping: Get_Chars gtkeditable.h gtk_editable_get_chars
   --  mapping: Get_Clipboard_Text gtkeditable.h GtkEditable->clipboard_text
   --  mapping: Get_Current_Pos gtkeditable.h GtkEditable->current_pos
   --  mapping: Get_Editable gtkeditable.h GtkEditable->editable
   --  mapping: Get_Has_Selection gtkeditable.h GtkEditable->has_selection
   --  mapping: Get_Selection_End_Pos gtkeditable.h \
   --  mapping:    GtkEditable->selection_end_pos
   --  mapping: Get_Selection_Start_Pos gtkeditable.h \
   --  mapping:    GtkEditable->selection_start_pos
   --  mapping: NOT_IMPLEMENTED gtkeditable.h gtk_editable_get_type
   --  mapping: Insert_Text gtkeditable.h gtk_editable_insert_text
   --  mapping: Paste_Clipboard gtkeditable.h gtk_editable_paste_clipboard
   --  mapping: Select_Region gtkeditable.h gtk_editable_select_region
end Gtk.Editable;
